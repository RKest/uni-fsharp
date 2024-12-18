open Falco
open Falco.Routing
open Falco.HostBuilder

open System.Threading.Tasks
open System.IO
open System.Diagnostics
open System.Text
open System

let syncTask<'a> : Task<'a> -> 'a = Async.AwaitTask >> Async.RunSynchronously

let plainRes code msg =
    Response.withStatusCode code >> Response.ofPlainText msg


let tryGetString id (f: FormCollectionReader) = f.TryGetString id
let tryGetRouteInt id (r: RouteCollectionReader) = r.TryGetInt id
let tryGetFormInt id (f: FormCollectionReader) = f.TryGetInt id

// Blackbird
let (>>>) f g x y = g (f x y)
// Starling
let (<*>) f g x = f x (g x)
let warbler f x = f x x
let phoenix f g h x = f (g x) (h x)
let kestrel x _ = x
let pheasant f g h x y = f (g x y) (h x y)
let ki _ y = y

module Db =
    open Dapper.FSharp.SQLite
    open Microsoft.Data.Sqlite
    open FSharp.Data.Dapper

    let initSQL =
        """
    CREATE TABLE IF NOT EXISTS notes (
        Id INTEGER PRIMARY KEY AUTOINCREMENT,
        Title TEXT NOT NULL UNIQUE,
        Html TEXT NOT NULL
    );

    CREATE TABLE IF NOT EXISTS tags (
        Id INTEGER PRIMARY KEY AUTOINCREMENT,
        Name TEXT NOT NULL UNIQUE
    );

    CREATE TABLE IF NOT EXISTS note_tags (
        NoteId INTEGER NOT NULL,
        TagId INTEGER NOT NULL,
        FOREIGN KEY (NoteId) REFERENCES notes(Id),
        FOREIGN KEY (TagId) REFERENCES tags(Id),
        PRIMARY KEY (NoteId, TagId)
    );
    """

    OptionTypes.register ()

    let connFn () =
        let conn = new SqliteConnection("Data Source=private/sqlite.db")
        conn.Open()
        conn

    let querySeq<'R> = querySeqAsync<'R> (fun () -> SqliteConnection(connFn ()))
    querySeq { script initSQL } |> Async.RunSynchronously |> ignore

    type Note =
        { Id: int; Title: string; Html: string }

    type Tag = { Id: int; Name: string }
    type NoteTag = { NoteId: int; TagId: int }

    type NotePreviewInternal =
        { NoteId: int
          NoteTitle: string
          TagId: int option
          TagName: string option }

    type NotePreview =
        { Id: int
          Title: string
          Tags: Tag array }

    type NoteUpdate = { Id: int; Html: string }

    let noteTable = table'<Note> "notes"
    let tagTable = table'<Tag> "tags"
    let noteTagTable = table'<NoteTag> "note_tags"

    let tags (conn: SqliteConnection) =
        select {
            for _ in tagTable do
                selectAll
        }
        |> conn.SelectAsync<Tag>
        |> syncTask

    let noteTags (conn: SqliteConnection) (noteId: int) =
        querySeqAsync<Tag> (fun () -> SqliteConnection conn) {
            script
                $"""
                    SELECT tags.Id, tags.Name FROM tags
                    JOIN note_tags ON note_tags.TagId = tags.Id
                    JOIN notes ON notes.Id = note_tags.NoteId
                    WHERE notes.Id = {noteId}
                """
        }
        |> Async.RunSynchronously
        |> Seq.toArray

    let notePreivewCombine notes =
        let combineOpt =
            function
            | Some id, Some name -> Some { Id = id; Name = name }
            | _ -> None

        let note = Seq.head notes

        let tags =
            notes
            |> Seq.map (fun p -> p.TagId, p.TagName)
            |> Seq.toArray
            |> Array.choose combineOpt

        { Id = note.NoteId
          Title = note.NoteTitle
          Tags = tags }

    let notePreviewById (conn: SqliteConnection) (id: int) =
        querySeqAsync<NotePreviewInternal> (fun () -> SqliteConnection conn) {
            script
                $"""
                    SELECT notes.Id as NoteId, notes.Title as NoteTitle, tags.Id as TagId, tags.Name as TagName FROM notes
                    LEFT JOIN note_tags ON notes.Id = note_tags.NoteId
                    LEFT JOIN tags ON note_tags.TagId = tags.Id
                    WHERE notes.Id = {id}
                """
        }
        |> Async.RunSynchronously
        |> notePreivewCombine

    let notePreivewsForTag (conn: SqliteConnection) (tagId: int) =
        querySeqAsync<NotePreviewInternal> (fun () -> SqliteConnection conn) {
            script
                $"""
                    SELECT notes.Id as NoteId, notes.Title as NoteTitle, NULL as TagId, NULL as TagName FROM notes
                    JOIN note_tags ON notes.Id = note_tags.NoteId
                    WHERE note_tags.TagId = {tagId}
                """
        }
        |> Async.RunSynchronously
        |> Seq.groupBy _.NoteId
        |> Seq.map (snd >> notePreivewCombine)

    let notePreviews (conn: SqliteConnection) =
        printfn "LOOKING FOR NOTE PREVIEWS"


        querySeqAsync<NotePreviewInternal> (fun () -> SqliteConnection conn) {
            script
                """
                    SELECT notes.Id as NoteId, notes.Title as NoteTitle, tags.Id as TagId, tags.Name as TagName FROM notes
                    LEFT JOIN note_tags ON notes.Id = note_tags.NoteId
                    LEFT JOIN tags ON note_tags.TagId = tags.Id
                """
        }
        |> Async.RunSynchronously
        |> Seq.groupBy _.NoteId
        |> Seq.map (snd >> notePreivewCombine)

    let newNote title =
        use conn = connFn ()

        querySingleAsync<int> (fun () -> SqliteConnection conn) {
            script
                $"""
                    INSERT INTO notes (Title, Html) VALUES ('{title}', '');
                    SELECT last_insert_rowid()
                """
        }
        |> Async.RunSynchronously
        |> notePreviewById conn

    let newTag conn tagName =
        querySingleAsync<int> (fun () -> SqliteConnection conn) {
            script
                $"""
                    INSERT INTO tags (Name) VALUES ('{tagName}');
                    SELECT last_insert_rowid()
                """
        }
        |> Async.RunSynchronously
        |> ignore

    let updateNoteHtml id html =
        update {
            for n in noteTable do
                where (n.Id = id)
                setColumn n.Html html
        }
        |> (connFn ()).UpdateAsync
        |> syncTask
        |> ignore

    let assignTag noteId tagId =
        insert {
            into noteTagTable
            value { NoteId = noteId; TagId = tagId }
        }
        |> (connFn ()).InsertAsync
        |> syncTask
        |> ignore

    let unassignTag noteId tagId =
        delete {
            for nt in noteTagTable do
                where (nt.NoteId = noteId && nt.TagId = tagId)
        }
        |> (connFn ()).DeleteAsync
        |> syncTask
        |> ignore

    let getNoteById id =
        select {
            for n in noteTable do
                where (n.Id = id)
        }
        |> (connFn ()).SelectAsync<Note>
        |> syncTask
        |> Seq.tryHead

module View =
    open Falco.Markup
    open Falco.Htmx

    let htmxSrc = "https://unpkg.com/htmx.org@2.0.4"
    let jsonEncSrc = "https://unpkg.com/htmx.org@1.9.12/dist/ext/json-enc.js"
    let notifBarId = "notif-bar"
    let notePreviewHomeId = "note-preview-home"
    let tagEditDropdownId = "tags-edit-dropdown"

    let javascriptSrc =
        """
        const selectImage = async () => {
            let sel = window.getSelection();
            let range = sel.getRangeAt(0);
            let clone = range.cloneContents();
            if (!clone.firstElementChild) {
                alert('Select an image first');
                return;
            }
            let body = clone.firstElementChild.getAttribute('src');
            if (!body) {
                alert('Select an image first');
                return;
            }
            let method = 'POST';
            let text = await fetch("/ocr", { body, method }).then((res) => res.text());
            range.insertNode(document.createTextNode(text));
        };

        htmx.defineExtension('path-params', {
            onEvent: function(name, evt) {
                if (name === "htmx:configRequest") {
                    evt.detail.path = evt.detail.path.replace(/{{([A-Za-z0-9_]+)}}/g, function (_, param) {
                        let val = evt.detail.parameters[param];
                        delete evt.detail.parameters[param]; // don't pass in query string since already in path
                        return val;
                  })
                }
            }
        });
        """

    let notifBar msg =
        Elem.div [ Attr.height "2rem"; Attr.id notifBarId ] [ Text.raw msg ]

    let template content =
        Elem.html
            []
            [ Elem.head
                  []
                  [ Elem.title [] [ Text.raw "Note" ]
                    Elem.script [ Attr.src htmxSrc ] []
                    Elem.script [ Attr.src jsonEncSrc ] []
                    Elem.script [] [ Text.raw javascriptSrc ] ]
              Elem.body [] [ content; notifBar "" ] ]


    let note (note: Db.Note) =
        let contentId = "note-content"

        let view =
            Elem.main
                [ Attr.style "margin-left: 20%; margin-right: 20%;" ]
                [ Elem.h1
                      [ Attr.style "height: 2rem; display: flex; justify-content: space-between; align-items: center;" ]
                      [ Elem.span [] [ Text.raw note.Title ]
                        Elem.button [ Attr.onclick "selectImage()" ] [ Text.raw "OCR" ] ]
                  Elem.div
                      [ Attr.id contentId
                        Attr.style "height: calc(100vh - 4rem); padding-left: 5px;"
                        Attr.contenteditable "true"
                        Hx.post $"/sync/{note.Id}"
                        Hx.trigger [ (Hx.Trigger.event ("input delay:500ms", None, [])) ]
                        Hx.valsJs $"html:htmx.find('#{contentId}').innerHTML"
                        Hx.target (Hx.Target.css $"#{notifBarId}") ]
                      [ Text.raw note.Html ] ]

        Response.ofHtml (template view)

    let notePreview (note: Db.NotePreview) =
        Elem.div
            [ Attr.style "display: flex; justify-content: space-between; gap: 1em;" ]
            [ Elem.h2 [] [ Elem.a [ Attr.href $"/note/{note.Id}" ] [ Text.raw note.Title ] ]
              Elem.div
                  [ Attr.style "position: relative; display: inline-block;"
                    Attr.id tagEditDropdownId ]
                  [ Elem.button
                        [ Hx.get $"/tags/{note.Id}"
                          Hx.target (Hx.Target.css $"#{tagEditDropdownId} > span")
                          Hx.swap Hx.Swap.afterend ]
                        [ Text.raw "Edit tags" ]
                    Elem.span [] [] ] ]

    let tagControlTemplate (verb: XmlAttribute) (submitText: string) (noteId: int) (tagId: int) =
        Elem.form
            [ verb
              Hx.trigger [ (Hx.Trigger.event ("submit", None, [])) ]
              Hx.target (Hx.Target.closest "form") ]
            [ Elem.input [ Attr.type' "hidden"; Attr.name "noteId"; Attr.value (string noteId) ]
              Elem.input [ Attr.type' "hidden"; Attr.name "tagId"; Attr.value (string tagId) ]
              Elem.input [ Attr.name "submit"; Attr.type' "submit"; Attr.value submitText ] ]

    let tagAssign = tagControlTemplate (Hx.post "/assignTag") "Assign tag"
    let tagDelete = tagControlTemplate (Hx.post "/unassignTag") "Unassign tag"

    let tagPreview (noteId: int) (tag: Db.Tag) controllFactory =
        let id = "dialog-root"

        Elem.div
            [ Attr.id id
              Attr.style "display: flex; justify-content: space-between; gap: 1em; white-space: nowrap;" ]
            [ Text.raw tag.Name; controllFactory noteId tag.Id ]

    let tagsFloat (noteId: int) (tags: Db.Tag array) (presentTags: Db.Tag array) =
        let rootId = "float-root"
        let presentTagIds = presentTags |> Array.map _.Id |> Set.ofArray

        let tagControlls =
            tags
            |> (Array.map (fun t -> if presentTagIds.Contains t.Id then tagDelete else tagAssign))

        Elem.div
            [ Attr.id rootId; Attr.style "position: absolute;" ]
            (Elem.button
                [ Attr.onclick $"document.getElementById('{rootId}').remove()"
                  Attr.style "width: 100%; text-align: right" ]
                [ Text.raw "X" ]
             :: ((tags, tagControlls) ||> Seq.map2 (tagPreview noteId) |> List.ofSeq))

    let newNote =
        Elem.form
            [ Hx.post "/newNote"
              Hx.trigger [ (Hx.Trigger.event ("submit", None, [])) ]
              Hx.target (Hx.Target.css $"#{notePreviewHomeId}")
              Hx.swap Hx.Swap.beforeend
              Hx.ext "json-enc" ]
            [ Elem.input [ Attr.name "title"; Attr.placeholder "New note title" ]
              Elem.input [ Attr.label "Add note"; Attr.type' "submit" ] ]

    let newTag =
        Elem.form
            [ Hx.post "/newTag"
              Hx.trigger [ (Hx.Trigger.event ("submit", None, [])) ]
              Hx.target (Hx.Target.closest "main")
              Hx.swap Hx.Swap.outerHTML
              Hx.ext "json-enc" ]
            [ Elem.input [ Attr.name "name"; Attr.placeholder "New tag name" ]
              Elem.input [ Attr.type' "submit"; Attr.value "Add tag" ] ]

    let searchByTag (tags: Db.Tag seq) =
        Elem.form
            [ Attr.style "display: flex; gap: 1em"
              Hx.get "/notePreviews/{{tagId}}"
              Hx.trigger [ (Hx.Trigger.event ("submit", None, [])) ]
              Hx.target (Hx.Target.css $"#{notePreviewHomeId}")
              Hx.ext "path-params" ]
            [ Elem.select
                  [ Attr.name "tagId" ]
                  ((Elem.option [] [])
                   :: (tags
                       |> Seq.map (fun t -> Elem.option [ Attr.value (string t.Id) ] [ Text.raw t.Name ])
                       |> List.ofSeq))
              Elem.input [ Attr.type' "submit"; Attr.value "Search" ] ]


    let menuBar (tags: Db.Tag seq) =
        Elem.div [ Attr.style "display: flex; justify-content: space-between" ] [ newNote; newTag; searchByTag tags ]

    let notePreviews (notePrevs: Db.NotePreview seq) =
        Elem.div [ Attr.id notePreviewHomeId; Attr.style "flex: 1" ] (Seq.map notePreview notePrevs |> List.ofSeq)

    let home (notePrevs: Db.NotePreview seq) (tags: Db.Tag seq) =
        let view =
            Elem.main
                [ Attr.style "margin-left: 20%; margin-right: 20%; height: calc(100vh - 2rem)" ]
                [ menuBar tags; notePreviews notePrevs ]

        Response.ofHtml (template view)

module Ocr =
    type Encoding =
        | Png
        | Jpeg

    type Base64Image = { Enc: Encoding; Data: byte array }

    let encodingExt =
        function
        | Png -> "png"
        | Jpeg -> "jpg"

    let ParseBase64Image (data: string) : Result<Base64Image, string> =
        let regex = RegularExpressions.Regex("data:image/(png|jpeg|jpg);base64,(.*)")
        let m = regex.Match(data)

        match m.Success with
        | true ->
            let data = Convert.FromBase64String m.Groups.[2].Value

            match m.Groups.[1].Value with
            | "png" -> Ok { Enc = Png; Data = data }
            | "jpeg"
            | "jpg" -> Ok { Enc = Jpeg; Data = data }
            | enc -> Error("Invalid enctype. " + enc)
        | false -> Error "Invalid base64 image."

    let Read{ Enc = enc; Data = data } =
        let filename = $"./private/image.{encodingExt enc}"
        File.WriteAllBytes(filename, data)
        let psi = ProcessStartInfo("tesseract", $"-l eng {filename} stdout")
        psi.RedirectStandardOutput <- true
        use proc = Process.Start(psi)
        let out = proc.StandardOutput.ReadToEnd()
        proc.WaitForExit()
        out

module Handlers =
    let ocr =
        Request.getBodyString
        >> syncTask
        >> Ocr.ParseBase64Image
        >> Result.map (plainRes 200 << Ocr.Read)
        >> Result.defaultWith (plainRes 400)
        |> warbler

    let tags context =
        let noteId = Request.getRoute context |> (fun r -> r.TryGetInt "id")

        context
        |> (noteId
            |> Option.map (fun noteId ->
                use conn = Db.connFn ()
                let allTags = Db.tags conn |> Seq.toArray
                let noteTags = Db.noteTags conn noteId
                Response.ofHtml (View.tagsFloat noteId allTags noteTags))
            |> Option.defaultValue (plainRes 400 "Note ID missing."))

    let assignmentTagTemplate dbFun viewFun =
        let noteId = (Request.getForm >> syncTask >> tryGetFormInt "noteId")
        let tagId = (Request.getForm >> syncTask >> tryGetFormInt "tagId")
        let viewHtml = viewFun >>> Response.ofHtml
        let badReq = Option.defaultValue (plainRes 400 "Note ID or Tag ID missing.")

        (Option.map2 (pheasant ki dbFun viewHtml) >>> badReq, noteId, tagId)
        |||> phoenix
        |> warbler

    let assignTag = assignmentTagTemplate Db.assignTag View.tagDelete
    let unassignTag = assignmentTagTemplate Db.unassignTag View.tagAssign

    let notePreviews context =
        use conn = Db.connFn ()

        context
        |> warbler (
            Request.getRoute
            >> tryGetRouteInt "tagId"
            >> Option.map (Db.notePreivewsForTag conn)
            >> Option.defaultValue (Db.notePreviews conn)
            >> View.notePreviews
            >> Response.ofHtml
        )

    let newNote context =
        context
        |> Request.mapJson (fun (n: {| title: string |}) ->
            n.title |> (Db.newNote >> View.notePreview >> Response.ofHtml))

    let note context =
        context
        |> (Request.getRoute
            >> tryGetRouteInt "id"
            >> Option.map Db.getNoteById
            >> Option.map (Option.map View.note >> Option.defaultValue (plainRes 404 "Note not found"))
            >> Option.defaultValue (plainRes 400 "Id missing")
            |> warbler)


    let syncNote context =
        context
        |> (phoenix
                (Option.map2 Db.updateNoteHtml
                 >>> Option.map (
                     kestrel (DateTime.Now.ToString("HH:mm:ss"))
                     >> sprintf "Synchronized %s"
                     >> plainRes 200
                 )
                 >>> Option.defaultValue (plainRes 400 "Id or HTML missing."))
                (Request.getRoute >> tryGetRouteInt "id")
                (Request.getForm >> syncTask >> tryGetString "html")
            |> warbler)

    let home conn context =
        phoenix View.home Db.notePreviews Db.tags conn context

    let home' context =
        use conn = Db.connFn ()
        home conn context

    let newTag context =
        use conn = Db.connFn ()

        Request.mapJson
            (fun (n: {| name: string |}) ->
                Db.newTag conn n.name
                home conn)
            context


webHost [||] {
    endpoints
        [ Handlers.home' |> get "/"
          Handlers.notePreviews |> get "/notePreviews"
          Handlers.notePreviews |> get "/notePreviews/{tagId}"
          Handlers.note |> get "/note/{id}"
          Handlers.tags |> get "/tags/{id}"

          Handlers.assignTag |> post "/assignTag"
          Handlers.unassignTag |> post "/unassignTag"

          Handlers.newNote |> post "/newNote"
          Handlers.syncNote |> post "/sync/{id}"
          Handlers.ocr |> post "/ocr"
          Handlers.newTag |> post "/newTag" ]
}
