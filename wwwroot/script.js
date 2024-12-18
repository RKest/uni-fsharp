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
