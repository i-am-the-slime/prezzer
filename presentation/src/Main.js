'use strict'

exports.onSlideChanged = function (listener) {
    return function() {
         Reveal.addEventListener('slidechanged', function(event) {
            listener(event)();
        });
    }
}
