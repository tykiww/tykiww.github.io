var jasmine = [74, 65, 83, 77, 73, 78, 69].join();

$(window).on('keypress', function(e) {
    var $this = $(this),
        gap = e.timeStamp - ($this.data('time') || e.timeStamp),
        chars = $this.data('chars') || [];

    if (gap > 1000) {
        chars = [];
    }

    chars.push(e.which);
    if (chars.join() === jasmine) {
        alert('I love you');
    }

    $this.data('chars', chars);
    $this.data('time', e.timeStamp);
});
