
// a key map of allowed keys
var allowedKeys = {
  74: 'j',
  65: 'a',
  83: 's',
  77: 'm',
  73: 'i',
  78: 'n',
  69: 'e'
};

// the 'official' Konami Code sequence
var konamiCode = ['j', 'a', 's', 'm', 'i', 'n', 'e'];

// a variable to remember the 'position' the user has reached so far.
var konamiCodePosition = 0;

// add keydown event listener
document.addEventListener('keydown', function(e) {
  // get the value of the key code from the key map
  var key = allowedKeys[e.keyCode];
  // get the value of the required key from the konami code
  var requiredKey = konamiCode[konamiCodePosition];

  // compare the key with the required key
  if (key == requiredKey) {

    // move to the next key in the konami code sequence
    konamiCodePosition++;

    // if the last key is reached, activate cheats
    if (konamiCodePosition == konamiCode.length) {
      activateCheats();
      konamiCodePosition = 0;
    }
  } else {
    konamiCodePosition = 0;
  }
});

function activateCheats() {
  document.body.style.backgroundImage = "url('images/cheatBackground.png')";

  var audio = new Audio('audio/a.mp3');
  audio.play();

  alert("I love you baby!");
}
