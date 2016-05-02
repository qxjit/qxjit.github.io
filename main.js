function init() {
  $('html').addClass('hasjs');
  $('html').on('click', hideAll);
  $('a').on('click', stop);
  $('.section h2').on('click', toggleSection);
}

function hideAll() {
  $('.section').removeClass('current')
}

function stop(evt) {
  evt.stopPropagation();
}

function toggleSection(evt) {
  var link = $(evt.target).ancestorOrSelf
  var section = $(this).parent('.section');
  var wasOpen = section.hasClass('current');

  hideAll();

  if (!wasOpen) {
    section.addClass('current');
  }

  evt.stopPropagation();
}

$(window).on('load', init);
