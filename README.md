

# Introduction

**helm-browser** is an Emacs package that is a helm interface to many popular web browser windows and tabs. **helm-browser** can be used to quickly find and switch between multiple browser windows and tabs.

Presently, **helm-browser** can be used to:

-   find and switch to selected browser windows/tabs
-   select and close one or multiple tabs
-   copy a tab's URL (to yank into a document)

This package is based on [helm-taskswitch](https://github.com/bdc34/helm-taskswitch)  (which is available via MELPA). Which is great except it does not deal with tabs. I have a hundreds of tabs open across half a dozen browser windows and needed some way to locate tabs.


## Dependencies

-   [helm](https://github.com/emacs-helm/helm)
-   [brotab](https://github.com/balta2ar/brotab)
-   [wmctrl](https://www.freedesktop.org/wiki/Software/wmctrl/)

This package will only work on Linux and only these web browsers: Firefox, Chromium, Chrome, Brave Browser. It has been most extensively tested on Firefox. 


# Installation

-   Make sure **helm** is installed, configured and working (please see helm documentation for installation & configuration). This is an excellent guide: [A Package in a league of its own: **Helm**](https://tuhdo.github.io/helm-intro.html).
-   Install **wmctrl** (use your distribution's package manager)
-   Install **brotab** 
    1.  Install/upgrade **pipx**: `pip install --user -U pipx`
    2.  Install **brotab**: `pipx install brotab`
        -   By default, **brotab** will be installed in: ~/.local/bin/.
        -   Optionally, you can add ~/.local/bin/ to the PATH.
    3.  Confirm that **brotab** is installed and working: `bt` or if not in PATH: `~/.local/bin/bt`
    4.  Install `brotab` native app manifests: `bt install`
    5.  Install one or more browser extensions:
        -   Firefox: <https://addons.mozilla.org/en-US/firefox/addon/brotab/>
        -   Chrome/Chromium/Brave: <https://chrome.google.com/webstore/detail/brotab/mhpeahbikehnfkfnmopaigggliclhmnc/>
    6.  Restart browser(s)


# Future Features (Maybe!)

-   Publish in MELPA
-   Bookmark one or multiple selected tabs
-   Search for text in one or multiple selected tabs
-   I <span class="underline">may</span> look into making this work on Windows & Mac


# Feedback

This is my first Emacs Lisp program (started learning Lisp just a few months ago), so I am sure there are things that may be considered sloppy Lisp :-) Please feel free to file bug reports, suggestions and feature requests.

