# dired-du-duc

Make [dired-du](https://elpa.gnu.org/packages/dired-du.html) faster, via the Linux utility [duc](https://github.com/zevv/duc).

Enable with `(global-dired-du-duc-mode)`.

You'll also want to **remove any call** to `dired-du-mode` in your initfiles.  While we rely on Dired-Du for the heavy lifting, the actual mode is confused about whether it is a global or local mode, so it had to be replaced.

The global mode does three things:

1. Asynchronously run "duc index" each time a Dired buffer is opened or refreshed.  Option `dired-du-duc-index-predicate` can be configured if you don't want this.

2. Turn on `dired-du-duc-mode` in relevant buffers when duc is ready.  Option `dired-du-duc-mode-predicate` can be configured to enable it always, if you are fine with the slow "du" as a fallback.

3. Regularly re-index the directories we have previously indexed.  Option `dired-du-duc-delay` controls how often to do this.

## Buffer-local

You can turn on `dired-du-duc-mode` buffer-locally.

In this case, it will never run "duc index" for you.  It's up to you to ensure that.

Where there is no usable index, it falls back on "du".

When falling back on "du", it is identical to upstream `dired-du-mode` except that it does not support ls-lisp.

If you want to emulate the way that the global mode reverts the Dired buffer after duc is done indexing that directory, use the provided function `dired-du-duc-index`.

## Find-Dired

Support for Find-Dired requires the global mode to be enabled.  See also variable `dired-du-on-find-dired-ok`.

## Bonus: Hooks

One of these hooks may be useful for running `updatedb` and other things of that nature.

- `dired-du-duc-before-index-functions`
- `dired-du-duc-after-re-index-hook`

## Privacy note

If file names are sensitive data for you, note that the index at `~/.cache/duc/duc.db` contains all file names ever indexed.
