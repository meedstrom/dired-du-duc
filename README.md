# dired-du-duc

Add indexing to [dired-du](https://elpa.gnu.org/packages/dired-du.html), via the Linux utility [duc](https://github.com/zevv/duc).

Enable with `(global-dired-du-duc-mode)`.
You'll also want to remove any call to `dired-du-mode` in your initfiles.

The global mode does three things:

1. Start regularly indexing a configurable set of directories.

2. Index every directory that the user actually visits in Dired.  This happens
   asynchronously, and when that index is done, revert the buffer so it shows
   correct sizes.

   (This is also repeated any time the user reverts the buffer for any reason.)
   
3. Manage turning `dired-du-mode` on or off in relevant buffers,
   in case you prefer to fall back on nothing when the index isn't ready,
   rather than fall back on the slow "du".

To configure this, type:

    M-x customize-group RET dired-du-duc RET
