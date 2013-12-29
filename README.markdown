# Timer

A project-based timer for tracking those billable hours.

## Installation

Until I get this up on [hackage](http://hackage.haskell.org/), it will need to
be installed from source.

### Dependencies

* [Haskell Platform](http://www.haskell.org/platform/) 2013.2.0.0 or greater
* [Cabal](http://www.haskell.org/cabal/) 1.18 or greater recommended (yay,
  sandboxing)

### Steps

It is probably best to take advantage of Cabal 1.18's sandboxing. Here is how
to do that.

```
$ git clone https://github.com/rjregenold/timer.git
$ cd timer
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure
$ cabal install
```

If all goes well, you will have the timer executable in
`./.cabal-sandbox/bin/timer`. You can make this globally available by either
adding that directory to your path (a bit silly) or by creating a `~/bin`
directory in your home directory, adding that to your path, then creating
a symlink inside there that points to the sandboxed executable.

```
$ mkdir ~/bin
$ cd ~/bin
$ ln -s ~/[path/to/timer/source]/.cabal-sandbox/bin/timer timer
```

Then make sure to edit your `~/.profile`, `~/.bashrc`, or whatever other shell
config file you need to in order to add `~/bin` to your `$PATH` environment
variable.

## Usage

The timer is project-based. That means most commands take a `NAME` which
should be the name of the project. For example, if you're working on a project
for `FooBar, Inc` your workflow might look something like this:

```
$ timer start foobar
# [do lots of work]
$ timer stop foobar
$ timer list foobar
Duration    Start                   End
01:34:15    12/29/13 08:00:00 AM    12/29/13 09:34:15 AM
```

### Start a timer

Use this when you start working.

```
$ timer start NAME
```

### Stop a timer

Use this when you finish working.

```
$ timer stop NAME
```

### Cancel a timer

Use this if you accidentally started a timer, or if you do not wish to record the
current timer entry.

```
$ timer cancel NAME
```

### List active timers

Use this to see a list of active timers, their duration, and when they were
started.

```
$ timer active
```

### List timer entries

Use this to see a list of entries for the given timer.

```
$ timer list NAME
```
