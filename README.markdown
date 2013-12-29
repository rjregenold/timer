# Timer

A project-based timer for tracking those billable hours.

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
