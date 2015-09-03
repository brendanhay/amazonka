# Amazon Simple Workflow Service SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

Amazon Simple Workflow Service

The Amazon Simple Workflow Service (Amazon SWF) makes it easy to build
applications that use Amazon\'s cloud to coordinate work across
distributed components. In Amazon SWF, a /task/ represents a logical
unit of work that is performed by a component of your workflow.
Coordinating tasks in a workflow involves managing intertask
dependencies, scheduling, and concurrency in accordance with the logical
flow of the application.

Amazon SWF gives you full control over implementing tasks and
coordinating them without worrying about underlying complexities such as
tracking their progress and maintaining their state.

This documentation serves as reference only. For a broader overview of
the Amazon SWF programming model, see the
<http://docs.aws.amazon.com/amazonswf/latest/developerguide/ Amazon SWF Developer Guide>.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-swf)
and the [AWS API Reference](http://docs.aws.amazon.com/amazonswf/latest/apireference/Welcome.html).

The types from this library are intended to be used with [amazonka](http://hackage.haskell.org/package/amazonka),
which provides mechanisms for specifying AuthN/AuthZ information and sending requests.

Use of lenses is required for constructing and manipulating types.
This is due to the amount of nesting of AWS types and transparency regarding
de/serialisation into more palatable Haskell values.
The provided lenses should be compatible with any of the major lens libraries
[lens](http://hackage.haskell.org/package/lens) or [lens-family-core](http://hackage.haskell.org/package/lens-family-core).

## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

> _Note:_ this library is an auto-generated Haskell package. Please see `amazonka-gen` for more information.


## Licence

`amazonka-swf` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
