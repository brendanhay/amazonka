# Amazon Data Pipeline SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

AWS Data Pipeline configures and manages a data-driven workflow called a
pipeline. AWS Data Pipeline handles the details of scheduling and
ensuring that data dependencies are met so that your application can
focus on processing the data.

AWS Data Pipeline provides a JAR implementation of a task runner called
AWS Data Pipeline Task Runner. AWS Data Pipeline Task Runner provides
logic for common data management scenarios, such as performing database
queries and running data analysis using Amazon Elastic MapReduce (Amazon
EMR). You can use AWS Data Pipeline Task Runner as your task runner, or
you can write your own task runner to provide custom data management.

AWS Data Pipeline implements two main sets of functionality. Use the
first set to create a pipeline and define data sources, schedules,
dependencies, and the transforms to be performed on the data. Use the
second set in your task runner application to receive the next task
ready for processing. The logic for performing the task, such as
querying the data, running data analysis, or converting the data from
one format to another, is contained within the task runner. The task
runner performs the task assigned to it by the web service, reporting
progress to the web service as it does so. When the task is done, the
task runner reports the final success or failure of the task to the web
service.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-datapipeline)
and the [AWS API Reference](http://docs.aws.amazon.com/datapipeline/latest/APIReference/Welcome.html).

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

`amazonka-datapipeline` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
