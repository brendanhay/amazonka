# Amazon CloudWatch SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.4.0`


## Description

Amazon CloudWatch monitors your Amazon Web Services (AWS) resources and
the applications you run on AWS in real-time. You can use CloudWatch to
collect and track metrics, which are the variables you want to measure
for your resources and applications.

CloudWatch alarms send notifications or automatically make changes to
the resources you are monitoring based on rules that you define. For
example, you can monitor the CPU usage and disk reads and writes of your
Amazon Elastic Compute Cloud (Amazon EC2) instances and then use this
data to determine whether you should launch additional instances to
handle increased load. You can also use this data to stop under-used
instances to save money.

In addition to monitoring the built-in metrics that come with AWS, you
can monitor your own custom metrics. With CloudWatch, you gain
system-wide visibility into resource utilization, application
performance, and operational health.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-cloudwatch)
and the [AWS API Reference](https://aws.amazon.com/documentation/).

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

`amazonka-cloudwatch` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
