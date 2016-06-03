# Amazon CloudWatch Events SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.4.2`


## Description

Amazon CloudWatch Events helps you to respond to state changes in your
AWS resources. When your resources change state they automatically send
events into an event stream. You can create rules that match selected
events in the stream and route them to targets to take action. You can
also use rules to take action on a pre-determined schedule. For example,
you can configure rules to:

-   Automatically invoke an AWS Lambda function to update DNS entries
    when an event notifies you that Amazon EC2 instance enters the
    running state.
-   Direct specific API records from CloudTrail to an Amazon Kinesis
    stream for detailed analysis of potential security or availability
    risks.
-   Periodically invoke a built-in target to create a snapshot of an
    Amazon EBS volume.

For more information about Amazon CloudWatch Events features, see the
<http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide Amazon CloudWatch Developer Guide>.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-cloudwatch-events)
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

`amazonka-cloudwatch-events` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
