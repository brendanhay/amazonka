# Amazon CloudWatch Logs SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.8`


## Description

Amazon CloudWatch Logs API Reference

You can use Amazon CloudWatch Logs to monitor, store, and access your
log files from Amazon Elastic Compute Cloud (Amazon EC2) instances,
Amazon CloudTrail, or other sources. You can then retrieve the
associated log data from CloudWatch Logs using the Amazon CloudWatch
console, the CloudWatch Logs commands in the AWS CLI, the CloudWatch
Logs API, or the CloudWatch Logs SDK.

You can use CloudWatch Logs to:

-   __Monitor Logs from Amazon EC2 Instances in Real-time__: You can use
    CloudWatch Logs to monitor applications and systems using log data.
    For example, CloudWatch Logs can track the number of errors that
    occur in your application logs and send you a notification whenever
    the rate of errors exceeds a threshold you specify. CloudWatch Logs
    uses your log data for monitoring; so, no code changes are required.
    For example, you can monitor application logs for specific literal
    terms (such as \"NullReferenceException\") or count the number of
    occurrences of a literal term at a particular position in log data
    (such as \"404\" status codes in an Apache access log). When the
    term you are searching for is found, CloudWatch Logs reports the
    data to a Amazon CloudWatch metric that you specify.

-   __Monitor Amazon CloudTrail Logged Events__: You can create alarms
    in Amazon CloudWatch and receive notifications of particular API
    activity as captured by CloudTrail and use the notification to
    perform troubleshooting.

-   __Archive Log Data__: You can use CloudWatch Logs to store your log
    data in highly durable storage. You can change the log retention
    setting so that any log events older than this setting are
    automatically deleted. The CloudWatch Logs agent makes it easy to
    quickly send both rotated and non-rotated log data off of a host and
    into the log service. You can then access the raw log data when you
    need it.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-cloudwatch-logs)
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

`amazonka-cloudwatch-logs` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
