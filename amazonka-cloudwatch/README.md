# Amazon CloudWatch SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

This is the /Amazon CloudWatch API Reference/. This guide provides
detailed information about Amazon CloudWatch actions, data types,
parameters, and errors. For detailed information about Amazon CloudWatch
features and their associated API calls, go to the
<http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide Amazon CloudWatch Developer Guide>.

Amazon CloudWatch is a web service that enables you to publish, monitor,
and manage various metrics, as well as configure alarm actions based on
data from metrics. For more information about this product go to
<http://aws.amazon.com/cloudwatch>.

For information about the namespace, metric names, and dimensions that
other Amazon Web Services products use to send metrics to Cloudwatch, go
to
<http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Metrics, Namespaces, and Dimensions Reference>
in the /Amazon CloudWatch Developer Guide/.

Use the following links to get started using the /Amazon CloudWatch API
Reference/:

-   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Operations.html Actions>:
    An alphabetical list of all Amazon CloudWatch actions.
-   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Types.html Data Types>:
    An alphabetical list of all Amazon CloudWatch data types.
-   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CommonParameters.html Common Parameters>:
    Parameters that all Query actions can use.
-   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CommonErrors.html Common Errors>:
    Client and server errors that all actions can return.
-   <http://docs.aws.amazon.com/general/latest/gr/index.html?rande.html Regions and Endpoints>:
    Itemized regions and endpoints for all AWS products.
-   <http://monitoring.amazonaws.com/doc/2010-08-01/CloudWatch.wsdl WSDL Location>:
    http:\/\/monitoring.amazonaws.com\/doc\/2010-08-01\/CloudWatch.wsdl

In addition to using the Amazon CloudWatch API, you can also use the
following SDKs and third-party libraries to access Amazon CloudWatch
programmatically.

-   <http://aws.amazon.com/documentation/sdkforjava/ AWS SDK for Java Documentation>
-   <http://aws.amazon.com/documentation/sdkfornet/ AWS SDK for .NET Documentation>
-   <http://aws.amazon.com/documentation/sdkforphp/ AWS SDK for PHP Documentation>
-   <http://aws.amazon.com/documentation/sdkforruby/ AWS SDK for Ruby Documentation>

Developers in the AWS developer community also provide their own
libraries, which you can find at the following AWS developer centers:

-   <http://aws.amazon.com/java/ AWS Java Developer Center>
-   <http://aws.amazon.com/php/ AWS PHP Developer Center>
-   <http://aws.amazon.com/python/ AWS Python Developer Center>
-   <http://aws.amazon.com/ruby/ AWS Ruby Developer Center>
-   <http://aws.amazon.com/net/ AWS Windows and .NET Developer Center>

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-cloudwatch)
and the [AWS API Reference](http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/Welcome.html).

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
