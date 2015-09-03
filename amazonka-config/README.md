# Amazon Config SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

AWS Config

AWS Config provides a way to keep track of the configurations of all the
AWS resources associated with your AWS account. You can use AWS Config
to get the current and historical configurations of each AWS resource
and also to get information about the relationship between the
resources. An AWS resource can be an Amazon Compute Cloud (Amazon EC2)
instance, an Elastic Block Store (EBS) volume, an Elastic network
Interface (ENI), or a security group. For a complete list of resources
currently supported by AWS Config, see
<http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resources>.

You can access and manage AWS Config through the AWS Management Console,
the AWS Command Line Interface (AWS CLI), the AWS Config API, or the AWS
SDKs for AWS Config

This reference guide contains documentation for the AWS Config API and
the AWS CLI commands that you can use to manage AWS Config.

The AWS Config API uses the Signature Version 4 protocol for signing
requests. For more information about how to sign a request with this
protocol, see
<http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.

For detailed information about AWS Config features and their associated
actions or commands, as well as how to work with AWS Management Console,
see
<http://docs.aws.amazon.com/config/latest/developerguide/WhatIsConfig.html What Is AWS Config?>
in the /AWS Config Developer Guide/.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-config)
and the [AWS API Reference](http://docs.aws.amazon.com/config/latest/APIReference/Welcome.html).

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

`amazonka-config` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
