# Amazon Simple Queue Service SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

Welcome to the /Amazon Simple Queue Service API Reference/. This section
describes who should read this guide, how the guide is organized, and
other resources related to the Amazon Simple Queue Service (Amazon SQS).

Amazon SQS offers reliable and scalable hosted queues for storing
messages as they travel between computers. By using Amazon SQS, you can
move data between distributed components of your applications that
perform different tasks without losing messages or requiring each
component to be always available.

Helpful Links:

-   <http://queue.amazonaws.com/doc/2012-11-05/QueueService.wsdl Current WSDL (2012-11-05)>
-   <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/MakingRequestsArticle.html Making API Requests>
-   <http://aws.amazon.com/sqs/ Amazon SQS product page>
-   <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html Using Amazon SQS Message Attributes>
-   <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSDeadLetterQueue.html Using Amazon SQS Dead Letter Queues>
-   <http://docs.aws.amazon.com/general/latest/gr/rande.html#sqs_region Regions and Endpoints>

We also provide SDKs that enable you to access Amazon SQS from your
preferred programming language. The SDKs contain functionality that
automatically takes care of tasks such as:

-   Cryptographically signing your service requests
-   Retrying requests
-   Handling error responses

For a list of available SDKs, go to
<http://aws.amazon.com/tools/ Tools for Amazon Web Services>.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-sqs)
and the [AWS API Reference](http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/Welcome.html).

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

`amazonka-sqs` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
