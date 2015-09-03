# Amazon CloudFront SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

Amazon CloudFront is a web service that speeds up distribution of your
static and dynamic web content, for example, .html, .css, .php, image,
and media files, to end users. CloudFront delivers your content through
a worldwide network of edge locations. When an end user requests content
that you\'re serving with CloudFront, the user is routed to the edge
location that provides the lowest latency, so content is delivered with
the best possible performance. If the content is already in that edge
location, CloudFront delivers it immediately. If the content is not
currently in that edge location, CloudFront retrieves it from an Amazon
S3 bucket or an HTTP server (for example, a web server) that you have
identified as the source for the definitive version of your content.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-cloudfront)
and the [AWS API Reference](http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/Welcome.html).

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

`amazonka-cloudfront` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
