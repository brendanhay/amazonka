# Amazon Identity and Access Management SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

AWS Identity and Access Management

AWS Identity and Access Management (IAM) is a web service that you can
use to manage users and user permissions under your AWS account. This
guide provides descriptions of IAM actions that you can call
programmatically. For general information about IAM, see
<http://aws.amazon.com/iam/ AWS Identity and Access Management (IAM)>.
For the user guide for IAM, see
<http://docs.aws.amazon.com/IAM/latest/UserGuide/ Using IAM>.

AWS provides SDKs that consist of libraries and sample code for various
programming languages and platforms (Java, Ruby, .NET, iOS, Android,
etc.). The SDKs provide a convenient way to create programmatic access
to IAM and AWS. For example, the SDKs take care of tasks such as
cryptographically signing requests (see below), managing errors, and
retrying requests automatically. For information about the AWS SDKs,
including how to download and install them, see the
<http://aws.amazon.com/tools/ Tools for Amazon Web Services> page.

We recommend that you use the AWS SDKs to make programmatic API calls to
IAM. However, you can also use the IAM Query API to make direct calls to
the IAM web service. To learn more about the IAM Query API, see
<http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests>
in the /Using IAM/ guide. IAM supports GET and POST requests for all
actions. That is, the API does not require you to use GET for some
actions and POST for others. However, GET requests are subject to the
limitation size of a URL. Therefore, for operations that require larger
sizes, use a POST request.

__Signing Requests__

Requests must be signed using an access key ID and a secret access key.
We strongly recommend that you do not use your AWS account access key ID
and secret access key for everyday work with IAM. You can use the access
key ID and secret access key for an IAM user or you can use the AWS
Security Token Service to generate temporary security credentials and
use those to sign requests.

To sign requests, we recommend that you use
<http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
If you have an existing application that uses Signature Version 2, you
do not have to update it to use Signature Version 4. However, some
operations now require Signature Version 4. The documentation for
operations that require version 4 indicate this requirement.

__Additional Resources__

For more information, see the following:

-   <http://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html AWS Security Credentials>.
    This topic provides general information about the types of
    credentials used for accessing AWS.
-   <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAMBestPractices.html IAM Best Practices>.
    This topic presents a list of suggestions for using the IAM service
    to help secure your AWS resources.
-   <http://docs.aws.amazon.com/STS/latest/UsingSTS/ AWS Security Token Service>.
    This guide describes how to create and use temporary security
    credentials.
-   <http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API Requests>.
    This set of topics walk you through the process of signing a request
    using an access key ID and secret access key.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-iam)
and the [AWS API Reference](http://docs.aws.amazon.com/IAM/latest/APIReference/Welcome.html).

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

`amazonka-iam` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
