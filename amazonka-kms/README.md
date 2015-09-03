# Amazon Key Management Service SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

AWS Key Management Service

AWS Key Management Service (KMS) is an encryption and key management web
service. This guide describes the KMS actions that you can call
programmatically. For general information about KMS, see the
<http://docs.aws.amazon.com/kms/latest/developerguide/overview.html AWS Key Management Service Developer Guide>

AWS provides SDKs that consist of libraries and sample code for various
programming languages and platforms (Java, Ruby, .Net, iOS, Android,
etc.). The SDKs provide a convenient way to create programmatic access
to KMS and AWS. For example, the SDKs take care of tasks such as signing
requests (see below), managing errors, and retrying requests
automatically. For more information about the AWS SDKs, including how to
download and install them, see
<http://aws.amazon.com/tools/ Tools for Amazon Web Services>.

We recommend that you use the AWS SDKs to make programmatic API calls to
KMS.

Clients must support TLS (Transport Layer Security) 1.0. We recommend
TLS 1.2. Clients must also support cipher suites with Perfect Forward
Secrecy (PFS) such as Ephemeral Diffie-Hellman (DHE) or Elliptic Curve
Ephemeral Diffie-Hellman (ECDHE). Most modern systems such as Java 7 and
later support these modes.

__Signing Requests__

Requests must be signed by using an access key ID and a secret access
key. We strongly recommend that you do not use your AWS account access
key ID and secret key for everyday work with KMS. Instead, use the
access key ID and secret access key for an IAM user, or you can use the
AWS Security Token Service to generate temporary security credentials
that you can use to sign requests.

All KMS operations require
<http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.

__Recording API Requests__

KMS supports AWS CloudTrail, a service that records AWS API calls and
related events for your AWS account and delivers them to an Amazon S3
bucket that you specify. By using the information collected by
CloudTrail, you can determine what requests were made to KMS, who made
the request, when it was made, and so on. To learn more about
CloudTrail, including how to turn it on and find your log files, see the
<http://docs.aws.amazon.com/awscloudtrail/latest/userguide/whatiscloudtrail.html AWS CloudTrail User Guide>

__Additional Resources__

For more information about credentials and request signing, see the
following:

-   <http://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html AWS Security Credentials>.
    This topic provides general information about the types of
    credentials used for accessing AWS.
-   <http://docs.aws.amazon.com/STS/latest/UsingSTS/ AWS Security Token Service>.
    This guide describes how to create and use temporary security
    credentials.
-   <http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API Requests>.
    This set of topics walks you through the process of signing a
    request using an access key ID and a secret access key.

__Commonly Used APIs__

Of the APIs discussed in this guide, the following will prove the most
useful for most applications. You will likely perform actions other than
these, such as creating keys and assigning policies, by using the
console.

-   Encrypt
-   Decrypt
-   GenerateDataKey
-   GenerateDataKeyWithoutPlaintext

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-kms)
and the [AWS API Reference](http://docs.aws.amazon.com/kms/latest/APIReference/Welcome.html).

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

`amazonka-kms` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
