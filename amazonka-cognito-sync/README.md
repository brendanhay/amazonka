# Amazon Cognito Sync SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

Amazon Cognito Sync

Amazon Cognito Sync provides an AWS service and client library that
enable cross-device syncing of application-related user data. High-level
client libraries are available for both iOS and Android. You can use
these libraries to persist data locally so that it\'s available even if
the device is offline. Developer credentials don\'t need to be stored on
the mobile device to access the service. You can use Amazon Cognito to
obtain a normalized user ID and credentials. User data is persisted in a
dataset that can store up to 1 MB of key-value pairs, and you can have
up to 20 datasets per user identity.

With Amazon Cognito Sync, the data stored for each identity is
accessible only to credentials assigned to that identity. In order to
use the Cognito Sync service, you need to make API calls using
credentials retrieved with
<http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/Welcome.html Amazon Cognito Identity service>.

If you want to use Cognito Sync in an Android or iOS application, you
will probably want to make API calls via the AWS Mobile SDK. To learn
more, see the
<http://docs.aws.amazon.com/mobile/sdkforandroid/developerguide/cognito-sync.html Developer Guide for Android>
and the
<http://docs.aws.amazon.com/mobile/sdkforios/developerguide/cognito-sync.html Developer Guide for iOS>.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-cognito-sync)
and the [AWS API Reference](http://docs.aws.amazon.com/cognitosync/latest/APIReference/Welcome.html).

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

`amazonka-cognito-sync` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
