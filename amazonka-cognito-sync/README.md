# Amazon Cognito Sync SDK

> _Warning:_ This is an experimental preview release which is still under heavy development and not intended for public consumption, _caveat emptor_!

* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)

## Description

Amazon Cognito Sync provides an AWS service and client library that enable
cross-device syncing of application-related user data. High-level client
libraries are available for both iOS and Android. You can use these libraries
to persist data locally so that it's available even if the device is offline.
Developer credentials don't need to be stored on the mobile device to access
the service. You can use Amazon Cognito to obtain a normalized user ID and
credentials. User data is persisted in a dataset that can store up to 1 MB of
key-value pairs, and you can have up to 20 datasets per user identity.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-cognito-sync)
and the [AWS API Reference](http://docs.aws.amazon.com/cognitosync/latest/APIReference/Welcome.html).


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

> _Note:_ this library is an auto-generated Haskell package. Please see `amazonka-gen` for more information.


## Licence

`amazonka-cognito-sync` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
