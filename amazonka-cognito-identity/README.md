# Amazon Cognito Identity SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.0`


## Description

Amazon Cognito

Amazon Cognito is a web service that delivers scoped temporary
credentials to mobile devices and other untrusted environments. Amazon
Cognito uniquely identifies a device and supplies the user with a
consistent identity over the lifetime of an application.

Using Amazon Cognito, you can enable authentication with one or more
third-party identity providers (Facebook, Google, or Login with Amazon),
and you can also choose to support unauthenticated access from your app.
Cognito delivers a unique identifier for each user and acts as an OpenID
token provider trusted by AWS Security Token Service (STS) to access
temporary, limited-privilege AWS credentials.

To provide end-user credentials, first make an unsigned call to GetId.
If the end user is authenticated with one of the supported identity
providers, set the @Logins@ map with the identity provider token.
@GetId@ returns a unique identifier for the user.

Next, make an unsigned call to GetCredentialsForIdentity. This call
expects the same @Logins@ map as the @GetId@ call, as well as the
@IdentityID@ originally returned by @GetId@. Assuming your identity pool
has been configured via the SetIdentityPoolRoles operation,
@GetCredentialsForIdentity@ will return AWS credentials for your use. If
your pool has not been configured with @SetIdentityPoolRoles@, or if you
want to follow legacy flow, make an unsigned call to GetOpenIdToken,
which returns the OpenID token necessary to call STS and retrieve AWS
credentials. This call expects the same @Logins@ map as the @GetId@
call, as well as the @IdentityID@ originally returned by @GetId@. The
token returned by @GetOpenIdToken@ can be passed to the STS operation
<http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html AssumeRoleWithWebIdentity>
to retrieve AWS credentials.

If you want to use Amazon Cognito in an Android, iOS, or Unity
application, you will probably want to make API calls via the AWS Mobile
SDK. To learn more, see the
<http://docs.aws.amazon.com/mobile/index.html AWS Mobile SDK Developer Guide>.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-cognito-identity)
and the [AWS API Reference](http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/Welcome.html).

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

`amazonka-cognito-identity` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
