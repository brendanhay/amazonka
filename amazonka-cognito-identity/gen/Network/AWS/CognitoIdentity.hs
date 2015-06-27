-- Module      : Network.AWS.CognitoIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Cognito
--
-- Amazon Cognito is a web service that delivers scoped temporary
-- credentials to mobile devices and other untrusted environments. Amazon
-- Cognito uniquely identifies a device and supplies the user with a
-- consistent identity over the lifetime of an application.
--
-- Using Amazon Cognito, you can enable authentication with one or more
-- third-party identity providers (Facebook, Google, or Login with Amazon),
-- and you can also choose to support unauthenticated access from your app.
-- Cognito delivers a unique identifier for each user and acts as an OpenID
-- token provider trusted by AWS Security Token Service (STS) to access
-- temporary, limited-privilege AWS credentials.
--
-- To provide end-user credentials, first make an unsigned call to GetId.
-- If the end user is authenticated with one of the supported identity
-- providers, set the @Logins@ map with the identity provider token.
-- @GetId@ returns a unique identifier for the user.
--
-- Next, make an unsigned call to GetCredentialsForIdentity. This call
-- expects the same @Logins@ map as the @GetId@ call, as well as the
-- @IdentityID@ originally returned by @GetId@. Assuming your identity pool
-- has been configured via the SetIdentityPoolRoles operation,
-- @GetCredentialsForIdentity@ will return AWS credentials for your use. If
-- your pool has not been configured with @SetIdentityPoolRoles@, or if you
-- want to follow legacy flow, make an unsigned call to GetOpenIdToken,
-- which returns the OpenID token necessary to call STS and retrieve AWS
-- credentials. This call expects the same @Logins@ map as the @GetId@
-- call, as well as the @IdentityID@ originally returned by @GetId@. The
-- token returned by @GetOpenIdToken@ can be passed to the STS operation
-- <http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html AssumeRoleWithWebIdentity>
-- to retrieve AWS credentials.
--
-- If you want to use Amazon Cognito in an Android, iOS, or Unity
-- application, you will probably want to make API calls via the AWS Mobile
-- SDK. To learn more, see the
-- <http://docs.aws.amazon.com/mobile/index.html AWS Mobile SDK Developer Guide>.
module Network.AWS.CognitoIdentity
    ( module Export
    ) where

import           Network.AWS.CognitoIdentity.CreateIdentityPool                 as Export
import           Network.AWS.CognitoIdentity.DeleteIdentities                   as Export
import           Network.AWS.CognitoIdentity.DeleteIdentityPool                 as Export
import           Network.AWS.CognitoIdentity.DescribeIdentity                   as Export
import           Network.AWS.CognitoIdentity.DescribeIdentityPool               as Export
import           Network.AWS.CognitoIdentity.GetCredentialsForIdentity          as Export
import           Network.AWS.CognitoIdentity.GetId                              as Export
import           Network.AWS.CognitoIdentity.GetIdentityPoolRoles               as Export
import           Network.AWS.CognitoIdentity.GetOpenIdToken                     as Export
import           Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity as Export
import           Network.AWS.CognitoIdentity.ListIdentities                     as Export
import           Network.AWS.CognitoIdentity.ListIdentityPools                  as Export
import           Network.AWS.CognitoIdentity.LookupDeveloperIdentity            as Export
import           Network.AWS.CognitoIdentity.MergeDeveloperIdentities           as Export
import           Network.AWS.CognitoIdentity.SetIdentityPoolRoles               as Export
import           Network.AWS.CognitoIdentity.Types                              as Export
import           Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity            as Export
import           Network.AWS.CognitoIdentity.UnlinkIdentity                     as Export
import           Network.AWS.CognitoIdentity.UpdateIdentityPool                 as Export
import           Network.AWS.CognitoIdentity.Waiters                            as Export
