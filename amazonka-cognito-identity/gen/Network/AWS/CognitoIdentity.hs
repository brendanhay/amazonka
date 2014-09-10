-- Module      : Network.AWS.CognitoIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Cognito is a web service that facilitates the delivery of scoped,
-- temporary credentials to mobile devices or other untrusted environments.
-- Amazon Cognito uniquely identifies a device or user and supplies the user
-- with a consistent identity throughout the lifetime of an application.
-- Amazon Cognito lets users authenticate with third-party identity providers
-- (Facebook, Google, or Login with Amazon). As a developer, you decide which
-- identity providers to trust. You can also choose to support unauthenticated
-- access from your application. Your users are provided with Cognito tokens
-- that uniquely identify their device and any information provided about
-- third-party logins.
module Network.AWS.CognitoIdentity
    ( module Network.AWS.CognitoIdentity.CreateIdentityPool
    , module Network.AWS.CognitoIdentity.DeleteIdentityPool
    , module Network.AWS.CognitoIdentity.DescribeIdentityPool
    , module Network.AWS.CognitoIdentity.GetId
    , module Network.AWS.CognitoIdentity.GetOpenIdToken
    , module Network.AWS.CognitoIdentity.ListIdentities
    , module Network.AWS.CognitoIdentity.ListIdentityPools
    , module Network.AWS.CognitoIdentity.Types
    , module Network.AWS.CognitoIdentity.UnlinkIdentity
    , module Network.AWS.CognitoIdentity.UpdateIdentityPool
    ) where

import Network.AWS.CognitoIdentity.CreateIdentityPool
import Network.AWS.CognitoIdentity.DeleteIdentityPool
import Network.AWS.CognitoIdentity.DescribeIdentityPool
import Network.AWS.CognitoIdentity.GetId
import Network.AWS.CognitoIdentity.GetOpenIdToken
import Network.AWS.CognitoIdentity.ListIdentities
import Network.AWS.CognitoIdentity.ListIdentityPools
import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.UnlinkIdentity
import Network.AWS.CognitoIdentity.UpdateIdentityPool
