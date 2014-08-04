-- Module      : Network.AWS.CognitoIdentity.V2014_06_30
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
module Network.AWS.CognitoIdentity.V2014_06_30 (module Export) where

import Network.AWS.CognitoIdentity.V2014_06_30.CreateIdentityPool as Export
import Network.AWS.CognitoIdentity.V2014_06_30.DeleteIdentityPool as Export
import Network.AWS.CognitoIdentity.V2014_06_30.DescribeIdentityPool as Export
import Network.AWS.CognitoIdentity.V2014_06_30.GetId as Export
import Network.AWS.CognitoIdentity.V2014_06_30.GetOpenIdToken as Export
import Network.AWS.CognitoIdentity.V2014_06_30.ListIdentities as Export
import Network.AWS.CognitoIdentity.V2014_06_30.ListIdentityPools as Export
import Network.AWS.CognitoIdentity.V2014_06_30.Types as Export
import Network.AWS.CognitoIdentity.V2014_06_30.UnlinkIdentity as Export
import Network.AWS.CognitoIdentity.V2014_06_30.UpdateIdentityPool as Export
