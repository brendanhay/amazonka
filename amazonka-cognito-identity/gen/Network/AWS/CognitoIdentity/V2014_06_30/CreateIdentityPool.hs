{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.CreateIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new identity pool. The identity pool is a store of user identity
-- information that is specific to your AWS account. CreateIdentityPool The
-- following example shows a request and response for a CreateIdentityPool
-- operation. { "IdentityPoolName": "MyIdentityPool",
-- "IdentityPoolDescription": "My identity pool", "Unauthenticated": true,
-- "SupportedLoginProviders": { "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID", "www.amazon.com": "Amazon_App_ID" }
-- } { "IdentityPoolDescription": "My identity pool", "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE", "IdentityPoolName":
-- "MyIdentityPool", "SupportedLoginProviders": { "www.amazon.com":
-- "Amazon_App_ID", "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID" }, "Unauthenticated": true }.
module Network.AWS.CognitoIdentity.V2014_06_30.CreateIdentityPool
    (
    -- * Request
      CreateIdentityPool
    -- ** Request constructor
    , mkCreateIdentityPool
    -- ** Request lenses
    , cipIdentityPoolName
    , cipAllowUnauthenticatedIdentities
    , cipSupportedLoginProviders

    -- * Response
    , CreateIdentityPoolResponse
    -- ** Response lenses
    , ciprsIdentityPoolId
    , ciprsIdentityPoolName
    , ciprsAllowUnauthenticatedIdentities
    , ciprsSupportedLoginProviders
    ) where

import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Input to the CreateIdentityPool action.
data CreateIdentityPool = CreateIdentityPool
    { _cipIdentityPoolName :: Text
    , _cipAllowUnauthenticatedIdentities :: Bool
    , _cipSupportedLoginProviders :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateIdentityPool' request.
mkCreateIdentityPool :: Text -- ^ 'cipIdentityPoolName'
                     -> Bool -- ^ 'cipAllowUnauthenticatedIdentities'
                     -> CreateIdentityPool
mkCreateIdentityPool p1 p2 = CreateIdentityPool
    { _cipIdentityPoolName = p1
    , _cipAllowUnauthenticatedIdentities = p2
    , _cipSupportedLoginProviders = mempty
    }

-- | A string that you provide.
cipIdentityPoolName :: Lens' CreateIdentityPool Text
cipIdentityPoolName =
    lens _cipIdentityPoolName (\s a -> s { _cipIdentityPoolName = a })

-- | TRUE if the identity pool supports unauthenticated logins.
cipAllowUnauthenticatedIdentities :: Lens' CreateIdentityPool Bool
cipAllowUnauthenticatedIdentities =
    lens _cipAllowUnauthenticatedIdentities
         (\s a -> s { _cipAllowUnauthenticatedIdentities = a })

-- | Optional key:value pairs mapping provider names to provider app IDs.
cipSupportedLoginProviders :: Lens' CreateIdentityPool (Map Text Text)
cipSupportedLoginProviders =
    lens _cipSupportedLoginProviders
         (\s a -> s { _cipSupportedLoginProviders = a })

instance ToPath CreateIdentityPool

instance ToQuery CreateIdentityPool

instance ToHeaders CreateIdentityPool

instance ToJSON CreateIdentityPool

-- | An object representing a Cognito identity pool.
data CreateIdentityPoolResponse = CreateIdentityPoolResponse
    { _ciprsIdentityPoolId :: Text
    , _ciprsIdentityPoolName :: Text
    , _ciprsAllowUnauthenticatedIdentities :: Bool
    , _ciprsSupportedLoginProviders :: Map Text Text
    } deriving (Show, Generic)

-- | An identity pool ID in the format REGION:GUID.
ciprsIdentityPoolId :: Lens' CreateIdentityPoolResponse Text
ciprsIdentityPoolId =
    lens _ciprsIdentityPoolId (\s a -> s { _ciprsIdentityPoolId = a })

-- | A string that you provide.
ciprsIdentityPoolName :: Lens' CreateIdentityPoolResponse Text
ciprsIdentityPoolName =
    lens _ciprsIdentityPoolName (\s a -> s { _ciprsIdentityPoolName = a })

-- | TRUE if the identity pool supports unauthenticated logins.
ciprsAllowUnauthenticatedIdentities :: Lens' CreateIdentityPoolResponse Bool
ciprsAllowUnauthenticatedIdentities =
    lens _ciprsAllowUnauthenticatedIdentities
         (\s a -> s { _ciprsAllowUnauthenticatedIdentities = a })

-- | Optional key:value pairs mapping provider names to provider app IDs.
ciprsSupportedLoginProviders :: Lens' CreateIdentityPoolResponse (Map Text Text)
ciprsSupportedLoginProviders =
    lens _ciprsSupportedLoginProviders
         (\s a -> s { _ciprsSupportedLoginProviders = a })

instance FromJSON CreateIdentityPoolResponse

instance AWSRequest CreateIdentityPool where
    type Sv CreateIdentityPool = CognitoIdentity
    type Rs CreateIdentityPool = CreateIdentityPoolResponse

    request = get
    response _ = jsonResponse
