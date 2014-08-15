{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
module Network.AWS.CognitoIdentity.V2014_06_30.CreateIdentityPool where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'CreateIdentityPool' request.
createIdentityPool :: Text -- ^ '_cipiIdentityPoolName'
                   -> Bool -- ^ '_cipiAllowUnauthenticatedIdentities'
                   -> CreateIdentityPool
createIdentityPool p1 p2 = CreateIdentityPool
    { _cipiIdentityPoolName = p1
    , _cipiAllowUnauthenticatedIdentities = p2
    , _cipiSupportedLoginProviders = mempty
    }

data CreateIdentityPool = CreateIdentityPool
    { _cipiIdentityPoolName :: Text
      -- ^ A string that you provide.
    , _cipiAllowUnauthenticatedIdentities :: Bool
      -- ^ TRUE if the identity pool supports unauthenticated logins.
    , _cipiSupportedLoginProviders :: Map Text Text
      -- ^ Optional key:value pairs mapping provider names to provider app
      -- IDs.
    } deriving (Show, Generic)

makeLenses ''CreateIdentityPool

instance ToPath CreateIdentityPool

instance ToQuery CreateIdentityPool

instance ToHeaders CreateIdentityPool

instance ToJSON CreateIdentityPool

data CreateIdentityPoolResponse = CreateIdentityPoolResponse
    { _iwIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    , _iwIdentityPoolName :: Text
      -- ^ A string that you provide.
    , _iwAllowUnauthenticatedIdentities :: Bool
      -- ^ TRUE if the identity pool supports unauthenticated logins.
    , _iwSupportedLoginProviders :: Map Text Text
      -- ^ Optional key:value pairs mapping provider names to provider app
      -- IDs.
    } deriving (Show, Generic)

makeLenses ''CreateIdentityPoolResponse

instance FromJSON CreateIdentityPoolResponse

instance AWSRequest CreateIdentityPool where
    type Sv CreateIdentityPool = CognitoIdentity
    type Rs CreateIdentityPool = CreateIdentityPoolResponse

    request = get
    response _ = jsonResponse
