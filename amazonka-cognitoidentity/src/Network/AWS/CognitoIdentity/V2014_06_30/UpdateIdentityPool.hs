{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.UpdateIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a user pool. UpdateIdentityPool The following are a request and
-- response for the UpdateIdentityPool action. { "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE", "IdentityPoolName":
-- "MyUpdatedPool", "IdentityPoolDescription": "An identity pool that needs
-- updating", "Unauthenticated": true, "SupportedLoginProviders": {
-- "www.amazon.com": "Amazon_App_ID", "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID" } } { "IdentityPoolDescription": "An
-- identity pool that needs updating", "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE", "IdentityPoolName":
-- "MyUpdatedPool", "SupportedLoginProviders": { "www.amazon.com":
-- "Amazon_App_ID", "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID" }, "AllowUnauthenticatedIdentities":
-- true }.
module Network.AWS.CognitoIdentity.V2014_06_30.UpdateIdentityPool where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'UpdateIdentityPool' request.
updateIdentityPool :: Text -- ^ '_irIdentityPoolId'
                   -> Text -- ^ '_irIdentityPoolName'
                   -> Bool -- ^ '_irAllowUnauthenticatedIdentities'
                   -> UpdateIdentityPool
updateIdentityPool p1 p2 p3 = UpdateIdentityPool
    { _irIdentityPoolId = p1
    , _irIdentityPoolName = p2
    , _irAllowUnauthenticatedIdentities = p3
    , _irSupportedLoginProviders = mempty
    }

data UpdateIdentityPool = UpdateIdentityPool
    { _irIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    , _irIdentityPoolName :: Text
      -- ^ A string that you provide.
    , _irAllowUnauthenticatedIdentities :: Bool
      -- ^ TRUE if the identity pool supports unauthenticated logins.
    , _irSupportedLoginProviders :: Map Text Text
      -- ^ Optional key:value pairs mapping provider names to provider app
      -- IDs.
    } deriving (Show, Generic)

makeLenses ''UpdateIdentityPool

instance ToPath UpdateIdentityPool

instance ToQuery UpdateIdentityPool

instance ToHeaders UpdateIdentityPool

instance ToJSON UpdateIdentityPool

data UpdateIdentityPoolResponse = UpdateIdentityPoolResponse
    { _itIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    , _itIdentityPoolName :: Text
      -- ^ A string that you provide.
    , _itAllowUnauthenticatedIdentities :: Bool
      -- ^ TRUE if the identity pool supports unauthenticated logins.
    , _itSupportedLoginProviders :: Map Text Text
      -- ^ Optional key:value pairs mapping provider names to provider app
      -- IDs.
    } deriving (Show, Generic)

makeLenses ''UpdateIdentityPoolResponse

instance FromJSON UpdateIdentityPoolResponse

instance AWSRequest UpdateIdentityPool where
    type Sv UpdateIdentityPool = CognitoIdentity
    type Rs UpdateIdentityPool = UpdateIdentityPoolResponse

    request = get
    response _ = jsonResponse
