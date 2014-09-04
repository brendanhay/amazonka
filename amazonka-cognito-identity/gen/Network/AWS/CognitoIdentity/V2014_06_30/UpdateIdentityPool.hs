{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.CognitoIdentity.V2014_06_30.UpdateIdentityPool
    (
    -- * Request
      UpdateIdentityPool
    -- ** Request constructor
    , updateIdentityPool
    -- ** Request lenses
    , iuIdentityPoolId
    , iuIdentityPoolName
    , iuAllowUnauthenticatedIdentities
    , iuSupportedLoginProviders

    -- * Response
    , UpdateIdentityPoolResponse
    -- ** Response lenses
    , iwIdentityPoolId
    , iwIdentityPoolName
    , iwAllowUnauthenticatedIdentities
    , iwSupportedLoginProviders
    ) where

import           Network.AWS.CognitoIdentity.V2014_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'UpdateIdentityPool' request.
updateIdentityPool :: Text -- ^ 'iuIdentityPoolId'
                   -> Text -- ^ 'iuIdentityPoolName'
                   -> Bool -- ^ 'iuAllowUnauthenticatedIdentities'
                   -> UpdateIdentityPool
updateIdentityPool p1 p2 p3 = UpdateIdentityPool
    { _iuIdentityPoolId = p1
    , _iuIdentityPoolName = p2
    , _iuAllowUnauthenticatedIdentities = p3
    , _iuSupportedLoginProviders = mempty
    }
{-# INLINE updateIdentityPool #-}

data UpdateIdentityPool = UpdateIdentityPool
    { _iuIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    , _iuIdentityPoolName :: Text
      -- ^ A string that you provide.
    , _iuAllowUnauthenticatedIdentities :: Bool
      -- ^ TRUE if the identity pool supports unauthenticated logins.
    , _iuSupportedLoginProviders :: Map Text Text
      -- ^ Optional key:value pairs mapping provider names to provider app
      -- IDs.
    } deriving (Show, Generic)

-- | An identity pool ID in the format REGION:GUID.
iuIdentityPoolId :: Lens' UpdateIdentityPool (Text)
iuIdentityPoolId f x =
    f (_iuIdentityPoolId x)
        <&> \y -> x { _iuIdentityPoolId = y }
{-# INLINE iuIdentityPoolId #-}

-- | A string that you provide.
iuIdentityPoolName :: Lens' UpdateIdentityPool (Text)
iuIdentityPoolName f x =
    f (_iuIdentityPoolName x)
        <&> \y -> x { _iuIdentityPoolName = y }
{-# INLINE iuIdentityPoolName #-}

-- | TRUE if the identity pool supports unauthenticated logins.
iuAllowUnauthenticatedIdentities :: Lens' UpdateIdentityPool (Bool)
iuAllowUnauthenticatedIdentities f x =
    f (_iuAllowUnauthenticatedIdentities x)
        <&> \y -> x { _iuAllowUnauthenticatedIdentities = y }
{-# INLINE iuAllowUnauthenticatedIdentities #-}

-- | Optional key:value pairs mapping provider names to provider app IDs.
iuSupportedLoginProviders :: Lens' UpdateIdentityPool (Map Text Text)
iuSupportedLoginProviders f x =
    f (_iuSupportedLoginProviders x)
        <&> \y -> x { _iuSupportedLoginProviders = y }
{-# INLINE iuSupportedLoginProviders #-}

instance ToPath UpdateIdentityPool

instance ToQuery UpdateIdentityPool

instance ToHeaders UpdateIdentityPool

instance ToJSON UpdateIdentityPool

data UpdateIdentityPoolResponse = UpdateIdentityPoolResponse
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

-- | An identity pool ID in the format REGION:GUID.
iwIdentityPoolId :: Lens' UpdateIdentityPoolResponse (Text)
iwIdentityPoolId f x =
    f (_iwIdentityPoolId x)
        <&> \y -> x { _iwIdentityPoolId = y }
{-# INLINE iwIdentityPoolId #-}

-- | A string that you provide.
iwIdentityPoolName :: Lens' UpdateIdentityPoolResponse (Text)
iwIdentityPoolName f x =
    f (_iwIdentityPoolName x)
        <&> \y -> x { _iwIdentityPoolName = y }
{-# INLINE iwIdentityPoolName #-}

-- | TRUE if the identity pool supports unauthenticated logins.
iwAllowUnauthenticatedIdentities :: Lens' UpdateIdentityPoolResponse (Bool)
iwAllowUnauthenticatedIdentities f x =
    f (_iwAllowUnauthenticatedIdentities x)
        <&> \y -> x { _iwAllowUnauthenticatedIdentities = y }
{-# INLINE iwAllowUnauthenticatedIdentities #-}

-- | Optional key:value pairs mapping provider names to provider app IDs.
iwSupportedLoginProviders :: Lens' UpdateIdentityPoolResponse (Map Text Text)
iwSupportedLoginProviders f x =
    f (_iwSupportedLoginProviders x)
        <&> \y -> x { _iwSupportedLoginProviders = y }
{-# INLINE iwSupportedLoginProviders #-}

instance FromJSON UpdateIdentityPoolResponse

instance AWSRequest UpdateIdentityPool where
    type Sv UpdateIdentityPool = CognitoIdentity
    type Rs UpdateIdentityPool = UpdateIdentityPoolResponse

    request = get
    response _ = jsonResponse
