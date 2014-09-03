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
    , createIdentityPool
    -- ** Request lenses
    , cipiIdentityPoolName
    , cipiAllowUnauthenticatedIdentities
    , cipiSupportedLoginProviders

    -- * Response
    , CreateIdentityPoolResponse
    -- ** Response lenses
    , iqIdentityPoolId
    , iqIdentityPoolName
    , iqAllowUnauthenticatedIdentities
    , iqSupportedLoginProviders
    ) where

import           Network.AWS.CognitoIdentity.V2014_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateIdentityPool' request.
createIdentityPool :: Text -- ^ 'cipiIdentityPoolName'
                   -> Bool -- ^ 'cipiAllowUnauthenticatedIdentities'
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

-- | A string that you provide.
cipiIdentityPoolName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateIdentityPool
    -> f CreateIdentityPool
cipiIdentityPoolName f x =
    (\y -> x { _cipiIdentityPoolName = y })
       <$> f (_cipiIdentityPoolName x)
{-# INLINE cipiIdentityPoolName #-}

-- | TRUE if the identity pool supports unauthenticated logins.
cipiAllowUnauthenticatedIdentities
    :: Functor f
    => (Bool
    -> f (Bool))
    -> CreateIdentityPool
    -> f CreateIdentityPool
cipiAllowUnauthenticatedIdentities f x =
    (\y -> x { _cipiAllowUnauthenticatedIdentities = y })
       <$> f (_cipiAllowUnauthenticatedIdentities x)
{-# INLINE cipiAllowUnauthenticatedIdentities #-}

-- | Optional key:value pairs mapping provider names to provider app IDs.
cipiSupportedLoginProviders
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> CreateIdentityPool
    -> f CreateIdentityPool
cipiSupportedLoginProviders f x =
    (\y -> x { _cipiSupportedLoginProviders = y })
       <$> f (_cipiSupportedLoginProviders x)
{-# INLINE cipiSupportedLoginProviders #-}

instance ToPath CreateIdentityPool

instance ToQuery CreateIdentityPool

instance ToHeaders CreateIdentityPool

instance ToJSON CreateIdentityPool

data CreateIdentityPoolResponse = CreateIdentityPoolResponse
    { _iqIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    , _iqIdentityPoolName :: Text
      -- ^ A string that you provide.
    , _iqAllowUnauthenticatedIdentities :: Bool
      -- ^ TRUE if the identity pool supports unauthenticated logins.
    , _iqSupportedLoginProviders :: Map Text Text
      -- ^ Optional key:value pairs mapping provider names to provider app
      -- IDs.
    } deriving (Show, Generic)

-- | An identity pool ID in the format REGION:GUID.
iqIdentityPoolId
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateIdentityPoolResponse
    -> f CreateIdentityPoolResponse
iqIdentityPoolId f x =
    (\y -> x { _iqIdentityPoolId = y })
       <$> f (_iqIdentityPoolId x)
{-# INLINE iqIdentityPoolId #-}

-- | A string that you provide.
iqIdentityPoolName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateIdentityPoolResponse
    -> f CreateIdentityPoolResponse
iqIdentityPoolName f x =
    (\y -> x { _iqIdentityPoolName = y })
       <$> f (_iqIdentityPoolName x)
{-# INLINE iqIdentityPoolName #-}

-- | TRUE if the identity pool supports unauthenticated logins.
iqAllowUnauthenticatedIdentities
    :: Functor f
    => (Bool
    -> f (Bool))
    -> CreateIdentityPoolResponse
    -> f CreateIdentityPoolResponse
iqAllowUnauthenticatedIdentities f x =
    (\y -> x { _iqAllowUnauthenticatedIdentities = y })
       <$> f (_iqAllowUnauthenticatedIdentities x)
{-# INLINE iqAllowUnauthenticatedIdentities #-}

-- | Optional key:value pairs mapping provider names to provider app IDs.
iqSupportedLoginProviders
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> CreateIdentityPoolResponse
    -> f CreateIdentityPoolResponse
iqSupportedLoginProviders f x =
    (\y -> x { _iqSupportedLoginProviders = y })
       <$> f (_iqSupportedLoginProviders x)
{-# INLINE iqSupportedLoginProviders #-}

instance FromJSON CreateIdentityPoolResponse

instance AWSRequest CreateIdentityPool where
    type Sv CreateIdentityPool = CognitoIdentity
    type Rs CreateIdentityPool = CreateIdentityPoolResponse

    request = get
    response _ = jsonResponse
