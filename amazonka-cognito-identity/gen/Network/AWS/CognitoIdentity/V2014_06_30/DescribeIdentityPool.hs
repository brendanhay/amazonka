{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.DescribeIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets details about a particular identity pool, including the pool name, ID
-- description, creation date, and current number of users.
-- DescribeIdentityPool The following are an example request and response for
-- the DescribeIdentityPool operation. { "IdentityPoolId":
-- "us-east-1:af4311ca-835e-4b49-814c-2290EXAMPLE1" } {
-- "IdentityPoolDescription": "My identity pool", "IdentityPoolId":
-- "us-east-1:af4311ca-835e-4b49-814c-2290EXAMPLE1", "IdentityPoolName":
-- "MyIdentityPool", "SupportedLoginProviders": { "www.amazon.com":
-- "Amazon_App_ID", "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID" }, "Unauthenticated": true }.
module Network.AWS.CognitoIdentity.V2014_06_30.DescribeIdentityPool
    (
    -- * Request
      DescribeIdentityPool
    -- ** Request constructor
    , describeIdentityPool
    -- ** Request lenses
    , dipjIdentityPoolId

    -- * Response
    , DescribeIdentityPoolResponse
    -- ** Response lenses
    , isIdentityPoolId
    , isIdentityPoolName
    , isAllowUnauthenticatedIdentities
    , isSupportedLoginProviders
    ) where

import           Network.AWS.CognitoIdentity.V2014_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeIdentityPool' request.
describeIdentityPool :: Text -- ^ 'dipjIdentityPoolId'
                     -> DescribeIdentityPool
describeIdentityPool p1 = DescribeIdentityPool
    { _dipjIdentityPoolId = p1
    }

data DescribeIdentityPool = DescribeIdentityPool
    { _dipjIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    } deriving (Show, Generic)

-- | An identity pool ID in the format REGION:GUID.
dipjIdentityPoolId
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeIdentityPool
    -> f DescribeIdentityPool
dipjIdentityPoolId f x =
    (\y -> x { _dipjIdentityPoolId = y })
       <$> f (_dipjIdentityPoolId x)
{-# INLINE dipjIdentityPoolId #-}

instance ToPath DescribeIdentityPool

instance ToQuery DescribeIdentityPool

instance ToHeaders DescribeIdentityPool

instance ToJSON DescribeIdentityPool

data DescribeIdentityPoolResponse = DescribeIdentityPoolResponse
    { _isIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    , _isIdentityPoolName :: Text
      -- ^ A string that you provide.
    , _isAllowUnauthenticatedIdentities :: Bool
      -- ^ TRUE if the identity pool supports unauthenticated logins.
    , _isSupportedLoginProviders :: Map Text Text
      -- ^ Optional key:value pairs mapping provider names to provider app
      -- IDs.
    } deriving (Show, Generic)

-- | An identity pool ID in the format REGION:GUID.
isIdentityPoolId
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeIdentityPoolResponse
    -> f DescribeIdentityPoolResponse
isIdentityPoolId f x =
    (\y -> x { _isIdentityPoolId = y })
       <$> f (_isIdentityPoolId x)
{-# INLINE isIdentityPoolId #-}

-- | A string that you provide.
isIdentityPoolName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeIdentityPoolResponse
    -> f DescribeIdentityPoolResponse
isIdentityPoolName f x =
    (\y -> x { _isIdentityPoolName = y })
       <$> f (_isIdentityPoolName x)
{-# INLINE isIdentityPoolName #-}

-- | TRUE if the identity pool supports unauthenticated logins.
isAllowUnauthenticatedIdentities
    :: Functor f
    => (Bool
    -> f (Bool))
    -> DescribeIdentityPoolResponse
    -> f DescribeIdentityPoolResponse
isAllowUnauthenticatedIdentities f x =
    (\y -> x { _isAllowUnauthenticatedIdentities = y })
       <$> f (_isAllowUnauthenticatedIdentities x)
{-# INLINE isAllowUnauthenticatedIdentities #-}

-- | Optional key:value pairs mapping provider names to provider app IDs.
isSupportedLoginProviders
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> DescribeIdentityPoolResponse
    -> f DescribeIdentityPoolResponse
isSupportedLoginProviders f x =
    (\y -> x { _isSupportedLoginProviders = y })
       <$> f (_isSupportedLoginProviders x)
{-# INLINE isSupportedLoginProviders #-}

instance FromJSON DescribeIdentityPoolResponse

instance AWSRequest DescribeIdentityPool where
    type Sv DescribeIdentityPool = CognitoIdentity
    type Rs DescribeIdentityPool = DescribeIdentityPoolResponse

    request = get
    response _ = jsonResponse
