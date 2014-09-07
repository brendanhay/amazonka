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
    , mkDescribeIdentityPool
    -- ** Request lenses
    , dip1IdentityPoolId

    -- * Response
    , DescribeIdentityPoolResponse
    -- ** Response lenses
    , diprsIdentityPoolId
    , diprsIdentityPoolName
    , diprsAllowUnauthenticatedIdentities
    , diprsSupportedLoginProviders
    ) where

import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Input to the DescribeIdentityPool action.
newtype DescribeIdentityPool = DescribeIdentityPool
    { _dip1IdentityPoolId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeIdentityPool' request.
mkDescribeIdentityPool :: Text -- ^ 'dip1IdentityPoolId'
                       -> DescribeIdentityPool
mkDescribeIdentityPool p1 = DescribeIdentityPool
    { _dip1IdentityPoolId = p1
    }

-- | An identity pool ID in the format REGION:GUID.
dip1IdentityPoolId :: Lens' DescribeIdentityPool Text
dip1IdentityPoolId =
    lens _dip1IdentityPoolId (\s a -> s { _dip1IdentityPoolId = a })

instance ToPath DescribeIdentityPool

instance ToQuery DescribeIdentityPool

instance ToHeaders DescribeIdentityPool

instance ToJSON DescribeIdentityPool

-- | An object representing a Cognito identity pool.
data DescribeIdentityPoolResponse = DescribeIdentityPoolResponse
    { _diprsIdentityPoolId :: Text
    , _diprsIdentityPoolName :: Text
    , _diprsAllowUnauthenticatedIdentities :: Bool
    , _diprsSupportedLoginProviders :: Map Text Text
    } deriving (Show, Generic)

-- | An identity pool ID in the format REGION:GUID.
diprsIdentityPoolId :: Lens' DescribeIdentityPoolResponse Text
diprsIdentityPoolId =
    lens _diprsIdentityPoolId (\s a -> s { _diprsIdentityPoolId = a })

-- | A string that you provide.
diprsIdentityPoolName :: Lens' DescribeIdentityPoolResponse Text
diprsIdentityPoolName =
    lens _diprsIdentityPoolName (\s a -> s { _diprsIdentityPoolName = a })

-- | TRUE if the identity pool supports unauthenticated logins.
diprsAllowUnauthenticatedIdentities :: Lens' DescribeIdentityPoolResponse Bool
diprsAllowUnauthenticatedIdentities =
    lens _diprsAllowUnauthenticatedIdentities
         (\s a -> s { _diprsAllowUnauthenticatedIdentities = a })

-- | Optional key:value pairs mapping provider names to provider app IDs.
diprsSupportedLoginProviders :: Lens' DescribeIdentityPoolResponse (Map Text Text)
diprsSupportedLoginProviders =
    lens _diprsSupportedLoginProviders
         (\s a -> s { _diprsSupportedLoginProviders = a })

instance FromJSON DescribeIdentityPoolResponse

instance AWSRequest DescribeIdentityPool where
    type Sv DescribeIdentityPool = CognitoIdentity
    type Rs DescribeIdentityPool = DescribeIdentityPoolResponse

    request = get
    response _ = jsonResponse
