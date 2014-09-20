{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.DescribeIdentityPool
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
module Network.AWS.CognitoIdentity.DescribeIdentityPool
    (
    -- * Request
      DescribeIdentityPool
    -- ** Request constructor
    , describeIdentityPool
    -- ** Request lenses
    , dip1IdentityPoolId

    -- * Response
    , DescribeIdentityPoolResponse
    -- ** Response constructor
    , describeIdentityPoolResponse
    -- ** Response lenses
    , diprIdentityPoolId
    , diprIdentityPoolName
    , diprAllowUnauthenticatedIdentities
    , diprSupportedLoginProviders
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Input to the DescribeIdentityPool action.
newtype DescribeIdentityPool = DescribeIdentityPool
    { _dip1IdentityPoolId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeIdentityPool' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityPoolId ::@ @Text@
--
describeIdentityPool :: Text -- ^ 'dip1IdentityPoolId'
                     -> DescribeIdentityPool
describeIdentityPool p1 = DescribeIdentityPool
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
    { _diprIdentityPoolId :: Text
    , _diprIdentityPoolName :: Text
    , _diprAllowUnauthenticatedIdentities :: !Bool
    , _diprSupportedLoginProviders :: Map Text Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeIdentityPoolResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityPoolId ::@ @Text@
--
-- * @IdentityPoolName ::@ @Text@
--
-- * @AllowUnauthenticatedIdentities ::@ @Bool@
--
-- * @SupportedLoginProviders ::@ @Map Text Text@
--
describeIdentityPoolResponse :: Text -- ^ 'diprIdentityPoolId'
                             -> Text -- ^ 'diprIdentityPoolName'
                             -> Bool -- ^ 'diprAllowUnauthenticatedIdentities'
                             -> DescribeIdentityPoolResponse
describeIdentityPoolResponse p1 p2 p3 = DescribeIdentityPoolResponse
    { _diprIdentityPoolId = p1
    , _diprIdentityPoolName = p2
    , _diprAllowUnauthenticatedIdentities = p3
    , _diprSupportedLoginProviders = mempty
    }

-- | An identity pool ID in the format REGION:GUID.
diprIdentityPoolId :: Lens' DescribeIdentityPoolResponse Text
diprIdentityPoolId =
    lens _diprIdentityPoolId (\s a -> s { _diprIdentityPoolId = a })

-- | A string that you provide.
diprIdentityPoolName :: Lens' DescribeIdentityPoolResponse Text
diprIdentityPoolName =
    lens _diprIdentityPoolName (\s a -> s { _diprIdentityPoolName = a })

-- | TRUE if the identity pool supports unauthenticated logins.
diprAllowUnauthenticatedIdentities :: Lens' DescribeIdentityPoolResponse Bool
diprAllowUnauthenticatedIdentities =
    lens _diprAllowUnauthenticatedIdentities
         (\s a -> s { _diprAllowUnauthenticatedIdentities = a })

-- | Optional key:value pairs mapping provider names to provider app IDs.
diprSupportedLoginProviders :: Lens' DescribeIdentityPoolResponse (Map Text Text)
diprSupportedLoginProviders =
    lens _diprSupportedLoginProviders
         (\s a -> s { _diprSupportedLoginProviders = a })

instance FromJSON DescribeIdentityPoolResponse

instance AWSRequest DescribeIdentityPool where
    type Sv DescribeIdentityPool = CognitoIdentity
    type Rs DescribeIdentityPool = DescribeIdentityPoolResponse

    request = get
    response _ = jsonResponse
