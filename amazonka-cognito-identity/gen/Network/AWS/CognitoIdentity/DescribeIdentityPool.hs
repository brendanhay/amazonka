{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.DescribeIdentityPool
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a particular identity pool, including the pool name,
-- ID description, creation date, and current number of users.
--
-- You must use AWS Developer credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_DescribeIdentityPool.html>
module Network.AWS.CognitoIdentity.DescribeIdentityPool
    (
    -- * Request
      DescribeIdentityPool
    -- ** Request constructor
    , describeIdentityPool
    -- ** Request lenses
    , diprqIdentityPoolId

    -- * Response
    , IdentityPool
    -- ** Response constructor
    , identityPool
    -- ** Response lenses
    , ipSupportedLoginProviders
    , ipDeveloperProviderName
    , ipOpenIdConnectProviderARNs
    , ipIdentityPoolId
    , ipIdentityPoolName
    , ipAllowUnauthenticatedIdentities
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the DescribeIdentityPool action.
--
-- /See:/ 'describeIdentityPool' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diprqIdentityPoolId'
newtype DescribeIdentityPool = DescribeIdentityPool'
    { _diprqIdentityPoolId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeIdentityPool' smart constructor.
describeIdentityPool :: Text -> DescribeIdentityPool
describeIdentityPool pIdentityPoolId =
    DescribeIdentityPool'
    { _diprqIdentityPoolId = pIdentityPoolId
    }

-- | An identity pool ID in the format REGION:GUID.
diprqIdentityPoolId :: Lens' DescribeIdentityPool Text
diprqIdentityPoolId = lens _diprqIdentityPoolId (\ s a -> s{_diprqIdentityPoolId = a});

instance AWSRequest DescribeIdentityPool where
        type Sv DescribeIdentityPool = CognitoIdentity
        type Rs DescribeIdentityPool = IdentityPool
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders DescribeIdentityPool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.DescribeIdentityPool" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeIdentityPool where
        toJSON DescribeIdentityPool'{..}
          = object ["IdentityPoolId" .= _diprqIdentityPoolId]

instance ToPath DescribeIdentityPool where
        toPath = const "/"

instance ToQuery DescribeIdentityPool where
        toQuery = const mempty
