{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DescribeIdentityPoolUsage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets usage details (for example, data storage) about a particular
-- identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_DescribeIdentityPoolUsage.html>
module Network.AWS.CognitoSync.DescribeIdentityPoolUsage
    (
    -- * Request
      DescribeIdentityPoolUsage
    -- ** Request constructor
    , describeIdentityPoolUsage
    -- ** Request lenses
    , dipuIdentityPoolId

    -- * Response
    , DescribeIdentityPoolUsageResponse
    -- ** Response constructor
    , describeIdentityPoolUsageResponse
    -- ** Response lenses
    , dipursIdentityPoolUsage
    , dipursStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request for usage information about the identity pool.
--
-- /See:/ 'describeIdentityPoolUsage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dipuIdentityPoolId'
newtype DescribeIdentityPoolUsage = DescribeIdentityPoolUsage'
    { _dipuIdentityPoolId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeIdentityPoolUsage' smart constructor.
describeIdentityPoolUsage :: Text -> DescribeIdentityPoolUsage
describeIdentityPoolUsage pIdentityPoolId_ =
    DescribeIdentityPoolUsage'
    { _dipuIdentityPoolId = pIdentityPoolId_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
dipuIdentityPoolId :: Lens' DescribeIdentityPoolUsage Text
dipuIdentityPoolId = lens _dipuIdentityPoolId (\ s a -> s{_dipuIdentityPoolId = a});

instance AWSRequest DescribeIdentityPoolUsage where
        type Sv DescribeIdentityPoolUsage = CognitoSync
        type Rs DescribeIdentityPoolUsage =
             DescribeIdentityPoolUsageResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 DescribeIdentityPoolUsageResponse' <$>
                   (x .?> "IdentityPoolUsage") <*> (pure (fromEnum s)))

instance ToHeaders DescribeIdentityPoolUsage where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeIdentityPoolUsage where
        toPath DescribeIdentityPoolUsage'{..}
          = mconcat
              ["/identitypools/", toPath _dipuIdentityPoolId]

instance ToQuery DescribeIdentityPoolUsage where
        toQuery = const mempty

-- | Response to a successful DescribeIdentityPoolUsage request.
--
-- /See:/ 'describeIdentityPoolUsageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dipursIdentityPoolUsage'
--
-- * 'dipursStatus'
data DescribeIdentityPoolUsageResponse = DescribeIdentityPoolUsageResponse'
    { _dipursIdentityPoolUsage :: !(Maybe IdentityPoolUsage)
    , _dipursStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeIdentityPoolUsageResponse' smart constructor.
describeIdentityPoolUsageResponse :: Int -> DescribeIdentityPoolUsageResponse
describeIdentityPoolUsageResponse pStatus_ =
    DescribeIdentityPoolUsageResponse'
    { _dipursIdentityPoolUsage = Nothing
    , _dipursStatus = pStatus_
    }

-- | Information about the usage of the identity pool.
dipursIdentityPoolUsage :: Lens' DescribeIdentityPoolUsageResponse (Maybe IdentityPoolUsage)
dipursIdentityPoolUsage = lens _dipursIdentityPoolUsage (\ s a -> s{_dipursIdentityPoolUsage = a});

-- | FIXME: Undocumented member.
dipursStatus :: Lens' DescribeIdentityPoolUsageResponse Int
dipursStatus = lens _dipursStatus (\ s a -> s{_dipursStatus = a});
