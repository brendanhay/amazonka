{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DescribeIdentityUsage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets usage information for an identity, including number of datasets and
-- data usage.
--
-- This API can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_DescribeIdentityUsage.html>
module Network.AWS.CognitoSync.DescribeIdentityUsage
    (
    -- * Request
      DescribeIdentityUsage
    -- ** Request constructor
    , describeIdentityUsage
    -- ** Request lenses
    , diuIdentityPoolId
    , diuIdentityId

    -- * Response
    , DescribeIdentityUsageResponse
    -- ** Response constructor
    , describeIdentityUsageResponse
    -- ** Response lenses
    , diursIdentityUsage
    , diursStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request for information about the usage of an identity pool.
--
-- /See:/ 'describeIdentityUsage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diuIdentityPoolId'
--
-- * 'diuIdentityId'
data DescribeIdentityUsage = DescribeIdentityUsage'
    { _diuIdentityPoolId :: !Text
    , _diuIdentityId     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeIdentityUsage' smart constructor.
describeIdentityUsage :: Text -> Text -> DescribeIdentityUsage
describeIdentityUsage pIdentityPoolId_ pIdentityId_ =
    DescribeIdentityUsage'
    { _diuIdentityPoolId = pIdentityPoolId_
    , _diuIdentityId = pIdentityId_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
diuIdentityPoolId :: Lens' DescribeIdentityUsage Text
diuIdentityPoolId = lens _diuIdentityPoolId (\ s a -> s{_diuIdentityPoolId = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
diuIdentityId :: Lens' DescribeIdentityUsage Text
diuIdentityId = lens _diuIdentityId (\ s a -> s{_diuIdentityId = a});

instance AWSRequest DescribeIdentityUsage where
        type Sv DescribeIdentityUsage = CognitoSync
        type Rs DescribeIdentityUsage =
             DescribeIdentityUsageResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 DescribeIdentityUsageResponse' <$>
                   (x .?> "IdentityUsage") <*> (pure (fromEnum s)))

instance ToHeaders DescribeIdentityUsage where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeIdentityUsage where
        toPath DescribeIdentityUsage'{..}
          = mconcat
              ["/identitypools/", toPath _diuIdentityPoolId,
               "/identities/", toPath _diuIdentityId]

instance ToQuery DescribeIdentityUsage where
        toQuery = const mempty

-- | The response to a successful DescribeIdentityUsage request.
--
-- /See:/ 'describeIdentityUsageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diursIdentityUsage'
--
-- * 'diursStatus'
data DescribeIdentityUsageResponse = DescribeIdentityUsageResponse'
    { _diursIdentityUsage :: !(Maybe IdentityUsage)
    , _diursStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeIdentityUsageResponse' smart constructor.
describeIdentityUsageResponse :: Int -> DescribeIdentityUsageResponse
describeIdentityUsageResponse pStatus_ =
    DescribeIdentityUsageResponse'
    { _diursIdentityUsage = Nothing
    , _diursStatus = pStatus_
    }

-- | Usage information for the identity.
diursIdentityUsage :: Lens' DescribeIdentityUsageResponse (Maybe IdentityUsage)
diursIdentityUsage = lens _diursIdentityUsage (\ s a -> s{_diursIdentityUsage = a});

-- | FIXME: Undocumented member.
diursStatus :: Lens' DescribeIdentityUsageResponse Int
diursStatus = lens _diursStatus (\ s a -> s{_diursStatus = a});
