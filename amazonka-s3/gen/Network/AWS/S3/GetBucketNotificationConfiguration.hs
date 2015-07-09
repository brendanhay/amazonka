{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketNotificationConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Returns the notification configuration of a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketNotificationConfiguration.html>
module Network.AWS.S3.GetBucketNotificationConfiguration
    (
    -- * Request
      GetBucketNotificationConfiguration
    -- ** Request constructor
    , getBucketNotificationConfiguration
    -- ** Request lenses
    , gbncBucket

    -- * Response
    , NotificationConfiguration
    -- ** Response constructor
    , notificationConfiguration
    -- ** Response lenses
    , ncQueueConfigurations
    , ncTopicConfigurations
    , ncLambdaFunctionConfigurations
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketNotificationConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbncBucket'
newtype GetBucketNotificationConfiguration = GetBucketNotificationConfiguration'
    { _gbncBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketNotificationConfiguration' smart constructor.
getBucketNotificationConfiguration :: BucketName -> GetBucketNotificationConfiguration
getBucketNotificationConfiguration pBucket =
    GetBucketNotificationConfiguration'
    { _gbncBucket = pBucket
    }

-- | Name of the buket to get the notification configuration for.
gbncBucket :: Lens' GetBucketNotificationConfiguration BucketName
gbncBucket = lens _gbncBucket (\ s a -> s{_gbncBucket = a});

instance AWSRequest
         GetBucketNotificationConfiguration where
        type Sv GetBucketNotificationConfiguration = S3
        type Rs GetBucketNotificationConfiguration =
             NotificationConfiguration
        request = get
        response = receiveXML (\ s h x -> parseXML x)

instance ToHeaders GetBucketNotificationConfiguration
         where
        toHeaders = const mempty

instance ToPath GetBucketNotificationConfiguration
         where
        toPath GetBucketNotificationConfiguration'{..}
          = mconcat ["/", toText _gbncBucket]

instance ToQuery GetBucketNotificationConfiguration
         where
        toQuery = const (mconcat ["notification"])
