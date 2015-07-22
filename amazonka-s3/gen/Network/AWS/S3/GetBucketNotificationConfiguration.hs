{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketNotificationConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the notification configuration of a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketNotificationConfiguration.html>
module Network.AWS.S3.GetBucketNotificationConfiguration
    (
    -- * Request
      GetBucketNotificationConfiguration
    -- ** Request constructor
    , getBucketNotificationConfiguration
    -- ** Request lenses
    , gbncrqBucket

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
-- * 'gbncrqBucket'
newtype GetBucketNotificationConfiguration = GetBucketNotificationConfiguration'
    { _gbncrqBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketNotificationConfiguration' smart constructor.
getBucketNotificationConfiguration :: BucketName -> GetBucketNotificationConfiguration
getBucketNotificationConfiguration pBucket =
    GetBucketNotificationConfiguration'
    { _gbncrqBucket = pBucket
    }

-- | Name of the buket to get the notification configuration for.
gbncrqBucket :: Lens' GetBucketNotificationConfiguration BucketName
gbncrqBucket = lens _gbncrqBucket (\ s a -> s{_gbncrqBucket = a});

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
          = mconcat ["/", toText _gbncrqBucket]

instance ToQuery GetBucketNotificationConfiguration
         where
        toQuery = const (mconcat ["notification"])
