{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketNotificationConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the notification configuration of a bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketNotificationConfiguration.html AWS API Reference> for GetBucketNotificationConfiguration.
module Network.AWS.S3.GetBucketNotificationConfiguration
    (
    -- * Creating a Request
      getBucketNotificationConfiguration
    , GetBucketNotificationConfiguration
    -- * Request Lenses
    , gbncBucket

    -- * Destructuring the Response
    , notificationConfiguration
    , NotificationConfiguration
    -- * Response Lenses
    , ncQueueConfigurations
    , ncTopicConfigurations
    , ncLambdaFunctionConfigurations
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketNotificationConfiguration' smart constructor.
newtype GetBucketNotificationConfiguration = GetBucketNotificationConfiguration'
    { _gbncBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketNotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbncBucket'
getBucketNotificationConfiguration
    :: BucketName -- ^ 'gbncBucket'
    -> GetBucketNotificationConfiguration
getBucketNotificationConfiguration pBucket_ =
    GetBucketNotificationConfiguration'
    { _gbncBucket = pBucket_
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
          = mconcat ["/", toBS _gbncBucket]

instance ToQuery GetBucketNotificationConfiguration
         where
        toQuery = const (mconcat ["notification"])
