{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketNotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables notifications of specified events for a bucket. For more information about event notifications, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> .
--
--
-- Using this API, you can replace an existing notification configuration. The configuration is an XML file that defines the event types that you want Amazon S3 to publish and the destination where you want Amazon S3 to publish an event notification when it detects an event of the specified type.
--
-- By default, your bucket has no event notifications configured. That is, the notification configuration will be an empty @NotificationConfiguration@ .
--
-- @<NotificationConfiguration>@
--
-- @</NotificationConfiguration>@
--
-- This operation replaces the existing notification configuration with the configuration you include in the request body.
--
-- After Amazon S3 receives this request, it first verifies that any Amazon Simple Notification Service (Amazon SNS) or Amazon Simple Queue Service (Amazon SQS) destination exists, and that the bucket owner has permission to publish to it by sending a test notification. In the case of AWS Lambda destinations, Amazon S3 verifies that the Lambda function permissions grant Amazon S3 permission to invoke the function from the Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Notifications for Amazon S3 Events> .
--
-- You can disable notifications by adding the empty NotificationConfiguration element.
--
-- By default, only the bucket owner can configure notifications on a bucket. However, bucket owners can use a bucket policy to grant permission to other users to set this configuration with @s3:PutBucketNotification@ permission.
--
-- __Responses__
--
-- If the configuration in the request body includes only one @TopicConfiguration@ specifying only the @s3:ReducedRedundancyLostObject@ event type, the response will also include the @x-amz-sns-test-message-id@ header containing the message ID of the test notification sent to the topic.
--
-- The following operation is related to @PutBucketNotificationConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketNotificationConfiguration.html GetBucketNotificationConfiguration>
module Network.AWS.S3.PutBucketNotificationConfiguration
  ( -- * Creating a Request
    putBucketNotificationConfiguration,
    PutBucketNotificationConfiguration,

    -- * Request Lenses
    pbncExpectedBucketOwner,
    pbncBucket,
    pbncNotificationConfiguration,

    -- * Destructuring the Response
    putBucketNotificationConfigurationResponse,
    PutBucketNotificationConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketNotificationConfiguration' smart constructor.
data PutBucketNotificationConfiguration = PutBucketNotificationConfiguration'
  { _pbncExpectedBucketOwner ::
      !(Maybe Text),
    _pbncBucket ::
      !BucketName,
    _pbncNotificationConfiguration ::
      !NotificationConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketNotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbncExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbncBucket' - The name of the bucket.
--
-- * 'pbncNotificationConfiguration' - Undocumented member.
putBucketNotificationConfiguration ::
  -- | 'pbncBucket'
  BucketName ->
  -- | 'pbncNotificationConfiguration'
  NotificationConfiguration ->
  PutBucketNotificationConfiguration
putBucketNotificationConfiguration
  pBucket_
  pNotificationConfiguration_ =
    PutBucketNotificationConfiguration'
      { _pbncExpectedBucketOwner =
          Nothing,
        _pbncBucket = pBucket_,
        _pbncNotificationConfiguration =
          pNotificationConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbncExpectedBucketOwner :: Lens' PutBucketNotificationConfiguration (Maybe Text)
pbncExpectedBucketOwner = lens _pbncExpectedBucketOwner (\s a -> s {_pbncExpectedBucketOwner = a})

-- | The name of the bucket.
pbncBucket :: Lens' PutBucketNotificationConfiguration BucketName
pbncBucket = lens _pbncBucket (\s a -> s {_pbncBucket = a})

-- | Undocumented member.
pbncNotificationConfiguration :: Lens' PutBucketNotificationConfiguration NotificationConfiguration
pbncNotificationConfiguration = lens _pbncNotificationConfiguration (\s a -> s {_pbncNotificationConfiguration = a})

instance AWSRequest PutBucketNotificationConfiguration where
  type
    Rs PutBucketNotificationConfiguration =
      PutBucketNotificationConfigurationResponse
  request = putXML s3
  response = receiveNull PutBucketNotificationConfigurationResponse'

instance Hashable PutBucketNotificationConfiguration

instance NFData PutBucketNotificationConfiguration

instance ToElement PutBucketNotificationConfiguration where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}NotificationConfiguration"
      . _pbncNotificationConfiguration

instance ToHeaders PutBucketNotificationConfiguration where
  toHeaders PutBucketNotificationConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _pbncExpectedBucketOwner]

instance ToPath PutBucketNotificationConfiguration where
  toPath PutBucketNotificationConfiguration' {..} =
    mconcat ["/", toBS _pbncBucket]

instance ToQuery PutBucketNotificationConfiguration where
  toQuery = const (mconcat ["notification"])

-- | /See:/ 'putBucketNotificationConfigurationResponse' smart constructor.
data PutBucketNotificationConfigurationResponse = PutBucketNotificationConfigurationResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'PutBucketNotificationConfigurationResponse' with the minimum fields required to make a request.
putBucketNotificationConfigurationResponse ::
  PutBucketNotificationConfigurationResponse
putBucketNotificationConfigurationResponse =
  PutBucketNotificationConfigurationResponse'

instance NFData PutBucketNotificationConfigurationResponse
