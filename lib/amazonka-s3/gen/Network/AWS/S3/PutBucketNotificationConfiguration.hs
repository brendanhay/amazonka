{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- Using this API, you can replace an existing notification configuration. The configuration is an XML file that defines the event types that you want Amazon S3 to publish and the destination where you want Amazon S3 to publish an event notification when it detects an event of the specified type.
-- By default, your bucket has no event notifications configured. That is, the notification configuration will be an empty @NotificationConfiguration@ .
-- @<NotificationConfiguration>@
-- @</NotificationConfiguration>@
-- This operation replaces the existing notification configuration with the configuration you include in the request body.
-- After Amazon S3 receives this request, it first verifies that any Amazon Simple Notification Service (Amazon SNS) or Amazon Simple Queue Service (Amazon SQS) destination exists, and that the bucket owner has permission to publish to it by sending a test notification. In the case of AWS Lambda destinations, Amazon S3 verifies that the Lambda function permissions grant Amazon S3 permission to invoke the function from the Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Notifications for Amazon S3 Events> .
-- You can disable notifications by adding the empty NotificationConfiguration element.
-- By default, only the bucket owner can configure notifications on a bucket. However, bucket owners can use a bucket policy to grant permission to other users to set this configuration with @s3:PutBucketNotification@ permission.
-- __Responses__
-- If the configuration in the request body includes only one @TopicConfiguration@ specifying only the @s3:ReducedRedundancyLostObject@ event type, the response will also include the @x-amz-sns-test-message-id@ header containing the message ID of the test notification sent to the topic.
-- The following operation is related to @PutBucketNotificationConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketNotificationConfiguration.html GetBucketNotificationConfiguration>
module Network.AWS.S3.PutBucketNotificationConfiguration
  ( -- * Creating a request
    PutBucketNotificationConfiguration (..),
    mkPutBucketNotificationConfiguration,

    -- ** Request lenses
    pbncNotificationConfiguration,
    pbncBucket,
    pbncExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketNotificationConfigurationResponse (..),
    mkPutBucketNotificationConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketNotificationConfiguration' smart constructor.
data PutBucketNotificationConfiguration = PutBucketNotificationConfiguration'
  { notificationConfiguration :: NotificationConfiguration,
    -- | The name of the bucket.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketNotificationConfiguration' with the minimum fields required to make a request.
--
-- * 'notificationConfiguration' -
-- * 'bucket' - The name of the bucket.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutBucketNotificationConfiguration ::
  -- | 'notificationConfiguration'
  NotificationConfiguration ->
  -- | 'bucket'
  BucketName ->
  PutBucketNotificationConfiguration
mkPutBucketNotificationConfiguration
  pNotificationConfiguration_
  pBucket_ =
    PutBucketNotificationConfiguration'
      { notificationConfiguration =
          pNotificationConfiguration_,
        bucket = pBucket_,
        expectedBucketOwner = Lude.Nothing
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbncNotificationConfiguration :: Lens.Lens' PutBucketNotificationConfiguration NotificationConfiguration
pbncNotificationConfiguration = Lens.lens (notificationConfiguration :: PutBucketNotificationConfiguration -> NotificationConfiguration) (\s a -> s {notificationConfiguration = a} :: PutBucketNotificationConfiguration)
{-# DEPRECATED pbncNotificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead." #-}

-- | The name of the bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbncBucket :: Lens.Lens' PutBucketNotificationConfiguration BucketName
pbncBucket = Lens.lens (bucket :: PutBucketNotificationConfiguration -> BucketName) (\s a -> s {bucket = a} :: PutBucketNotificationConfiguration)
{-# DEPRECATED pbncBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbncExpectedBucketOwner :: Lens.Lens' PutBucketNotificationConfiguration (Lude.Maybe Lude.Text)
pbncExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketNotificationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketNotificationConfiguration)
{-# DEPRECATED pbncExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutBucketNotificationConfiguration where
  type
    Rs PutBucketNotificationConfiguration =
      PutBucketNotificationConfigurationResponse
  request = Req.putXML s3Service
  response =
    Res.receiveNull PutBucketNotificationConfigurationResponse'

instance Lude.ToElement PutBucketNotificationConfiguration where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}NotificationConfiguration"
      Lude.. notificationConfiguration

instance Lude.ToHeaders PutBucketNotificationConfiguration where
  toHeaders PutBucketNotificationConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath PutBucketNotificationConfiguration where
  toPath PutBucketNotificationConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketNotificationConfiguration where
  toQuery = Lude.const (Lude.mconcat ["notification"])

-- | /See:/ 'mkPutBucketNotificationConfigurationResponse' smart constructor.
data PutBucketNotificationConfigurationResponse = PutBucketNotificationConfigurationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketNotificationConfigurationResponse' with the minimum fields required to make a request.
mkPutBucketNotificationConfigurationResponse ::
  PutBucketNotificationConfigurationResponse
mkPutBucketNotificationConfigurationResponse =
  PutBucketNotificationConfigurationResponse'
