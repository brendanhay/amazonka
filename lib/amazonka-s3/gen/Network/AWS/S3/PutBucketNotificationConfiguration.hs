{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.S3.PutBucketNotificationConfiguration
    (
    -- * Creating a request
      PutBucketNotificationConfiguration (..)
    , mkPutBucketNotificationConfiguration
    -- ** Request lenses
    , pbncBucket
    , pbncNotificationConfiguration
    , pbncExpectedBucketOwner

    -- * Destructuring the response
    , PutBucketNotificationConfigurationResponse (..)
    , mkPutBucketNotificationConfigurationResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketNotificationConfiguration' smart constructor.
data PutBucketNotificationConfiguration = PutBucketNotificationConfiguration'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket.
  , notificationConfiguration :: Types.NotificationConfiguration
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketNotificationConfiguration' value with any optional fields omitted.
mkPutBucketNotificationConfiguration
    :: Types.BucketName -- ^ 'bucket'
    -> Types.NotificationConfiguration -- ^ 'notificationConfiguration'
    -> PutBucketNotificationConfiguration
mkPutBucketNotificationConfiguration bucket
  notificationConfiguration
  = PutBucketNotificationConfiguration'{bucket,
                                        notificationConfiguration,
                                        expectedBucketOwner = Core.Nothing}

-- | The name of the bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbncBucket :: Lens.Lens' PutBucketNotificationConfiguration Types.BucketName
pbncBucket = Lens.field @"bucket"
{-# INLINEABLE pbncBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbncNotificationConfiguration :: Lens.Lens' PutBucketNotificationConfiguration Types.NotificationConfiguration
pbncNotificationConfiguration = Lens.field @"notificationConfiguration"
{-# INLINEABLE pbncNotificationConfiguration #-}
{-# DEPRECATED notificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbncExpectedBucketOwner :: Lens.Lens' PutBucketNotificationConfiguration (Core.Maybe Types.ExpectedBucketOwner)
pbncExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE pbncExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery PutBucketNotificationConfiguration where
        toQuery PutBucketNotificationConfiguration{..}
          = Core.toQueryPair "notification" ("" :: Core.Text)

instance Core.ToHeaders PutBucketNotificationConfiguration where
        toHeaders PutBucketNotificationConfiguration{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest PutBucketNotificationConfiguration where
        type Rs PutBucketNotificationConfiguration =
             PutBucketNotificationConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull PutBucketNotificationConfigurationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBucketNotificationConfigurationResponse' smart constructor.
data PutBucketNotificationConfigurationResponse = PutBucketNotificationConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketNotificationConfigurationResponse' value with any optional fields omitted.
mkPutBucketNotificationConfigurationResponse
    :: PutBucketNotificationConfigurationResponse
mkPutBucketNotificationConfigurationResponse
  = PutBucketNotificationConfigurationResponse'
