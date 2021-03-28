{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketNotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the notification configuration of a bucket.
--
-- If notifications are not enabled on the bucket, the operation returns an empty @NotificationConfiguration@ element.
-- By default, you must be the bucket owner to read the notification configuration of a bucket. However, the bucket owner can use a bucket policy to grant permission to other users to read this configuration with the @s3:GetBucketNotification@ permission.
-- For more information about setting and reading the notification configuration on a bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Setting Up Notification of Bucket Events> . For more information about bucket policies, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies> .
-- The following operation is related to @GetBucketNotification@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketNotification.html PutBucketNotification> 
--
--
module Network.AWS.S3.GetBucketNotificationConfiguration
    (
    -- * Creating a request
      GetBucketNotificationConfiguration (..)
    , mkGetBucketNotificationConfiguration
    -- ** Request lenses
    , gbncBucket
    , gbncExpectedBucketOwner

     -- * Destructuring the response
    , Types.NotificationConfiguration (..)
    , Types.mkNotificationConfiguration
    -- ** Response lenses
    , Types.ncLambdaFunctionConfigurations
    , Types.ncQueueConfigurations
    , Types.ncTopicConfigurations
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketNotificationConfiguration' smart constructor.
data GetBucketNotificationConfiguration = GetBucketNotificationConfiguration'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket for which to get the notification configuration.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketNotificationConfiguration' value with any optional fields omitted.
mkGetBucketNotificationConfiguration
    :: Types.BucketName -- ^ 'bucket'
    -> GetBucketNotificationConfiguration
mkGetBucketNotificationConfiguration bucket
  = GetBucketNotificationConfiguration'{bucket,
                                        expectedBucketOwner = Core.Nothing}

-- | The name of the bucket for which to get the notification configuration.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbncBucket :: Lens.Lens' GetBucketNotificationConfiguration Types.BucketName
gbncBucket = Lens.field @"bucket"
{-# INLINEABLE gbncBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbncExpectedBucketOwner :: Lens.Lens' GetBucketNotificationConfiguration (Core.Maybe Types.ExpectedBucketOwner)
gbncExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE gbncExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery GetBucketNotificationConfiguration where
        toQuery GetBucketNotificationConfiguration{..}
          = Core.toQueryPair "notification" ("" :: Core.Text)

instance Core.ToHeaders GetBucketNotificationConfiguration where
        toHeaders GetBucketNotificationConfiguration{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest GetBucketNotificationConfiguration where
        type Rs GetBucketNotificationConfiguration =
             Types.NotificationConfiguration
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveXML (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
