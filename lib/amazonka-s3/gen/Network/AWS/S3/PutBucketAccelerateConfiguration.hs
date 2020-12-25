{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketAccelerateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the accelerate configuration of an existing bucket. Amazon S3 Transfer Acceleration is a bucket-level feature that enables you to perform faster data transfers to Amazon S3.
--
-- To use this operation, you must have permission to perform the s3:PutAccelerateConfiguration action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- The Transfer Acceleration state of a bucket can be set to one of the following two values:
--
--     * Enabled – Enables accelerated data transfers to the bucket.
--
--
--     * Suspended – Disables accelerated data transfers to the bucket.
--
--
-- The <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAccelerateConfiguration.html GetBucketAccelerateConfiguration> operation returns the transfer acceleration state of a bucket.
-- After setting the Transfer Acceleration state of a bucket to Enabled, it might take up to thirty minutes before the data transfer rates to the bucket increase.
-- The name of the bucket used for Transfer Acceleration must be DNS-compliant and must not contain periods (".").
-- For more information about transfer acceleration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Transfer Acceleration> .
-- The following operations are related to @PutBucketAccelerateConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAccelerateConfiguration.html GetBucketAccelerateConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
module Network.AWS.S3.PutBucketAccelerateConfiguration
  ( -- * Creating a request
    PutBucketAccelerateConfiguration (..),
    mkPutBucketAccelerateConfiguration,

    -- ** Request lenses
    pbacBucket,
    pbacAccelerateConfiguration,
    pbacExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketAccelerateConfigurationResponse (..),
    mkPutBucketAccelerateConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketAccelerateConfiguration' smart constructor.
data PutBucketAccelerateConfiguration = PutBucketAccelerateConfiguration'
  { -- | The name of the bucket for which the accelerate configuration is set.
    bucket :: Types.BucketName,
    -- | Container for setting the transfer acceleration state.
    accelerateConfiguration :: Types.AccelerateConfiguration,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketAccelerateConfiguration' value with any optional fields omitted.
mkPutBucketAccelerateConfiguration ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'accelerateConfiguration'
  Types.AccelerateConfiguration ->
  PutBucketAccelerateConfiguration
mkPutBucketAccelerateConfiguration bucket accelerateConfiguration =
  PutBucketAccelerateConfiguration'
    { bucket,
      accelerateConfiguration,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the bucket for which the accelerate configuration is set.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbacBucket :: Lens.Lens' PutBucketAccelerateConfiguration Types.BucketName
pbacBucket = Lens.field @"bucket"
{-# DEPRECATED pbacBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Container for setting the transfer acceleration state.
--
-- /Note:/ Consider using 'accelerateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbacAccelerateConfiguration :: Lens.Lens' PutBucketAccelerateConfiguration Types.AccelerateConfiguration
pbacAccelerateConfiguration = Lens.field @"accelerateConfiguration"
{-# DEPRECATED pbacAccelerateConfiguration "Use generic-lens or generic-optics with 'accelerateConfiguration' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbacExpectedBucketOwner :: Lens.Lens' PutBucketAccelerateConfiguration (Core.Maybe Types.AccountId)
pbacExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED pbacExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest PutBucketAccelerateConfiguration where
  type
    Rs PutBucketAccelerateConfiguration =
      PutBucketAccelerateConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("accelerate", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveNull PutBucketAccelerateConfigurationResponse'

-- | /See:/ 'mkPutBucketAccelerateConfigurationResponse' smart constructor.
data PutBucketAccelerateConfigurationResponse = PutBucketAccelerateConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketAccelerateConfigurationResponse' value with any optional fields omitted.
mkPutBucketAccelerateConfigurationResponse ::
  PutBucketAccelerateConfigurationResponse
mkPutBucketAccelerateConfigurationResponse =
  PutBucketAccelerateConfigurationResponse'
