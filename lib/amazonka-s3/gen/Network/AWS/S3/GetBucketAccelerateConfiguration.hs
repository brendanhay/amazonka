{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketAccelerateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the GET operation uses the @accelerate@ subresource to return the Transfer Acceleration state of a bucket, which is either @Enabled@ or @Suspended@ . Amazon S3 Transfer Acceleration is a bucket-level feature that enables you to perform faster data transfers to and from Amazon S3.
--
-- To use this operation, you must have permission to perform the @s3:GetAccelerateConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to your Amazon S3 Resources> in the /Amazon Simple Storage Service Developer Guide/ .
-- You set the Transfer Acceleration state of an existing bucket to @Enabled@ or @Suspended@ by using the <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAccelerateConfiguration.html PutBucketAccelerateConfiguration> operation.
-- A GET @accelerate@ request does not return a state value for a bucket that has no transfer acceleration state. A bucket has no Transfer Acceleration state if a state has never been set on the bucket.
-- For more information about transfer acceleration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Transfer Acceleration> in the Amazon Simple Storage Service Developer Guide.
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAccelerateConfiguration.html PutBucketAccelerateConfiguration>
module Network.AWS.S3.GetBucketAccelerateConfiguration
  ( -- * Creating a request
    GetBucketAccelerateConfiguration (..),
    mkGetBucketAccelerateConfiguration,

    -- ** Request lenses
    gbacBucket,
    gbacExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketAccelerateConfigurationResponse (..),
    mkGetBucketAccelerateConfigurationResponse,

    -- ** Response lenses
    gbacrrsStatus,
    gbacrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketAccelerateConfiguration' smart constructor.
data GetBucketAccelerateConfiguration = GetBucketAccelerateConfiguration'
  { -- | The name of the bucket for which the accelerate configuration is retrieved.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketAccelerateConfiguration' value with any optional fields omitted.
mkGetBucketAccelerateConfiguration ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketAccelerateConfiguration
mkGetBucketAccelerateConfiguration bucket =
  GetBucketAccelerateConfiguration'
    { bucket,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the bucket for which the accelerate configuration is retrieved.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacBucket :: Lens.Lens' GetBucketAccelerateConfiguration Types.BucketName
gbacBucket = Lens.field @"bucket"
{-# DEPRECATED gbacBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacExpectedBucketOwner :: Lens.Lens' GetBucketAccelerateConfiguration (Core.Maybe Types.ExpectedBucketOwner)
gbacExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gbacExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketAccelerateConfiguration where
  type
    Rs GetBucketAccelerateConfiguration =
      GetBucketAccelerateConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("accelerate", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketAccelerateConfigurationResponse'
            Core.<$> (x Core..@? "Status") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketAccelerateConfigurationResponse' smart constructor.
data GetBucketAccelerateConfigurationResponse = GetBucketAccelerateConfigurationResponse'
  { -- | The accelerate configuration of the bucket.
    status :: Core.Maybe Types.BucketAccelerateStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketAccelerateConfigurationResponse' value with any optional fields omitted.
mkGetBucketAccelerateConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBucketAccelerateConfigurationResponse
mkGetBucketAccelerateConfigurationResponse responseStatus =
  GetBucketAccelerateConfigurationResponse'
    { status = Core.Nothing,
      responseStatus
    }

-- | The accelerate configuration of the bucket.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacrrsStatus :: Lens.Lens' GetBucketAccelerateConfigurationResponse (Core.Maybe Types.BucketAccelerateStatus)
gbacrrsStatus = Lens.field @"status"
{-# DEPRECATED gbacrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacrrsResponseStatus :: Lens.Lens' GetBucketAccelerateConfigurationResponse Core.Int
gbacrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbacrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
