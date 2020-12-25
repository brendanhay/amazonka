{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an analytics configuration for the bucket (specified by the analytics configuration ID).
--
-- To use this operation, you must have permissions to perform the @s3:PutAnalyticsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about the Amazon S3 analytics feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis> .
-- The following operations are related to @DeleteBucketAnalyticsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAnalyticsConfiguration.html GetBucketAnalyticsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration>
module Network.AWS.S3.DeleteBucketAnalyticsConfiguration
  ( -- * Creating a request
    DeleteBucketAnalyticsConfiguration (..),
    mkDeleteBucketAnalyticsConfiguration,

    -- ** Request lenses
    dbacBucket,
    dbacId,
    dbacExpectedBucketOwner,

    -- * Destructuring the response
    DeleteBucketAnalyticsConfigurationResponse (..),
    mkDeleteBucketAnalyticsConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteBucketAnalyticsConfiguration' smart constructor.
data DeleteBucketAnalyticsConfiguration = DeleteBucketAnalyticsConfiguration'
  { -- | The name of the bucket from which an analytics configuration is deleted.
    bucket :: Types.BucketName,
    -- | The ID that identifies the analytics configuration.
    id :: Types.Id,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketAnalyticsConfiguration' value with any optional fields omitted.
mkDeleteBucketAnalyticsConfiguration ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'id'
  Types.Id ->
  DeleteBucketAnalyticsConfiguration
mkDeleteBucketAnalyticsConfiguration bucket id =
  DeleteBucketAnalyticsConfiguration'
    { bucket,
      id,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the bucket from which an analytics configuration is deleted.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbacBucket :: Lens.Lens' DeleteBucketAnalyticsConfiguration Types.BucketName
dbacBucket = Lens.field @"bucket"
{-# DEPRECATED dbacBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID that identifies the analytics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbacId :: Lens.Lens' DeleteBucketAnalyticsConfiguration Types.Id
dbacId = Lens.field @"id"
{-# DEPRECATED dbacId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbacExpectedBucketOwner :: Lens.Lens' DeleteBucketAnalyticsConfiguration (Core.Maybe Types.ExpectedBucketOwner)
dbacExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED dbacExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest DeleteBucketAnalyticsConfiguration where
  type
    Rs DeleteBucketAnalyticsConfiguration =
      DeleteBucketAnalyticsConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery =
          Core.toQueryValue "id" id Core.<> (Core.pure ("analytics", "")),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveNull DeleteBucketAnalyticsConfigurationResponse'

-- | /See:/ 'mkDeleteBucketAnalyticsConfigurationResponse' smart constructor.
data DeleteBucketAnalyticsConfigurationResponse = DeleteBucketAnalyticsConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketAnalyticsConfigurationResponse' value with any optional fields omitted.
mkDeleteBucketAnalyticsConfigurationResponse ::
  DeleteBucketAnalyticsConfigurationResponse
mkDeleteBucketAnalyticsConfigurationResponse =
  DeleteBucketAnalyticsConfigurationResponse'
