{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListBucketInventoryConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of inventory configurations for the bucket. You can have up to 1,000 analytics configurations per bucket.
--
-- This operation supports list pagination and does not return more than 100 configurations at a time. Always check the @IsTruncated@ element in the response. If there are no more configurations to list, @IsTruncated@ is set to false. If there are more configurations to list, @IsTruncated@ is set to true, and there is a value in @NextContinuationToken@ . You use the @NextContinuationToken@ value to continue the pagination of the list by passing the value in continuation-token in the request to @GET@ the next page.
-- To use this operation, you must have permissions to perform the @s3:GetInventoryConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about the Amazon S3 inventory feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory>
-- The following operations are related to @ListBucketInventoryConfigurations@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration>
module Network.AWS.S3.ListBucketInventoryConfigurations
  ( -- * Creating a request
    ListBucketInventoryConfigurations (..),
    mkListBucketInventoryConfigurations,

    -- ** Request lenses
    lbicBucket,
    lbicContinuationToken,
    lbicExpectedBucketOwner,

    -- * Destructuring the response
    ListBucketInventoryConfigurationsResponse (..),
    mkListBucketInventoryConfigurationsResponse,

    -- ** Response lenses
    lbicrrsContinuationToken,
    lbicrrsInventoryConfigurationList,
    lbicrrsIsTruncated,
    lbicrrsNextContinuationToken,
    lbicrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkListBucketInventoryConfigurations' smart constructor.
data ListBucketInventoryConfigurations = ListBucketInventoryConfigurations'
  { -- | The name of the bucket containing the inventory configurations to retrieve.
    bucket :: Types.BucketName,
    -- | The marker used to continue an inventory configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
    continuationToken :: Core.Maybe Types.ContinuationToken,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBucketInventoryConfigurations' value with any optional fields omitted.
mkListBucketInventoryConfigurations ::
  -- | 'bucket'
  Types.BucketName ->
  ListBucketInventoryConfigurations
mkListBucketInventoryConfigurations bucket =
  ListBucketInventoryConfigurations'
    { bucket,
      continuationToken = Core.Nothing,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the bucket containing the inventory configurations to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicBucket :: Lens.Lens' ListBucketInventoryConfigurations Types.BucketName
lbicBucket = Lens.field @"bucket"
{-# DEPRECATED lbicBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The marker used to continue an inventory configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicContinuationToken :: Lens.Lens' ListBucketInventoryConfigurations (Core.Maybe Types.ContinuationToken)
lbicContinuationToken = Lens.field @"continuationToken"
{-# DEPRECATED lbicContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicExpectedBucketOwner :: Lens.Lens' ListBucketInventoryConfigurations (Core.Maybe Types.ExpectedBucketOwner)
lbicExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED lbicExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest ListBucketInventoryConfigurations where
  type
    Rs ListBucketInventoryConfigurations =
      ListBucketInventoryConfigurationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery =
          Core.toQueryValue "continuation-token" Core.<$> continuationToken
            Core.<> (Core.pure ("inventory", "")),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListBucketInventoryConfigurationsResponse'
            Core.<$> (x Core..@? "ContinuationToken")
            Core.<*> (x Core..@? "InventoryConfiguration")
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "NextContinuationToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListBucketInventoryConfigurationsResponse' smart constructor.
data ListBucketInventoryConfigurationsResponse = ListBucketInventoryConfigurationsResponse'
  { -- | If sent in the request, the marker that is used as a starting point for this inventory configuration list response.
    continuationToken :: Core.Maybe Types.Token,
    -- | The list of inventory configurations for a bucket.
    inventoryConfigurationList :: Core.Maybe [Types.InventoryConfiguration],
    -- | Tells whether the returned list of inventory configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken is provided for a subsequent request.
    isTruncated :: Core.Maybe Core.Bool,
    -- | The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
    nextContinuationToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBucketInventoryConfigurationsResponse' value with any optional fields omitted.
mkListBucketInventoryConfigurationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListBucketInventoryConfigurationsResponse
mkListBucketInventoryConfigurationsResponse responseStatus =
  ListBucketInventoryConfigurationsResponse'
    { continuationToken =
        Core.Nothing,
      inventoryConfigurationList = Core.Nothing,
      isTruncated = Core.Nothing,
      nextContinuationToken = Core.Nothing,
      responseStatus
    }

-- | If sent in the request, the marker that is used as a starting point for this inventory configuration list response.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicrrsContinuationToken :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Core.Maybe Types.Token)
lbicrrsContinuationToken = Lens.field @"continuationToken"
{-# DEPRECATED lbicrrsContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The list of inventory configurations for a bucket.
--
-- /Note:/ Consider using 'inventoryConfigurationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicrrsInventoryConfigurationList :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Core.Maybe [Types.InventoryConfiguration])
lbicrrsInventoryConfigurationList = Lens.field @"inventoryConfigurationList"
{-# DEPRECATED lbicrrsInventoryConfigurationList "Use generic-lens or generic-optics with 'inventoryConfigurationList' instead." #-}

-- | Tells whether the returned list of inventory configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken is provided for a subsequent request.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicrrsIsTruncated :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Core.Maybe Core.Bool)
lbicrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lbicrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
--
-- /Note:/ Consider using 'nextContinuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicrrsNextContinuationToken :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Core.Maybe Types.NextToken)
lbicrrsNextContinuationToken = Lens.field @"nextContinuationToken"
{-# DEPRECATED lbicrrsNextContinuationToken "Use generic-lens or generic-optics with 'nextContinuationToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicrrsResponseStatus :: Lens.Lens' ListBucketInventoryConfigurationsResponse Core.Int
lbicrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lbicrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
