{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketInventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an inventory configuration (identified by the inventory configuration ID) from the bucket.
--
-- To use this operation, you must have permissions to perform the @s3:GetInventoryConfiguration@ action. The bucket owner has this permission by default and can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about the Amazon S3 inventory feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory> .
-- The following operations are related to @GetBucketInventoryConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration>
module Network.AWS.S3.GetBucketInventoryConfiguration
  ( -- * Creating a request
    GetBucketInventoryConfiguration (..),
    mkGetBucketInventoryConfiguration,

    -- ** Request lenses
    gbicBucket,
    gbicId,
    gbicExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketInventoryConfigurationResponse (..),
    mkGetBucketInventoryConfigurationResponse,

    -- ** Response lenses
    gbicrrsInventoryConfiguration,
    gbicrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketInventoryConfiguration' smart constructor.
data GetBucketInventoryConfiguration = GetBucketInventoryConfiguration'
  { -- | The name of the bucket containing the inventory configuration to retrieve.
    bucket :: Types.BucketName,
    -- | The ID used to identify the inventory configuration.
    id :: Types.InventoryId,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketInventoryConfiguration' value with any optional fields omitted.
mkGetBucketInventoryConfiguration ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'id'
  Types.InventoryId ->
  GetBucketInventoryConfiguration
mkGetBucketInventoryConfiguration bucket id =
  GetBucketInventoryConfiguration'
    { bucket,
      id,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the bucket containing the inventory configuration to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbicBucket :: Lens.Lens' GetBucketInventoryConfiguration Types.BucketName
gbicBucket = Lens.field @"bucket"
{-# DEPRECATED gbicBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID used to identify the inventory configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbicId :: Lens.Lens' GetBucketInventoryConfiguration Types.InventoryId
gbicId = Lens.field @"id"
{-# DEPRECATED gbicId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbicExpectedBucketOwner :: Lens.Lens' GetBucketInventoryConfiguration (Core.Maybe Types.AccountId)
gbicExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gbicExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketInventoryConfiguration where
  type
    Rs GetBucketInventoryConfiguration =
      GetBucketInventoryConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery =
          Core.toQueryValue "id" id Core.<> (Core.pure ("inventory", "")),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketInventoryConfigurationResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketInventoryConfigurationResponse' smart constructor.
data GetBucketInventoryConfigurationResponse = GetBucketInventoryConfigurationResponse'
  { -- | Specifies the inventory configuration.
    inventoryConfiguration :: Core.Maybe Types.InventoryConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketInventoryConfigurationResponse' value with any optional fields omitted.
mkGetBucketInventoryConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBucketInventoryConfigurationResponse
mkGetBucketInventoryConfigurationResponse responseStatus =
  GetBucketInventoryConfigurationResponse'
    { inventoryConfiguration =
        Core.Nothing,
      responseStatus
    }

-- | Specifies the inventory configuration.
--
-- /Note:/ Consider using 'inventoryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbicrrsInventoryConfiguration :: Lens.Lens' GetBucketInventoryConfigurationResponse (Core.Maybe Types.InventoryConfiguration)
gbicrrsInventoryConfiguration = Lens.field @"inventoryConfiguration"
{-# DEPRECATED gbicrrsInventoryConfiguration "Use generic-lens or generic-optics with 'inventoryConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbicrrsResponseStatus :: Lens.Lens' GetBucketInventoryConfigurationResponse Core.Int
gbicrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbicrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
