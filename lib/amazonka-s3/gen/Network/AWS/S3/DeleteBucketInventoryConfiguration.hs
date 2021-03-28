{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketInventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an inventory configuration (identified by the inventory ID) from the bucket.
--
-- To use this operation, you must have permissions to perform the @s3:PutInventoryConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about the Amazon S3 inventory feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory> .
-- Operations related to @DeleteBucketInventoryConfiguration@ include: 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations> 
--
--
module Network.AWS.S3.DeleteBucketInventoryConfiguration
    (
    -- * Creating a request
      DeleteBucketInventoryConfiguration (..)
    , mkDeleteBucketInventoryConfiguration
    -- ** Request lenses
    , dbicBucket
    , dbicId
    , dbicExpectedBucketOwner

    -- * Destructuring the response
    , DeleteBucketInventoryConfigurationResponse (..)
    , mkDeleteBucketInventoryConfigurationResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteBucketInventoryConfiguration' smart constructor.
data DeleteBucketInventoryConfiguration = DeleteBucketInventoryConfiguration'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket containing the inventory configuration to delete.
  , id :: Types.Id
    -- ^ The ID used to identify the inventory configuration.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketInventoryConfiguration' value with any optional fields omitted.
mkDeleteBucketInventoryConfiguration
    :: Types.BucketName -- ^ 'bucket'
    -> Types.Id -- ^ 'id'
    -> DeleteBucketInventoryConfiguration
mkDeleteBucketInventoryConfiguration bucket id
  = DeleteBucketInventoryConfiguration'{bucket, id,
                                        expectedBucketOwner = Core.Nothing}

-- | The name of the bucket containing the inventory configuration to delete.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbicBucket :: Lens.Lens' DeleteBucketInventoryConfiguration Types.BucketName
dbicBucket = Lens.field @"bucket"
{-# INLINEABLE dbicBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The ID used to identify the inventory configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbicId :: Lens.Lens' DeleteBucketInventoryConfiguration Types.Id
dbicId = Lens.field @"id"
{-# INLINEABLE dbicId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbicExpectedBucketOwner :: Lens.Lens' DeleteBucketInventoryConfiguration (Core.Maybe Types.ExpectedBucketOwner)
dbicExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE dbicExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery DeleteBucketInventoryConfiguration where
        toQuery DeleteBucketInventoryConfiguration{..}
          = Core.toQueryPair "id" id Core.<>
              Core.toQueryPair "inventory" ("" :: Core.Text)

instance Core.ToHeaders DeleteBucketInventoryConfiguration where
        toHeaders DeleteBucketInventoryConfiguration{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest DeleteBucketInventoryConfiguration where
        type Rs DeleteBucketInventoryConfiguration =
             DeleteBucketInventoryConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteBucketInventoryConfigurationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBucketInventoryConfigurationResponse' smart constructor.
data DeleteBucketInventoryConfigurationResponse = DeleteBucketInventoryConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketInventoryConfigurationResponse' value with any optional fields omitted.
mkDeleteBucketInventoryConfigurationResponse
    :: DeleteBucketInventoryConfigurationResponse
mkDeleteBucketInventoryConfigurationResponse
  = DeleteBucketInventoryConfigurationResponse'
