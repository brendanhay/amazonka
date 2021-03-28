{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketInventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @PUT@ operation adds an inventory configuration (identified by the inventory ID) to the bucket. You can have up to 1,000 inventory configurations per bucket. 
--
-- Amazon S3 inventory generates inventories of the objects in the bucket on a daily or weekly basis, and the results are published to a flat file. The bucket that is inventoried is called the /source/ bucket, and the bucket where the inventory flat file is stored is called the /destination/ bucket. The /destination/ bucket must be in the same AWS Region as the /source/ bucket. 
-- When you configure an inventory for a /source/ bucket, you specify the /destination/ bucket where you want the inventory to be stored, and whether to generate the inventory daily or weekly. You can also configure what object metadata to include and whether to inventory all object versions or only current versions. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory> in the Amazon Simple Storage Service Developer Guide.
-- /Important:/ You must create a bucket policy on the /destination/ bucket to grant permissions to Amazon S3 to write objects to the bucket in the defined location. For an example policy, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/example-bucket-policies.html#example-bucket-policies-use-case-9 Granting Permissions for Amazon S3 Inventory and Storage Class Analysis> .
-- To use this operation, you must have permissions to perform the @s3:PutInventoryConfiguration@ action. The bucket owner has this permission by default and can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> in the Amazon Simple Storage Service Developer Guide.
-- __Special Errors__ 
--
--     * __HTTP 400 Bad Request Error__ 
--
--     * /Code:/ InvalidArgument
--
--
--     * /Cause:/ Invalid Argument
--
--
--
--
--     * __HTTP 400 Bad Request Error__ 
--
--     * /Code:/ TooManyConfigurations
--
--
--     * /Cause:/ You are attempting to create a new configuration but have already reached the 1,000-configuration limit. 
--
--
--
--
--     * __HTTP 403 Forbidden Error__ 
--
--     * /Code:/ AccessDenied
--
--
--     * /Cause:/ You are not the owner of the specified bucket, or you do not have the @s3:PutInventoryConfiguration@ bucket permission to set the configuration on the bucket. 
--
--
--
--
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations> 
--
--
module Network.AWS.S3.PutBucketInventoryConfiguration
    (
    -- * Creating a request
      PutBucketInventoryConfiguration (..)
    , mkPutBucketInventoryConfiguration
    -- ** Request lenses
    , pbicBucket
    , pbicId
    , pbicInventoryConfiguration
    , pbicExpectedBucketOwner

    -- * Destructuring the response
    , PutBucketInventoryConfigurationResponse (..)
    , mkPutBucketInventoryConfigurationResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketInventoryConfiguration' smart constructor.
data PutBucketInventoryConfiguration = PutBucketInventoryConfiguration'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket where the inventory configuration will be stored.
  , id :: Types.Id
    -- ^ The ID used to identify the inventory configuration.
  , inventoryConfiguration :: Types.InventoryConfiguration
    -- ^ Specifies the inventory configuration.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketInventoryConfiguration' value with any optional fields omitted.
mkPutBucketInventoryConfiguration
    :: Types.BucketName -- ^ 'bucket'
    -> Types.Id -- ^ 'id'
    -> Types.InventoryConfiguration -- ^ 'inventoryConfiguration'
    -> PutBucketInventoryConfiguration
mkPutBucketInventoryConfiguration bucket id inventoryConfiguration
  = PutBucketInventoryConfiguration'{bucket, id,
                                     inventoryConfiguration, expectedBucketOwner = Core.Nothing}

-- | The name of the bucket where the inventory configuration will be stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbicBucket :: Lens.Lens' PutBucketInventoryConfiguration Types.BucketName
pbicBucket = Lens.field @"bucket"
{-# INLINEABLE pbicBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The ID used to identify the inventory configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbicId :: Lens.Lens' PutBucketInventoryConfiguration Types.Id
pbicId = Lens.field @"id"
{-# INLINEABLE pbicId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Specifies the inventory configuration.
--
-- /Note:/ Consider using 'inventoryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbicInventoryConfiguration :: Lens.Lens' PutBucketInventoryConfiguration Types.InventoryConfiguration
pbicInventoryConfiguration = Lens.field @"inventoryConfiguration"
{-# INLINEABLE pbicInventoryConfiguration #-}
{-# DEPRECATED inventoryConfiguration "Use generic-lens or generic-optics with 'inventoryConfiguration' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbicExpectedBucketOwner :: Lens.Lens' PutBucketInventoryConfiguration (Core.Maybe Types.ExpectedBucketOwner)
pbicExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE pbicExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery PutBucketInventoryConfiguration where
        toQuery PutBucketInventoryConfiguration{..}
          = Core.toQueryPair "id" id Core.<>
              Core.toQueryPair "inventory" ("" :: Core.Text)

instance Core.ToHeaders PutBucketInventoryConfiguration where
        toHeaders PutBucketInventoryConfiguration{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest PutBucketInventoryConfiguration where
        type Rs PutBucketInventoryConfiguration =
             PutBucketInventoryConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull PutBucketInventoryConfigurationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBucketInventoryConfigurationResponse' smart constructor.
data PutBucketInventoryConfigurationResponse = PutBucketInventoryConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketInventoryConfigurationResponse' value with any optional fields omitted.
mkPutBucketInventoryConfigurationResponse
    :: PutBucketInventoryConfigurationResponse
mkPutBucketInventoryConfigurationResponse
  = PutBucketInventoryConfigurationResponse'
