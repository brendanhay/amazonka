{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeletePublicAccessBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the @PublicAccessBlock@ configuration for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketPublicAccessBlock@ permission. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- The following operations are related to @DeletePublicAccessBlock@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketPolicyStatus.html GetBucketPolicyStatus> 
--
--
module Network.AWS.S3.DeletePublicAccessBlock
    (
    -- * Creating a request
      DeletePublicAccessBlock (..)
    , mkDeletePublicAccessBlock
    -- ** Request lenses
    , dpabBucket
    , dpabExpectedBucketOwner

    -- * Destructuring the response
    , DeletePublicAccessBlockResponse (..)
    , mkDeletePublicAccessBlockResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeletePublicAccessBlock' smart constructor.
data DeletePublicAccessBlock = DeletePublicAccessBlock'
  { bucket :: Types.BucketName
    -- ^ The Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to delete. 
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePublicAccessBlock' value with any optional fields omitted.
mkDeletePublicAccessBlock
    :: Types.BucketName -- ^ 'bucket'
    -> DeletePublicAccessBlock
mkDeletePublicAccessBlock bucket
  = DeletePublicAccessBlock'{bucket,
                             expectedBucketOwner = Core.Nothing}

-- | The Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to delete. 
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpabBucket :: Lens.Lens' DeletePublicAccessBlock Types.BucketName
dpabBucket = Lens.field @"bucket"
{-# INLINEABLE dpabBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpabExpectedBucketOwner :: Lens.Lens' DeletePublicAccessBlock (Core.Maybe Types.ExpectedBucketOwner)
dpabExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE dpabExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery DeletePublicAccessBlock where
        toQuery DeletePublicAccessBlock{..}
          = Core.toQueryPair "publicAccessBlock" ("" :: Core.Text)

instance Core.ToHeaders DeletePublicAccessBlock where
        toHeaders DeletePublicAccessBlock{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest DeletePublicAccessBlock where
        type Rs DeletePublicAccessBlock = DeletePublicAccessBlockResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeletePublicAccessBlockResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePublicAccessBlockResponse' smart constructor.
data DeletePublicAccessBlockResponse = DeletePublicAccessBlockResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePublicAccessBlockResponse' value with any optional fields omitted.
mkDeletePublicAccessBlockResponse
    :: DeletePublicAccessBlockResponse
mkDeletePublicAccessBlockResponse
  = DeletePublicAccessBlockResponse'
