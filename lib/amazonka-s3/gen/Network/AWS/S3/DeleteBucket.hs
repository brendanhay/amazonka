{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the S3 bucket. All objects (including all object versions and delete markers) in the bucket must be deleted before the bucket itself can be deleted.
--
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject> 
--
--
module Network.AWS.S3.DeleteBucket
    (
    -- * Creating a request
      DeleteBucket (..)
    , mkDeleteBucket
    -- ** Request lenses
    , dbBucket
    , dbExpectedBucketOwner

    -- * Destructuring the response
    , DeleteBucketResponse (..)
    , mkDeleteBucketResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteBucket' smart constructor.
data DeleteBucket = DeleteBucket'
  { bucket :: Types.BucketName
    -- ^ Specifies the bucket being deleted.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucket' value with any optional fields omitted.
mkDeleteBucket
    :: Types.BucketName -- ^ 'bucket'
    -> DeleteBucket
mkDeleteBucket bucket
  = DeleteBucket'{bucket, expectedBucketOwner = Core.Nothing}

-- | Specifies the bucket being deleted.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBucket :: Lens.Lens' DeleteBucket Types.BucketName
dbBucket = Lens.field @"bucket"
{-# INLINEABLE dbBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbExpectedBucketOwner :: Lens.Lens' DeleteBucket (Core.Maybe Types.AccountId)
dbExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE dbExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery DeleteBucket where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBucket where
        toHeaders DeleteBucket{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest DeleteBucket where
        type Rs DeleteBucket = DeleteBucketResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteBucketResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBucketResponse' smart constructor.
data DeleteBucketResponse = DeleteBucketResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketResponse' value with any optional fields omitted.
mkDeleteBucketResponse
    :: DeleteBucketResponse
mkDeleteBucketResponse = DeleteBucketResponse'
