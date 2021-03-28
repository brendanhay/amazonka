{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketLifecycle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the lifecycle configuration from the specified bucket. Amazon S3 removes all the lifecycle configuration rules in the lifecycle subresource associated with the bucket. Your objects never expire, and Amazon S3 no longer automatically deletes any objects on the basis of rules contained in the deleted lifecycle configuration.
--
-- To use this operation, you must have permission to perform the @s3:PutLifecycleConfiguration@ action. By default, the bucket owner has this permission and the bucket owner can grant this permission to others.
-- There is usually some time lag before lifecycle configuration deletion is fully propagated to all the Amazon S3 systems.
-- For more information about the object expiration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#intro-lifecycle-rules-actions Elements to Describe Lifecycle Actions> .
-- Related actions include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycleConfiguration.html GetBucketLifecycleConfiguration> 
--
--
module Network.AWS.S3.DeleteBucketLifecycle
    (
    -- * Creating a request
      DeleteBucketLifecycle (..)
    , mkDeleteBucketLifecycle
    -- ** Request lenses
    , dblBucket
    , dblExpectedBucketOwner

    -- * Destructuring the response
    , DeleteBucketLifecycleResponse (..)
    , mkDeleteBucketLifecycleResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteBucketLifecycle' smart constructor.
data DeleteBucketLifecycle = DeleteBucketLifecycle'
  { bucket :: Types.BucketName
    -- ^ The bucket name of the lifecycle to delete.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketLifecycle' value with any optional fields omitted.
mkDeleteBucketLifecycle
    :: Types.BucketName -- ^ 'bucket'
    -> DeleteBucketLifecycle
mkDeleteBucketLifecycle bucket
  = DeleteBucketLifecycle'{bucket,
                           expectedBucketOwner = Core.Nothing}

-- | The bucket name of the lifecycle to delete.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dblBucket :: Lens.Lens' DeleteBucketLifecycle Types.BucketName
dblBucket = Lens.field @"bucket"
{-# INLINEABLE dblBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dblExpectedBucketOwner :: Lens.Lens' DeleteBucketLifecycle (Core.Maybe Types.ExpectedBucketOwner)
dblExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE dblExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery DeleteBucketLifecycle where
        toQuery DeleteBucketLifecycle{..}
          = Core.toQueryPair "lifecycle" ("" :: Core.Text)

instance Core.ToHeaders DeleteBucketLifecycle where
        toHeaders DeleteBucketLifecycle{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest DeleteBucketLifecycle where
        type Rs DeleteBucketLifecycle = DeleteBucketLifecycleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteBucketLifecycleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBucketLifecycleResponse' smart constructor.
data DeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketLifecycleResponse' value with any optional fields omitted.
mkDeleteBucketLifecycleResponse
    :: DeleteBucketLifecycleResponse
mkDeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse'
