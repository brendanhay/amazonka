{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketCors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the @cors@ configuration information set for the bucket.
--
-- To use this operation, you must have permission to perform the @s3:PutBucketCORS@ action. The bucket owner has this permission by default and can grant this permission to others. 
-- For information about @cors@ , see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
-- __Related Resources:__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketCors.html PutBucketCors> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTOPTIONSobject.html RESTOPTIONSobject> 
--
--
module Network.AWS.S3.DeleteBucketCors
    (
    -- * Creating a request
      DeleteBucketCors (..)
    , mkDeleteBucketCors
    -- ** Request lenses
    , dbcBucket
    , dbcExpectedBucketOwner

    -- * Destructuring the response
    , DeleteBucketCorsResponse (..)
    , mkDeleteBucketCorsResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteBucketCors' smart constructor.
data DeleteBucketCors = DeleteBucketCors'
  { bucket :: Types.BucketName
    -- ^ Specifies the bucket whose @cors@ configuration is being deleted.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketCors' value with any optional fields omitted.
mkDeleteBucketCors
    :: Types.BucketName -- ^ 'bucket'
    -> DeleteBucketCors
mkDeleteBucketCors bucket
  = DeleteBucketCors'{bucket, expectedBucketOwner = Core.Nothing}

-- | Specifies the bucket whose @cors@ configuration is being deleted.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcBucket :: Lens.Lens' DeleteBucketCors Types.BucketName
dbcBucket = Lens.field @"bucket"
{-# INLINEABLE dbcBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcExpectedBucketOwner :: Lens.Lens' DeleteBucketCors (Core.Maybe Types.ExpectedBucketOwner)
dbcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE dbcExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery DeleteBucketCors where
        toQuery DeleteBucketCors{..}
          = Core.toQueryPair "cors" ("" :: Core.Text)

instance Core.ToHeaders DeleteBucketCors where
        toHeaders DeleteBucketCors{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest DeleteBucketCors where
        type Rs DeleteBucketCors = DeleteBucketCorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteBucketCorsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBucketCorsResponse' smart constructor.
data DeleteBucketCorsResponse = DeleteBucketCorsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketCorsResponse' value with any optional fields omitted.
mkDeleteBucketCorsResponse
    :: DeleteBucketCorsResponse
mkDeleteBucketCorsResponse = DeleteBucketCorsResponse'
