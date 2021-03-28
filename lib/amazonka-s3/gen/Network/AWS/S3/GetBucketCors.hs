{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketCors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the cors configuration information set for the bucket.
--
-- To use this operation, you must have permission to perform the s3:GetBucketCORS action. By default, the bucket owner has this permission and can grant it to others.
-- For more information about cors, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> .
-- The following operations are related to @GetBucketCors@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketCors.html PutBucketCors> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketCors.html DeleteBucketCors> 
--
--
module Network.AWS.S3.GetBucketCors
    (
    -- * Creating a request
      GetBucketCors (..)
    , mkGetBucketCors
    -- ** Request lenses
    , gbcBucket
    , gbcExpectedBucketOwner

    -- * Destructuring the response
    , GetBucketCorsResponse (..)
    , mkGetBucketCorsResponse
    -- ** Response lenses
    , gbcrrsCORSRules
    , gbcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketCors' smart constructor.
data GetBucketCors = GetBucketCors'
  { bucket :: Types.BucketName
    -- ^ The bucket name for which to get the cors configuration.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketCors' value with any optional fields omitted.
mkGetBucketCors
    :: Types.BucketName -- ^ 'bucket'
    -> GetBucketCors
mkGetBucketCors bucket
  = GetBucketCors'{bucket, expectedBucketOwner = Core.Nothing}

-- | The bucket name for which to get the cors configuration.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcBucket :: Lens.Lens' GetBucketCors Types.BucketName
gbcBucket = Lens.field @"bucket"
{-# INLINEABLE gbcBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcExpectedBucketOwner :: Lens.Lens' GetBucketCors (Core.Maybe Types.AccountId)
gbcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE gbcExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery GetBucketCors where
        toQuery GetBucketCors{..}
          = Core.toQueryPair "cors" ("" :: Core.Text)

instance Core.ToHeaders GetBucketCors where
        toHeaders GetBucketCors{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest GetBucketCors where
        type Rs GetBucketCors = GetBucketCorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetBucketCorsResponse' Core.<$>
                   (x Core..@? "CORSRule") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBucketCorsResponse' smart constructor.
data GetBucketCorsResponse = GetBucketCorsResponse'
  { cORSRules :: Core.Maybe [Types.CORSRule]
    -- ^ A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketCorsResponse' value with any optional fields omitted.
mkGetBucketCorsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBucketCorsResponse
mkGetBucketCorsResponse responseStatus
  = GetBucketCorsResponse'{cORSRules = Core.Nothing, responseStatus}

-- | A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
--
-- /Note:/ Consider using 'cORSRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcrrsCORSRules :: Lens.Lens' GetBucketCorsResponse (Core.Maybe [Types.CORSRule])
gbcrrsCORSRules = Lens.field @"cORSRules"
{-# INLINEABLE gbcrrsCORSRules #-}
{-# DEPRECATED cORSRules "Use generic-lens or generic-optics with 'cORSRules' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcrrsResponseStatus :: Lens.Lens' GetBucketCorsResponse Core.Int
gbcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
