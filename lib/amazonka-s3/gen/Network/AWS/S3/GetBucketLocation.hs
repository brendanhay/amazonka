{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Region the bucket resides in. You set the bucket's Region using the @LocationConstraint@ request parameter in a @CreateBucket@ request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket> .
--
-- To use this implementation of the operation, you must be the bucket owner.
-- The following operations are related to @GetBucketLocation@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket> 
--
--
module Network.AWS.S3.GetBucketLocation
    (
    -- * Creating a request
      GetBucketLocation (..)
    , mkGetBucketLocation
    -- ** Request lenses
    , gblBucket
    , gblExpectedBucketOwner

    -- * Destructuring the response
    , GetBucketLocationResponse (..)
    , mkGetBucketLocationResponse
    -- ** Response lenses
    , gblrrsLocationConstraint
    , gblrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketLocation' smart constructor.
data GetBucketLocation = GetBucketLocation'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket for which to get the location.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketLocation' value with any optional fields omitted.
mkGetBucketLocation
    :: Types.BucketName -- ^ 'bucket'
    -> GetBucketLocation
mkGetBucketLocation bucket
  = GetBucketLocation'{bucket, expectedBucketOwner = Core.Nothing}

-- | The name of the bucket for which to get the location.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblBucket :: Lens.Lens' GetBucketLocation Types.BucketName
gblBucket = Lens.field @"bucket"
{-# INLINEABLE gblBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblExpectedBucketOwner :: Lens.Lens' GetBucketLocation (Core.Maybe Types.ExpectedBucketOwner)
gblExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE gblExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery GetBucketLocation where
        toQuery GetBucketLocation{..}
          = Core.toQueryPair "location" ("" :: Core.Text)

instance Core.ToHeaders GetBucketLocation where
        toHeaders GetBucketLocation{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest GetBucketLocation where
        type Rs GetBucketLocation = GetBucketLocationResponse
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
                 GetBucketLocationResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBucketLocationResponse' smart constructor.
data GetBucketLocationResponse = GetBucketLocationResponse'
  { locationConstraint :: Types.LocationConstraint
    -- ^ Specifies the Region where the bucket resides. For a list of all the Amazon S3 supported location constraints by Region, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints> . Buckets in Region @us-east-1@ have a LocationConstraint of @null@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketLocationResponse' value with any optional fields omitted.
mkGetBucketLocationResponse
    :: Types.LocationConstraint -- ^ 'locationConstraint'
    -> Core.Int -- ^ 'responseStatus'
    -> GetBucketLocationResponse
mkGetBucketLocationResponse locationConstraint responseStatus
  = GetBucketLocationResponse'{locationConstraint, responseStatus}

-- | Specifies the Region where the bucket resides. For a list of all the Amazon S3 supported location constraints by Region, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints> . Buckets in Region @us-east-1@ have a LocationConstraint of @null@ .
--
-- /Note:/ Consider using 'locationConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblrrsLocationConstraint :: Lens.Lens' GetBucketLocationResponse Types.LocationConstraint
gblrrsLocationConstraint = Lens.field @"locationConstraint"
{-# INLINEABLE gblrrsLocationConstraint #-}
{-# DEPRECATED locationConstraint "Use generic-lens or generic-optics with 'locationConstraint' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblrrsResponseStatus :: Lens.Lens' GetBucketLocationResponse Core.Int
gblrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gblrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
