{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.S3.GetBucketLocation
  ( -- * Creating a request
    GetBucketLocation (..),
    mkGetBucketLocation,

    -- ** Request lenses
    gblBucket,
    gblExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketLocationResponse (..),
    mkGetBucketLocationResponse,

    -- ** Response lenses
    gblrrsLocationConstraint,
    gblrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketLocation' smart constructor.
data GetBucketLocation = GetBucketLocation'
  { -- | The name of the bucket for which to get the location.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketLocation' value with any optional fields omitted.
mkGetBucketLocation ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketLocation
mkGetBucketLocation bucket =
  GetBucketLocation' {bucket, expectedBucketOwner = Core.Nothing}

-- | The name of the bucket for which to get the location.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblBucket :: Lens.Lens' GetBucketLocation Types.BucketName
gblBucket = Lens.field @"bucket"
{-# DEPRECATED gblBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblExpectedBucketOwner :: Lens.Lens' GetBucketLocation (Core.Maybe Types.ExpectedBucketOwner)
gblExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gblExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketLocation where
  type Rs GetBucketLocation = GetBucketLocationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("location", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketLocationResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketLocationResponse' smart constructor.
data GetBucketLocationResponse = GetBucketLocationResponse'
  { -- | Specifies the Region where the bucket resides. For a list of all the Amazon S3 supported location constraints by Region, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints> . Buckets in Region @us-east-1@ have a LocationConstraint of @null@ .
    locationConstraint :: Types.LocationConstraint,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketLocationResponse' value with any optional fields omitted.
mkGetBucketLocationResponse ::
  -- | 'locationConstraint'
  Types.LocationConstraint ->
  -- | 'responseStatus'
  Core.Int ->
  GetBucketLocationResponse
mkGetBucketLocationResponse locationConstraint responseStatus =
  GetBucketLocationResponse' {locationConstraint, responseStatus}

-- | Specifies the Region where the bucket resides. For a list of all the Amazon S3 supported location constraints by Region, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints> . Buckets in Region @us-east-1@ have a LocationConstraint of @null@ .
--
-- /Note:/ Consider using 'locationConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblrrsLocationConstraint :: Lens.Lens' GetBucketLocationResponse Types.LocationConstraint
gblrrsLocationConstraint = Lens.field @"locationConstraint"
{-# DEPRECATED gblrrsLocationConstraint "Use generic-lens or generic-optics with 'locationConstraint' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblrrsResponseStatus :: Lens.Lens' GetBucketLocationResponse Core.Int
gblrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gblrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
