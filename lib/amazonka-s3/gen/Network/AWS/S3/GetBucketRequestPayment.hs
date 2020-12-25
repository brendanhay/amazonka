{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketRequestPayment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the request payment configuration of a bucket. To use this version of the operation, you must be the bucket owner. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> .
--
-- The following operations are related to @GetBucketRequestPayment@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjects.html ListObjects>
module Network.AWS.S3.GetBucketRequestPayment
  ( -- * Creating a request
    GetBucketRequestPayment (..),
    mkGetBucketRequestPayment,

    -- ** Request lenses
    gbrpBucket,
    gbrpExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketRequestPaymentResponse (..),
    mkGetBucketRequestPaymentResponse,

    -- ** Response lenses
    gbrprrsPayer,
    gbrprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketRequestPayment' smart constructor.
data GetBucketRequestPayment = GetBucketRequestPayment'
  { -- | The name of the bucket for which to get the payment request configuration
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketRequestPayment' value with any optional fields omitted.
mkGetBucketRequestPayment ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketRequestPayment
mkGetBucketRequestPayment bucket =
  GetBucketRequestPayment'
    { bucket,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the bucket for which to get the payment request configuration
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrpBucket :: Lens.Lens' GetBucketRequestPayment Types.BucketName
gbrpBucket = Lens.field @"bucket"
{-# DEPRECATED gbrpBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrpExpectedBucketOwner :: Lens.Lens' GetBucketRequestPayment (Core.Maybe Types.ExpectedBucketOwner)
gbrpExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gbrpExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketRequestPayment where
  type Rs GetBucketRequestPayment = GetBucketRequestPaymentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("requestPayment", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketRequestPaymentResponse'
            Core.<$> (x Core..@? "Payer") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketRequestPaymentResponse' smart constructor.
data GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse'
  { -- | Specifies who pays for the download and request fees.
    payer :: Core.Maybe Types.Payer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketRequestPaymentResponse' value with any optional fields omitted.
mkGetBucketRequestPaymentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBucketRequestPaymentResponse
mkGetBucketRequestPaymentResponse responseStatus =
  GetBucketRequestPaymentResponse'
    { payer = Core.Nothing,
      responseStatus
    }

-- | Specifies who pays for the download and request fees.
--
-- /Note:/ Consider using 'payer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrprrsPayer :: Lens.Lens' GetBucketRequestPaymentResponse (Core.Maybe Types.Payer)
gbrprrsPayer = Lens.field @"payer"
{-# DEPRECATED gbrprrsPayer "Use generic-lens or generic-optics with 'payer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrprrsResponseStatus :: Lens.Lens' GetBucketRequestPaymentResponse Core.Int
gbrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
