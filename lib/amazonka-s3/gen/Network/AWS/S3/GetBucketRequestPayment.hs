{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gbrpExpectedBucketOwner,
    gbrpBucket,

    -- * Destructuring the response
    GetBucketRequestPaymentResponse (..),
    mkGetBucketRequestPaymentResponse,

    -- ** Response lenses
    gbrprsPayer,
    gbrprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketRequestPayment' smart constructor.
data GetBucketRequestPayment = GetBucketRequestPayment'
  { expectedBucketOwner ::
      Lude.Maybe Lude.Text,
    bucket :: BucketName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketRequestPayment' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket for which to get the payment request configuration
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketRequestPayment ::
  -- | 'bucket'
  BucketName ->
  GetBucketRequestPayment
mkGetBucketRequestPayment pBucket_ =
  GetBucketRequestPayment'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrpExpectedBucketOwner :: Lens.Lens' GetBucketRequestPayment (Lude.Maybe Lude.Text)
gbrpExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketRequestPayment -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketRequestPayment)
{-# DEPRECATED gbrpExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket for which to get the payment request configuration
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrpBucket :: Lens.Lens' GetBucketRequestPayment BucketName
gbrpBucket = Lens.lens (bucket :: GetBucketRequestPayment -> BucketName) (\s a -> s {bucket = a} :: GetBucketRequestPayment)
{-# DEPRECATED gbrpBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest GetBucketRequestPayment where
  type Rs GetBucketRequestPayment = GetBucketRequestPaymentResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketRequestPaymentResponse'
            Lude.<$> (x Lude..@? "Payer") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketRequestPayment where
  toHeaders GetBucketRequestPayment' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketRequestPayment where
  toPath GetBucketRequestPayment' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketRequestPayment where
  toQuery = Lude.const (Lude.mconcat ["requestPayment"])

-- | /See:/ 'mkGetBucketRequestPaymentResponse' smart constructor.
data GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse'
  { payer ::
      Lude.Maybe Payer,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketRequestPaymentResponse' with the minimum fields required to make a request.
--
-- * 'payer' - Specifies who pays for the download and request fees.
-- * 'responseStatus' - The response status code.
mkGetBucketRequestPaymentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketRequestPaymentResponse
mkGetBucketRequestPaymentResponse pResponseStatus_ =
  GetBucketRequestPaymentResponse'
    { payer = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Specifies who pays for the download and request fees.
--
-- /Note:/ Consider using 'payer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrprsPayer :: Lens.Lens' GetBucketRequestPaymentResponse (Lude.Maybe Payer)
gbrprsPayer = Lens.lens (payer :: GetBucketRequestPaymentResponse -> Lude.Maybe Payer) (\s a -> s {payer = a} :: GetBucketRequestPaymentResponse)
{-# DEPRECATED gbrprsPayer "Use generic-lens or generic-optics with 'payer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrprsResponseStatus :: Lens.Lens' GetBucketRequestPaymentResponse Lude.Int
gbrprsResponseStatus = Lens.lens (responseStatus :: GetBucketRequestPaymentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketRequestPaymentResponse)
{-# DEPRECATED gbrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
