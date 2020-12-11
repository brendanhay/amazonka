{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gblExpectedBucketOwner,
    gblBucket,

    -- * Destructuring the response
    GetBucketLocationResponse (..),
    mkGetBucketLocationResponse,

    -- ** Response lenses
    gblbrsResponseStatus,
    gblbrsLocationConstraint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketLocation' smart constructor.
data GetBucketLocation = GetBucketLocation'
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

-- | Creates a value of 'GetBucketLocation' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket for which to get the location.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketLocation ::
  -- | 'bucket'
  BucketName ->
  GetBucketLocation
mkGetBucketLocation pBucket_ =
  GetBucketLocation'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblExpectedBucketOwner :: Lens.Lens' GetBucketLocation (Lude.Maybe Lude.Text)
gblExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketLocation -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketLocation)
{-# DEPRECATED gblExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket for which to get the location.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblBucket :: Lens.Lens' GetBucketLocation BucketName
gblBucket = Lens.lens (bucket :: GetBucketLocation -> BucketName) (\s a -> s {bucket = a} :: GetBucketLocation)
{-# DEPRECATED gblBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest GetBucketLocation where
  type Rs GetBucketLocation = GetBucketLocationResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketLocationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.parseXML x)
      )

instance Lude.ToHeaders GetBucketLocation where
  toHeaders GetBucketLocation' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketLocation where
  toPath GetBucketLocation' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketLocation where
  toQuery = Lude.const (Lude.mconcat ["location"])

-- | /See:/ 'mkGetBucketLocationResponse' smart constructor.
data GetBucketLocationResponse = GetBucketLocationResponse'
  { responseStatus ::
      Lude.Int,
    locationConstraint ::
      LocationConstraint
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketLocationResponse' with the minimum fields required to make a request.
--
-- * 'locationConstraint' - Specifies the Region where the bucket resides. For a list of all the Amazon S3 supported location constraints by Region, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints> . Buckets in Region @us-east-1@ have a LocationConstraint of @null@ .
-- * 'responseStatus' - The response status code.
mkGetBucketLocationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'locationConstraint'
  LocationConstraint ->
  GetBucketLocationResponse
mkGetBucketLocationResponse pResponseStatus_ pLocationConstraint_ =
  GetBucketLocationResponse'
    { responseStatus = pResponseStatus_,
      locationConstraint = pLocationConstraint_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblbrsResponseStatus :: Lens.Lens' GetBucketLocationResponse Lude.Int
gblbrsResponseStatus = Lens.lens (responseStatus :: GetBucketLocationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketLocationResponse)
{-# DEPRECATED gblbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Specifies the Region where the bucket resides. For a list of all the Amazon S3 supported location constraints by Region, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints> . Buckets in Region @us-east-1@ have a LocationConstraint of @null@ .
--
-- /Note:/ Consider using 'locationConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblbrsLocationConstraint :: Lens.Lens' GetBucketLocationResponse LocationConstraint
gblbrsLocationConstraint = Lens.lens (locationConstraint :: GetBucketLocationResponse -> LocationConstraint) (\s a -> s {locationConstraint = a} :: GetBucketLocationResponse)
{-# DEPRECATED gblbrsLocationConstraint "Use generic-lens or generic-optics with 'locationConstraint' instead." #-}
