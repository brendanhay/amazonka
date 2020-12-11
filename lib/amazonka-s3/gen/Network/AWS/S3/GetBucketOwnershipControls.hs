{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketOwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves @OwnershipControls@ for an Amazon S3 bucket. To use this operation, you must have the @s3:GetBucketOwnershipControls@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
-- For information about Amazon S3 Object Ownership, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/about-object-ownership.html Using Object Ownership> .
-- The following operations are related to @GetBucketOwnershipControls@ :
--
--     * 'PutBucketOwnershipControls'
--
--
--     * 'DeleteBucketOwnershipControls'
module Network.AWS.S3.GetBucketOwnershipControls
  ( -- * Creating a request
    GetBucketOwnershipControls (..),
    mkGetBucketOwnershipControls,

    -- ** Request lenses
    gbocExpectedBucketOwner,
    gbocBucket,

    -- * Destructuring the response
    GetBucketOwnershipControlsResponse (..),
    mkGetBucketOwnershipControlsResponse,

    -- ** Response lenses
    gbocrsOwnershipControls,
    gbocrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketOwnershipControls' smart constructor.
data GetBucketOwnershipControls = GetBucketOwnershipControls'
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

-- | Creates a value of 'GetBucketOwnershipControls' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the Amazon S3 bucket whose @OwnershipControls@ you want to retrieve.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketOwnershipControls ::
  -- | 'bucket'
  BucketName ->
  GetBucketOwnershipControls
mkGetBucketOwnershipControls pBucket_ =
  GetBucketOwnershipControls'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbocExpectedBucketOwner :: Lens.Lens' GetBucketOwnershipControls (Lude.Maybe Lude.Text)
gbocExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketOwnershipControls -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketOwnershipControls)
{-# DEPRECATED gbocExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbocBucket :: Lens.Lens' GetBucketOwnershipControls BucketName
gbocBucket = Lens.lens (bucket :: GetBucketOwnershipControls -> BucketName) (\s a -> s {bucket = a} :: GetBucketOwnershipControls)
{-# DEPRECATED gbocBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest GetBucketOwnershipControls where
  type
    Rs GetBucketOwnershipControls =
      GetBucketOwnershipControlsResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketOwnershipControlsResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketOwnershipControls where
  toHeaders GetBucketOwnershipControls' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketOwnershipControls where
  toPath GetBucketOwnershipControls' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketOwnershipControls where
  toQuery = Lude.const (Lude.mconcat ["ownershipControls"])

-- | /See:/ 'mkGetBucketOwnershipControlsResponse' smart constructor.
data GetBucketOwnershipControlsResponse = GetBucketOwnershipControlsResponse'
  { ownershipControls ::
      Lude.Maybe
        OwnershipControls,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketOwnershipControlsResponse' with the minimum fields required to make a request.
--
-- * 'ownershipControls' - The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) currently in effect for this Amazon S3 bucket.
-- * 'responseStatus' - The response status code.
mkGetBucketOwnershipControlsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketOwnershipControlsResponse
mkGetBucketOwnershipControlsResponse pResponseStatus_ =
  GetBucketOwnershipControlsResponse'
    { ownershipControls =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) currently in effect for this Amazon S3 bucket.
--
-- /Note:/ Consider using 'ownershipControls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbocrsOwnershipControls :: Lens.Lens' GetBucketOwnershipControlsResponse (Lude.Maybe OwnershipControls)
gbocrsOwnershipControls = Lens.lens (ownershipControls :: GetBucketOwnershipControlsResponse -> Lude.Maybe OwnershipControls) (\s a -> s {ownershipControls = a} :: GetBucketOwnershipControlsResponse)
{-# DEPRECATED gbocrsOwnershipControls "Use generic-lens or generic-optics with 'ownershipControls' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbocrsResponseStatus :: Lens.Lens' GetBucketOwnershipControlsResponse Lude.Int
gbocrsResponseStatus = Lens.lens (responseStatus :: GetBucketOwnershipControlsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketOwnershipControlsResponse)
{-# DEPRECATED gbocrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
