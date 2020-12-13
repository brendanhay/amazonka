{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketOwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or modifies @OwnershipControls@ for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketOwnershipControls@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
-- For information about Amazon S3 Object Ownership, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/about-object-ownership.html Using Object Ownership> .
-- The following operations are related to @PutBucketOwnershipControls@ :
--
--     * 'GetBucketOwnershipControls'
--
--
--     * 'DeleteBucketOwnershipControls'
module Network.AWS.S3.PutBucketOwnershipControls
  ( -- * Creating a request
    PutBucketOwnershipControls (..),
    mkPutBucketOwnershipControls,

    -- ** Request lenses
    pbocBucket,
    pbocOwnershipControls,
    pbocContentMD5,
    pbocExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketOwnershipControlsResponse (..),
    mkPutBucketOwnershipControlsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketOwnershipControls' smart constructor.
data PutBucketOwnershipControls = PutBucketOwnershipControls'
  { -- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to set.
    bucket :: BucketName,
    -- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you want to apply to this Amazon S3 bucket.
    ownershipControls :: OwnershipControls,
    -- | The MD5 hash of the @OwnershipControls@ request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketOwnershipControls' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the Amazon S3 bucket whose @OwnershipControls@ you want to set.
-- * 'ownershipControls' - The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you want to apply to this Amazon S3 bucket.
-- * 'contentMD5' - The MD5 hash of the @OwnershipControls@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutBucketOwnershipControls ::
  -- | 'bucket'
  BucketName ->
  -- | 'ownershipControls'
  OwnershipControls ->
  PutBucketOwnershipControls
mkPutBucketOwnershipControls pBucket_ pOwnershipControls_ =
  PutBucketOwnershipControls'
    { bucket = pBucket_,
      ownershipControls = pOwnershipControls_,
      contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to set.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbocBucket :: Lens.Lens' PutBucketOwnershipControls BucketName
pbocBucket = Lens.lens (bucket :: PutBucketOwnershipControls -> BucketName) (\s a -> s {bucket = a} :: PutBucketOwnershipControls)
{-# DEPRECATED pbocBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you want to apply to this Amazon S3 bucket.
--
-- /Note:/ Consider using 'ownershipControls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbocOwnershipControls :: Lens.Lens' PutBucketOwnershipControls OwnershipControls
pbocOwnershipControls = Lens.lens (ownershipControls :: PutBucketOwnershipControls -> OwnershipControls) (\s a -> s {ownershipControls = a} :: PutBucketOwnershipControls)
{-# DEPRECATED pbocOwnershipControls "Use generic-lens or generic-optics with 'ownershipControls' instead." #-}

-- | The MD5 hash of the @OwnershipControls@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbocContentMD5 :: Lens.Lens' PutBucketOwnershipControls (Lude.Maybe Lude.Text)
pbocContentMD5 = Lens.lens (contentMD5 :: PutBucketOwnershipControls -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutBucketOwnershipControls)
{-# DEPRECATED pbocContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbocExpectedBucketOwner :: Lens.Lens' PutBucketOwnershipControls (Lude.Maybe Lude.Text)
pbocExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketOwnershipControls -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketOwnershipControls)
{-# DEPRECATED pbocExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutBucketOwnershipControls where
  type
    Rs PutBucketOwnershipControls =
      PutBucketOwnershipControlsResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutBucketOwnershipControlsResponse'

instance Lude.ToElement PutBucketOwnershipControls where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}OwnershipControls"
      Lude.. ownershipControls

instance Lude.ToHeaders PutBucketOwnershipControls where
  toHeaders PutBucketOwnershipControls' {..} =
    Lude.mconcat
      [ "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutBucketOwnershipControls where
  toPath PutBucketOwnershipControls' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketOwnershipControls where
  toQuery = Lude.const (Lude.mconcat ["ownershipControls"])

-- | /See:/ 'mkPutBucketOwnershipControlsResponse' smart constructor.
data PutBucketOwnershipControlsResponse = PutBucketOwnershipControlsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketOwnershipControlsResponse' with the minimum fields required to make a request.
mkPutBucketOwnershipControlsResponse ::
  PutBucketOwnershipControlsResponse
mkPutBucketOwnershipControlsResponse =
  PutBucketOwnershipControlsResponse'
