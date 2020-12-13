{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies an Amazon S3 bucket policy to an Amazon S3 bucket. If you are using an identity other than the root user of the AWS account that owns the bucket, the calling identity must have the @PutBucketPolicy@ permissions on the specified bucket and belong to the bucket owner's account in order to use this operation.
--
-- If you don't have @PutBucketPolicy@ permissions, Amazon S3 returns a @403 Access Denied@ error. If you have the correct permissions, but you're not using an identity that belongs to the bucket owner's account, Amazon S3 returns a @405 Method Not Allowed@ error.
-- /Important:/ As a security precaution, the root user of the AWS account that owns a bucket can always use this operation, even if the policy explicitly denies the root user the ability to perform this action.
-- For more information about bucket policies, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies and User Policies> .
-- The following operations are related to @PutBucketPolicy@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
module Network.AWS.S3.PutBucketPolicy
  ( -- * Creating a request
    PutBucketPolicy (..),
    mkPutBucketPolicy,

    -- ** Request lenses
    pbpBucket,
    pbpConfirmRemoveSelfBucketAccess,
    pbpContentMD5,
    pbpPolicy,
    pbpExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketPolicyResponse (..),
    mkPutBucketPolicyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketPolicy' smart constructor.
data PutBucketPolicy = PutBucketPolicy'
  { -- | The name of the bucket.
    bucket :: BucketName,
    -- | Set this parameter to true to confirm that you want to remove your permissions to change this bucket policy in the future.
    confirmRemoveSelfBucketAccess :: Lude.Maybe Lude.Bool,
    -- | The MD5 hash of the request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | The bucket policy as a JSON document.
    policy :: Lude.ByteString,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketPolicy' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket.
-- * 'confirmRemoveSelfBucketAccess' - Set this parameter to true to confirm that you want to remove your permissions to change this bucket policy in the future.
-- * 'contentMD5' - The MD5 hash of the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'policy' - The bucket policy as a JSON document.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutBucketPolicy ::
  -- | 'bucket'
  BucketName ->
  -- | 'policy'
  Lude.ByteString ->
  PutBucketPolicy
mkPutBucketPolicy pBucket_ pPolicy_ =
  PutBucketPolicy'
    { bucket = pBucket_,
      confirmRemoveSelfBucketAccess = Lude.Nothing,
      contentMD5 = Lude.Nothing,
      policy = pPolicy_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpBucket :: Lens.Lens' PutBucketPolicy BucketName
pbpBucket = Lens.lens (bucket :: PutBucketPolicy -> BucketName) (\s a -> s {bucket = a} :: PutBucketPolicy)
{-# DEPRECATED pbpBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Set this parameter to true to confirm that you want to remove your permissions to change this bucket policy in the future.
--
-- /Note:/ Consider using 'confirmRemoveSelfBucketAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpConfirmRemoveSelfBucketAccess :: Lens.Lens' PutBucketPolicy (Lude.Maybe Lude.Bool)
pbpConfirmRemoveSelfBucketAccess = Lens.lens (confirmRemoveSelfBucketAccess :: PutBucketPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {confirmRemoveSelfBucketAccess = a} :: PutBucketPolicy)
{-# DEPRECATED pbpConfirmRemoveSelfBucketAccess "Use generic-lens or generic-optics with 'confirmRemoveSelfBucketAccess' instead." #-}

-- | The MD5 hash of the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpContentMD5 :: Lens.Lens' PutBucketPolicy (Lude.Maybe Lude.Text)
pbpContentMD5 = Lens.lens (contentMD5 :: PutBucketPolicy -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutBucketPolicy)
{-# DEPRECATED pbpContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The bucket policy as a JSON document.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpPolicy :: Lens.Lens' PutBucketPolicy Lude.ByteString
pbpPolicy = Lens.lens (policy :: PutBucketPolicy -> Lude.ByteString) (\s a -> s {policy = a} :: PutBucketPolicy)
{-# DEPRECATED pbpPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpExpectedBucketOwner :: Lens.Lens' PutBucketPolicy (Lude.Maybe Lude.Text)
pbpExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketPolicy -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketPolicy)
{-# DEPRECATED pbpExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutBucketPolicy where
  type Rs PutBucketPolicy = PutBucketPolicyResponse
  request = contentMD5Header Lude.. Req.putBody s3Service
  response = Res.receiveNull PutBucketPolicyResponse'

instance Lude.ToBody PutBucketPolicy where
  toBody = Lude.toBody Lude.. policy

instance Lude.ToHeaders PutBucketPolicy where
  toHeaders PutBucketPolicy' {..} =
    Lude.mconcat
      [ "x-amz-confirm-remove-self-bucket-access"
          Lude.=# confirmRemoveSelfBucketAccess,
        "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutBucketPolicy where
  toPath PutBucketPolicy' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketPolicy where
  toQuery = Lude.const (Lude.mconcat ["policy"])

-- | /See:/ 'mkPutBucketPolicyResponse' smart constructor.
data PutBucketPolicyResponse = PutBucketPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketPolicyResponse' with the minimum fields required to make a request.
mkPutBucketPolicyResponse ::
  PutBucketPolicyResponse
mkPutBucketPolicyResponse = PutBucketPolicyResponse'
