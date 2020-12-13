{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the policy of a specified bucket. If you are using an identity other than the root user of the AWS account that owns the bucket, the calling identity must have the @GetBucketPolicy@ permissions on the specified bucket and belong to the bucket owner's account in order to use this operation.
--
-- If you don't have @GetBucketPolicy@ permissions, Amazon S3 returns a @403 Access Denied@ error. If you have the correct permissions, but you're not using an identity that belongs to the bucket owner's account, Amazon S3 returns a @405 Method Not Allowed@ error.
-- /Important:/ As a security precaution, the root user of the AWS account that owns a bucket can always use this operation, even if the policy explicitly denies the root user the ability to perform this action.
-- For more information about bucket policies, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies and User Policies> .
-- The following operation is related to @GetBucketPolicy@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Network.AWS.S3.GetBucketPolicy
  ( -- * Creating a request
    GetBucketPolicy (..),
    mkGetBucketPolicy,

    -- ** Request lenses
    gbpBucket,
    gbpExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketPolicyResponse (..),
    mkGetBucketPolicyResponse,

    -- ** Response lenses
    gbprsPolicy,
    gbprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketPolicy' smart constructor.
data GetBucketPolicy = GetBucketPolicy'
  { -- | The bucket name for which to get the bucket policy.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketPolicy' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name for which to get the bucket policy.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketPolicy ::
  -- | 'bucket'
  BucketName ->
  GetBucketPolicy
mkGetBucketPolicy pBucket_ =
  GetBucketPolicy'
    { bucket = pBucket_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The bucket name for which to get the bucket policy.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpBucket :: Lens.Lens' GetBucketPolicy BucketName
gbpBucket = Lens.lens (bucket :: GetBucketPolicy -> BucketName) (\s a -> s {bucket = a} :: GetBucketPolicy)
{-# DEPRECATED gbpBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpExpectedBucketOwner :: Lens.Lens' GetBucketPolicy (Lude.Maybe Lude.Text)
gbpExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketPolicy -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketPolicy)
{-# DEPRECATED gbpExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest GetBucketPolicy where
  type Rs GetBucketPolicy = GetBucketPolicyResponse
  request = Req.get s3Service
  response =
    Res.receiveBytes
      ( \s h x ->
          GetBucketPolicyResponse'
            Lude.<$> (Lude.pure x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketPolicy where
  toHeaders GetBucketPolicy' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketPolicy where
  toPath GetBucketPolicy' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketPolicy where
  toQuery = Lude.const (Lude.mconcat ["policy"])

-- | /See:/ 'mkGetBucketPolicyResponse' smart constructor.
data GetBucketPolicyResponse = GetBucketPolicyResponse'
  { -- | The bucket policy as a JSON document.
    policy :: Lude.ByteString,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - The bucket policy as a JSON document.
-- * 'responseStatus' - The response status code.
mkGetBucketPolicyResponse ::
  -- | 'policy'
  Lude.ByteString ->
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketPolicyResponse
mkGetBucketPolicyResponse pPolicy_ pResponseStatus_ =
  GetBucketPolicyResponse'
    { policy = pPolicy_,
      responseStatus = pResponseStatus_
    }

-- | The bucket policy as a JSON document.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsPolicy :: Lens.Lens' GetBucketPolicyResponse Lude.ByteString
gbprsPolicy = Lens.lens (policy :: GetBucketPolicyResponse -> Lude.ByteString) (\s a -> s {policy = a} :: GetBucketPolicyResponse)
{-# DEPRECATED gbprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsResponseStatus :: Lens.Lens' GetBucketPolicyResponse Lude.Int
gbprsResponseStatus = Lens.lens (responseStatus :: GetBucketPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketPolicyResponse)
{-# DEPRECATED gbprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
