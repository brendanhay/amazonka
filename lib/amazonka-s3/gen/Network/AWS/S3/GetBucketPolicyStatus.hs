{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketPolicyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the policy status for an Amazon S3 bucket, indicating whether the bucket is public. In order to use this operation, you must have the @s3:GetBucketPolicyStatus@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
-- For more information about when Amazon S3 considers a bucket public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> .
-- The following operations are related to @GetBucketPolicyStatus@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
module Network.AWS.S3.GetBucketPolicyStatus
  ( -- * Creating a request
    GetBucketPolicyStatus (..),
    mkGetBucketPolicyStatus,

    -- ** Request lenses
    gbpsExpectedBucketOwner,
    gbpsBucket,

    -- * Destructuring the response
    GetBucketPolicyStatusResponse (..),
    mkGetBucketPolicyStatusResponse,

    -- ** Response lenses
    gbpsrsPolicyStatus,
    gbpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketPolicyStatus' smart constructor.
data GetBucketPolicyStatus = GetBucketPolicyStatus'
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

-- | Creates a value of 'GetBucketPolicyStatus' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the Amazon S3 bucket whose policy status you want to retrieve.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketPolicyStatus ::
  -- | 'bucket'
  BucketName ->
  GetBucketPolicyStatus
mkGetBucketPolicyStatus pBucket_ =
  GetBucketPolicyStatus'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpsExpectedBucketOwner :: Lens.Lens' GetBucketPolicyStatus (Lude.Maybe Lude.Text)
gbpsExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketPolicyStatus -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketPolicyStatus)
{-# DEPRECATED gbpsExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the Amazon S3 bucket whose policy status you want to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpsBucket :: Lens.Lens' GetBucketPolicyStatus BucketName
gbpsBucket = Lens.lens (bucket :: GetBucketPolicyStatus -> BucketName) (\s a -> s {bucket = a} :: GetBucketPolicyStatus)
{-# DEPRECATED gbpsBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest GetBucketPolicyStatus where
  type Rs GetBucketPolicyStatus = GetBucketPolicyStatusResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketPolicyStatusResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketPolicyStatus where
  toHeaders GetBucketPolicyStatus' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketPolicyStatus where
  toPath GetBucketPolicyStatus' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketPolicyStatus where
  toQuery = Lude.const (Lude.mconcat ["policyStatus"])

-- | /See:/ 'mkGetBucketPolicyStatusResponse' smart constructor.
data GetBucketPolicyStatusResponse = GetBucketPolicyStatusResponse'
  { policyStatus ::
      Lude.Maybe PolicyStatus,
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

-- | Creates a value of 'GetBucketPolicyStatusResponse' with the minimum fields required to make a request.
--
-- * 'policyStatus' - The policy status for the specified bucket.
-- * 'responseStatus' - The response status code.
mkGetBucketPolicyStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketPolicyStatusResponse
mkGetBucketPolicyStatusResponse pResponseStatus_ =
  GetBucketPolicyStatusResponse'
    { policyStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The policy status for the specified bucket.
--
-- /Note:/ Consider using 'policyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpsrsPolicyStatus :: Lens.Lens' GetBucketPolicyStatusResponse (Lude.Maybe PolicyStatus)
gbpsrsPolicyStatus = Lens.lens (policyStatus :: GetBucketPolicyStatusResponse -> Lude.Maybe PolicyStatus) (\s a -> s {policyStatus = a} :: GetBucketPolicyStatusResponse)
{-# DEPRECATED gbpsrsPolicyStatus "Use generic-lens or generic-optics with 'policyStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpsrsResponseStatus :: Lens.Lens' GetBucketPolicyStatusResponse Lude.Int
gbpsrsResponseStatus = Lens.lens (responseStatus :: GetBucketPolicyStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketPolicyStatusResponse)
{-# DEPRECATED gbpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
