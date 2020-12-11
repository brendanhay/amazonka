{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the DELETE operation uses the policy subresource to delete the policy of a specified bucket. If you are using an identity other than the root user of the AWS account that owns the bucket, the calling identity must have the @DeleteBucketPolicy@ permissions on the specified bucket and belong to the bucket owner's account to use this operation.
--
-- If you don't have @DeleteBucketPolicy@ permissions, Amazon S3 returns a @403 Access Denied@ error. If you have the correct permissions, but you're not using an identity that belongs to the bucket owner's account, Amazon S3 returns a @405 Method Not Allowed@ error.
-- /Important:/ As a security precaution, the root user of the AWS account that owns a bucket can always use this operation, even if the policy explicitly denies the root user the ability to perform this action.
-- For more information about bucket policies, see < https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies and UserPolicies> .
-- The following operations are related to @DeleteBucketPolicy@
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Network.AWS.S3.DeleteBucketPolicy
  ( -- * Creating a request
    DeleteBucketPolicy (..),
    mkDeleteBucketPolicy,

    -- ** Request lenses
    dbpExpectedBucketOwner,
    dbpBucket,

    -- * Destructuring the response
    DeleteBucketPolicyResponse (..),
    mkDeleteBucketPolicyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketPolicy' smart constructor.
data DeleteBucketPolicy = DeleteBucketPolicy'
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

-- | Creates a value of 'DeleteBucketPolicy' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteBucketPolicy ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketPolicy
mkDeleteBucketPolicy pBucket_ =
  DeleteBucketPolicy'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpExpectedBucketOwner :: Lens.Lens' DeleteBucketPolicy (Lude.Maybe Lude.Text)
dbpExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucketPolicy -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucketPolicy)
{-# DEPRECATED dbpExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpBucket :: Lens.Lens' DeleteBucketPolicy BucketName
dbpBucket = Lens.lens (bucket :: DeleteBucketPolicy -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketPolicy)
{-# DEPRECATED dbpBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest DeleteBucketPolicy where
  type Rs DeleteBucketPolicy = DeleteBucketPolicyResponse
  request = Req.delete s3Service
  response = Res.receiveNull DeleteBucketPolicyResponse'

instance Lude.ToHeaders DeleteBucketPolicy where
  toHeaders DeleteBucketPolicy' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucketPolicy where
  toPath DeleteBucketPolicy' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketPolicy where
  toQuery = Lude.const (Lude.mconcat ["policy"])

-- | /See:/ 'mkDeleteBucketPolicyResponse' smart constructor.
data DeleteBucketPolicyResponse = DeleteBucketPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketPolicyResponse' with the minimum fields required to make a request.
mkDeleteBucketPolicyResponse ::
  DeleteBucketPolicyResponse
mkDeleteBucketPolicyResponse = DeleteBucketPolicyResponse'
