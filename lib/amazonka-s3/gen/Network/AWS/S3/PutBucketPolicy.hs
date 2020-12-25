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
    pbpPolicy,
    pbpConfirmRemoveSelfBucketAccess,
    pbpContentMD5,
    pbpExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketPolicyResponse (..),
    mkPutBucketPolicyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketPolicy' smart constructor.
data PutBucketPolicy = PutBucketPolicy'
  { -- | The name of the bucket.
    bucket :: Types.BucketName,
    -- | The bucket policy as a JSON document.
    policy :: Core.ByteString,
    -- | Set this parameter to true to confirm that you want to remove your permissions to change this bucket policy in the future.
    confirmRemoveSelfBucketAccess :: Core.Maybe Core.Bool,
    -- | The MD5 hash of the request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Core.Maybe Types.ContentMD5,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketPolicy' value with any optional fields omitted.
mkPutBucketPolicy ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'policy'
  Core.ByteString ->
  PutBucketPolicy
mkPutBucketPolicy bucket policy =
  PutBucketPolicy'
    { bucket,
      policy,
      confirmRemoveSelfBucketAccess = Core.Nothing,
      contentMD5 = Core.Nothing,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpBucket :: Lens.Lens' PutBucketPolicy Types.BucketName
pbpBucket = Lens.field @"bucket"
{-# DEPRECATED pbpBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The bucket policy as a JSON document.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpPolicy :: Lens.Lens' PutBucketPolicy Core.ByteString
pbpPolicy = Lens.field @"policy"
{-# DEPRECATED pbpPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | Set this parameter to true to confirm that you want to remove your permissions to change this bucket policy in the future.
--
-- /Note:/ Consider using 'confirmRemoveSelfBucketAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpConfirmRemoveSelfBucketAccess :: Lens.Lens' PutBucketPolicy (Core.Maybe Core.Bool)
pbpConfirmRemoveSelfBucketAccess = Lens.field @"confirmRemoveSelfBucketAccess"
{-# DEPRECATED pbpConfirmRemoveSelfBucketAccess "Use generic-lens or generic-optics with 'confirmRemoveSelfBucketAccess' instead." #-}

-- | The MD5 hash of the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpContentMD5 :: Lens.Lens' PutBucketPolicy (Core.Maybe Types.ContentMD5)
pbpContentMD5 = Lens.field @"contentMD5"
{-# DEPRECATED pbpContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpExpectedBucketOwner :: Lens.Lens' PutBucketPolicy (Core.Maybe Types.ExpectedBucketOwner)
pbpExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED pbpExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest PutBucketPolicy where
  type Rs PutBucketPolicy = PutBucketPolicyResponse
  request x@Core.Request {..} =
    Request.contentMD5Header Core.$
      Core.Request
        { Core._rqService = Types.mkServiceConfig,
          Core._rqMethod = Request.PUT,
          Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
          Core._rqQuery = Core.pure ("policy", ""),
          Core._rqHeaders =
            Core.toHeaders
              "x-amz-confirm-remove-self-bucket-access"
              confirmRemoveSelfBucketAccess
              Core.<> (Core.toHeaders "Content-MD5" contentMD5)
              Core.<> (Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner),
          Core._rqBody = Core.toBody policy
        }
  response = Response.receiveNull PutBucketPolicyResponse'

-- | /See:/ 'mkPutBucketPolicyResponse' smart constructor.
data PutBucketPolicyResponse = PutBucketPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketPolicyResponse' value with any optional fields omitted.
mkPutBucketPolicyResponse ::
  PutBucketPolicyResponse
mkPutBucketPolicyResponse = PutBucketPolicyResponse'
