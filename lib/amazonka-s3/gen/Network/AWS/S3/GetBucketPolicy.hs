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
    gbprrsPolicy,
    gbprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketPolicy' smart constructor.
data GetBucketPolicy = GetBucketPolicy'
  { -- | The bucket name for which to get the bucket policy.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketPolicy' value with any optional fields omitted.
mkGetBucketPolicy ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketPolicy
mkGetBucketPolicy bucket =
  GetBucketPolicy' {bucket, expectedBucketOwner = Core.Nothing}

-- | The bucket name for which to get the bucket policy.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpBucket :: Lens.Lens' GetBucketPolicy Types.BucketName
gbpBucket = Lens.field @"bucket"
{-# DEPRECATED gbpBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpExpectedBucketOwner :: Lens.Lens' GetBucketPolicy (Core.Maybe Types.ExpectedBucketOwner)
gbpExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gbpExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketPolicy where
  type Rs GetBucketPolicy = GetBucketPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("policy", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveBytes
      ( \s h x ->
          GetBucketPolicyResponse'
            Core.<$> (Core.pure (Core.Just x)) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketPolicyResponse' smart constructor.
data GetBucketPolicyResponse = GetBucketPolicyResponse'
  { -- | The bucket policy as a JSON document.
    policy :: Core.ByteString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketPolicyResponse' value with any optional fields omitted.
mkGetBucketPolicyResponse ::
  -- | 'policy'
  Core.ByteString ->
  -- | 'responseStatus'
  Core.Int ->
  GetBucketPolicyResponse
mkGetBucketPolicyResponse policy responseStatus =
  GetBucketPolicyResponse' {policy, responseStatus}

-- | The bucket policy as a JSON document.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsPolicy :: Lens.Lens' GetBucketPolicyResponse Core.ByteString
gbprrsPolicy = Lens.field @"policy"
{-# DEPRECATED gbprrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsResponseStatus :: Lens.Lens' GetBucketPolicyResponse Core.Int
gbprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
