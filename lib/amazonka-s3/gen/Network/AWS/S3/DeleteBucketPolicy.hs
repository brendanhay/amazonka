{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dbpBucket,
    dbpExpectedBucketOwner,

    -- * Destructuring the response
    DeleteBucketPolicyResponse (..),
    mkDeleteBucketPolicyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteBucketPolicy' smart constructor.
data DeleteBucketPolicy = DeleteBucketPolicy'
  { -- | The bucket name.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketPolicy' value with any optional fields omitted.
mkDeleteBucketPolicy ::
  -- | 'bucket'
  Types.BucketName ->
  DeleteBucketPolicy
mkDeleteBucketPolicy bucket =
  DeleteBucketPolicy' {bucket, expectedBucketOwner = Core.Nothing}

-- | The bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpBucket :: Lens.Lens' DeleteBucketPolicy Types.BucketName
dbpBucket = Lens.field @"bucket"
{-# DEPRECATED dbpBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpExpectedBucketOwner :: Lens.Lens' DeleteBucketPolicy (Core.Maybe Types.ExpectedBucketOwner)
dbpExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED dbpExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest DeleteBucketPolicy where
  type Rs DeleteBucketPolicy = DeleteBucketPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("policy", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteBucketPolicyResponse'

-- | /See:/ 'mkDeleteBucketPolicyResponse' smart constructor.
data DeleteBucketPolicyResponse = DeleteBucketPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketPolicyResponse' value with any optional fields omitted.
mkDeleteBucketPolicyResponse ::
  DeleteBucketPolicyResponse
mkDeleteBucketPolicyResponse = DeleteBucketPolicyResponse'
