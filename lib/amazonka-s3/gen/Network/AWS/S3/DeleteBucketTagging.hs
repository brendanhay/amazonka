{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the tags from the bucket.
--
-- To use this operation, you must have permission to perform the @s3:PutBucketTagging@ action. By default, the bucket owner has this permission and can grant this permission to others.
-- The following operations are related to @DeleteBucketTagging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketTagging.html GetBucketTagging>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketTagging.html PutBucketTagging>
module Network.AWS.S3.DeleteBucketTagging
  ( -- * Creating a request
    DeleteBucketTagging (..),
    mkDeleteBucketTagging,

    -- ** Request lenses
    dbtBucket,
    dbtExpectedBucketOwner,

    -- * Destructuring the response
    DeleteBucketTaggingResponse (..),
    mkDeleteBucketTaggingResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteBucketTagging' smart constructor.
data DeleteBucketTagging = DeleteBucketTagging'
  { -- | The bucket that has the tag set to be removed.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketTagging' value with any optional fields omitted.
mkDeleteBucketTagging ::
  -- | 'bucket'
  Types.BucketName ->
  DeleteBucketTagging
mkDeleteBucketTagging bucket =
  DeleteBucketTagging' {bucket, expectedBucketOwner = Core.Nothing}

-- | The bucket that has the tag set to be removed.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtBucket :: Lens.Lens' DeleteBucketTagging Types.BucketName
dbtBucket = Lens.field @"bucket"
{-# DEPRECATED dbtBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtExpectedBucketOwner :: Lens.Lens' DeleteBucketTagging (Core.Maybe Types.AccountId)
dbtExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED dbtExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest DeleteBucketTagging where
  type Rs DeleteBucketTagging = DeleteBucketTaggingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("tagging", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteBucketTaggingResponse'

-- | /See:/ 'mkDeleteBucketTaggingResponse' smart constructor.
data DeleteBucketTaggingResponse = DeleteBucketTaggingResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketTaggingResponse' value with any optional fields omitted.
mkDeleteBucketTaggingResponse ::
  DeleteBucketTaggingResponse
mkDeleteBucketTaggingResponse = DeleteBucketTaggingResponse'
