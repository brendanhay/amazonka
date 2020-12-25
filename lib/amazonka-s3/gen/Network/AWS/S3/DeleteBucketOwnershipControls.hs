{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketOwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes @OwnershipControls@ for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketOwnershipControls@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
-- For information about Amazon S3 Object Ownership, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/about-object-ownership.html Using Object Ownership> .
-- The following operations are related to @DeleteBucketOwnershipControls@ :
--
--     * 'GetBucketOwnershipControls'
--
--
--     * 'PutBucketOwnershipControls'
module Network.AWS.S3.DeleteBucketOwnershipControls
  ( -- * Creating a request
    DeleteBucketOwnershipControls (..),
    mkDeleteBucketOwnershipControls,

    -- ** Request lenses
    dbocBucket,
    dbocExpectedBucketOwner,

    -- * Destructuring the response
    DeleteBucketOwnershipControlsResponse (..),
    mkDeleteBucketOwnershipControlsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteBucketOwnershipControls' smart constructor.
data DeleteBucketOwnershipControls = DeleteBucketOwnershipControls'
  { -- | The Amazon S3 bucket whose @OwnershipControls@ you want to delete.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketOwnershipControls' value with any optional fields omitted.
mkDeleteBucketOwnershipControls ::
  -- | 'bucket'
  Types.BucketName ->
  DeleteBucketOwnershipControls
mkDeleteBucketOwnershipControls bucket =
  DeleteBucketOwnershipControls'
    { bucket,
      expectedBucketOwner = Core.Nothing
    }

-- | The Amazon S3 bucket whose @OwnershipControls@ you want to delete.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbocBucket :: Lens.Lens' DeleteBucketOwnershipControls Types.BucketName
dbocBucket = Lens.field @"bucket"
{-# DEPRECATED dbocBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbocExpectedBucketOwner :: Lens.Lens' DeleteBucketOwnershipControls (Core.Maybe Types.AccountId)
dbocExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED dbocExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest DeleteBucketOwnershipControls where
  type
    Rs DeleteBucketOwnershipControls =
      DeleteBucketOwnershipControlsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("ownershipControls", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveNull DeleteBucketOwnershipControlsResponse'

-- | /See:/ 'mkDeleteBucketOwnershipControlsResponse' smart constructor.
data DeleteBucketOwnershipControlsResponse = DeleteBucketOwnershipControlsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketOwnershipControlsResponse' value with any optional fields omitted.
mkDeleteBucketOwnershipControlsResponse ::
  DeleteBucketOwnershipControlsResponse
mkDeleteBucketOwnershipControlsResponse =
  DeleteBucketOwnershipControlsResponse'
