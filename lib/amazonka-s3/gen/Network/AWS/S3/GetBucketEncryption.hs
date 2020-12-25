{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default encryption configuration for an Amazon S3 bucket. For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> .
--
-- To use this operation, you must have permission to perform the @s3:GetEncryptionConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- The following operations are related to @GetBucketEncryption@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketEncryption.html PutBucketEncryption>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketEncryption.html DeleteBucketEncryption>
module Network.AWS.S3.GetBucketEncryption
  ( -- * Creating a request
    GetBucketEncryption (..),
    mkGetBucketEncryption,

    -- ** Request lenses
    gbeBucket,
    gbeExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketEncryptionResponse (..),
    mkGetBucketEncryptionResponse,

    -- ** Response lenses
    gberrsServerSideEncryptionConfiguration,
    gberrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketEncryption' smart constructor.
data GetBucketEncryption = GetBucketEncryption'
  { -- | The name of the bucket from which the server-side encryption configuration is retrieved.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketEncryption' value with any optional fields omitted.
mkGetBucketEncryption ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketEncryption
mkGetBucketEncryption bucket =
  GetBucketEncryption' {bucket, expectedBucketOwner = Core.Nothing}

-- | The name of the bucket from which the server-side encryption configuration is retrieved.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbeBucket :: Lens.Lens' GetBucketEncryption Types.BucketName
gbeBucket = Lens.field @"bucket"
{-# DEPRECATED gbeBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbeExpectedBucketOwner :: Lens.Lens' GetBucketEncryption (Core.Maybe Types.ExpectedBucketOwner)
gbeExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gbeExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketEncryption where
  type Rs GetBucketEncryption = GetBucketEncryptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("encryption", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketEncryptionResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketEncryptionResponse' smart constructor.
data GetBucketEncryptionResponse = GetBucketEncryptionResponse'
  { serverSideEncryptionConfiguration :: Core.Maybe Types.ServerSideEncryptionConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketEncryptionResponse' value with any optional fields omitted.
mkGetBucketEncryptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBucketEncryptionResponse
mkGetBucketEncryptionResponse responseStatus =
  GetBucketEncryptionResponse'
    { serverSideEncryptionConfiguration =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'serverSideEncryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gberrsServerSideEncryptionConfiguration :: Lens.Lens' GetBucketEncryptionResponse (Core.Maybe Types.ServerSideEncryptionConfiguration)
gberrsServerSideEncryptionConfiguration = Lens.field @"serverSideEncryptionConfiguration"
{-# DEPRECATED gberrsServerSideEncryptionConfiguration "Use generic-lens or generic-optics with 'serverSideEncryptionConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gberrsResponseStatus :: Lens.Lens' GetBucketEncryptionResponse Core.Int
gberrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gberrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
