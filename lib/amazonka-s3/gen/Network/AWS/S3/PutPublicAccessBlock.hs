{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutPublicAccessBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or modifies the @PublicAccessBlock@ configuration for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketPublicAccessBlock@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
-- /Important:/ When Amazon S3 evaluates the @PublicAccessBlock@ configuration for a bucket or an object, it checks the @PublicAccessBlock@ configuration for both the bucket (or the bucket that contains the object) and the bucket owner's account. If the @PublicAccessBlock@ configurations are different between the bucket and the account, Amazon S3 uses the most restrictive combination of the bucket-level and account-level settings.
-- For more information about when Amazon S3 considers a bucket or an object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> .
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketPolicyStatus.html GetBucketPolicyStatus>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
module Network.AWS.S3.PutPublicAccessBlock
  ( -- * Creating a request
    PutPublicAccessBlock (..),
    mkPutPublicAccessBlock,

    -- ** Request lenses
    ppabBucket,
    ppabPublicAccessBlockConfiguration,
    ppabContentMD5,
    ppabExpectedBucketOwner,

    -- * Destructuring the response
    PutPublicAccessBlockResponse (..),
    mkPutPublicAccessBlockResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutPublicAccessBlock' smart constructor.
data PutPublicAccessBlock = PutPublicAccessBlock'
  { -- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to set.
    bucket :: Types.BucketName,
    -- | The @PublicAccessBlock@ configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
    publicAccessBlockConfiguration :: Types.PublicAccessBlockConfiguration,
    -- | The MD5 hash of the @PutPublicAccessBlock@ request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Core.Maybe Types.ContentMD5,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutPublicAccessBlock' value with any optional fields omitted.
mkPutPublicAccessBlock ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'publicAccessBlockConfiguration'
  Types.PublicAccessBlockConfiguration ->
  PutPublicAccessBlock
mkPutPublicAccessBlock bucket publicAccessBlockConfiguration =
  PutPublicAccessBlock'
    { bucket,
      publicAccessBlockConfiguration,
      contentMD5 = Core.Nothing,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to set.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppabBucket :: Lens.Lens' PutPublicAccessBlock Types.BucketName
ppabBucket = Lens.field @"bucket"
{-# DEPRECATED ppabBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The @PublicAccessBlock@ configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'publicAccessBlockConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppabPublicAccessBlockConfiguration :: Lens.Lens' PutPublicAccessBlock Types.PublicAccessBlockConfiguration
ppabPublicAccessBlockConfiguration = Lens.field @"publicAccessBlockConfiguration"
{-# DEPRECATED ppabPublicAccessBlockConfiguration "Use generic-lens or generic-optics with 'publicAccessBlockConfiguration' instead." #-}

-- | The MD5 hash of the @PutPublicAccessBlock@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppabContentMD5 :: Lens.Lens' PutPublicAccessBlock (Core.Maybe Types.ContentMD5)
ppabContentMD5 = Lens.field @"contentMD5"
{-# DEPRECATED ppabContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppabExpectedBucketOwner :: Lens.Lens' PutPublicAccessBlock (Core.Maybe Types.ExpectedBucketOwner)
ppabExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED ppabExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest PutPublicAccessBlock where
  type Rs PutPublicAccessBlock = PutPublicAccessBlockResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("publicAccessBlock", ""),
        Core._rqHeaders =
          Core.toHeaders "Content-MD5" contentMD5
            Core.<> (Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner),
        Core._rqBody = Core.toXMLBody x
      }
  response = Response.receiveNull PutPublicAccessBlockResponse'

-- | /See:/ 'mkPutPublicAccessBlockResponse' smart constructor.
data PutPublicAccessBlockResponse = PutPublicAccessBlockResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutPublicAccessBlockResponse' value with any optional fields omitted.
mkPutPublicAccessBlockResponse ::
  PutPublicAccessBlockResponse
mkPutPublicAccessBlockResponse = PutPublicAccessBlockResponse'
