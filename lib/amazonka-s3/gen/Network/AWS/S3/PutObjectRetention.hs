{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObjectRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places an Object Retention configuration on an object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>
module Network.AWS.S3.PutObjectRetention
  ( -- * Creating a request
    PutObjectRetention (..),
    mkPutObjectRetention,

    -- ** Request lenses
    porBucket,
    porKey,
    porBypassGovernanceRetention,
    porContentMD5,
    porExpectedBucketOwner,
    porRequestPayer,
    porRetention,
    porVersionId,

    -- * Destructuring the response
    PutObjectRetentionResponse (..),
    mkPutObjectRetentionResponse,

    -- ** Response lenses
    porrrsRequestCharged,
    porrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutObjectRetention' smart constructor.
data PutObjectRetention = PutObjectRetention'
  { -- | The bucket name that contains the object you want to apply this Object Retention configuration to.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | The key name for the object that you want to apply this Object Retention configuration to.
    key :: Types.ObjectKey,
    -- | Indicates whether this operation should bypass Governance-mode restrictions.
    bypassGovernanceRetention :: Core.Maybe Core.Bool,
    -- | The MD5 hash for the request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Core.Maybe Types.ContentMD5,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId,
    requestPayer :: Core.Maybe Types.RequestPayer,
    -- | The container element for the Object Retention configuration.
    retention :: Core.Maybe Types.ObjectLockRetention,
    -- | The version ID for the object that you want to apply this Object Retention configuration to.
    versionId :: Core.Maybe Types.ObjectVersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutObjectRetention' value with any optional fields omitted.
mkPutObjectRetention ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'key'
  Types.ObjectKey ->
  PutObjectRetention
mkPutObjectRetention bucket key =
  PutObjectRetention'
    { bucket,
      key,
      bypassGovernanceRetention = Core.Nothing,
      contentMD5 = Core.Nothing,
      expectedBucketOwner = Core.Nothing,
      requestPayer = Core.Nothing,
      retention = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The bucket name that contains the object you want to apply this Object Retention configuration to.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porBucket :: Lens.Lens' PutObjectRetention Types.BucketName
porBucket = Lens.field @"bucket"
{-# DEPRECATED porBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The key name for the object that you want to apply this Object Retention configuration to.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porKey :: Lens.Lens' PutObjectRetention Types.ObjectKey
porKey = Lens.field @"key"
{-# DEPRECATED porKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Indicates whether this operation should bypass Governance-mode restrictions.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porBypassGovernanceRetention :: Lens.Lens' PutObjectRetention (Core.Maybe Core.Bool)
porBypassGovernanceRetention = Lens.field @"bypassGovernanceRetention"
{-# DEPRECATED porBypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead." #-}

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porContentMD5 :: Lens.Lens' PutObjectRetention (Core.Maybe Types.ContentMD5)
porContentMD5 = Lens.field @"contentMD5"
{-# DEPRECATED porContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porExpectedBucketOwner :: Lens.Lens' PutObjectRetention (Core.Maybe Types.AccountId)
porExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED porExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porRequestPayer :: Lens.Lens' PutObjectRetention (Core.Maybe Types.RequestPayer)
porRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED porRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | The container element for the Object Retention configuration.
--
-- /Note:/ Consider using 'retention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porRetention :: Lens.Lens' PutObjectRetention (Core.Maybe Types.ObjectLockRetention)
porRetention = Lens.field @"retention"
{-# DEPRECATED porRetention "Use generic-lens or generic-optics with 'retention' instead." #-}

-- | The version ID for the object that you want to apply this Object Retention configuration to.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porVersionId :: Lens.Lens' PutObjectRetention (Core.Maybe Types.ObjectVersionId)
porVersionId = Lens.field @"versionId"
{-# DEPRECATED porVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest PutObjectRetention where
  type Rs PutObjectRetention = PutObjectRetentionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText bucket) Core.<> ("/")
                Core.<> (Core.toText key)
            ),
        Core._rqQuery =
          Core.toQueryValue "versionId" Core.<$> versionId
            Core.<> (Core.pure ("retention", "")),
        Core._rqHeaders =
          Core.toHeaders
            "x-amz-bypass-governance-retention"
            bypassGovernanceRetention
            Core.<> (Core.toHeaders "Content-MD5" contentMD5)
            Core.<> (Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner)
            Core.<> (Core.toHeaders "x-amz-request-payer" requestPayer),
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectRetentionResponse'
            Core.<$> (Core.parseHeaderMaybe "x-amz-request-charged" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutObjectRetentionResponse' smart constructor.
data PutObjectRetentionResponse = PutObjectRetentionResponse'
  { requestCharged :: Core.Maybe Types.RequestCharged,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectRetentionResponse' value with any optional fields omitted.
mkPutObjectRetentionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutObjectRetentionResponse
mkPutObjectRetentionResponse responseStatus =
  PutObjectRetentionResponse'
    { requestCharged = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrrsRequestCharged :: Lens.Lens' PutObjectRetentionResponse (Core.Maybe Types.RequestCharged)
porrrsRequestCharged = Lens.field @"requestCharged"
{-# DEPRECATED porrrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrrsResponseStatus :: Lens.Lens' PutObjectRetentionResponse Core.Int
porrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED porrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
