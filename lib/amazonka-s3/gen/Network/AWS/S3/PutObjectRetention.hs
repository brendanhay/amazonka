{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.S3.PutObjectRetention
    (
    -- * Creating a request
      PutObjectRetention (..)
    , mkPutObjectRetention
    -- ** Request lenses
    , porBucket
    , porKey
    , porBypassGovernanceRetention
    , porContentMD5
    , porExpectedBucketOwner
    , porRequestPayer
    , porRetention
    , porVersionId

    -- * Destructuring the response
    , PutObjectRetentionResponse (..)
    , mkPutObjectRetentionResponse
    -- ** Response lenses
    , porrrsRequestCharged
    , porrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutObjectRetention' smart constructor.
data PutObjectRetention = PutObjectRetention'
  { bucket :: Types.BucketName
    -- ^ The bucket name that contains the object you want to apply this Object Retention configuration to. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
  , key :: Types.ObjectKey
    -- ^ The key name for the object that you want to apply this Object Retention configuration to.
  , bypassGovernanceRetention :: Core.Maybe Core.Bool
    -- ^ Indicates whether this operation should bypass Governance-mode restrictions.
  , contentMD5 :: Core.Maybe Types.ContentMD5
    -- ^ The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , requestPayer :: Core.Maybe Types.RequestPayer
  , retention :: Core.Maybe Types.ObjectLockRetention
    -- ^ The container element for the Object Retention configuration.
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ The version ID for the object that you want to apply this Object Retention configuration to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutObjectRetention' value with any optional fields omitted.
mkPutObjectRetention
    :: Types.BucketName -- ^ 'bucket'
    -> Types.ObjectKey -- ^ 'key'
    -> PutObjectRetention
mkPutObjectRetention bucket key
  = PutObjectRetention'{bucket, key,
                        bypassGovernanceRetention = Core.Nothing,
                        contentMD5 = Core.Nothing, expectedBucketOwner = Core.Nothing,
                        requestPayer = Core.Nothing, retention = Core.Nothing,
                        versionId = Core.Nothing}

-- | The bucket name that contains the object you want to apply this Object Retention configuration to. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porBucket :: Lens.Lens' PutObjectRetention Types.BucketName
porBucket = Lens.field @"bucket"
{-# INLINEABLE porBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The key name for the object that you want to apply this Object Retention configuration to.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porKey :: Lens.Lens' PutObjectRetention Types.ObjectKey
porKey = Lens.field @"key"
{-# INLINEABLE porKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Indicates whether this operation should bypass Governance-mode restrictions.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porBypassGovernanceRetention :: Lens.Lens' PutObjectRetention (Core.Maybe Core.Bool)
porBypassGovernanceRetention = Lens.field @"bypassGovernanceRetention"
{-# INLINEABLE porBypassGovernanceRetention #-}
{-# DEPRECATED bypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead"  #-}

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porContentMD5 :: Lens.Lens' PutObjectRetention (Core.Maybe Types.ContentMD5)
porContentMD5 = Lens.field @"contentMD5"
{-# INLINEABLE porContentMD5 #-}
{-# DEPRECATED contentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porExpectedBucketOwner :: Lens.Lens' PutObjectRetention (Core.Maybe Types.AccountId)
porExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE porExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porRequestPayer :: Lens.Lens' PutObjectRetention (Core.Maybe Types.RequestPayer)
porRequestPayer = Lens.field @"requestPayer"
{-# INLINEABLE porRequestPayer #-}
{-# DEPRECATED requestPayer "Use generic-lens or generic-optics with 'requestPayer' instead"  #-}

-- | The container element for the Object Retention configuration.
--
-- /Note:/ Consider using 'retention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porRetention :: Lens.Lens' PutObjectRetention (Core.Maybe Types.ObjectLockRetention)
porRetention = Lens.field @"retention"
{-# INLINEABLE porRetention #-}
{-# DEPRECATED retention "Use generic-lens or generic-optics with 'retention' instead"  #-}

-- | The version ID for the object that you want to apply this Object Retention configuration to.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porVersionId :: Lens.Lens' PutObjectRetention (Core.Maybe Types.ObjectVersionId)
porVersionId = Lens.field @"versionId"
{-# INLINEABLE porVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery PutObjectRetention where
        toQuery PutObjectRetention{..}
          = Core.maybe Core.mempty (Core.toQueryPair "versionId") versionId
              Core.<> Core.toQueryPair "retention" ("" :: Core.Text)

instance Core.ToHeaders PutObjectRetention where
        toHeaders PutObjectRetention{..}
          = Core.toHeaders "x-amz-bypass-governance-retention"
              bypassGovernanceRetention
              Core.<> Core.toHeaders "Content-MD5" contentMD5
              Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
              Core.<> Core.toHeaders "x-amz-request-payer" requestPayer

instance Core.AWSRequest PutObjectRetention where
        type Rs PutObjectRetention = PutObjectRetentionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/" Core.<> Core.toText bucket Core.<> "/" Core.<> Core.toText key,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutObjectRetentionResponse' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-request-charged" h) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutObjectRetentionResponse' smart constructor.
data PutObjectRetentionResponse = PutObjectRetentionResponse'
  { requestCharged :: Core.Maybe Types.RequestCharged
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectRetentionResponse' value with any optional fields omitted.
mkPutObjectRetentionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutObjectRetentionResponse
mkPutObjectRetentionResponse responseStatus
  = PutObjectRetentionResponse'{requestCharged = Core.Nothing,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrrsRequestCharged :: Lens.Lens' PutObjectRetentionResponse (Core.Maybe Types.RequestCharged)
porrrsRequestCharged = Lens.field @"requestCharged"
{-# INLINEABLE porrrsRequestCharged #-}
{-# DEPRECATED requestCharged "Use generic-lens or generic-optics with 'requestCharged' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrrsResponseStatus :: Lens.Lens' PutObjectRetentionResponse Core.Int
porrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE porrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
