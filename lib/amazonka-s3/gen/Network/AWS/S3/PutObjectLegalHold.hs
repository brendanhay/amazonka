{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObjectLegalHold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a Legal Hold configuration to the specified object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects> 
--
--
module Network.AWS.S3.PutObjectLegalHold
    (
    -- * Creating a request
      PutObjectLegalHold (..)
    , mkPutObjectLegalHold
    -- ** Request lenses
    , polhBucket
    , polhKey
    , polhContentMD5
    , polhExpectedBucketOwner
    , polhLegalHold
    , polhRequestPayer
    , polhVersionId

    -- * Destructuring the response
    , PutObjectLegalHoldResponse (..)
    , mkPutObjectLegalHoldResponse
    -- ** Response lenses
    , polhrrsRequestCharged
    , polhrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutObjectLegalHold' smart constructor.
data PutObjectLegalHold = PutObjectLegalHold'
  { bucket :: Types.BucketName
    -- ^ The bucket name containing the object that you want to place a Legal Hold on. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
  , key :: Types.ObjectKey
    -- ^ The key name for the object that you want to place a Legal Hold on.
  , contentMD5 :: Core.Maybe Types.ContentMD5
    -- ^ The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , legalHold :: Core.Maybe Types.ObjectLockLegalHold
    -- ^ Container element for the Legal Hold configuration you want to apply to the specified object.
  , requestPayer :: Core.Maybe Types.RequestPayer
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ The version ID of the object that you want to place a Legal Hold on.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectLegalHold' value with any optional fields omitted.
mkPutObjectLegalHold
    :: Types.BucketName -- ^ 'bucket'
    -> Types.ObjectKey -- ^ 'key'
    -> PutObjectLegalHold
mkPutObjectLegalHold bucket key
  = PutObjectLegalHold'{bucket, key, contentMD5 = Core.Nothing,
                        expectedBucketOwner = Core.Nothing, legalHold = Core.Nothing,
                        requestPayer = Core.Nothing, versionId = Core.Nothing}

-- | The bucket name containing the object that you want to place a Legal Hold on. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhBucket :: Lens.Lens' PutObjectLegalHold Types.BucketName
polhBucket = Lens.field @"bucket"
{-# INLINEABLE polhBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The key name for the object that you want to place a Legal Hold on.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhKey :: Lens.Lens' PutObjectLegalHold Types.ObjectKey
polhKey = Lens.field @"key"
{-# INLINEABLE polhKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhContentMD5 :: Lens.Lens' PutObjectLegalHold (Core.Maybe Types.ContentMD5)
polhContentMD5 = Lens.field @"contentMD5"
{-# INLINEABLE polhContentMD5 #-}
{-# DEPRECATED contentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhExpectedBucketOwner :: Lens.Lens' PutObjectLegalHold (Core.Maybe Types.AccountId)
polhExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE polhExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | Container element for the Legal Hold configuration you want to apply to the specified object.
--
-- /Note:/ Consider using 'legalHold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhLegalHold :: Lens.Lens' PutObjectLegalHold (Core.Maybe Types.ObjectLockLegalHold)
polhLegalHold = Lens.field @"legalHold"
{-# INLINEABLE polhLegalHold #-}
{-# DEPRECATED legalHold "Use generic-lens or generic-optics with 'legalHold' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhRequestPayer :: Lens.Lens' PutObjectLegalHold (Core.Maybe Types.RequestPayer)
polhRequestPayer = Lens.field @"requestPayer"
{-# INLINEABLE polhRequestPayer #-}
{-# DEPRECATED requestPayer "Use generic-lens or generic-optics with 'requestPayer' instead"  #-}

-- | The version ID of the object that you want to place a Legal Hold on.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhVersionId :: Lens.Lens' PutObjectLegalHold (Core.Maybe Types.ObjectVersionId)
polhVersionId = Lens.field @"versionId"
{-# INLINEABLE polhVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery PutObjectLegalHold where
        toQuery PutObjectLegalHold{..}
          = Core.maybe Core.mempty (Core.toQueryPair "versionId") versionId
              Core.<> Core.toQueryPair "legal-hold" ("" :: Core.Text)

instance Core.ToHeaders PutObjectLegalHold where
        toHeaders PutObjectLegalHold{..}
          = Core.toHeaders "Content-MD5" contentMD5 Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
              Core.<> Core.toHeaders "x-amz-request-payer" requestPayer

instance Core.AWSRequest PutObjectLegalHold where
        type Rs PutObjectLegalHold = PutObjectLegalHoldResponse
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
                 PutObjectLegalHoldResponse' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-request-charged" h) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutObjectLegalHoldResponse' smart constructor.
data PutObjectLegalHoldResponse = PutObjectLegalHoldResponse'
  { requestCharged :: Core.Maybe Types.RequestCharged
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectLegalHoldResponse' value with any optional fields omitted.
mkPutObjectLegalHoldResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutObjectLegalHoldResponse
mkPutObjectLegalHoldResponse responseStatus
  = PutObjectLegalHoldResponse'{requestCharged = Core.Nothing,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhrrsRequestCharged :: Lens.Lens' PutObjectLegalHoldResponse (Core.Maybe Types.RequestCharged)
polhrrsRequestCharged = Lens.field @"requestCharged"
{-# INLINEABLE polhrrsRequestCharged #-}
{-# DEPRECATED requestCharged "Use generic-lens or generic-optics with 'requestCharged' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
polhrrsResponseStatus :: Lens.Lens' PutObjectLegalHoldResponse Core.Int
polhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE polhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
