{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectLegalHold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an object's current Legal Hold status. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects> .
--
-- This action is not supported by Amazon S3 on Outposts.
module Network.AWS.S3.GetObjectLegalHold
    (
    -- * Creating a request
      GetObjectLegalHold (..)
    , mkGetObjectLegalHold
    -- ** Request lenses
    , golhBucket
    , golhKey
    , golhExpectedBucketOwner
    , golhRequestPayer
    , golhVersionId

    -- * Destructuring the response
    , GetObjectLegalHoldResponse (..)
    , mkGetObjectLegalHoldResponse
    -- ** Response lenses
    , golhrrsLegalHold
    , golhrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetObjectLegalHold' smart constructor.
data GetObjectLegalHold = GetObjectLegalHold'
  { bucket :: Types.BucketName
    -- ^ The bucket name containing the object whose Legal Hold status you want to retrieve. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
  , key :: Types.Key
    -- ^ The key name for the object whose Legal Hold status you want to retrieve.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , requestPayer :: Core.Maybe Types.RequestPayer
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ The version ID of the object whose Legal Hold status you want to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectLegalHold' value with any optional fields omitted.
mkGetObjectLegalHold
    :: Types.BucketName -- ^ 'bucket'
    -> Types.Key -- ^ 'key'
    -> GetObjectLegalHold
mkGetObjectLegalHold bucket key
  = GetObjectLegalHold'{bucket, key,
                        expectedBucketOwner = Core.Nothing, requestPayer = Core.Nothing,
                        versionId = Core.Nothing}

-- | The bucket name containing the object whose Legal Hold status you want to retrieve. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhBucket :: Lens.Lens' GetObjectLegalHold Types.BucketName
golhBucket = Lens.field @"bucket"
{-# INLINEABLE golhBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The key name for the object whose Legal Hold status you want to retrieve.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhKey :: Lens.Lens' GetObjectLegalHold Types.Key
golhKey = Lens.field @"key"
{-# INLINEABLE golhKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhExpectedBucketOwner :: Lens.Lens' GetObjectLegalHold (Core.Maybe Types.ExpectedBucketOwner)
golhExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE golhExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhRequestPayer :: Lens.Lens' GetObjectLegalHold (Core.Maybe Types.RequestPayer)
golhRequestPayer = Lens.field @"requestPayer"
{-# INLINEABLE golhRequestPayer #-}
{-# DEPRECATED requestPayer "Use generic-lens or generic-optics with 'requestPayer' instead"  #-}

-- | The version ID of the object whose Legal Hold status you want to retrieve.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhVersionId :: Lens.Lens' GetObjectLegalHold (Core.Maybe Types.ObjectVersionId)
golhVersionId = Lens.field @"versionId"
{-# INLINEABLE golhVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery GetObjectLegalHold where
        toQuery GetObjectLegalHold{..}
          = Core.maybe Core.mempty (Core.toQueryPair "versionId") versionId
              Core.<> Core.toQueryPair "legal-hold" ("" :: Core.Text)

instance Core.ToHeaders GetObjectLegalHold where
        toHeaders GetObjectLegalHold{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
              Core.<> Core.toHeaders "x-amz-request-payer" requestPayer

instance Core.AWSRequest GetObjectLegalHold where
        type Rs GetObjectLegalHold = GetObjectLegalHoldResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/" Core.<> Core.toText bucket Core.<> "/" Core.<> Core.toText key,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetObjectLegalHoldResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetObjectLegalHoldResponse' smart constructor.
data GetObjectLegalHoldResponse = GetObjectLegalHoldResponse'
  { legalHold :: Core.Maybe Types.ObjectLockLegalHold
    -- ^ The current Legal Hold status for the specified object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectLegalHoldResponse' value with any optional fields omitted.
mkGetObjectLegalHoldResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetObjectLegalHoldResponse
mkGetObjectLegalHoldResponse responseStatus
  = GetObjectLegalHoldResponse'{legalHold = Core.Nothing,
                                responseStatus}

-- | The current Legal Hold status for the specified object.
--
-- /Note:/ Consider using 'legalHold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhrrsLegalHold :: Lens.Lens' GetObjectLegalHoldResponse (Core.Maybe Types.ObjectLockLegalHold)
golhrrsLegalHold = Lens.field @"legalHold"
{-# INLINEABLE golhrrsLegalHold #-}
{-# DEPRECATED legalHold "Use generic-lens or generic-optics with 'legalHold' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golhrrsResponseStatus :: Lens.Lens' GetObjectLegalHoldResponse Core.Int
golhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE golhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
