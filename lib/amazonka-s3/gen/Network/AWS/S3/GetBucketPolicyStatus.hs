{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketPolicyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the policy status for an Amazon S3 bucket, indicating whether the bucket is public. In order to use this operation, you must have the @s3:GetBucketPolicyStatus@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
-- For more information about when Amazon S3 considers a bucket public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> . 
-- The following operations are related to @GetBucketPolicyStatus@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock> 
--
--
module Network.AWS.S3.GetBucketPolicyStatus
    (
    -- * Creating a request
      GetBucketPolicyStatus (..)
    , mkGetBucketPolicyStatus
    -- ** Request lenses
    , gbpsBucket
    , gbpsExpectedBucketOwner

    -- * Destructuring the response
    , GetBucketPolicyStatusResponse (..)
    , mkGetBucketPolicyStatusResponse
    -- ** Response lenses
    , gbpsrrsPolicyStatus
    , gbpsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketPolicyStatus' smart constructor.
data GetBucketPolicyStatus = GetBucketPolicyStatus'
  { bucket :: Types.BucketName
    -- ^ The name of the Amazon S3 bucket whose policy status you want to retrieve.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketPolicyStatus' value with any optional fields omitted.
mkGetBucketPolicyStatus
    :: Types.BucketName -- ^ 'bucket'
    -> GetBucketPolicyStatus
mkGetBucketPolicyStatus bucket
  = GetBucketPolicyStatus'{bucket,
                           expectedBucketOwner = Core.Nothing}

-- | The name of the Amazon S3 bucket whose policy status you want to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpsBucket :: Lens.Lens' GetBucketPolicyStatus Types.BucketName
gbpsBucket = Lens.field @"bucket"
{-# INLINEABLE gbpsBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpsExpectedBucketOwner :: Lens.Lens' GetBucketPolicyStatus (Core.Maybe Types.ExpectedBucketOwner)
gbpsExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE gbpsExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery GetBucketPolicyStatus where
        toQuery GetBucketPolicyStatus{..}
          = Core.toQueryPair "policyStatus" ("" :: Core.Text)

instance Core.ToHeaders GetBucketPolicyStatus where
        toHeaders GetBucketPolicyStatus{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest GetBucketPolicyStatus where
        type Rs GetBucketPolicyStatus = GetBucketPolicyStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetBucketPolicyStatusResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBucketPolicyStatusResponse' smart constructor.
data GetBucketPolicyStatusResponse = GetBucketPolicyStatusResponse'
  { policyStatus :: Core.Maybe Types.PolicyStatus
    -- ^ The policy status for the specified bucket.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketPolicyStatusResponse' value with any optional fields omitted.
mkGetBucketPolicyStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBucketPolicyStatusResponse
mkGetBucketPolicyStatusResponse responseStatus
  = GetBucketPolicyStatusResponse'{policyStatus = Core.Nothing,
                                   responseStatus}

-- | The policy status for the specified bucket.
--
-- /Note:/ Consider using 'policyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpsrrsPolicyStatus :: Lens.Lens' GetBucketPolicyStatusResponse (Core.Maybe Types.PolicyStatus)
gbpsrrsPolicyStatus = Lens.field @"policyStatus"
{-# INLINEABLE gbpsrrsPolicyStatus #-}
{-# DEPRECATED policyStatus "Use generic-lens or generic-optics with 'policyStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpsrrsResponseStatus :: Lens.Lens' GetBucketPolicyStatusResponse Core.Int
gbpsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbpsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
