{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the tags for a bucket.
--
-- Use tags to organize your AWS bill to reflect your own cost structure. To do this, sign up to get your AWS account bill with tag key values included. Then, to see the cost of combined resources, organize your billing information according to resources with the same tag key values. For example, you can tag several resources with a specific application name, and then organize your billing information to see the total cost of that application across several services. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Cost Allocation and Tagging> .
-- To use this operation, you must have permissions to perform the @s3:PutBucketTagging@ action. The bucket owner has this permission by default and can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- @PutBucketTagging@ has the following special errors:
--
--     * Error code: @InvalidTagError@ 
--
--     * Description: The tag provided was not a valid tag. This error can occur if the tag did not pass input validation. For information about tag restrictions, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-Defined Tag Restrictions> and <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/aws-tag-restrictions.html AWS-Generated Cost Allocation Tag Restrictions> .
--
--
--
--
--     * Error code: @MalformedXMLError@ 
--
--     * Description: The XML provided does not match the schema.
--
--
--
--
--     * Error code: @OperationAbortedError @ 
--
--     * Description: A conflicting conditional operation is currently in progress against this resource. Please try again.
--
--
--
--
--     * Error code: @InternalError@ 
--
--     * Description: The service was unable to apply the provided tag to the bucket.
--
--
--
--
-- The following operations are related to @PutBucketTagging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketTagging.html GetBucketTagging> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketTagging.html DeleteBucketTagging> 
--
--
module Network.AWS.S3.PutBucketTagging
    (
    -- * Creating a request
      PutBucketTagging (..)
    , mkPutBucketTagging
    -- ** Request lenses
    , pbtBucket
    , pbtTagging
    , pbtContentMD5
    , pbtExpectedBucketOwner

    -- * Destructuring the response
    , PutBucketTaggingResponse (..)
    , mkPutBucketTaggingResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketTagging' smart constructor.
data PutBucketTagging = PutBucketTagging'
  { bucket :: Types.BucketName
    -- ^ The bucket name.
  , tagging :: Types.Tagging
    -- ^ Container for the @TagSet@ and @Tag@ elements.
  , contentMD5 :: Core.Maybe Types.ContentMD5
    -- ^ The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketTagging' value with any optional fields omitted.
mkPutBucketTagging
    :: Types.BucketName -- ^ 'bucket'
    -> Types.Tagging -- ^ 'tagging'
    -> PutBucketTagging
mkPutBucketTagging bucket tagging
  = PutBucketTagging'{bucket, tagging, contentMD5 = Core.Nothing,
                      expectedBucketOwner = Core.Nothing}

-- | The bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbtBucket :: Lens.Lens' PutBucketTagging Types.BucketName
pbtBucket = Lens.field @"bucket"
{-# INLINEABLE pbtBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Container for the @TagSet@ and @Tag@ elements.
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbtTagging :: Lens.Lens' PutBucketTagging Types.Tagging
pbtTagging = Lens.field @"tagging"
{-# INLINEABLE pbtTagging #-}
{-# DEPRECATED tagging "Use generic-lens or generic-optics with 'tagging' instead"  #-}

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbtContentMD5 :: Lens.Lens' PutBucketTagging (Core.Maybe Types.ContentMD5)
pbtContentMD5 = Lens.field @"contentMD5"
{-# INLINEABLE pbtContentMD5 #-}
{-# DEPRECATED contentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbtExpectedBucketOwner :: Lens.Lens' PutBucketTagging (Core.Maybe Types.AccountId)
pbtExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE pbtExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery PutBucketTagging where
        toQuery PutBucketTagging{..}
          = Core.toQueryPair "tagging" ("" :: Core.Text)

instance Core.ToHeaders PutBucketTagging where
        toHeaders PutBucketTagging{..}
          = Core.toHeaders "Content-MD5" contentMD5 Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest PutBucketTagging where
        type Rs PutBucketTagging = PutBucketTaggingResponse
        toRequest x@Core.Request{..}
          = Request.contentMD5Header Core.$
              Core.Request{Core._rqService = Types.mkServiceConfig,
                           Core._rqMethod = Request.PUT,
                           Core._rqPath = "/" Core.<> Core.toText bucket,
                           Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                           Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PutBucketTaggingResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBucketTaggingResponse' smart constructor.
data PutBucketTaggingResponse = PutBucketTaggingResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketTaggingResponse' value with any optional fields omitted.
mkPutBucketTaggingResponse
    :: PutBucketTaggingResponse
mkPutBucketTaggingResponse = PutBucketTaggingResponse'
