{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObjectTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the supplied tag-set to an object that already exists in a bucket.
--
-- A tag is a key-value pair. You can associate tags with an object by sending a PUT request against the tagging subresource that is associated with the object. You can retrieve tags by sending a GET request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging> .
-- For tagging-related restrictions related to characters and encodings, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html Tag Restrictions> . Note that Amazon S3 limits the maximum number of tags to 10 tags per object.
-- To use this operation, you must have permission to perform the @s3:PutObjectTagging@ action. By default, the bucket owner has this permission and can grant this permission to others.
-- To put tags of any other version, use the @versionId@ query parameter. You also need permission for the @s3:PutObjectVersionTagging@ action.
-- For information about the Amazon S3 object tagging feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging> .
-- __Special Errors__ 
--
--     * 
--     * /Code: InvalidTagError / 
--
--
--     * /Cause: The tag provided was not a valid tag. This error can occur if the tag did not pass input validation. For more information, see <https:\/\/docs.aws.amazon.com\/AmazonS3\/latest\/dev\/object-tagging.html Object Tagging> ./ 
--
--
--
--
--     * 
--     * /Code: MalformedXMLError / 
--
--
--     * /Cause: The XML provided does not match the schema./ 
--
--
--
--
--     * 
--     * /Code: OperationAbortedError / 
--
--
--     * /Cause: A conflicting conditional operation is currently in progress against this resource. Please try again./ 
--
--
--
--
--     * 
--     * /Code: InternalError/ 
--
--
--     * /Cause: The service was unable to apply the provided tag to the object./ 
--
--
--
--
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging> 
--
--
module Network.AWS.S3.PutObjectTagging
    (
    -- * Creating a request
      PutObjectTagging (..)
    , mkPutObjectTagging
    -- ** Request lenses
    , potBucket
    , potKey
    , potTagging
    , potContentMD5
    , potExpectedBucketOwner
    , potVersionId

    -- * Destructuring the response
    , PutObjectTaggingResponse (..)
    , mkPutObjectTaggingResponse
    -- ** Response lenses
    , potrrsVersionId
    , potrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutObjectTagging' smart constructor.
data PutObjectTagging = PutObjectTagging'
  { bucket :: Types.BucketName
    -- ^ The bucket name containing the object. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
  , key :: Types.ObjectKey
    -- ^ Name of the object key.
  , tagging :: Types.Tagging
    -- ^ Container for the @TagSet@ and @Tag@ elements
  , contentMD5 :: Core.Maybe Types.ContentMD5
    -- ^ The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ The versionId of the object that the tag-set will be added to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectTagging' value with any optional fields omitted.
mkPutObjectTagging
    :: Types.BucketName -- ^ 'bucket'
    -> Types.ObjectKey -- ^ 'key'
    -> Types.Tagging -- ^ 'tagging'
    -> PutObjectTagging
mkPutObjectTagging bucket key tagging
  = PutObjectTagging'{bucket, key, tagging,
                      contentMD5 = Core.Nothing, expectedBucketOwner = Core.Nothing,
                      versionId = Core.Nothing}

-- | The bucket name containing the object. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potBucket :: Lens.Lens' PutObjectTagging Types.BucketName
potBucket = Lens.field @"bucket"
{-# INLINEABLE potBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Name of the object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potKey :: Lens.Lens' PutObjectTagging Types.ObjectKey
potKey = Lens.field @"key"
{-# INLINEABLE potKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Container for the @TagSet@ and @Tag@ elements
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potTagging :: Lens.Lens' PutObjectTagging Types.Tagging
potTagging = Lens.field @"tagging"
{-# INLINEABLE potTagging #-}
{-# DEPRECATED tagging "Use generic-lens or generic-optics with 'tagging' instead"  #-}

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potContentMD5 :: Lens.Lens' PutObjectTagging (Core.Maybe Types.ContentMD5)
potContentMD5 = Lens.field @"contentMD5"
{-# INLINEABLE potContentMD5 #-}
{-# DEPRECATED contentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potExpectedBucketOwner :: Lens.Lens' PutObjectTagging (Core.Maybe Types.AccountId)
potExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE potExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | The versionId of the object that the tag-set will be added to.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potVersionId :: Lens.Lens' PutObjectTagging (Core.Maybe Types.ObjectVersionId)
potVersionId = Lens.field @"versionId"
{-# INLINEABLE potVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery PutObjectTagging where
        toQuery PutObjectTagging{..}
          = Core.maybe Core.mempty (Core.toQueryPair "versionId") versionId
              Core.<> Core.toQueryPair "tagging" ("" :: Core.Text)

instance Core.ToHeaders PutObjectTagging where
        toHeaders PutObjectTagging{..}
          = Core.toHeaders "Content-MD5" contentMD5 Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest PutObjectTagging where
        type Rs PutObjectTagging = PutObjectTaggingResponse
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
                 PutObjectTaggingResponse' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-version-id" h) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutObjectTaggingResponse' smart constructor.
data PutObjectTaggingResponse = PutObjectTaggingResponse'
  { versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ The versionId of the object the tag-set was added to.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectTaggingResponse' value with any optional fields omitted.
mkPutObjectTaggingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutObjectTaggingResponse
mkPutObjectTaggingResponse responseStatus
  = PutObjectTaggingResponse'{versionId = Core.Nothing,
                              responseStatus}

-- | The versionId of the object the tag-set was added to.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potrrsVersionId :: Lens.Lens' PutObjectTaggingResponse (Core.Maybe Types.ObjectVersionId)
potrrsVersionId = Lens.field @"versionId"
{-# INLINEABLE potrrsVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potrrsResponseStatus :: Lens.Lens' PutObjectTaggingResponse Core.Int
potrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE potrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
