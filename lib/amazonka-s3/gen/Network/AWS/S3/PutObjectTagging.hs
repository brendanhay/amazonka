{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.S3.PutObjectTagging
  ( -- * Creating a request
    PutObjectTagging (..),
    mkPutObjectTagging,

    -- ** Request lenses
    potVersionId,
    potBucket,
    potKey,
    potTagging,
    potContentMD5,
    potExpectedBucketOwner,

    -- * Destructuring the response
    PutObjectTaggingResponse (..),
    mkPutObjectTaggingResponse,

    -- ** Response lenses
    potrsVersionId,
    potrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutObjectTagging' smart constructor.
data PutObjectTagging = PutObjectTagging'
  { -- | The versionId of the object that the tag-set will be added to.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | The bucket name containing the object.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: BucketName,
    -- | Name of the object key.
    key :: ObjectKey,
    -- | Container for the @TagSet@ and @Tag@ elements
    tagging :: Tagging,
    -- | The MD5 hash for the request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutObjectTagging' with the minimum fields required to make a request.
--
-- * 'versionId' - The versionId of the object that the tag-set will be added to.
-- * 'bucket' - The bucket name containing the object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'key' - Name of the object key.
-- * 'tagging' - Container for the @TagSet@ and @Tag@ elements
-- * 'contentMD5' - The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutObjectTagging ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'tagging'
  Tagging ->
  PutObjectTagging
mkPutObjectTagging pBucket_ pKey_ pTagging_ =
  PutObjectTagging'
    { versionId = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      tagging = pTagging_,
      contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | The versionId of the object that the tag-set will be added to.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potVersionId :: Lens.Lens' PutObjectTagging (Lude.Maybe ObjectVersionId)
potVersionId = Lens.lens (versionId :: PutObjectTagging -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: PutObjectTagging)
{-# DEPRECATED potVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The bucket name containing the object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potBucket :: Lens.Lens' PutObjectTagging BucketName
potBucket = Lens.lens (bucket :: PutObjectTagging -> BucketName) (\s a -> s {bucket = a} :: PutObjectTagging)
{-# DEPRECATED potBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Name of the object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potKey :: Lens.Lens' PutObjectTagging ObjectKey
potKey = Lens.lens (key :: PutObjectTagging -> ObjectKey) (\s a -> s {key = a} :: PutObjectTagging)
{-# DEPRECATED potKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Container for the @TagSet@ and @Tag@ elements
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potTagging :: Lens.Lens' PutObjectTagging Tagging
potTagging = Lens.lens (tagging :: PutObjectTagging -> Tagging) (\s a -> s {tagging = a} :: PutObjectTagging)
{-# DEPRECATED potTagging "Use generic-lens or generic-optics with 'tagging' instead." #-}

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potContentMD5 :: Lens.Lens' PutObjectTagging (Lude.Maybe Lude.Text)
potContentMD5 = Lens.lens (contentMD5 :: PutObjectTagging -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutObjectTagging)
{-# DEPRECATED potContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potExpectedBucketOwner :: Lens.Lens' PutObjectTagging (Lude.Maybe Lude.Text)
potExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutObjectTagging -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutObjectTagging)
{-# DEPRECATED potExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutObjectTagging where
  type Rs PutObjectTagging = PutObjectTaggingResponse
  request = Req.putXML s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutObjectTaggingResponse'
            Lude.<$> (h Lude..#? "x-amz-version-id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement PutObjectTagging where
  toElement =
    Lude.mkElement "{http://s3.amazonaws.com/doc/2006-03-01/}Tagging"
      Lude.. tagging

instance Lude.ToHeaders PutObjectTagging where
  toHeaders PutObjectTagging' {..} =
    Lude.mconcat
      [ "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutObjectTagging where
  toPath PutObjectTagging' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery PutObjectTagging where
  toQuery PutObjectTagging' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId, "tagging"]

-- | /See:/ 'mkPutObjectTaggingResponse' smart constructor.
data PutObjectTaggingResponse = PutObjectTaggingResponse'
  { -- | The versionId of the object the tag-set was added to.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutObjectTaggingResponse' with the minimum fields required to make a request.
--
-- * 'versionId' - The versionId of the object the tag-set was added to.
-- * 'responseStatus' - The response status code.
mkPutObjectTaggingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutObjectTaggingResponse
mkPutObjectTaggingResponse pResponseStatus_ =
  PutObjectTaggingResponse'
    { versionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The versionId of the object the tag-set was added to.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potrsVersionId :: Lens.Lens' PutObjectTaggingResponse (Lude.Maybe ObjectVersionId)
potrsVersionId = Lens.lens (versionId :: PutObjectTaggingResponse -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: PutObjectTaggingResponse)
{-# DEPRECATED potrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
potrsResponseStatus :: Lens.Lens' PutObjectTaggingResponse Lude.Int
potrsResponseStatus = Lens.lens (responseStatus :: PutObjectTaggingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutObjectTaggingResponse)
{-# DEPRECATED potrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
