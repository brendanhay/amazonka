{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tag-set of an object. You send the GET request against the tagging subresource associated with the object.
--
-- To use this operation, you must have permission to perform the @s3:GetObjectTagging@ action. By default, the GET operation returns information about current version of an object. For a versioned bucket, you can have multiple versions of an object in your bucket. To retrieve tags of any other version, use the versionId query parameter. You also need permission for the @s3:GetObjectVersionTagging@ action.
-- By default, the bucket owner has this permission and can grant this permission to others.
-- For information about the Amazon S3 object tagging feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging> .
-- The following operation is related to @GetObjectTagging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObjectTagging.html PutObjectTagging>
module Network.AWS.S3.GetObjectTagging
  ( -- * Creating a request
    GetObjectTagging (..),
    mkGetObjectTagging,

    -- ** Request lenses
    gVersionId,
    gBucket,
    gKey,
    gExpectedBucketOwner,

    -- * Destructuring the response
    GetObjectTaggingResponse (..),
    mkGetObjectTaggingResponse,

    -- ** Response lenses
    gotfrsVersionId,
    gotfrsTagSet,
    gotfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetObjectTagging' smart constructor.
data GetObjectTagging = GetObjectTagging'
  { -- | The versionId of the object for which to get the tagging information.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | The bucket name containing the object for which to get the tagging information.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: BucketName,
    -- | Object key for which to get the tagging information.
    key :: ObjectKey,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetObjectTagging' with the minimum fields required to make a request.
--
-- * 'versionId' - The versionId of the object for which to get the tagging information.
-- * 'bucket' - The bucket name containing the object for which to get the tagging information.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'key' - Object key for which to get the tagging information.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetObjectTagging ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectTagging
mkGetObjectTagging pBucket_ pKey_ =
  GetObjectTagging'
    { versionId = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The versionId of the object for which to get the tagging information.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gVersionId :: Lens.Lens' GetObjectTagging (Lude.Maybe ObjectVersionId)
gVersionId = Lens.lens (versionId :: GetObjectTagging -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: GetObjectTagging)
{-# DEPRECATED gVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The bucket name containing the object for which to get the tagging information.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gBucket :: Lens.Lens' GetObjectTagging BucketName
gBucket = Lens.lens (bucket :: GetObjectTagging -> BucketName) (\s a -> s {bucket = a} :: GetObjectTagging)
{-# DEPRECATED gBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Object key for which to get the tagging information.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gKey :: Lens.Lens' GetObjectTagging ObjectKey
gKey = Lens.lens (key :: GetObjectTagging -> ObjectKey) (\s a -> s {key = a} :: GetObjectTagging)
{-# DEPRECATED gKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gExpectedBucketOwner :: Lens.Lens' GetObjectTagging (Lude.Maybe Lude.Text)
gExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetObjectTagging -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetObjectTagging)
{-# DEPRECATED gExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest GetObjectTagging where
  type Rs GetObjectTagging = GetObjectTaggingResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetObjectTaggingResponse'
            Lude.<$> (h Lude..#? "x-amz-version-id")
            Lude.<*> ( x Lude..@? "TagSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "Tag"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetObjectTagging where
  toHeaders GetObjectTagging' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetObjectTagging where
  toPath GetObjectTagging' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery GetObjectTagging where
  toQuery GetObjectTagging' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId, "tagging"]

-- | /See:/ 'mkGetObjectTaggingResponse' smart constructor.
data GetObjectTaggingResponse = GetObjectTaggingResponse'
  { -- | The versionId of the object for which you got the tagging information.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | Contains the tag set.
    tagSet :: [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetObjectTaggingResponse' with the minimum fields required to make a request.
--
-- * 'versionId' - The versionId of the object for which you got the tagging information.
-- * 'tagSet' - Contains the tag set.
-- * 'responseStatus' - The response status code.
mkGetObjectTaggingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetObjectTaggingResponse
mkGetObjectTaggingResponse pResponseStatus_ =
  GetObjectTaggingResponse'
    { versionId = Lude.Nothing,
      tagSet = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The versionId of the object for which you got the tagging information.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotfrsVersionId :: Lens.Lens' GetObjectTaggingResponse (Lude.Maybe ObjectVersionId)
gotfrsVersionId = Lens.lens (versionId :: GetObjectTaggingResponse -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: GetObjectTaggingResponse)
{-# DEPRECATED gotfrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Contains the tag set.
--
-- /Note:/ Consider using 'tagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotfrsTagSet :: Lens.Lens' GetObjectTaggingResponse [Tag]
gotfrsTagSet = Lens.lens (tagSet :: GetObjectTaggingResponse -> [Tag]) (\s a -> s {tagSet = a} :: GetObjectTaggingResponse)
{-# DEPRECATED gotfrsTagSet "Use generic-lens or generic-optics with 'tagSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotfrsResponseStatus :: Lens.Lens' GetObjectTaggingResponse Lude.Int
gotfrsResponseStatus = Lens.lens (responseStatus :: GetObjectTaggingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetObjectTaggingResponse)
{-# DEPRECATED gotfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
