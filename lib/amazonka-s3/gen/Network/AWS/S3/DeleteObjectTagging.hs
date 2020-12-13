{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteObjectTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the entire tag set from the specified object. For more information about managing object tags, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging> .
--
-- To use this operation, you must have permission to perform the @s3:DeleteObjectTagging@ action.
-- To delete tags of a specific object version, add the @versionId@ query parameter in the request. You will need permission for the @s3:DeleteObjectVersionTagging@ action.
-- The following operations are related to @DeleteBucketMetricsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObjectTagging.html PutObjectTagging>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging>
module Network.AWS.S3.DeleteObjectTagging
  ( -- * Creating a request
    DeleteObjectTagging (..),
    mkDeleteObjectTagging,

    -- ** Request lenses
    dotVersionId,
    dotBucket,
    dotKey,
    dotExpectedBucketOwner,

    -- * Destructuring the response
    DeleteObjectTaggingResponse (..),
    mkDeleteObjectTaggingResponse,

    -- ** Response lenses
    dotrsVersionId,
    dotrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteObjectTagging' smart constructor.
data DeleteObjectTagging = DeleteObjectTagging'
  { -- | The versionId of the object that the tag-set will be removed from.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | The bucket name containing the objects from which to remove the tags.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: BucketName,
    -- | Name of the object key.
    key :: ObjectKey,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteObjectTagging' with the minimum fields required to make a request.
--
-- * 'versionId' - The versionId of the object that the tag-set will be removed from.
-- * 'bucket' - The bucket name containing the objects from which to remove the tags.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'key' - Name of the object key.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteObjectTagging ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  DeleteObjectTagging
mkDeleteObjectTagging pBucket_ pKey_ =
  DeleteObjectTagging'
    { versionId = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The versionId of the object that the tag-set will be removed from.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotVersionId :: Lens.Lens' DeleteObjectTagging (Lude.Maybe ObjectVersionId)
dotVersionId = Lens.lens (versionId :: DeleteObjectTagging -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: DeleteObjectTagging)
{-# DEPRECATED dotVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The bucket name containing the objects from which to remove the tags.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotBucket :: Lens.Lens' DeleteObjectTagging BucketName
dotBucket = Lens.lens (bucket :: DeleteObjectTagging -> BucketName) (\s a -> s {bucket = a} :: DeleteObjectTagging)
{-# DEPRECATED dotBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Name of the object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotKey :: Lens.Lens' DeleteObjectTagging ObjectKey
dotKey = Lens.lens (key :: DeleteObjectTagging -> ObjectKey) (\s a -> s {key = a} :: DeleteObjectTagging)
{-# DEPRECATED dotKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotExpectedBucketOwner :: Lens.Lens' DeleteObjectTagging (Lude.Maybe Lude.Text)
dotExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteObjectTagging -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteObjectTagging)
{-# DEPRECATED dotExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest DeleteObjectTagging where
  type Rs DeleteObjectTagging = DeleteObjectTaggingResponse
  request = Req.delete s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteObjectTaggingResponse'
            Lude.<$> (h Lude..#? "x-amz-version-id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteObjectTagging where
  toHeaders DeleteObjectTagging' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteObjectTagging where
  toPath DeleteObjectTagging' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery DeleteObjectTagging where
  toQuery DeleteObjectTagging' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId, "tagging"]

-- | /See:/ 'mkDeleteObjectTaggingResponse' smart constructor.
data DeleteObjectTaggingResponse = DeleteObjectTaggingResponse'
  { -- | The versionId of the object the tag-set was removed from.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteObjectTaggingResponse' with the minimum fields required to make a request.
--
-- * 'versionId' - The versionId of the object the tag-set was removed from.
-- * 'responseStatus' - The response status code.
mkDeleteObjectTaggingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteObjectTaggingResponse
mkDeleteObjectTaggingResponse pResponseStatus_ =
  DeleteObjectTaggingResponse'
    { versionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The versionId of the object the tag-set was removed from.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotrsVersionId :: Lens.Lens' DeleteObjectTaggingResponse (Lude.Maybe ObjectVersionId)
dotrsVersionId = Lens.lens (versionId :: DeleteObjectTaggingResponse -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: DeleteObjectTaggingResponse)
{-# DEPRECATED dotrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotrsResponseStatus :: Lens.Lens' DeleteObjectTaggingResponse Lude.Int
dotrsResponseStatus = Lens.lens (responseStatus :: DeleteObjectTaggingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteObjectTaggingResponse)
{-# DEPRECATED dotrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
