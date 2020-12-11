{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the null version (if there is one) of an object and inserts a delete marker, which becomes the latest version of the object. If there isn't a null version, Amazon S3 does not remove any objects.
--
-- To remove a specific version, you must be the bucket owner and you must use the version Id subresource. Using this subresource permanently deletes the version. If the object deleted is a delete marker, Amazon S3 sets the response header, @x-amz-delete-marker@ , to true.
-- If the object you want to delete is in a bucket where the bucket versioning configuration is MFA Delete enabled, you must include the @x-amz-mfa@ request header in the DELETE @versionId@ request. Requests that include @x-amz-mfa@ must use HTTPS.
-- For more information about MFA Delete, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMFADelete.html Using MFA Delete> . To see sample requests that use versioning, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectDELETE.html#ExampleVersionObjectDelete Sample Request> .
-- You can delete objects by explicitly calling the DELETE Object API or configure its lifecycle (<https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycle.html PutBucketLifecycle> ) to enable Amazon S3 to remove them for you. If you want to block users or accounts from removing or deleting objects from your bucket, you must deny them the @s3:DeleteObject@ , @s3:DeleteObjectVersion@ , and @s3:PutLifeCycleConfiguration@ actions.
-- The following operation is related to @DeleteObject@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
module Network.AWS.S3.DeleteObject
  ( -- * Creating a request
    DeleteObject (..),
    mkDeleteObject,

    -- ** Request lenses
    doVersionId,
    doMFA,
    doRequestPayer,
    doBypassGovernanceRetention,
    doExpectedBucketOwner,
    doBucket,
    doKey,

    -- * Destructuring the response
    DeleteObjectResponse (..),
    mkDeleteObjectResponse,

    -- ** Response lenses
    dorsRequestCharged,
    dorsVersionId,
    dorsDeleteMarker,
    dorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteObject' smart constructor.
data DeleteObject = DeleteObject'
  { versionId ::
      Lude.Maybe ObjectVersionId,
    mfa :: Lude.Maybe Lude.Text,
    requestPayer :: Lude.Maybe RequestPayer,
    bypassGovernanceRetention :: Lude.Maybe Lude.Bool,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    bucket :: BucketName,
    key :: ObjectKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteObject' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name of the bucket containing the object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'bypassGovernanceRetention' - Indicates whether S3 Object Lock should bypass Governance-mode restrictions to process this operation.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'key' - Key name of the object to delete.
-- * 'mfa' - The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
-- * 'requestPayer' - Undocumented field.
-- * 'versionId' - VersionId used to reference a specific version of the object.
mkDeleteObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  DeleteObject
mkDeleteObject pBucket_ pKey_ =
  DeleteObject'
    { versionId = Lude.Nothing,
      mfa = Lude.Nothing,
      requestPayer = Lude.Nothing,
      bypassGovernanceRetention = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | VersionId used to reference a specific version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doVersionId :: Lens.Lens' DeleteObject (Lude.Maybe ObjectVersionId)
doVersionId = Lens.lens (versionId :: DeleteObject -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: DeleteObject)
{-# DEPRECATED doVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
--
-- /Note:/ Consider using 'mfa' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doMFA :: Lens.Lens' DeleteObject (Lude.Maybe Lude.Text)
doMFA = Lens.lens (mfa :: DeleteObject -> Lude.Maybe Lude.Text) (\s a -> s {mfa = a} :: DeleteObject)
{-# DEPRECATED doMFA "Use generic-lens or generic-optics with 'mfa' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doRequestPayer :: Lens.Lens' DeleteObject (Lude.Maybe RequestPayer)
doRequestPayer = Lens.lens (requestPayer :: DeleteObject -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: DeleteObject)
{-# DEPRECATED doRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Indicates whether S3 Object Lock should bypass Governance-mode restrictions to process this operation.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doBypassGovernanceRetention :: Lens.Lens' DeleteObject (Lude.Maybe Lude.Bool)
doBypassGovernanceRetention = Lens.lens (bypassGovernanceRetention :: DeleteObject -> Lude.Maybe Lude.Bool) (\s a -> s {bypassGovernanceRetention = a} :: DeleteObject)
{-# DEPRECATED doBypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doExpectedBucketOwner :: Lens.Lens' DeleteObject (Lude.Maybe Lude.Text)
doExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteObject -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteObject)
{-# DEPRECATED doExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name of the bucket containing the object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doBucket :: Lens.Lens' DeleteObject BucketName
doBucket = Lens.lens (bucket :: DeleteObject -> BucketName) (\s a -> s {bucket = a} :: DeleteObject)
{-# DEPRECATED doBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Key name of the object to delete.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doKey :: Lens.Lens' DeleteObject ObjectKey
doKey = Lens.lens (key :: DeleteObject -> ObjectKey) (\s a -> s {key = a} :: DeleteObject)
{-# DEPRECATED doKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.AWSRequest DeleteObject where
  type Rs DeleteObject = DeleteObjectResponse
  request = Req.delete s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteObjectResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (h Lude..#? "x-amz-version-id")
            Lude.<*> (h Lude..#? "x-amz-delete-marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteObject where
  toHeaders DeleteObject' {..} =
    Lude.mconcat
      [ "x-amz-mfa" Lude.=# mfa,
        "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-bypass-governance-retention"
          Lude.=# bypassGovernanceRetention,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath DeleteObject where
  toPath DeleteObject' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery DeleteObject where
  toQuery DeleteObject' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId]

-- | /See:/ 'mkDeleteObjectResponse' smart constructor.
data DeleteObjectResponse = DeleteObjectResponse'
  { requestCharged ::
      Lude.Maybe RequestCharged,
    versionId :: Lude.Maybe ObjectVersionId,
    deleteMarker :: Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteObjectResponse' with the minimum fields required to make a request.
--
-- * 'deleteMarker' - Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker.
-- * 'requestCharged' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'versionId' - Returns the version ID of the delete marker created as a result of the DELETE operation.
mkDeleteObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteObjectResponse
mkDeleteObjectResponse pResponseStatus_ =
  DeleteObjectResponse'
    { requestCharged = Lude.Nothing,
      versionId = Lude.Nothing,
      deleteMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsRequestCharged :: Lens.Lens' DeleteObjectResponse (Lude.Maybe RequestCharged)
dorsRequestCharged = Lens.lens (requestCharged :: DeleteObjectResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: DeleteObjectResponse)
{-# DEPRECATED dorsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | Returns the version ID of the delete marker created as a result of the DELETE operation.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsVersionId :: Lens.Lens' DeleteObjectResponse (Lude.Maybe ObjectVersionId)
dorsVersionId = Lens.lens (versionId :: DeleteObjectResponse -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: DeleteObjectResponse)
{-# DEPRECATED dorsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker.
--
-- /Note:/ Consider using 'deleteMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsDeleteMarker :: Lens.Lens' DeleteObjectResponse (Lude.Maybe Lude.Bool)
dorsDeleteMarker = Lens.lens (deleteMarker :: DeleteObjectResponse -> Lude.Maybe Lude.Bool) (\s a -> s {deleteMarker = a} :: DeleteObjectResponse)
{-# DEPRECATED dorsDeleteMarker "Use generic-lens or generic-optics with 'deleteMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsResponseStatus :: Lens.Lens' DeleteObjectResponse Lude.Int
dorsResponseStatus = Lens.lens (responseStatus :: DeleteObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteObjectResponse)
{-# DEPRECATED dorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
