{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation enables you to delete multiple objects from a bucket using a single HTTP request. If you know the object keys that you want to delete, then this operation provides a suitable alternative to sending individual delete requests, reducing per-request overhead.
--
-- The request contains a list of up to 1000 keys that you want to delete. In the XML, you provide the object key names, and optionally, version IDs if you want to delete a specific version of the object from a versioning-enabled bucket. For each key, Amazon S3 performs a delete operation and returns the result of that delete, success, or failure, in the response. Note that if the object specified in the request is not found, Amazon S3 returns the result as deleted.
-- The operation supports two modes for the response: verbose and quiet. By default, the operation uses verbose mode in which the response includes the result of deletion of each key in your request. In quiet mode the response includes only keys where the delete operation encountered an error. For a successful deletion, the operation does not return any information about the delete in the response body.
-- When performing this operation on an MFA Delete enabled bucket, that attempts to delete any versioned objects, you must include an MFA token. If you do not provide one, the entire request will fail, even if there are non-versioned objects you are trying to delete. If you provide an invalid token, whether there are versioned keys in the request or not, the entire Multi-Object Delete request will fail. For information about MFA Delete, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/Versioning.html#MultiFactorAuthenticationDelete MFA Delete> .
-- Finally, the Content-MD5 header is required for all Multi-Object Delete requests. Amazon S3 uses the header value to ensure that your request body has not been altered in transit.
-- The following operations are related to @DeleteObjects@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
module Network.AWS.S3.DeleteObjects
  ( -- * Creating a request
    DeleteObjects (..),
    mkDeleteObjects,

    -- ** Request lenses
    doMFA,
    doBucket,
    doRequestPayer,
    doBypassGovernanceRetention,
    doExpectedBucketOwner,
    doDelete,

    -- * Destructuring the response
    DeleteObjectsResponse (..),
    mkDeleteObjectsResponse,

    -- ** Response lenses
    dorsRequestCharged,
    dorsDeleted,
    dorsErrors,
    dorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteObjects' smart constructor.
data DeleteObjects = DeleteObjects'
  { -- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
    mfa :: Lude.Maybe Lude.Text,
    -- | The bucket name containing the objects to delete.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: BucketName,
    requestPayer :: Lude.Maybe RequestPayer,
    -- | Specifies whether you want to delete this object even if it has a Governance-type Object Lock in place. You must have sufficient permissions to perform this operation.
    bypassGovernanceRetention :: Lude.Maybe Lude.Bool,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    -- | Container for the request.
    delete :: Delete
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteObjects' with the minimum fields required to make a request.
--
-- * 'mfa' - The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
-- * 'bucket' - The bucket name containing the objects to delete.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'requestPayer' -
-- * 'bypassGovernanceRetention' - Specifies whether you want to delete this object even if it has a Governance-type Object Lock in place. You must have sufficient permissions to perform this operation.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'delete' - Container for the request.
mkDeleteObjects ::
  -- | 'bucket'
  BucketName ->
  -- | 'delete'
  Delete ->
  DeleteObjects
mkDeleteObjects pBucket_ pDelete_ =
  DeleteObjects'
    { mfa = Lude.Nothing,
      bucket = pBucket_,
      requestPayer = Lude.Nothing,
      bypassGovernanceRetention = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      delete = pDelete_
    }

-- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
--
-- /Note:/ Consider using 'mfa' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doMFA :: Lens.Lens' DeleteObjects (Lude.Maybe Lude.Text)
doMFA = Lens.lens (mfa :: DeleteObjects -> Lude.Maybe Lude.Text) (\s a -> s {mfa = a} :: DeleteObjects)
{-# DEPRECATED doMFA "Use generic-lens or generic-optics with 'mfa' instead." #-}

-- | The bucket name containing the objects to delete.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doBucket :: Lens.Lens' DeleteObjects BucketName
doBucket = Lens.lens (bucket :: DeleteObjects -> BucketName) (\s a -> s {bucket = a} :: DeleteObjects)
{-# DEPRECATED doBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doRequestPayer :: Lens.Lens' DeleteObjects (Lude.Maybe RequestPayer)
doRequestPayer = Lens.lens (requestPayer :: DeleteObjects -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: DeleteObjects)
{-# DEPRECATED doRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Specifies whether you want to delete this object even if it has a Governance-type Object Lock in place. You must have sufficient permissions to perform this operation.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doBypassGovernanceRetention :: Lens.Lens' DeleteObjects (Lude.Maybe Lude.Bool)
doBypassGovernanceRetention = Lens.lens (bypassGovernanceRetention :: DeleteObjects -> Lude.Maybe Lude.Bool) (\s a -> s {bypassGovernanceRetention = a} :: DeleteObjects)
{-# DEPRECATED doBypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doExpectedBucketOwner :: Lens.Lens' DeleteObjects (Lude.Maybe Lude.Text)
doExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteObjects -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteObjects)
{-# DEPRECATED doExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Container for the request.
--
-- /Note:/ Consider using 'delete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDelete :: Lens.Lens' DeleteObjects Delete
doDelete = Lens.lens (delete :: DeleteObjects -> Delete) (\s a -> s {delete = a} :: DeleteObjects)
{-# DEPRECATED doDelete "Use generic-lens or generic-optics with 'delete' instead." #-}

instance Lude.AWSRequest DeleteObjects where
  type Rs DeleteObjects = DeleteObjectsResponse
  request = contentMD5Header Lude.. Req.postXML s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteObjectsResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (Lude.may (Lude.parseXMLList "Deleted") x)
            Lude.<*> (Lude.may (Lude.parseXMLList "Error") x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement DeleteObjects where
  toElement =
    Lude.mkElement "{http://s3.amazonaws.com/doc/2006-03-01/}Delete"
      Lude.. delete

instance Lude.ToHeaders DeleteObjects where
  toHeaders DeleteObjects' {..} =
    Lude.mconcat
      [ "x-amz-mfa" Lude.=# mfa,
        "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-bypass-governance-retention"
          Lude.=# bypassGovernanceRetention,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath DeleteObjects where
  toPath DeleteObjects' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteObjects where
  toQuery = Lude.const (Lude.mconcat ["delete"])

-- | /See:/ 'mkDeleteObjectsResponse' smart constructor.
data DeleteObjectsResponse = DeleteObjectsResponse'
  { requestCharged :: Lude.Maybe RequestCharged,
    -- | Container element for a successful delete. It identifies the object that was successfully deleted.
    deleted :: Lude.Maybe [DeletedObject],
    -- | Container for a failed delete operation that describes the object that Amazon S3 attempted to delete and the error it encountered.
    errors :: Lude.Maybe [S3ServiceError],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteObjectsResponse' with the minimum fields required to make a request.
--
-- * 'requestCharged' -
-- * 'deleted' - Container element for a successful delete. It identifies the object that was successfully deleted.
-- * 'errors' - Container for a failed delete operation that describes the object that Amazon S3 attempted to delete and the error it encountered.
-- * 'responseStatus' - The response status code.
mkDeleteObjectsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteObjectsResponse
mkDeleteObjectsResponse pResponseStatus_ =
  DeleteObjectsResponse'
    { requestCharged = Lude.Nothing,
      deleted = Lude.Nothing,
      errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsRequestCharged :: Lens.Lens' DeleteObjectsResponse (Lude.Maybe RequestCharged)
dorsRequestCharged = Lens.lens (requestCharged :: DeleteObjectsResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: DeleteObjectsResponse)
{-# DEPRECATED dorsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | Container element for a successful delete. It identifies the object that was successfully deleted.
--
-- /Note:/ Consider using 'deleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsDeleted :: Lens.Lens' DeleteObjectsResponse (Lude.Maybe [DeletedObject])
dorsDeleted = Lens.lens (deleted :: DeleteObjectsResponse -> Lude.Maybe [DeletedObject]) (\s a -> s {deleted = a} :: DeleteObjectsResponse)
{-# DEPRECATED dorsDeleted "Use generic-lens or generic-optics with 'deleted' instead." #-}

-- | Container for a failed delete operation that describes the object that Amazon S3 attempted to delete and the error it encountered.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsErrors :: Lens.Lens' DeleteObjectsResponse (Lude.Maybe [S3ServiceError])
dorsErrors = Lens.lens (errors :: DeleteObjectsResponse -> Lude.Maybe [S3ServiceError]) (\s a -> s {errors = a} :: DeleteObjectsResponse)
{-# DEPRECATED dorsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsResponseStatus :: Lens.Lens' DeleteObjectsResponse Lude.Int
dorsResponseStatus = Lens.lens (responseStatus :: DeleteObjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteObjectsResponse)
{-# DEPRECATED dorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
