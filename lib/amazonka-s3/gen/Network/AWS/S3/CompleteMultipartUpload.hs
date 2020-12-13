{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.CompleteMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Completes a multipart upload by assembling previously uploaded parts.
--
-- You first initiate the multipart upload and then upload all parts using the <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart> operation. After successfully uploading all relevant parts of an upload, you call this operation to complete the upload. Upon receiving this request, Amazon S3 concatenates all the parts in ascending order by part number to create a new object. In the Complete Multipart Upload request, you must provide the parts list. You must ensure that the parts list is complete. This operation concatenates the parts that you provide in the list. For each part in the list, you must provide the part number and the @ETag@ value, returned after that part was uploaded.
-- Processing of a Complete Multipart Upload request could take several minutes to complete. After Amazon S3 begins processing the request, it sends an HTTP response header that specifies a 200 OK response. While processing is in progress, Amazon S3 periodically sends white space characters to keep the connection from timing out. Because a request could fail after the initial 200 OK response has been sent, it is important that you check the response body to determine whether the request succeeded.
-- Note that if @CompleteMultipartUpload@ fails, applications should be prepared to retry the failed requests. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ErrorBestPractices.html Amazon S3 Error Best Practices> .
-- For more information about multipart uploads, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload> .
-- For information about permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> .
-- @CompleteMultipartUpload@ has the following special errors:
--
--     * Error code: @EntityTooSmall@
--
--     * Description: Your proposed upload is smaller than the minimum allowed object size. Each part must be at least 5 MB in size, except the last part.
--
--
--     * 400 Bad Request
--
--
--
--
--     * Error code: @InvalidPart@
--
--     * Description: One or more of the specified parts could not be found. The part might not have been uploaded, or the specified entity tag might not have matched the part's entity tag.
--
--
--     * 400 Bad Request
--
--
--
--
--     * Error code: @InvalidPartOrder@
--
--     * Description: The list of parts was not in ascending order. The parts list must be specified in order by part number.
--
--
--     * 400 Bad Request
--
--
--
--
--     * Error code: @NoSuchUpload@
--
--     * Description: The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed.
--
--
--     * 404 Not Found
--
--
--
--
-- The following operations are related to @CompleteMultipartUpload@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.CompleteMultipartUpload
  ( -- * Creating a request
    CompleteMultipartUpload (..),
    mkCompleteMultipartUpload,

    -- ** Request lenses
    cmuBucket,
    cmuRequestPayer,
    cmuKey,
    cmuMultipartUpload,
    cmuUploadId,
    cmuExpectedBucketOwner,

    -- * Destructuring the response
    CompleteMultipartUploadResponse (..),
    mkCompleteMultipartUploadResponse,

    -- ** Response lenses
    cmursRequestCharged,
    cmursETag,
    cmursVersionId,
    cmursLocation,
    cmursExpiration,
    cmursBucket,
    cmursKey,
    cmursSSEKMSKeyId,
    cmursServerSideEncryption,
    cmursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkCompleteMultipartUpload' smart constructor.
data CompleteMultipartUpload = CompleteMultipartUpload'
  { -- | Name of the bucket to which the multipart upload was initiated.
    bucket :: BucketName,
    requestPayer :: Lude.Maybe RequestPayer,
    -- | Object key for which the multipart upload was initiated.
    key :: ObjectKey,
    -- | The container for the multipart upload request information.
    multipartUpload :: Lude.Maybe CompletedMultipartUpload,
    -- | ID for the initiated multipart upload.
    uploadId :: Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompleteMultipartUpload' with the minimum fields required to make a request.
--
-- * 'bucket' - Name of the bucket to which the multipart upload was initiated.
-- * 'requestPayer' -
-- * 'key' - Object key for which the multipart upload was initiated.
-- * 'multipartUpload' - The container for the multipart upload request information.
-- * 'uploadId' - ID for the initiated multipart upload.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkCompleteMultipartUpload ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'uploadId'
  Lude.Text ->
  CompleteMultipartUpload
mkCompleteMultipartUpload pBucket_ pKey_ pUploadId_ =
  CompleteMultipartUpload'
    { bucket = pBucket_,
      requestPayer = Lude.Nothing,
      key = pKey_,
      multipartUpload = Lude.Nothing,
      uploadId = pUploadId_,
      expectedBucketOwner = Lude.Nothing
    }

-- | Name of the bucket to which the multipart upload was initiated.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuBucket :: Lens.Lens' CompleteMultipartUpload BucketName
cmuBucket = Lens.lens (bucket :: CompleteMultipartUpload -> BucketName) (\s a -> s {bucket = a} :: CompleteMultipartUpload)
{-# DEPRECATED cmuBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuRequestPayer :: Lens.Lens' CompleteMultipartUpload (Lude.Maybe RequestPayer)
cmuRequestPayer = Lens.lens (requestPayer :: CompleteMultipartUpload -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: CompleteMultipartUpload)
{-# DEPRECATED cmuRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuKey :: Lens.Lens' CompleteMultipartUpload ObjectKey
cmuKey = Lens.lens (key :: CompleteMultipartUpload -> ObjectKey) (\s a -> s {key = a} :: CompleteMultipartUpload)
{-# DEPRECATED cmuKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The container for the multipart upload request information.
--
-- /Note:/ Consider using 'multipartUpload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuMultipartUpload :: Lens.Lens' CompleteMultipartUpload (Lude.Maybe CompletedMultipartUpload)
cmuMultipartUpload = Lens.lens (multipartUpload :: CompleteMultipartUpload -> Lude.Maybe CompletedMultipartUpload) (\s a -> s {multipartUpload = a} :: CompleteMultipartUpload)
{-# DEPRECATED cmuMultipartUpload "Use generic-lens or generic-optics with 'multipartUpload' instead." #-}

-- | ID for the initiated multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuUploadId :: Lens.Lens' CompleteMultipartUpload Lude.Text
cmuUploadId = Lens.lens (uploadId :: CompleteMultipartUpload -> Lude.Text) (\s a -> s {uploadId = a} :: CompleteMultipartUpload)
{-# DEPRECATED cmuUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuExpectedBucketOwner :: Lens.Lens' CompleteMultipartUpload (Lude.Maybe Lude.Text)
cmuExpectedBucketOwner = Lens.lens (expectedBucketOwner :: CompleteMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: CompleteMultipartUpload)
{-# DEPRECATED cmuExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest CompleteMultipartUpload where
  type Rs CompleteMultipartUpload = CompleteMultipartUploadResponse
  request = Req.postXML s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          CompleteMultipartUploadResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (x Lude..@? "ETag")
            Lude.<*> (h Lude..#? "x-amz-version-id")
            Lude.<*> (x Lude..@? "Location")
            Lude.<*> (h Lude..#? "x-amz-expiration")
            Lude.<*> (x Lude..@? "Bucket")
            Lude.<*> (x Lude..@? "Key")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-aws-kms-key-id")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CompleteMultipartUpload where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}CompleteMultipartUpload"
      Lude.. multipartUpload

instance Lude.ToHeaders CompleteMultipartUpload where
  toHeaders CompleteMultipartUpload' {..} =
    Lude.mconcat
      [ "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath CompleteMultipartUpload where
  toPath CompleteMultipartUpload' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery CompleteMultipartUpload where
  toQuery CompleteMultipartUpload' {..} =
    Lude.mconcat ["uploadId" Lude.=: uploadId]

-- | /See:/ 'mkCompleteMultipartUploadResponse' smart constructor.
data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse'
  { requestCharged :: Lude.Maybe RequestCharged,
    -- | Entity tag that identifies the newly created object's data. Objects with different object data will have different entity tags. The entity tag is an opaque string. The entity tag may or may not be an MD5 digest of the object data. If the entity tag is not an MD5 digest of the object data, it will contain one or more nonhexadecimal characters and/or will consist of less than 32 or more than 32 hexadecimal digits.
    eTag :: Lude.Maybe ETag,
    -- | Version ID of the newly created object, in case the bucket has versioning turned on.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | The URI that identifies the newly created object.
    location :: Lude.Maybe Lude.Text,
    -- | If the object expiration is configured, this will contain the expiration date (expiry-date) and rule ID (rule-id). The value of rule-id is URL encoded.
    expiration :: Lude.Maybe Lude.Text,
    -- | The name of the bucket that contains the newly created object.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Lude.Maybe BucketName,
    -- | The object key of the newly created object.
    key :: Lude.Maybe ObjectKey,
    -- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
    sSEKMSKeyId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | If you specified server-side encryption either with an Amazon S3-managed encryption key or an AWS KMS customer master key (CMK) in your initiate multipart upload request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
    serverSideEncryption :: Lude.Maybe ServerSideEncryption,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompleteMultipartUploadResponse' with the minimum fields required to make a request.
--
-- * 'requestCharged' -
-- * 'eTag' - Entity tag that identifies the newly created object's data. Objects with different object data will have different entity tags. The entity tag is an opaque string. The entity tag may or may not be an MD5 digest of the object data. If the entity tag is not an MD5 digest of the object data, it will contain one or more nonhexadecimal characters and/or will consist of less than 32 or more than 32 hexadecimal digits.
-- * 'versionId' - Version ID of the newly created object, in case the bucket has versioning turned on.
-- * 'location' - The URI that identifies the newly created object.
-- * 'expiration' - If the object expiration is configured, this will contain the expiration date (expiry-date) and rule ID (rule-id). The value of rule-id is URL encoded.
-- * 'bucket' - The name of the bucket that contains the newly created object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'key' - The object key of the newly created object.
-- * 'sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
-- * 'serverSideEncryption' - If you specified server-side encryption either with an Amazon S3-managed encryption key or an AWS KMS customer master key (CMK) in your initiate multipart upload request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
-- * 'responseStatus' - The response status code.
mkCompleteMultipartUploadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CompleteMultipartUploadResponse
mkCompleteMultipartUploadResponse pResponseStatus_ =
  CompleteMultipartUploadResponse'
    { requestCharged = Lude.Nothing,
      eTag = Lude.Nothing,
      versionId = Lude.Nothing,
      location = Lude.Nothing,
      expiration = Lude.Nothing,
      bucket = Lude.Nothing,
      key = Lude.Nothing,
      sSEKMSKeyId = Lude.Nothing,
      serverSideEncryption = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmursRequestCharged :: Lens.Lens' CompleteMultipartUploadResponse (Lude.Maybe RequestCharged)
cmursRequestCharged = Lens.lens (requestCharged :: CompleteMultipartUploadResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: CompleteMultipartUploadResponse)
{-# DEPRECATED cmursRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | Entity tag that identifies the newly created object's data. Objects with different object data will have different entity tags. The entity tag is an opaque string. The entity tag may or may not be an MD5 digest of the object data. If the entity tag is not an MD5 digest of the object data, it will contain one or more nonhexadecimal characters and/or will consist of less than 32 or more than 32 hexadecimal digits.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmursETag :: Lens.Lens' CompleteMultipartUploadResponse (Lude.Maybe ETag)
cmursETag = Lens.lens (eTag :: CompleteMultipartUploadResponse -> Lude.Maybe ETag) (\s a -> s {eTag = a} :: CompleteMultipartUploadResponse)
{-# DEPRECATED cmursETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Version ID of the newly created object, in case the bucket has versioning turned on.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmursVersionId :: Lens.Lens' CompleteMultipartUploadResponse (Lude.Maybe ObjectVersionId)
cmursVersionId = Lens.lens (versionId :: CompleteMultipartUploadResponse -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: CompleteMultipartUploadResponse)
{-# DEPRECATED cmursVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The URI that identifies the newly created object.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmursLocation :: Lens.Lens' CompleteMultipartUploadResponse (Lude.Maybe Lude.Text)
cmursLocation = Lens.lens (location :: CompleteMultipartUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CompleteMultipartUploadResponse)
{-# DEPRECATED cmursLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | If the object expiration is configured, this will contain the expiration date (expiry-date) and rule ID (rule-id). The value of rule-id is URL encoded.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmursExpiration :: Lens.Lens' CompleteMultipartUploadResponse (Lude.Maybe Lude.Text)
cmursExpiration = Lens.lens (expiration :: CompleteMultipartUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {expiration = a} :: CompleteMultipartUploadResponse)
{-# DEPRECATED cmursExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | The name of the bucket that contains the newly created object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmursBucket :: Lens.Lens' CompleteMultipartUploadResponse (Lude.Maybe BucketName)
cmursBucket = Lens.lens (bucket :: CompleteMultipartUploadResponse -> Lude.Maybe BucketName) (\s a -> s {bucket = a} :: CompleteMultipartUploadResponse)
{-# DEPRECATED cmursBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The object key of the newly created object.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmursKey :: Lens.Lens' CompleteMultipartUploadResponse (Lude.Maybe ObjectKey)
cmursKey = Lens.lens (key :: CompleteMultipartUploadResponse -> Lude.Maybe ObjectKey) (\s a -> s {key = a} :: CompleteMultipartUploadResponse)
{-# DEPRECATED cmursKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmursSSEKMSKeyId :: Lens.Lens' CompleteMultipartUploadResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
cmursSSEKMSKeyId = Lens.lens (sSEKMSKeyId :: CompleteMultipartUploadResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSKeyId = a} :: CompleteMultipartUploadResponse)
{-# DEPRECATED cmursSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | If you specified server-side encryption either with an Amazon S3-managed encryption key or an AWS KMS customer master key (CMK) in your initiate multipart upload request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmursServerSideEncryption :: Lens.Lens' CompleteMultipartUploadResponse (Lude.Maybe ServerSideEncryption)
cmursServerSideEncryption = Lens.lens (serverSideEncryption :: CompleteMultipartUploadResponse -> Lude.Maybe ServerSideEncryption) (\s a -> s {serverSideEncryption = a} :: CompleteMultipartUploadResponse)
{-# DEPRECATED cmursServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmursResponseStatus :: Lens.Lens' CompleteMultipartUploadResponse Lude.Int
cmursResponseStatus = Lens.lens (responseStatus :: CompleteMultipartUploadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CompleteMultipartUploadResponse)
{-# DEPRECATED cmursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
