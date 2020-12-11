{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.UploadPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a part in a multipart upload.
--
-- You must initiate a multipart upload (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> ) before you can upload any part. In response to your initiate request, Amazon S3 returns an upload ID, a unique identifier, that you must include in your upload part request.
-- Part numbers can be any number from 1 to 10,000, inclusive. A part number uniquely identifies a part and also defines its position within the object being created. If you upload a new part using the same part number that was used with a previous part, the previously uploaded part is overwritten. Each part must be at least 5 MB in size, except the last part. There is no size limit on the last part of your multipart upload.
-- To ensure that data is not corrupted when traversing the network, specify the @Content-MD5@ header in the upload part request. Amazon S3 checks the part data against the provided MD5 value. If they do not match, Amazon S3 returns an error.
-- If the upload request is signed with Signature Version 4, then AWS S3 uses the @x-amz-content-sha256@ header as a checksum instead of @Content-MD5@ . For more information see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html Authenticating Requests: Using the Authorization Header (AWS Signature Version 4)> .
-- __Note:__ After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.
-- For more information on multipart uploads, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart Upload Overview> in the /Amazon Simple Storage Service Developer Guide / .
-- For information on the permissions required to use the multipart upload API, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> in the /Amazon Simple Storage Service Developer Guide/ .
-- You can optionally request server-side encryption where Amazon S3 encrypts your data as it writes it to disks in its data centers and decrypts it for you when you access it. You have the option of providing your own encryption key, or you can use the AWS managed encryption keys. If you choose to provide your own encryption key, the request headers you provide in the request must match the headers you used in the request to initiate the upload by using <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> . For more information, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Using Server-Side Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
-- Server-side encryption is supported by the S3 Multipart Upload actions. Unless you are using a customer-provided encryption key, you don't need to specify the encryption parameters in each UploadPart request. Instead, you only need to specify the server-side encryption parameters in the initial Initiate Multipart request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> .
-- If you requested server-side encryption using a customer-provided encryption key in your initiate multipart upload request, you must provide identical encryption information in each part upload using the following headers.
--
--     * x-amz-server-side-encryption-customer-algorithm
--
--
--     * x-amz-server-side-encryption-customer-key
--
--
--     * x-amz-server-side-encryption-customer-key-MD5
--
--
-- __Special Errors__
--
--     *
--     * /Code: NoSuchUpload/
--
--
--     * /Cause: The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed./
--
--
--     * /HTTP Status Code: 404 Not Found /
--
--
--     * /SOAP Fault Code Prefix: Client/
--
--
--
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.UploadPart
  ( -- * Creating a request
    UploadPart (..),
    mkUploadPart,

    -- ** Request lenses
    upContentLength,
    upSSECustomerAlgorithm,
    upSSECustomerKey,
    upRequestPayer,
    upSSECustomerKeyMD5,
    upContentMD5,
    upExpectedBucketOwner,
    upBucket,
    upKey,
    upPartNumber,
    upUploadId,
    upBody,

    -- * Destructuring the response
    UploadPartResponse (..),
    mkUploadPartResponse,

    -- ** Response lenses
    uprsRequestCharged,
    uprsETag,
    uprsSSECustomerAlgorithm,
    uprsSSECustomerKeyMD5,
    uprsSSEKMSKeyId,
    uprsServerSideEncryption,
    uprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkUploadPart' smart constructor.
data UploadPart = UploadPart'
  { contentLength ::
      Lude.Maybe Lude.Integer,
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    sSECustomerKey :: Lude.Maybe (Lude.Sensitive Lude.Text),
    requestPayer :: Lude.Maybe RequestPayer,
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    contentMD5 :: Lude.Maybe Lude.Text,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    bucket :: BucketName,
    key :: ObjectKey,
    partNumber :: Lude.Int,
    uploadId :: Lude.Text,
    body :: Lude.RqBody
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'UploadPart' with the minimum fields required to make a request.
--
-- * 'body' - Object data.
-- * 'bucket' - The name of the bucket to which the multipart upload was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'contentLength' - Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
-- * 'contentMD5' - The base64-encoded 128-bit MD5 digest of the part data. This parameter is auto-populated when using the command from the CLI. This parameter is required if object lock parameters are specified.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'key' - Object key for which the multipart upload was initiated.
-- * 'partNumber' - Part number of part being uploaded. This is a positive integer between 1 and 10,000.
-- * 'requestPayer' - Undocumented field.
-- * 'sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
-- * 'sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm header@ . This must be the same encryption key specified in the initiate multipart upload request.
-- * 'sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
-- * 'uploadId' - Upload ID identifying the multipart upload whose part is being uploaded.
mkUploadPart ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'partNumber'
  Lude.Int ->
  -- | 'uploadId'
  Lude.Text ->
  -- | 'body'
  Lude.RqBody ->
  UploadPart
mkUploadPart pBucket_ pKey_ pPartNumber_ pUploadId_ pBody_ =
  UploadPart'
    { contentLength = Lude.Nothing,
      sSECustomerAlgorithm = Lude.Nothing,
      sSECustomerKey = Lude.Nothing,
      requestPayer = Lude.Nothing,
      sSECustomerKeyMD5 = Lude.Nothing,
      contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      partNumber = pPartNumber_,
      uploadId = pUploadId_,
      body = pBody_
    }

-- | Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upContentLength :: Lens.Lens' UploadPart (Lude.Maybe Lude.Integer)
upContentLength = Lens.lens (contentLength :: UploadPart -> Lude.Maybe Lude.Integer) (\s a -> s {contentLength = a} :: UploadPart)
{-# DEPRECATED upContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSSECustomerAlgorithm :: Lens.Lens' UploadPart (Lude.Maybe Lude.Text)
upSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: UploadPart -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: UploadPart)
{-# DEPRECATED upSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm header@ . This must be the same encryption key specified in the initiate multipart upload request.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSSECustomerKey :: Lens.Lens' UploadPart (Lude.Maybe (Lude.Sensitive Lude.Text))
upSSECustomerKey = Lens.lens (sSECustomerKey :: UploadPart -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSECustomerKey = a} :: UploadPart)
{-# DEPRECATED upSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upRequestPayer :: Lens.Lens' UploadPart (Lude.Maybe RequestPayer)
upRequestPayer = Lens.lens (requestPayer :: UploadPart -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: UploadPart)
{-# DEPRECATED upRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSSECustomerKeyMD5 :: Lens.Lens' UploadPart (Lude.Maybe Lude.Text)
upSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: UploadPart -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: UploadPart)
{-# DEPRECATED upSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | The base64-encoded 128-bit MD5 digest of the part data. This parameter is auto-populated when using the command from the CLI. This parameter is required if object lock parameters are specified.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upContentMD5 :: Lens.Lens' UploadPart (Lude.Maybe Lude.Text)
upContentMD5 = Lens.lens (contentMD5 :: UploadPart -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: UploadPart)
{-# DEPRECATED upContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upExpectedBucketOwner :: Lens.Lens' UploadPart (Lude.Maybe Lude.Text)
upExpectedBucketOwner = Lens.lens (expectedBucketOwner :: UploadPart -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: UploadPart)
{-# DEPRECATED upExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket to which the multipart upload was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upBucket :: Lens.Lens' UploadPart BucketName
upBucket = Lens.lens (bucket :: UploadPart -> BucketName) (\s a -> s {bucket = a} :: UploadPart)
{-# DEPRECATED upBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upKey :: Lens.Lens' UploadPart ObjectKey
upKey = Lens.lens (key :: UploadPart -> ObjectKey) (\s a -> s {key = a} :: UploadPart)
{-# DEPRECATED upKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Part number of part being uploaded. This is a positive integer between 1 and 10,000.
--
-- /Note:/ Consider using 'partNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPartNumber :: Lens.Lens' UploadPart Lude.Int
upPartNumber = Lens.lens (partNumber :: UploadPart -> Lude.Int) (\s a -> s {partNumber = a} :: UploadPart)
{-# DEPRECATED upPartNumber "Use generic-lens or generic-optics with 'partNumber' instead." #-}

-- | Upload ID identifying the multipart upload whose part is being uploaded.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upUploadId :: Lens.Lens' UploadPart Lude.Text
upUploadId = Lens.lens (uploadId :: UploadPart -> Lude.Text) (\s a -> s {uploadId = a} :: UploadPart)
{-# DEPRECATED upUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | Object data.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upBody :: Lens.Lens' UploadPart Lude.RqBody
upBody = Lens.lens (body :: UploadPart -> Lude.RqBody) (\s a -> s {body = a} :: UploadPart)
{-# DEPRECATED upBody "Use generic-lens or generic-optics with 'body' instead." #-}

instance Lude.AWSRequest UploadPart where
  type Rs UploadPart = UploadPartResponse
  request = Req.putBody s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          UploadPartResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-algorithm")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-key-MD5")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-aws-kms-key-id")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody UploadPart where
  toBody = Lude.toBody Lude.. body

instance Lude.ToHeaders UploadPart where
  toHeaders UploadPart' {..} =
    Lude.mconcat
      [ "Content-Length" Lude.=# contentLength,
        "x-amz-server-side-encryption-customer-algorithm"
          Lude.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" Lude.=# sSECustomerKey,
        "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-server-side-encryption-customer-key-MD5"
          Lude.=# sSECustomerKeyMD5,
        "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath UploadPart where
  toPath UploadPart' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery UploadPart where
  toQuery UploadPart' {..} =
    Lude.mconcat
      ["partNumber" Lude.=: partNumber, "uploadId" Lude.=: uploadId]

-- | /See:/ 'mkUploadPartResponse' smart constructor.
data UploadPartResponse = UploadPartResponse'
  { requestCharged ::
      Lude.Maybe RequestCharged,
    eTag :: Lude.Maybe ETag,
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    sSEKMSKeyId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    serverSideEncryption ::
      Lude.Maybe ServerSideEncryption,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadPartResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - Entity tag for the uploaded object.
-- * 'requestCharged' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
-- * 'sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
-- * 'sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) was used for the object.
-- * 'serverSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
mkUploadPartResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UploadPartResponse
mkUploadPartResponse pResponseStatus_ =
  UploadPartResponse'
    { requestCharged = Lude.Nothing,
      eTag = Lude.Nothing,
      sSECustomerAlgorithm = Lude.Nothing,
      sSECustomerKeyMD5 = Lude.Nothing,
      sSEKMSKeyId = Lude.Nothing,
      serverSideEncryption = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsRequestCharged :: Lens.Lens' UploadPartResponse (Lude.Maybe RequestCharged)
uprsRequestCharged = Lens.lens (requestCharged :: UploadPartResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: UploadPartResponse)
{-# DEPRECATED uprsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | Entity tag for the uploaded object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsETag :: Lens.Lens' UploadPartResponse (Lude.Maybe ETag)
uprsETag = Lens.lens (eTag :: UploadPartResponse -> Lude.Maybe ETag) (\s a -> s {eTag = a} :: UploadPartResponse)
{-# DEPRECATED uprsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsSSECustomerAlgorithm :: Lens.Lens' UploadPartResponse (Lude.Maybe Lude.Text)
uprsSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: UploadPartResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: UploadPartResponse)
{-# DEPRECATED uprsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsSSECustomerKeyMD5 :: Lens.Lens' UploadPartResponse (Lude.Maybe Lude.Text)
uprsSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: UploadPartResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: UploadPartResponse)
{-# DEPRECATED uprsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsSSEKMSKeyId :: Lens.Lens' UploadPartResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
uprsSSEKMSKeyId = Lens.lens (sSEKMSKeyId :: UploadPartResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSKeyId = a} :: UploadPartResponse)
{-# DEPRECATED uprsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsServerSideEncryption :: Lens.Lens' UploadPartResponse (Lude.Maybe ServerSideEncryption)
uprsServerSideEncryption = Lens.lens (serverSideEncryption :: UploadPartResponse -> Lude.Maybe ServerSideEncryption) (\s a -> s {serverSideEncryption = a} :: UploadPartResponse)
{-# DEPRECATED uprsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UploadPartResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UploadPartResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UploadPartResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
