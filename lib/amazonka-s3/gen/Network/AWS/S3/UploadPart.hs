{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
--
-- You must initiate a multipart upload (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> ) before you can upload any part. In response to your initiate request, Amazon S3 returns an upload ID, a unique identifier, that you must include in your upload part request.
--
-- Part numbers can be any number from 1 to 10,000, inclusive. A part number uniquely identifies a part and also defines its position within the object being created. If you upload a new part using the same part number that was used with a previous part, the previously uploaded part is overwritten. Each part must be at least 5 MB in size, except the last part. There is no size limit on the last part of your multipart upload.
--
-- To ensure that data is not corrupted when traversing the network, specify the @Content-MD5@ header in the upload part request. Amazon S3 checks the part data against the provided MD5 value. If they do not match, Amazon S3 returns an error.
--
-- If the upload request is signed with Signature Version 4, then AWS S3 uses the @x-amz-content-sha256@ header as a checksum instead of @Content-MD5@ . For more information see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html Authenticating Requests: Using the Authorization Header (AWS Signature Version 4)> .
--
-- __Note:__ After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.
--
-- For more information on multipart uploads, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart Upload Overview> in the /Amazon Simple Storage Service Developer Guide / .
--
-- For information on the permissions required to use the multipart upload API, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- You can optionally request server-side encryption where Amazon S3 encrypts your data as it writes it to disks in its data centers and decrypts it for you when you access it. You have the option of providing your own encryption key, or you can use the AWS managed encryption keys. If you choose to provide your own encryption key, the request headers you provide in the request must match the headers you used in the request to initiate the upload by using <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> . For more information, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Using Server-Side Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- Server-side encryption is supported by the S3 Multipart Upload actions. Unless you are using a customer-provided encryption key, you don't need to specify the encryption parameters in each UploadPart request. Instead, you only need to specify the server-side encryption parameters in the initial Initiate Multipart request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> .
--
-- If you requested server-side encryption using a customer-provided encryption key in your initiate multipart upload request, you must provide identical encryption information in each part upload using the following headers.
--
--     * x-amz-server-side-encryption-customer-algorithm
--
--     * x-amz-server-side-encryption-customer-key
--
--     * x-amz-server-side-encryption-customer-key-MD5
--
--
--
-- __Special Errors__
--
--     *     * /Code: NoSuchUpload/
--
--     * /Cause: The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed./
--
--     * /HTTP Status Code: 404 Not Found /
--
--     * /SOAP Fault Code Prefix: Client/
--
--
--
--
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.UploadPart
  ( -- * Creating a Request
    uploadPart,
    UploadPart,

    -- * Request Lenses
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

    -- * Destructuring the Response
    uploadPartResponse,
    UploadPartResponse,

    -- * Response Lenses
    uprsRequestCharged,
    uprsETag,
    uprsSSECustomerAlgorithm,
    uprsSSECustomerKeyMD5,
    uprsSSEKMSKeyId,
    uprsServerSideEncryption,
    uprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'uploadPart' smart constructor.
data UploadPart = UploadPart'
  { _upContentLength :: !(Maybe Integer),
    _upSSECustomerAlgorithm :: !(Maybe Text),
    _upSSECustomerKey :: !(Maybe (Sensitive Text)),
    _upRequestPayer :: !(Maybe RequestPayer),
    _upSSECustomerKeyMD5 :: !(Maybe Text),
    _upContentMD5 :: !(Maybe Text),
    _upExpectedBucketOwner :: !(Maybe Text),
    _upBucket :: !BucketName,
    _upKey :: !ObjectKey,
    _upPartNumber :: !Int,
    _upUploadId :: !Text,
    _upBody :: !RqBody
  }
  deriving (Show, Generic)

-- | Creates a value of 'UploadPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upContentLength' - Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
--
-- * 'upSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- * 'upSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm header@ . This must be the same encryption key specified in the initiate multipart upload request.
--
-- * 'upRequestPayer' - Undocumented member.
--
-- * 'upSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- * 'upContentMD5' - The base64-encoded 128-bit MD5 digest of the part data. This parameter is auto-populated when using the command from the CLI. This parameter is required if object lock parameters are specified.
--
-- * 'upExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'upBucket' - The name of the bucket to which the multipart upload was initiated. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'upKey' - Object key for which the multipart upload was initiated.
--
-- * 'upPartNumber' - Part number of part being uploaded. This is a positive integer between 1 and 10,000.
--
-- * 'upUploadId' - Upload ID identifying the multipart upload whose part is being uploaded.
--
-- * 'upBody' - Object data.
uploadPart ::
  -- | 'upBucket'
  BucketName ->
  -- | 'upKey'
  ObjectKey ->
  -- | 'upPartNumber'
  Int ->
  -- | 'upUploadId'
  Text ->
  -- | 'upBody'
  RqBody ->
  UploadPart
uploadPart pBucket_ pKey_ pPartNumber_ pUploadId_ pBody_ =
  UploadPart'
    { _upContentLength = Nothing,
      _upSSECustomerAlgorithm = Nothing,
      _upSSECustomerKey = Nothing,
      _upRequestPayer = Nothing,
      _upSSECustomerKeyMD5 = Nothing,
      _upContentMD5 = Nothing,
      _upExpectedBucketOwner = Nothing,
      _upBucket = pBucket_,
      _upKey = pKey_,
      _upPartNumber = pPartNumber_,
      _upUploadId = pUploadId_,
      _upBody = pBody_
    }

-- | Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
upContentLength :: Lens' UploadPart (Maybe Integer)
upContentLength = lens _upContentLength (\s a -> s {_upContentLength = a})

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
upSSECustomerAlgorithm :: Lens' UploadPart (Maybe Text)
upSSECustomerAlgorithm = lens _upSSECustomerAlgorithm (\s a -> s {_upSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm header@ . This must be the same encryption key specified in the initiate multipart upload request.
upSSECustomerKey :: Lens' UploadPart (Maybe Text)
upSSECustomerKey = lens _upSSECustomerKey (\s a -> s {_upSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
upRequestPayer :: Lens' UploadPart (Maybe RequestPayer)
upRequestPayer = lens _upRequestPayer (\s a -> s {_upRequestPayer = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
upSSECustomerKeyMD5 :: Lens' UploadPart (Maybe Text)
upSSECustomerKeyMD5 = lens _upSSECustomerKeyMD5 (\s a -> s {_upSSECustomerKeyMD5 = a})

-- | The base64-encoded 128-bit MD5 digest of the part data. This parameter is auto-populated when using the command from the CLI. This parameter is required if object lock parameters are specified.
upContentMD5 :: Lens' UploadPart (Maybe Text)
upContentMD5 = lens _upContentMD5 (\s a -> s {_upContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
upExpectedBucketOwner :: Lens' UploadPart (Maybe Text)
upExpectedBucketOwner = lens _upExpectedBucketOwner (\s a -> s {_upExpectedBucketOwner = a})

-- | The name of the bucket to which the multipart upload was initiated. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
upBucket :: Lens' UploadPart BucketName
upBucket = lens _upBucket (\s a -> s {_upBucket = a})

-- | Object key for which the multipart upload was initiated.
upKey :: Lens' UploadPart ObjectKey
upKey = lens _upKey (\s a -> s {_upKey = a})

-- | Part number of part being uploaded. This is a positive integer between 1 and 10,000.
upPartNumber :: Lens' UploadPart Int
upPartNumber = lens _upPartNumber (\s a -> s {_upPartNumber = a})

-- | Upload ID identifying the multipart upload whose part is being uploaded.
upUploadId :: Lens' UploadPart Text
upUploadId = lens _upUploadId (\s a -> s {_upUploadId = a})

-- | Object data.
upBody :: Lens' UploadPart RqBody
upBody = lens _upBody (\s a -> s {_upBody = a})

instance AWSRequest UploadPart where
  type Rs UploadPart = UploadPartResponse
  request = putBody s3
  response =
    receiveEmpty
      ( \s h x ->
          UploadPartResponse'
            <$> (h .#? "x-amz-request-charged")
            <*> (h .#? "ETag")
            <*> (h .#? "x-amz-server-side-encryption-customer-algorithm")
            <*> (h .#? "x-amz-server-side-encryption-customer-key-MD5")
            <*> (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
            <*> (h .#? "x-amz-server-side-encryption")
            <*> (pure (fromEnum s))
      )

instance ToBody UploadPart where
  toBody = toBody . _upBody

instance ToHeaders UploadPart where
  toHeaders UploadPart' {..} =
    mconcat
      [ "Content-Length" =# _upContentLength,
        "x-amz-server-side-encryption-customer-algorithm"
          =# _upSSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" =# _upSSECustomerKey,
        "x-amz-request-payer" =# _upRequestPayer,
        "x-amz-server-side-encryption-customer-key-MD5"
          =# _upSSECustomerKeyMD5,
        "Content-MD5" =# _upContentMD5,
        "x-amz-expected-bucket-owner" =# _upExpectedBucketOwner
      ]

instance ToPath UploadPart where
  toPath UploadPart' {..} =
    mconcat ["/", toBS _upBucket, "/", toBS _upKey]

instance ToQuery UploadPart where
  toQuery UploadPart' {..} =
    mconcat
      ["partNumber" =: _upPartNumber, "uploadId" =: _upUploadId]

-- | /See:/ 'uploadPartResponse' smart constructor.
data UploadPartResponse = UploadPartResponse'
  { _uprsRequestCharged ::
      !(Maybe RequestCharged),
    _uprsETag :: !(Maybe ETag),
    _uprsSSECustomerAlgorithm :: !(Maybe Text),
    _uprsSSECustomerKeyMD5 :: !(Maybe Text),
    _uprsSSEKMSKeyId :: !(Maybe (Sensitive Text)),
    _uprsServerSideEncryption ::
      !(Maybe ServerSideEncryption),
    _uprsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UploadPartResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprsRequestCharged' - Undocumented member.
--
-- * 'uprsETag' - Entity tag for the uploaded object.
--
-- * 'uprsSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'uprsSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- * 'uprsSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) was used for the object.
--
-- * 'uprsServerSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- * 'uprsResponseStatus' - -- | The response status code.
uploadPartResponse ::
  -- | 'uprsResponseStatus'
  Int ->
  UploadPartResponse
uploadPartResponse pResponseStatus_ =
  UploadPartResponse'
    { _uprsRequestCharged = Nothing,
      _uprsETag = Nothing,
      _uprsSSECustomerAlgorithm = Nothing,
      _uprsSSECustomerKeyMD5 = Nothing,
      _uprsSSEKMSKeyId = Nothing,
      _uprsServerSideEncryption = Nothing,
      _uprsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
uprsRequestCharged :: Lens' UploadPartResponse (Maybe RequestCharged)
uprsRequestCharged = lens _uprsRequestCharged (\s a -> s {_uprsRequestCharged = a})

-- | Entity tag for the uploaded object.
uprsETag :: Lens' UploadPartResponse (Maybe ETag)
uprsETag = lens _uprsETag (\s a -> s {_uprsETag = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
uprsSSECustomerAlgorithm :: Lens' UploadPartResponse (Maybe Text)
uprsSSECustomerAlgorithm = lens _uprsSSECustomerAlgorithm (\s a -> s {_uprsSSECustomerAlgorithm = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
uprsSSECustomerKeyMD5 :: Lens' UploadPartResponse (Maybe Text)
uprsSSECustomerKeyMD5 = lens _uprsSSECustomerKeyMD5 (\s a -> s {_uprsSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) was used for the object.
uprsSSEKMSKeyId :: Lens' UploadPartResponse (Maybe Text)
uprsSSEKMSKeyId = lens _uprsSSEKMSKeyId (\s a -> s {_uprsSSEKMSKeyId = a}) . mapping _Sensitive

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
uprsServerSideEncryption :: Lens' UploadPartResponse (Maybe ServerSideEncryption)
uprsServerSideEncryption = lens _uprsServerSideEncryption (\s a -> s {_uprsServerSideEncryption = a})

-- | -- | The response status code.
uprsResponseStatus :: Lens' UploadPartResponse Int
uprsResponseStatus = lens _uprsResponseStatus (\s a -> s {_uprsResponseStatus = a})

instance NFData UploadPartResponse
