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
-- Module      : Network.AWS.S3.CompleteMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Completes a multipart upload by assembling previously uploaded parts.
--
--
-- You first initiate the multipart upload and then upload all parts using the <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart> operation. After successfully uploading all relevant parts of an upload, you call this operation to complete the upload. Upon receiving this request, Amazon S3 concatenates all the parts in ascending order by part number to create a new object. In the Complete Multipart Upload request, you must provide the parts list. You must ensure that the parts list is complete. This operation concatenates the parts that you provide in the list. For each part in the list, you must provide the part number and the @ETag@ value, returned after that part was uploaded.
--
-- Processing of a Complete Multipart Upload request could take several minutes to complete. After Amazon S3 begins processing the request, it sends an HTTP response header that specifies a 200 OK response. While processing is in progress, Amazon S3 periodically sends white space characters to keep the connection from timing out. Because a request could fail after the initial 200 OK response has been sent, it is important that you check the response body to determine whether the request succeeded.
--
-- Note that if @CompleteMultipartUpload@ fails, applications should be prepared to retry the failed requests. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ErrorBestPractices.html Amazon S3 Error Best Practices> .
--
-- For more information about multipart uploads, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload> .
--
-- For information about permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> .
--
-- @CompleteMultipartUpload@ has the following special errors:
--
--     * Error code: @EntityTooSmall@
--
--     * Description: Your proposed upload is smaller than the minimum allowed object size. Each part must be at least 5 MB in size, except the last part.
--
--     * 400 Bad Request
--
--
--
--     * Error code: @InvalidPart@
--
--     * Description: One or more of the specified parts could not be found. The part might not have been uploaded, or the specified entity tag might not have matched the part's entity tag.
--
--     * 400 Bad Request
--
--
--
--     * Error code: @InvalidPartOrder@
--
--     * Description: The list of parts was not in ascending order. The parts list must be specified in order by part number.
--
--     * 400 Bad Request
--
--
--
--     * Error code: @NoSuchUpload@
--
--     * Description: The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed.
--
--     * 404 Not Found
--
--
--
--
--
-- The following operations are related to @CompleteMultipartUpload@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.CompleteMultipartUpload
  ( -- * Creating a Request
    completeMultipartUpload,
    CompleteMultipartUpload,

    -- * Request Lenses
    cRequestPayer,
    cMultipartUpload,
    cExpectedBucketOwner,
    cBucket,
    cKey,
    cUploadId,

    -- * Destructuring the Response
    completeMultipartUploadResponse,
    CompleteMultipartUploadResponse,

    -- * Response Lenses
    crsRequestCharged,
    crsETag,
    crsVersionId,
    crsLocation,
    crsExpiration,
    crsBucket,
    crsKey,
    crsSSEKMSKeyId,
    crsServerSideEncryption,
    crsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'completeMultipartUpload' smart constructor.
data CompleteMultipartUpload = CompleteMultipartUpload'
  { _cRequestPayer ::
      !(Maybe RequestPayer),
    _cMultipartUpload ::
      !(Maybe CompletedMultipartUpload),
    _cExpectedBucketOwner :: !(Maybe Text),
    _cBucket :: !BucketName,
    _cKey :: !ObjectKey,
    _cUploadId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CompleteMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cRequestPayer' - Undocumented member.
--
-- * 'cMultipartUpload' - The container for the multipart upload request information.
--
-- * 'cExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'cBucket' - Name of the bucket to which the multipart upload was initiated.
--
-- * 'cKey' - Object key for which the multipart upload was initiated.
--
-- * 'cUploadId' - ID for the initiated multipart upload.
completeMultipartUpload ::
  -- | 'cBucket'
  BucketName ->
  -- | 'cKey'
  ObjectKey ->
  -- | 'cUploadId'
  Text ->
  CompleteMultipartUpload
completeMultipartUpload pBucket_ pKey_ pUploadId_ =
  CompleteMultipartUpload'
    { _cRequestPayer = Nothing,
      _cMultipartUpload = Nothing,
      _cExpectedBucketOwner = Nothing,
      _cBucket = pBucket_,
      _cKey = pKey_,
      _cUploadId = pUploadId_
    }

-- | Undocumented member.
cRequestPayer :: Lens' CompleteMultipartUpload (Maybe RequestPayer)
cRequestPayer = lens _cRequestPayer (\s a -> s {_cRequestPayer = a})

-- | The container for the multipart upload request information.
cMultipartUpload :: Lens' CompleteMultipartUpload (Maybe CompletedMultipartUpload)
cMultipartUpload = lens _cMultipartUpload (\s a -> s {_cMultipartUpload = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
cExpectedBucketOwner :: Lens' CompleteMultipartUpload (Maybe Text)
cExpectedBucketOwner = lens _cExpectedBucketOwner (\s a -> s {_cExpectedBucketOwner = a})

-- | Name of the bucket to which the multipart upload was initiated.
cBucket :: Lens' CompleteMultipartUpload BucketName
cBucket = lens _cBucket (\s a -> s {_cBucket = a})

-- | Object key for which the multipart upload was initiated.
cKey :: Lens' CompleteMultipartUpload ObjectKey
cKey = lens _cKey (\s a -> s {_cKey = a})

-- | ID for the initiated multipart upload.
cUploadId :: Lens' CompleteMultipartUpload Text
cUploadId = lens _cUploadId (\s a -> s {_cUploadId = a})

instance AWSRequest CompleteMultipartUpload where
  type Rs CompleteMultipartUpload = CompleteMultipartUploadResponse
  request = postXML s3
  response =
    receiveXML
      ( \s h x ->
          CompleteMultipartUploadResponse'
            <$> (h .#? "x-amz-request-charged")
            <*> (x .@? "ETag")
            <*> (h .#? "x-amz-version-id")
            <*> (x .@? "Location")
            <*> (h .#? "x-amz-expiration")
            <*> (x .@? "Bucket")
            <*> (x .@? "Key")
            <*> (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
            <*> (h .#? "x-amz-server-side-encryption")
            <*> (pure (fromEnum s))
      )

instance Hashable CompleteMultipartUpload

instance NFData CompleteMultipartUpload

instance ToElement CompleteMultipartUpload where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}CompleteMultipartUpload"
      . _cMultipartUpload

instance ToHeaders CompleteMultipartUpload where
  toHeaders CompleteMultipartUpload' {..} =
    mconcat
      [ "x-amz-request-payer" =# _cRequestPayer,
        "x-amz-expected-bucket-owner" =# _cExpectedBucketOwner
      ]

instance ToPath CompleteMultipartUpload where
  toPath CompleteMultipartUpload' {..} =
    mconcat ["/", toBS _cBucket, "/", toBS _cKey]

instance ToQuery CompleteMultipartUpload where
  toQuery CompleteMultipartUpload' {..} =
    mconcat ["uploadId" =: _cUploadId]

-- | /See:/ 'completeMultipartUploadResponse' smart constructor.
data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse'
  { _crsRequestCharged ::
      !(Maybe RequestCharged),
    _crsETag :: !(Maybe ETag),
    _crsVersionId ::
      !(Maybe ObjectVersionId),
    _crsLocation ::
      !(Maybe Text),
    _crsExpiration ::
      !(Maybe Text),
    _crsBucket ::
      !(Maybe BucketName),
    _crsKey ::
      !(Maybe ObjectKey),
    _crsSSEKMSKeyId ::
      !(Maybe (Sensitive Text)),
    _crsServerSideEncryption ::
      !( Maybe
           ServerSideEncryption
       ),
    _crsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CompleteMultipartUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsRequestCharged' - Undocumented member.
--
-- * 'crsETag' - Entity tag that identifies the newly created object's data. Objects with different object data will have different entity tags. The entity tag is an opaque string. The entity tag may or may not be an MD5 digest of the object data. If the entity tag is not an MD5 digest of the object data, it will contain one or more nonhexadecimal characters and/or will consist of less than 32 or more than 32 hexadecimal digits.
--
-- * 'crsVersionId' - Version ID of the newly created object, in case the bucket has versioning turned on.
--
-- * 'crsLocation' - The URI that identifies the newly created object.
--
-- * 'crsExpiration' - If the object expiration is configured, this will contain the expiration date (expiry-date) and rule ID (rule-id). The value of rule-id is URL encoded.
--
-- * 'crsBucket' - The name of the bucket that contains the newly created object. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'crsKey' - The object key of the newly created object.
--
-- * 'crsSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- * 'crsServerSideEncryption' - If you specified server-side encryption either with an Amazon S3-managed encryption key or an AWS KMS customer master key (CMK) in your initiate multipart upload request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
--
-- * 'crsResponseStatus' - -- | The response status code.
completeMultipartUploadResponse ::
  -- | 'crsResponseStatus'
  Int ->
  CompleteMultipartUploadResponse
completeMultipartUploadResponse pResponseStatus_ =
  CompleteMultipartUploadResponse'
    { _crsRequestCharged = Nothing,
      _crsETag = Nothing,
      _crsVersionId = Nothing,
      _crsLocation = Nothing,
      _crsExpiration = Nothing,
      _crsBucket = Nothing,
      _crsKey = Nothing,
      _crsSSEKMSKeyId = Nothing,
      _crsServerSideEncryption = Nothing,
      _crsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
crsRequestCharged :: Lens' CompleteMultipartUploadResponse (Maybe RequestCharged)
crsRequestCharged = lens _crsRequestCharged (\s a -> s {_crsRequestCharged = a})

-- | Entity tag that identifies the newly created object's data. Objects with different object data will have different entity tags. The entity tag is an opaque string. The entity tag may or may not be an MD5 digest of the object data. If the entity tag is not an MD5 digest of the object data, it will contain one or more nonhexadecimal characters and/or will consist of less than 32 or more than 32 hexadecimal digits.
crsETag :: Lens' CompleteMultipartUploadResponse (Maybe ETag)
crsETag = lens _crsETag (\s a -> s {_crsETag = a})

-- | Version ID of the newly created object, in case the bucket has versioning turned on.
crsVersionId :: Lens' CompleteMultipartUploadResponse (Maybe ObjectVersionId)
crsVersionId = lens _crsVersionId (\s a -> s {_crsVersionId = a})

-- | The URI that identifies the newly created object.
crsLocation :: Lens' CompleteMultipartUploadResponse (Maybe Text)
crsLocation = lens _crsLocation (\s a -> s {_crsLocation = a})

-- | If the object expiration is configured, this will contain the expiration date (expiry-date) and rule ID (rule-id). The value of rule-id is URL encoded.
crsExpiration :: Lens' CompleteMultipartUploadResponse (Maybe Text)
crsExpiration = lens _crsExpiration (\s a -> s {_crsExpiration = a})

-- | The name of the bucket that contains the newly created object. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
crsBucket :: Lens' CompleteMultipartUploadResponse (Maybe BucketName)
crsBucket = lens _crsBucket (\s a -> s {_crsBucket = a})

-- | The object key of the newly created object.
crsKey :: Lens' CompleteMultipartUploadResponse (Maybe ObjectKey)
crsKey = lens _crsKey (\s a -> s {_crsKey = a})

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
crsSSEKMSKeyId :: Lens' CompleteMultipartUploadResponse (Maybe Text)
crsSSEKMSKeyId = lens _crsSSEKMSKeyId (\s a -> s {_crsSSEKMSKeyId = a}) . mapping _Sensitive

-- | If you specified server-side encryption either with an Amazon S3-managed encryption key or an AWS KMS customer master key (CMK) in your initiate multipart upload request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
crsServerSideEncryption :: Lens' CompleteMultipartUploadResponse (Maybe ServerSideEncryption)
crsServerSideEncryption = lens _crsServerSideEncryption (\s a -> s {_crsServerSideEncryption = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CompleteMultipartUploadResponse Int
crsResponseStatus = lens _crsResponseStatus (\s a -> s {_crsResponseStatus = a})

instance NFData CompleteMultipartUploadResponse
