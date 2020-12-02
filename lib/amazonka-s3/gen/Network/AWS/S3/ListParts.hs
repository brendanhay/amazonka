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
-- Module      : Network.AWS.S3.ListParts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the parts that have been uploaded for a specific multipart upload. This operation must include the upload ID, which you obtain by sending the initiate multipart upload request (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> ). This request returns a maximum of 1,000 uploaded parts. The default number of parts returned is 1,000 parts. You can restrict the number of parts returned by specifying the @max-parts@ request parameter. If your multipart upload consists of more than 1,000 parts, the response returns an @IsTruncated@ field with the value of true, and a @NextPartNumberMarker@ element. In subsequent @ListParts@ requests you can include the part-number-marker query string parameter and set its value to the @NextPartNumberMarker@ field value from the previous response.
--
--
-- For more information on multipart uploads, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload> .
--
-- For information on permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> .
--
-- The following operations are related to @ListParts@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.S3.ListParts
  ( -- * Creating a Request
    listParts,
    ListParts,

    -- * Request Lenses
    lpMaxParts,
    lpRequestPayer,
    lpPartNumberMarker,
    lpExpectedBucketOwner,
    lpBucket,
    lpKey,
    lpUploadId,

    -- * Destructuring the Response
    listPartsResponse,
    ListPartsResponse,

    -- * Response Lenses
    lprsParts,
    lprsRequestCharged,
    lprsMaxParts,
    lprsInitiator,
    lprsBucket,
    lprsAbortDate,
    lprsNextPartNumberMarker,
    lprsAbortRuleId,
    lprsOwner,
    lprsKey,
    lprsStorageClass,
    lprsIsTruncated,
    lprsPartNumberMarker,
    lprsUploadId,
    lprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'listParts' smart constructor.
data ListParts = ListParts'
  { _lpMaxParts :: !(Maybe Int),
    _lpRequestPayer :: !(Maybe RequestPayer),
    _lpPartNumberMarker :: !(Maybe Int),
    _lpExpectedBucketOwner :: !(Maybe Text),
    _lpBucket :: !BucketName,
    _lpKey :: !ObjectKey,
    _lpUploadId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListParts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpMaxParts' - Sets the maximum number of parts to return.
--
-- * 'lpRequestPayer' - Undocumented member.
--
-- * 'lpPartNumberMarker' - Specifies the part after which listing should begin. Only parts with higher part numbers will be listed.
--
-- * 'lpExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'lpBucket' - The name of the bucket to which the parts are being uploaded.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'lpKey' - Object key for which the multipart upload was initiated.
--
-- * 'lpUploadId' - Upload ID identifying the multipart upload whose parts are being listed.
listParts ::
  -- | 'lpBucket'
  BucketName ->
  -- | 'lpKey'
  ObjectKey ->
  -- | 'lpUploadId'
  Text ->
  ListParts
listParts pBucket_ pKey_ pUploadId_ =
  ListParts'
    { _lpMaxParts = Nothing,
      _lpRequestPayer = Nothing,
      _lpPartNumberMarker = Nothing,
      _lpExpectedBucketOwner = Nothing,
      _lpBucket = pBucket_,
      _lpKey = pKey_,
      _lpUploadId = pUploadId_
    }

-- | Sets the maximum number of parts to return.
lpMaxParts :: Lens' ListParts (Maybe Int)
lpMaxParts = lens _lpMaxParts (\s a -> s {_lpMaxParts = a})

-- | Undocumented member.
lpRequestPayer :: Lens' ListParts (Maybe RequestPayer)
lpRequestPayer = lens _lpRequestPayer (\s a -> s {_lpRequestPayer = a})

-- | Specifies the part after which listing should begin. Only parts with higher part numbers will be listed.
lpPartNumberMarker :: Lens' ListParts (Maybe Int)
lpPartNumberMarker = lens _lpPartNumberMarker (\s a -> s {_lpPartNumberMarker = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
lpExpectedBucketOwner :: Lens' ListParts (Maybe Text)
lpExpectedBucketOwner = lens _lpExpectedBucketOwner (\s a -> s {_lpExpectedBucketOwner = a})

-- | The name of the bucket to which the parts are being uploaded.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
lpBucket :: Lens' ListParts BucketName
lpBucket = lens _lpBucket (\s a -> s {_lpBucket = a})

-- | Object key for which the multipart upload was initiated.
lpKey :: Lens' ListParts ObjectKey
lpKey = lens _lpKey (\s a -> s {_lpKey = a})

-- | Upload ID identifying the multipart upload whose parts are being listed.
lpUploadId :: Lens' ListParts Text
lpUploadId = lens _lpUploadId (\s a -> s {_lpUploadId = a})

instance AWSPager ListParts where
  page rq rs
    | stop (rs ^. lprsIsTruncated) = Nothing
    | isNothing (rs ^. lprsNextPartNumberMarker) = Nothing
    | otherwise =
      Just $ rq & lpPartNumberMarker .~ rs ^. lprsNextPartNumberMarker

instance AWSRequest ListParts where
  type Rs ListParts = ListPartsResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          ListPartsResponse'
            <$> (may (parseXMLList "Part") x)
            <*> (h .#? "x-amz-request-charged")
            <*> (x .@? "MaxParts")
            <*> (x .@? "Initiator")
            <*> (x .@? "Bucket")
            <*> (h .#? "x-amz-abort-date")
            <*> (x .@? "NextPartNumberMarker")
            <*> (h .#? "x-amz-abort-rule-id")
            <*> (x .@? "Owner")
            <*> (x .@? "Key")
            <*> (x .@? "StorageClass")
            <*> (x .@? "IsTruncated")
            <*> (x .@? "PartNumberMarker")
            <*> (x .@? "UploadId")
            <*> (pure (fromEnum s))
      )

instance Hashable ListParts

instance NFData ListParts

instance ToHeaders ListParts where
  toHeaders ListParts' {..} =
    mconcat
      [ "x-amz-request-payer" =# _lpRequestPayer,
        "x-amz-expected-bucket-owner" =# _lpExpectedBucketOwner
      ]

instance ToPath ListParts where
  toPath ListParts' {..} =
    mconcat ["/", toBS _lpBucket, "/", toBS _lpKey]

instance ToQuery ListParts where
  toQuery ListParts' {..} =
    mconcat
      [ "max-parts" =: _lpMaxParts,
        "part-number-marker" =: _lpPartNumberMarker,
        "uploadId" =: _lpUploadId
      ]

-- | /See:/ 'listPartsResponse' smart constructor.
data ListPartsResponse = ListPartsResponse'
  { _lprsParts ::
      !(Maybe [Part]),
    _lprsRequestCharged :: !(Maybe RequestCharged),
    _lprsMaxParts :: !(Maybe Int),
    _lprsInitiator :: !(Maybe Initiator),
    _lprsBucket :: !(Maybe BucketName),
    _lprsAbortDate :: !(Maybe ISO8601),
    _lprsNextPartNumberMarker :: !(Maybe Int),
    _lprsAbortRuleId :: !(Maybe Text),
    _lprsOwner :: !(Maybe Owner),
    _lprsKey :: !(Maybe ObjectKey),
    _lprsStorageClass :: !(Maybe StorageClass),
    _lprsIsTruncated :: !(Maybe Bool),
    _lprsPartNumberMarker :: !(Maybe Int),
    _lprsUploadId :: !(Maybe Text),
    _lprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPartsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsParts' - Container for elements related to a particular part. A response can contain zero or more @Part@ elements.
--
-- * 'lprsRequestCharged' - Undocumented member.
--
-- * 'lprsMaxParts' - Maximum number of parts that were allowed in the response.
--
-- * 'lprsInitiator' - Container element that identifies who initiated the multipart upload. If the initiator is an AWS account, this element provides the same information as the @Owner@ element. If the initiator is an IAM User, this element provides the user ARN and display name.
--
-- * 'lprsBucket' - The name of the bucket to which the multipart upload was initiated.
--
-- * 'lprsAbortDate' - If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, then the response includes this header indicating when the initiated multipart upload will become eligible for abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> . The response will also include the @x-amz-abort-rule-id@ header that will provide the ID of the lifecycle configuration rule that defines this action.
--
-- * 'lprsNextPartNumberMarker' - When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
--
-- * 'lprsAbortRuleId' - This header is returned along with the @x-amz-abort-date@ header. It identifies applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
--
-- * 'lprsOwner' - Container element that identifies the object owner, after the object is created. If multipart upload is initiated by an IAM user, this element provides the parent account ID and display name.
--
-- * 'lprsKey' - Object key for which the multipart upload was initiated.
--
-- * 'lprsStorageClass' - Class of storage (STANDARD or REDUCED_REDUNDANCY) used to store the uploaded object.
--
-- * 'lprsIsTruncated' - Indicates whether the returned list of parts is truncated. A true value indicates that the list was truncated. A list can be truncated if the number of parts exceeds the limit returned in the MaxParts element.
--
-- * 'lprsPartNumberMarker' - When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
--
-- * 'lprsUploadId' - Upload ID identifying the multipart upload whose parts are being listed.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPartsResponse ::
  -- | 'lprsResponseStatus'
  Int ->
  ListPartsResponse
listPartsResponse pResponseStatus_ =
  ListPartsResponse'
    { _lprsParts = Nothing,
      _lprsRequestCharged = Nothing,
      _lprsMaxParts = Nothing,
      _lprsInitiator = Nothing,
      _lprsBucket = Nothing,
      _lprsAbortDate = Nothing,
      _lprsNextPartNumberMarker = Nothing,
      _lprsAbortRuleId = Nothing,
      _lprsOwner = Nothing,
      _lprsKey = Nothing,
      _lprsStorageClass = Nothing,
      _lprsIsTruncated = Nothing,
      _lprsPartNumberMarker = Nothing,
      _lprsUploadId = Nothing,
      _lprsResponseStatus = pResponseStatus_
    }

-- | Container for elements related to a particular part. A response can contain zero or more @Part@ elements.
lprsParts :: Lens' ListPartsResponse [Part]
lprsParts = lens _lprsParts (\s a -> s {_lprsParts = a}) . _Default . _Coerce

-- | Undocumented member.
lprsRequestCharged :: Lens' ListPartsResponse (Maybe RequestCharged)
lprsRequestCharged = lens _lprsRequestCharged (\s a -> s {_lprsRequestCharged = a})

-- | Maximum number of parts that were allowed in the response.
lprsMaxParts :: Lens' ListPartsResponse (Maybe Int)
lprsMaxParts = lens _lprsMaxParts (\s a -> s {_lprsMaxParts = a})

-- | Container element that identifies who initiated the multipart upload. If the initiator is an AWS account, this element provides the same information as the @Owner@ element. If the initiator is an IAM User, this element provides the user ARN and display name.
lprsInitiator :: Lens' ListPartsResponse (Maybe Initiator)
lprsInitiator = lens _lprsInitiator (\s a -> s {_lprsInitiator = a})

-- | The name of the bucket to which the multipart upload was initiated.
lprsBucket :: Lens' ListPartsResponse (Maybe BucketName)
lprsBucket = lens _lprsBucket (\s a -> s {_lprsBucket = a})

-- | If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, then the response includes this header indicating when the initiated multipart upload will become eligible for abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> . The response will also include the @x-amz-abort-rule-id@ header that will provide the ID of the lifecycle configuration rule that defines this action.
lprsAbortDate :: Lens' ListPartsResponse (Maybe UTCTime)
lprsAbortDate = lens _lprsAbortDate (\s a -> s {_lprsAbortDate = a}) . mapping _Time

-- | When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
lprsNextPartNumberMarker :: Lens' ListPartsResponse (Maybe Int)
lprsNextPartNumberMarker = lens _lprsNextPartNumberMarker (\s a -> s {_lprsNextPartNumberMarker = a})

-- | This header is returned along with the @x-amz-abort-date@ header. It identifies applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
lprsAbortRuleId :: Lens' ListPartsResponse (Maybe Text)
lprsAbortRuleId = lens _lprsAbortRuleId (\s a -> s {_lprsAbortRuleId = a})

-- | Container element that identifies the object owner, after the object is created. If multipart upload is initiated by an IAM user, this element provides the parent account ID and display name.
lprsOwner :: Lens' ListPartsResponse (Maybe Owner)
lprsOwner = lens _lprsOwner (\s a -> s {_lprsOwner = a})

-- | Object key for which the multipart upload was initiated.
lprsKey :: Lens' ListPartsResponse (Maybe ObjectKey)
lprsKey = lens _lprsKey (\s a -> s {_lprsKey = a})

-- | Class of storage (STANDARD or REDUCED_REDUNDANCY) used to store the uploaded object.
lprsStorageClass :: Lens' ListPartsResponse (Maybe StorageClass)
lprsStorageClass = lens _lprsStorageClass (\s a -> s {_lprsStorageClass = a})

-- | Indicates whether the returned list of parts is truncated. A true value indicates that the list was truncated. A list can be truncated if the number of parts exceeds the limit returned in the MaxParts element.
lprsIsTruncated :: Lens' ListPartsResponse (Maybe Bool)
lprsIsTruncated = lens _lprsIsTruncated (\s a -> s {_lprsIsTruncated = a})

-- | When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
lprsPartNumberMarker :: Lens' ListPartsResponse (Maybe Int)
lprsPartNumberMarker = lens _lprsPartNumberMarker (\s a -> s {_lprsPartNumberMarker = a})

-- | Upload ID identifying the multipart upload whose parts are being listed.
lprsUploadId :: Lens' ListPartsResponse (Maybe Text)
lprsUploadId = lens _lprsUploadId (\s a -> s {_lprsUploadId = a})

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPartsResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\s a -> s {_lprsResponseStatus = a})

instance NFData ListPartsResponse
