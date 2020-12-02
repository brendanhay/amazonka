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
-- Module      : Network.AWS.S3.AbortMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation aborts a multipart upload. After a multipart upload is aborted, no additional parts can be uploaded using that upload ID. The storage consumed by any previously uploaded parts will be freed. However, if any part uploads are currently in progress, those part uploads might or might not succeed. As a result, it might be necessary to abort a given multipart upload multiple times in order to completely free all storage consumed by all parts.
--
--
-- To verify that all parts have been removed, so you don't get charged for the part storage, you should call the <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts> operation and ensure that the parts list is empty.
--
-- For information about permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> .
--
-- The following operations are related to @AbortMultipartUpload@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.AbortMultipartUpload
  ( -- * Creating a Request
    abortMultipartUpload,
    AbortMultipartUpload,

    -- * Request Lenses
    amuRequestPayer,
    amuExpectedBucketOwner,
    amuBucket,
    amuKey,
    amuUploadId,

    -- * Destructuring the Response
    abortMultipartUploadResponse,
    AbortMultipartUploadResponse,

    -- * Response Lenses
    amursRequestCharged,
    amursResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'abortMultipartUpload' smart constructor.
data AbortMultipartUpload = AbortMultipartUpload'
  { _amuRequestPayer ::
      !(Maybe RequestPayer),
    _amuExpectedBucketOwner :: !(Maybe Text),
    _amuBucket :: !BucketName,
    _amuKey :: !ObjectKey,
    _amuUploadId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AbortMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amuRequestPayer' - Undocumented member.
--
-- * 'amuExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'amuBucket' - The bucket name to which the upload was taking place.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'amuKey' - Key of the object for which the multipart upload was initiated.
--
-- * 'amuUploadId' - Upload ID that identifies the multipart upload.
abortMultipartUpload ::
  -- | 'amuBucket'
  BucketName ->
  -- | 'amuKey'
  ObjectKey ->
  -- | 'amuUploadId'
  Text ->
  AbortMultipartUpload
abortMultipartUpload pBucket_ pKey_ pUploadId_ =
  AbortMultipartUpload'
    { _amuRequestPayer = Nothing,
      _amuExpectedBucketOwner = Nothing,
      _amuBucket = pBucket_,
      _amuKey = pKey_,
      _amuUploadId = pUploadId_
    }

-- | Undocumented member.
amuRequestPayer :: Lens' AbortMultipartUpload (Maybe RequestPayer)
amuRequestPayer = lens _amuRequestPayer (\s a -> s {_amuRequestPayer = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
amuExpectedBucketOwner :: Lens' AbortMultipartUpload (Maybe Text)
amuExpectedBucketOwner = lens _amuExpectedBucketOwner (\s a -> s {_amuExpectedBucketOwner = a})

-- | The bucket name to which the upload was taking place.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
amuBucket :: Lens' AbortMultipartUpload BucketName
amuBucket = lens _amuBucket (\s a -> s {_amuBucket = a})

-- | Key of the object for which the multipart upload was initiated.
amuKey :: Lens' AbortMultipartUpload ObjectKey
amuKey = lens _amuKey (\s a -> s {_amuKey = a})

-- | Upload ID that identifies the multipart upload.
amuUploadId :: Lens' AbortMultipartUpload Text
amuUploadId = lens _amuUploadId (\s a -> s {_amuUploadId = a})

instance AWSRequest AbortMultipartUpload where
  type Rs AbortMultipartUpload = AbortMultipartUploadResponse
  request = delete s3
  response =
    receiveEmpty
      ( \s h x ->
          AbortMultipartUploadResponse'
            <$> (h .#? "x-amz-request-charged") <*> (pure (fromEnum s))
      )

instance Hashable AbortMultipartUpload

instance NFData AbortMultipartUpload

instance ToHeaders AbortMultipartUpload where
  toHeaders AbortMultipartUpload' {..} =
    mconcat
      [ "x-amz-request-payer" =# _amuRequestPayer,
        "x-amz-expected-bucket-owner" =# _amuExpectedBucketOwner
      ]

instance ToPath AbortMultipartUpload where
  toPath AbortMultipartUpload' {..} =
    mconcat ["/", toBS _amuBucket, "/", toBS _amuKey]

instance ToQuery AbortMultipartUpload where
  toQuery AbortMultipartUpload' {..} =
    mconcat ["uploadId" =: _amuUploadId]

-- | /See:/ 'abortMultipartUploadResponse' smart constructor.
data AbortMultipartUploadResponse = AbortMultipartUploadResponse'
  { _amursRequestCharged ::
      !(Maybe RequestCharged),
    _amursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AbortMultipartUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amursRequestCharged' - Undocumented member.
--
-- * 'amursResponseStatus' - -- | The response status code.
abortMultipartUploadResponse ::
  -- | 'amursResponseStatus'
  Int ->
  AbortMultipartUploadResponse
abortMultipartUploadResponse pResponseStatus_ =
  AbortMultipartUploadResponse'
    { _amursRequestCharged = Nothing,
      _amursResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
amursRequestCharged :: Lens' AbortMultipartUploadResponse (Maybe RequestCharged)
amursRequestCharged = lens _amursRequestCharged (\s a -> s {_amursRequestCharged = a})

-- | -- | The response status code.
amursResponseStatus :: Lens' AbortMultipartUploadResponse Int
amursResponseStatus = lens _amursResponseStatus (\s a -> s {_amursResponseStatus = a})

instance NFData AbortMultipartUploadResponse
