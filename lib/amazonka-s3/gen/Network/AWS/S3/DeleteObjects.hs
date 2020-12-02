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
-- Module      : Network.AWS.S3.DeleteObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation enables you to delete multiple objects from a bucket using a single HTTP request. If you know the object keys that you want to delete, then this operation provides a suitable alternative to sending individual delete requests, reducing per-request overhead.
--
--
-- The request contains a list of up to 1000 keys that you want to delete. In the XML, you provide the object key names, and optionally, version IDs if you want to delete a specific version of the object from a versioning-enabled bucket. For each key, Amazon S3 performs a delete operation and returns the result of that delete, success, or failure, in the response. Note that if the object specified in the request is not found, Amazon S3 returns the result as deleted.
--
-- The operation supports two modes for the response: verbose and quiet. By default, the operation uses verbose mode in which the response includes the result of deletion of each key in your request. In quiet mode the response includes only keys where the delete operation encountered an error. For a successful deletion, the operation does not return any information about the delete in the response body.
--
-- When performing this operation on an MFA Delete enabled bucket, that attempts to delete any versioned objects, you must include an MFA token. If you do not provide one, the entire request will fail, even if there are non-versioned objects you are trying to delete. If you provide an invalid token, whether there are versioned keys in the request or not, the entire Multi-Object Delete request will fail. For information about MFA Delete, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/Versioning.html#MultiFactorAuthenticationDelete MFA Delete> .
--
-- Finally, the Content-MD5 header is required for all Multi-Object Delete requests. Amazon S3 uses the header value to ensure that your request body has not been altered in transit.
--
-- The following operations are related to @DeleteObjects@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
module Network.AWS.S3.DeleteObjects
  ( -- * Creating a Request
    deleteObjects,
    DeleteObjects,

    -- * Request Lenses
    dosMFA,
    dosRequestPayer,
    dosBypassGovernanceRetention,
    dosExpectedBucketOwner,
    dosBucket,
    dosDelete,

    -- * Destructuring the Response
    deleteObjectsResponse,
    DeleteObjectsResponse,

    -- * Response Lenses
    drsRequestCharged,
    drsDeleted,
    drsErrors,
    drsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteObjects' smart constructor.
data DeleteObjects = DeleteObjects'
  { _dosMFA :: !(Maybe Text),
    _dosRequestPayer :: !(Maybe RequestPayer),
    _dosBypassGovernanceRetention :: !(Maybe Bool),
    _dosExpectedBucketOwner :: !(Maybe Text),
    _dosBucket :: !BucketName,
    _dosDelete :: !Delete
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteObjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dosMFA' - The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
--
-- * 'dosRequestPayer' - Undocumented member.
--
-- * 'dosBypassGovernanceRetention' - Specifies whether you want to delete this object even if it has a Governance-type Object Lock in place. You must have sufficient permissions to perform this operation.
--
-- * 'dosExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dosBucket' - The bucket name containing the objects to delete.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'dosDelete' - Container for the request.
deleteObjects ::
  -- | 'dosBucket'
  BucketName ->
  -- | 'dosDelete'
  Delete ->
  DeleteObjects
deleteObjects pBucket_ pDelete_ =
  DeleteObjects'
    { _dosMFA = Nothing,
      _dosRequestPayer = Nothing,
      _dosBypassGovernanceRetention = Nothing,
      _dosExpectedBucketOwner = Nothing,
      _dosBucket = pBucket_,
      _dosDelete = pDelete_
    }

-- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
dosMFA :: Lens' DeleteObjects (Maybe Text)
dosMFA = lens _dosMFA (\s a -> s {_dosMFA = a})

-- | Undocumented member.
dosRequestPayer :: Lens' DeleteObjects (Maybe RequestPayer)
dosRequestPayer = lens _dosRequestPayer (\s a -> s {_dosRequestPayer = a})

-- | Specifies whether you want to delete this object even if it has a Governance-type Object Lock in place. You must have sufficient permissions to perform this operation.
dosBypassGovernanceRetention :: Lens' DeleteObjects (Maybe Bool)
dosBypassGovernanceRetention = lens _dosBypassGovernanceRetention (\s a -> s {_dosBypassGovernanceRetention = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dosExpectedBucketOwner :: Lens' DeleteObjects (Maybe Text)
dosExpectedBucketOwner = lens _dosExpectedBucketOwner (\s a -> s {_dosExpectedBucketOwner = a})

-- | The bucket name containing the objects to delete.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
dosBucket :: Lens' DeleteObjects BucketName
dosBucket = lens _dosBucket (\s a -> s {_dosBucket = a})

-- | Container for the request.
dosDelete :: Lens' DeleteObjects Delete
dosDelete = lens _dosDelete (\s a -> s {_dosDelete = a})

instance AWSRequest DeleteObjects where
  type Rs DeleteObjects = DeleteObjectsResponse
  request = contentMD5Header . postXML s3
  response =
    receiveXML
      ( \s h x ->
          DeleteObjectsResponse'
            <$> (h .#? "x-amz-request-charged")
            <*> (may (parseXMLList "Deleted") x)
            <*> (may (parseXMLList "Error") x)
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteObjects

instance NFData DeleteObjects

instance ToElement DeleteObjects where
  toElement =
    mkElement "{http://s3.amazonaws.com/doc/2006-03-01/}Delete"
      . _dosDelete

instance ToHeaders DeleteObjects where
  toHeaders DeleteObjects' {..} =
    mconcat
      [ "x-amz-mfa" =# _dosMFA,
        "x-amz-request-payer" =# _dosRequestPayer,
        "x-amz-bypass-governance-retention"
          =# _dosBypassGovernanceRetention,
        "x-amz-expected-bucket-owner" =# _dosExpectedBucketOwner
      ]

instance ToPath DeleteObjects where
  toPath DeleteObjects' {..} = mconcat ["/", toBS _dosBucket]

instance ToQuery DeleteObjects where
  toQuery = const (mconcat ["delete"])

-- | /See:/ 'deleteObjectsResponse' smart constructor.
data DeleteObjectsResponse = DeleteObjectsResponse'
  { _drsRequestCharged ::
      !(Maybe RequestCharged),
    _drsDeleted :: !(Maybe [DeletedObject]),
    _drsErrors :: !(Maybe [S3ServiceError]),
    _drsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteObjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsRequestCharged' - Undocumented member.
--
-- * 'drsDeleted' - Container element for a successful delete. It identifies the object that was successfully deleted.
--
-- * 'drsErrors' - Container for a failed delete operation that describes the object that Amazon S3 attempted to delete and the error it encountered.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteObjectsResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DeleteObjectsResponse
deleteObjectsResponse pResponseStatus_ =
  DeleteObjectsResponse'
    { _drsRequestCharged = Nothing,
      _drsDeleted = Nothing,
      _drsErrors = Nothing,
      _drsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
drsRequestCharged :: Lens' DeleteObjectsResponse (Maybe RequestCharged)
drsRequestCharged = lens _drsRequestCharged (\s a -> s {_drsRequestCharged = a})

-- | Container element for a successful delete. It identifies the object that was successfully deleted.
drsDeleted :: Lens' DeleteObjectsResponse [DeletedObject]
drsDeleted = lens _drsDeleted (\s a -> s {_drsDeleted = a}) . _Default . _Coerce

-- | Container for a failed delete operation that describes the object that Amazon S3 attempted to delete and the error it encountered.
drsErrors :: Lens' DeleteObjectsResponse [S3ServiceError]
drsErrors = lens _drsErrors (\s a -> s {_drsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteObjectsResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DeleteObjectsResponse
