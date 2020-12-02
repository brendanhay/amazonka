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
-- Module      : Network.AWS.S3.DeleteObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the null version (if there is one) of an object and inserts a delete marker, which becomes the latest version of the object. If there isn't a null version, Amazon S3 does not remove any objects.
--
--
-- To remove a specific version, you must be the bucket owner and you must use the version Id subresource. Using this subresource permanently deletes the version. If the object deleted is a delete marker, Amazon S3 sets the response header, @x-amz-delete-marker@ , to true.
--
-- If the object you want to delete is in a bucket where the bucket versioning configuration is MFA Delete enabled, you must include the @x-amz-mfa@ request header in the DELETE @versionId@ request. Requests that include @x-amz-mfa@ must use HTTPS.
--
-- For more information about MFA Delete, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMFADelete.html Using MFA Delete> . To see sample requests that use versioning, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectDELETE.html#ExampleVersionObjectDelete Sample Request> .
--
-- You can delete objects by explicitly calling the DELETE Object API or configure its lifecycle (<https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycle.html PutBucketLifecycle> ) to enable Amazon S3 to remove them for you. If you want to block users or accounts from removing or deleting objects from your bucket, you must deny them the @s3:DeleteObject@ , @s3:DeleteObjectVersion@ , and @s3:PutLifeCycleConfiguration@ actions.
--
-- The following operation is related to @DeleteObject@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
module Network.AWS.S3.DeleteObject
  ( -- * Creating a Request
    deleteObject,
    DeleteObject,

    -- * Request Lenses
    doVersionId,
    doMFA,
    doRequestPayer,
    doBypassGovernanceRetention,
    doExpectedBucketOwner,
    doBucket,
    doKey,

    -- * Destructuring the Response
    deleteObjectResponse,
    DeleteObjectResponse,

    -- * Response Lenses
    dorsRequestCharged,
    dorsVersionId,
    dorsDeleteMarker,
    dorsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteObject' smart constructor.
data DeleteObject = DeleteObject'
  { _doVersionId ::
      !(Maybe ObjectVersionId),
    _doMFA :: !(Maybe Text),
    _doRequestPayer :: !(Maybe RequestPayer),
    _doBypassGovernanceRetention :: !(Maybe Bool),
    _doExpectedBucketOwner :: !(Maybe Text),
    _doBucket :: !BucketName,
    _doKey :: !ObjectKey
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doVersionId' - VersionId used to reference a specific version of the object.
--
-- * 'doMFA' - The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
--
-- * 'doRequestPayer' - Undocumented member.
--
-- * 'doBypassGovernanceRetention' - Indicates whether S3 Object Lock should bypass Governance-mode restrictions to process this operation.
--
-- * 'doExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'doBucket' - The bucket name of the bucket containing the object.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'doKey' - Key name of the object to delete.
deleteObject ::
  -- | 'doBucket'
  BucketName ->
  -- | 'doKey'
  ObjectKey ->
  DeleteObject
deleteObject pBucket_ pKey_ =
  DeleteObject'
    { _doVersionId = Nothing,
      _doMFA = Nothing,
      _doRequestPayer = Nothing,
      _doBypassGovernanceRetention = Nothing,
      _doExpectedBucketOwner = Nothing,
      _doBucket = pBucket_,
      _doKey = pKey_
    }

-- | VersionId used to reference a specific version of the object.
doVersionId :: Lens' DeleteObject (Maybe ObjectVersionId)
doVersionId = lens _doVersionId (\s a -> s {_doVersionId = a})

-- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
doMFA :: Lens' DeleteObject (Maybe Text)
doMFA = lens _doMFA (\s a -> s {_doMFA = a})

-- | Undocumented member.
doRequestPayer :: Lens' DeleteObject (Maybe RequestPayer)
doRequestPayer = lens _doRequestPayer (\s a -> s {_doRequestPayer = a})

-- | Indicates whether S3 Object Lock should bypass Governance-mode restrictions to process this operation.
doBypassGovernanceRetention :: Lens' DeleteObject (Maybe Bool)
doBypassGovernanceRetention = lens _doBypassGovernanceRetention (\s a -> s {_doBypassGovernanceRetention = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
doExpectedBucketOwner :: Lens' DeleteObject (Maybe Text)
doExpectedBucketOwner = lens _doExpectedBucketOwner (\s a -> s {_doExpectedBucketOwner = a})

-- | The bucket name of the bucket containing the object.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
doBucket :: Lens' DeleteObject BucketName
doBucket = lens _doBucket (\s a -> s {_doBucket = a})

-- | Key name of the object to delete.
doKey :: Lens' DeleteObject ObjectKey
doKey = lens _doKey (\s a -> s {_doKey = a})

instance AWSRequest DeleteObject where
  type Rs DeleteObject = DeleteObjectResponse
  request = delete s3
  response =
    receiveEmpty
      ( \s h x ->
          DeleteObjectResponse'
            <$> (h .#? "x-amz-request-charged")
            <*> (h .#? "x-amz-version-id")
            <*> (h .#? "x-amz-delete-marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteObject

instance NFData DeleteObject

instance ToHeaders DeleteObject where
  toHeaders DeleteObject' {..} =
    mconcat
      [ "x-amz-mfa" =# _doMFA,
        "x-amz-request-payer" =# _doRequestPayer,
        "x-amz-bypass-governance-retention"
          =# _doBypassGovernanceRetention,
        "x-amz-expected-bucket-owner" =# _doExpectedBucketOwner
      ]

instance ToPath DeleteObject where
  toPath DeleteObject' {..} =
    mconcat ["/", toBS _doBucket, "/", toBS _doKey]

instance ToQuery DeleteObject where
  toQuery DeleteObject' {..} = mconcat ["versionId" =: _doVersionId]

-- | /See:/ 'deleteObjectResponse' smart constructor.
data DeleteObjectResponse = DeleteObjectResponse'
  { _dorsRequestCharged ::
      !(Maybe RequestCharged),
    _dorsVersionId :: !(Maybe ObjectVersionId),
    _dorsDeleteMarker :: !(Maybe Bool),
    _dorsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dorsRequestCharged' - Undocumented member.
--
-- * 'dorsVersionId' - Returns the version ID of the delete marker created as a result of the DELETE operation.
--
-- * 'dorsDeleteMarker' - Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker.
--
-- * 'dorsResponseStatus' - -- | The response status code.
deleteObjectResponse ::
  -- | 'dorsResponseStatus'
  Int ->
  DeleteObjectResponse
deleteObjectResponse pResponseStatus_ =
  DeleteObjectResponse'
    { _dorsRequestCharged = Nothing,
      _dorsVersionId = Nothing,
      _dorsDeleteMarker = Nothing,
      _dorsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dorsRequestCharged :: Lens' DeleteObjectResponse (Maybe RequestCharged)
dorsRequestCharged = lens _dorsRequestCharged (\s a -> s {_dorsRequestCharged = a})

-- | Returns the version ID of the delete marker created as a result of the DELETE operation.
dorsVersionId :: Lens' DeleteObjectResponse (Maybe ObjectVersionId)
dorsVersionId = lens _dorsVersionId (\s a -> s {_dorsVersionId = a})

-- | Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker.
dorsDeleteMarker :: Lens' DeleteObjectResponse (Maybe Bool)
dorsDeleteMarker = lens _dorsDeleteMarker (\s a -> s {_dorsDeleteMarker = a})

-- | -- | The response status code.
dorsResponseStatus :: Lens' DeleteObjectResponse Int
dorsResponseStatus = lens _dorsResponseStatus (\s a -> s {_dorsResponseStatus = a})

instance NFData DeleteObjectResponse
