{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object. If there
-- isn\'t a null version, Amazon S3 does not remove any objects.
--
-- To remove a specific version, you must be the bucket owner and you must
-- use the version Id subresource. Using this subresource permanently
-- deletes the version. If the object deleted is a delete marker, Amazon S3
-- sets the response header, @x-amz-delete-marker@, to true.
--
-- If the object you want to delete is in a bucket where the bucket
-- versioning configuration is MFA Delete enabled, you must include the
-- @x-amz-mfa@ request header in the DELETE @versionId@ request. Requests
-- that include @x-amz-mfa@ must use HTTPS.
--
-- For more information about MFA Delete, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMFADelete.html Using MFA Delete>.
-- To see sample requests that use versioning, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectDELETE.html#ExampleVersionObjectDelete Sample Request>.
--
-- You can delete objects by explicitly calling the DELETE Object API or
-- configure its lifecycle
-- (<https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycle.html PutBucketLifecycle>)
-- to enable Amazon S3 to remove them for you. If you want to block users
-- or accounts from removing or deleting objects from your bucket, you must
-- deny them the @s3:DeleteObject@, @s3:DeleteObjectVersion@, and
-- @s3:PutLifeCycleConfiguration@ actions.
--
-- The following operation is related to @DeleteObject@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
module Network.AWS.S3.DeleteObject
  ( -- * Creating a Request
    DeleteObject (..),
    newDeleteObject,

    -- * Request Lenses
    deleteObject_expectedBucketOwner,
    deleteObject_bypassGovernanceRetention,
    deleteObject_versionId,
    deleteObject_requestPayer,
    deleteObject_mfa,
    deleteObject_bucket,
    deleteObject_key,

    -- * Destructuring the Response
    DeleteObjectResponse (..),
    newDeleteObjectResponse,

    -- * Response Lenses
    deleteObjectResponse_requestCharged,
    deleteObjectResponse_deleteMarker,
    deleteObjectResponse_versionId,
    deleteObjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteObject' smart constructor.
data DeleteObject = DeleteObject'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | Indicates whether S3 Object Lock should bypass Governance-mode
    -- restrictions to process this operation.
    bypassGovernanceRetention :: Core.Maybe Core.Bool,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Core.Maybe ObjectVersionId,
    requestPayer :: Core.Maybe RequestPayer,
    -- | The concatenation of the authentication device\'s serial number, a
    -- space, and the value that is displayed on your authentication device.
    -- Required to permanently delete a versioned object if versioning is
    -- configured with MFA delete enabled.
    mfa :: Core.Maybe Core.Text,
    -- | The bucket name of the bucket containing the object.
    --
    -- When using this API with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this operation with an access point through the AWS SDKs, you
    -- provide the access point ARN in place of the bucket name. For more
    -- information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    --
    -- When using this API with Amazon S3 on Outposts, you must direct requests
    -- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
    -- form
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this operation using S3 on Outposts through the AWS SDKs, you
    -- provide the Outposts bucket ARN in place of the bucket name. For more
    -- information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    bucket :: BucketName,
    -- | Key name of the object to delete.
    key :: ObjectKey
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteObject_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bypassGovernanceRetention', 'deleteObject_bypassGovernanceRetention' - Indicates whether S3 Object Lock should bypass Governance-mode
-- restrictions to process this operation.
--
-- 'versionId', 'deleteObject_versionId' - VersionId used to reference a specific version of the object.
--
-- 'requestPayer', 'deleteObject_requestPayer' - Undocumented member.
--
-- 'mfa', 'deleteObject_mfa' - The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
-- Required to permanently delete a versioned object if versioning is
-- configured with MFA delete enabled.
--
-- 'bucket', 'deleteObject_bucket' - The bucket name of the bucket containing the object.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- 'key', 'deleteObject_key' - Key name of the object to delete.
newDeleteObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  DeleteObject
newDeleteObject pBucket_ pKey_ =
  DeleteObject'
    { expectedBucketOwner = Core.Nothing,
      bypassGovernanceRetention = Core.Nothing,
      versionId = Core.Nothing,
      requestPayer = Core.Nothing,
      mfa = Core.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteObject_expectedBucketOwner :: Lens.Lens' DeleteObject (Core.Maybe Core.Text)
deleteObject_expectedBucketOwner = Lens.lens (\DeleteObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteObject' {} a -> s {expectedBucketOwner = a} :: DeleteObject)

-- | Indicates whether S3 Object Lock should bypass Governance-mode
-- restrictions to process this operation.
deleteObject_bypassGovernanceRetention :: Lens.Lens' DeleteObject (Core.Maybe Core.Bool)
deleteObject_bypassGovernanceRetention = Lens.lens (\DeleteObject' {bypassGovernanceRetention} -> bypassGovernanceRetention) (\s@DeleteObject' {} a -> s {bypassGovernanceRetention = a} :: DeleteObject)

-- | VersionId used to reference a specific version of the object.
deleteObject_versionId :: Lens.Lens' DeleteObject (Core.Maybe ObjectVersionId)
deleteObject_versionId = Lens.lens (\DeleteObject' {versionId} -> versionId) (\s@DeleteObject' {} a -> s {versionId = a} :: DeleteObject)

-- | Undocumented member.
deleteObject_requestPayer :: Lens.Lens' DeleteObject (Core.Maybe RequestPayer)
deleteObject_requestPayer = Lens.lens (\DeleteObject' {requestPayer} -> requestPayer) (\s@DeleteObject' {} a -> s {requestPayer = a} :: DeleteObject)

-- | The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
-- Required to permanently delete a versioned object if versioning is
-- configured with MFA delete enabled.
deleteObject_mfa :: Lens.Lens' DeleteObject (Core.Maybe Core.Text)
deleteObject_mfa = Lens.lens (\DeleteObject' {mfa} -> mfa) (\s@DeleteObject' {} a -> s {mfa = a} :: DeleteObject)

-- | The bucket name of the bucket containing the object.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
deleteObject_bucket :: Lens.Lens' DeleteObject BucketName
deleteObject_bucket = Lens.lens (\DeleteObject' {bucket} -> bucket) (\s@DeleteObject' {} a -> s {bucket = a} :: DeleteObject)

-- | Key name of the object to delete.
deleteObject_key :: Lens.Lens' DeleteObject ObjectKey
deleteObject_key = Lens.lens (\DeleteObject' {key} -> key) (\s@DeleteObject' {} a -> s {key = a} :: DeleteObject)

instance Core.AWSRequest DeleteObject where
  type AWSResponse DeleteObject = DeleteObjectResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteObjectResponse'
            Core.<$> (h Core..#? "x-amz-request-charged")
            Core.<*> (h Core..#? "x-amz-delete-marker")
            Core.<*> (h Core..#? "x-amz-version-id")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteObject

instance Core.NFData DeleteObject

instance Core.ToHeaders DeleteObject where
  toHeaders DeleteObject' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-bypass-governance-retention"
          Core.=# bypassGovernanceRetention,
        "x-amz-request-payer" Core.=# requestPayer,
        "x-amz-mfa" Core.=# mfa
      ]

instance Core.ToPath DeleteObject where
  toPath DeleteObject' {..} =
    Core.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery DeleteObject where
  toQuery DeleteObject' {..} =
    Core.mconcat ["versionId" Core.=: versionId]

-- | /See:/ 'newDeleteObjectResponse' smart constructor.
data DeleteObjectResponse = DeleteObjectResponse'
  { requestCharged :: Core.Maybe RequestCharged,
    -- | Specifies whether the versioned object that was permanently deleted was
    -- (true) or was not (false) a delete marker.
    deleteMarker :: Core.Maybe Core.Bool,
    -- | Returns the version ID of the delete marker created as a result of the
    -- DELETE operation.
    versionId :: Core.Maybe ObjectVersionId,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'deleteObjectResponse_requestCharged' - Undocumented member.
--
-- 'deleteMarker', 'deleteObjectResponse_deleteMarker' - Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker.
--
-- 'versionId', 'deleteObjectResponse_versionId' - Returns the version ID of the delete marker created as a result of the
-- DELETE operation.
--
-- 'httpStatus', 'deleteObjectResponse_httpStatus' - The response's http status code.
newDeleteObjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteObjectResponse
newDeleteObjectResponse pHttpStatus_ =
  DeleteObjectResponse'
    { requestCharged =
        Core.Nothing,
      deleteMarker = Core.Nothing,
      versionId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteObjectResponse_requestCharged :: Lens.Lens' DeleteObjectResponse (Core.Maybe RequestCharged)
deleteObjectResponse_requestCharged = Lens.lens (\DeleteObjectResponse' {requestCharged} -> requestCharged) (\s@DeleteObjectResponse' {} a -> s {requestCharged = a} :: DeleteObjectResponse)

-- | Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker.
deleteObjectResponse_deleteMarker :: Lens.Lens' DeleteObjectResponse (Core.Maybe Core.Bool)
deleteObjectResponse_deleteMarker = Lens.lens (\DeleteObjectResponse' {deleteMarker} -> deleteMarker) (\s@DeleteObjectResponse' {} a -> s {deleteMarker = a} :: DeleteObjectResponse)

-- | Returns the version ID of the delete marker created as a result of the
-- DELETE operation.
deleteObjectResponse_versionId :: Lens.Lens' DeleteObjectResponse (Core.Maybe ObjectVersionId)
deleteObjectResponse_versionId = Lens.lens (\DeleteObjectResponse' {versionId} -> versionId) (\s@DeleteObjectResponse' {} a -> s {versionId = a} :: DeleteObjectResponse)

-- | The response's http status code.
deleteObjectResponse_httpStatus :: Lens.Lens' DeleteObjectResponse Core.Int
deleteObjectResponse_httpStatus = Lens.lens (\DeleteObjectResponse' {httpStatus} -> httpStatus) (\s@DeleteObjectResponse' {} a -> s {httpStatus = a} :: DeleteObjectResponse)

instance Core.NFData DeleteObjectResponse
