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
-- Module      : Amazonka.S3.DeleteObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object. If there
-- isn\'t a null version, Amazon S3 does not remove any objects but will
-- still respond that the command was successful.
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
-- You can delete objects by explicitly calling DELETE Object or configure
-- its lifecycle
-- (<https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycle.html PutBucketLifecycle>)
-- to enable Amazon S3 to remove them for you. If you want to block users
-- or accounts from removing or deleting objects from your bucket, you must
-- deny them the @s3:DeleteObject@, @s3:DeleteObjectVersion@, and
-- @s3:PutLifeCycleConfiguration@ actions.
--
-- The following action is related to @DeleteObject@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
module Amazonka.S3.DeleteObject
  ( -- * Creating a Request
    DeleteObject (..),
    newDeleteObject,

    -- * Request Lenses
    deleteObject_bypassGovernanceRetention,
    deleteObject_expectedBucketOwner,
    deleteObject_mfa,
    deleteObject_requestPayer,
    deleteObject_versionId,
    deleteObject_bucket,
    deleteObject_key,

    -- * Destructuring the Response
    DeleteObjectResponse (..),
    newDeleteObjectResponse,

    -- * Response Lenses
    deleteObjectResponse_deleteMarker,
    deleteObjectResponse_requestCharged,
    deleteObjectResponse_versionId,
    deleteObjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeleteObject' smart constructor.
data DeleteObject = DeleteObject'
  { -- | Indicates whether S3 Object Lock should bypass Governance-mode
    -- restrictions to process this operation. To use this header, you must
    -- have the @s3:BypassGovernanceRetention@ permission.
    bypassGovernanceRetention :: Prelude.Maybe Prelude.Bool,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The concatenation of the authentication device\'s serial number, a
    -- space, and the value that is displayed on your authentication device.
    -- Required to permanently delete a versioned object if versioning is
    -- configured with MFA delete enabled.
    mfa :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The bucket name of the bucket containing the object.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    --
    -- When using this action with Amazon S3 on Outposts, you must direct
    -- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
    -- takes the form
    -- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
    -- When using this action with S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | Key name of the object to delete.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bypassGovernanceRetention', 'deleteObject_bypassGovernanceRetention' - Indicates whether S3 Object Lock should bypass Governance-mode
-- restrictions to process this operation. To use this header, you must
-- have the @s3:BypassGovernanceRetention@ permission.
--
-- 'expectedBucketOwner', 'deleteObject_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'mfa', 'deleteObject_mfa' - The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
-- Required to permanently delete a versioned object if versioning is
-- configured with MFA delete enabled.
--
-- 'requestPayer', 'deleteObject_requestPayer' - Undocumented member.
--
-- 'versionId', 'deleteObject_versionId' - VersionId used to reference a specific version of the object.
--
-- 'bucket', 'deleteObject_bucket' - The bucket name of the bucket containing the object.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
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
    { bypassGovernanceRetention =
        Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      mfa = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | Indicates whether S3 Object Lock should bypass Governance-mode
-- restrictions to process this operation. To use this header, you must
-- have the @s3:BypassGovernanceRetention@ permission.
deleteObject_bypassGovernanceRetention :: Lens.Lens' DeleteObject (Prelude.Maybe Prelude.Bool)
deleteObject_bypassGovernanceRetention = Lens.lens (\DeleteObject' {bypassGovernanceRetention} -> bypassGovernanceRetention) (\s@DeleteObject' {} a -> s {bypassGovernanceRetention = a} :: DeleteObject)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
deleteObject_expectedBucketOwner :: Lens.Lens' DeleteObject (Prelude.Maybe Prelude.Text)
deleteObject_expectedBucketOwner = Lens.lens (\DeleteObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteObject' {} a -> s {expectedBucketOwner = a} :: DeleteObject)

-- | The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
-- Required to permanently delete a versioned object if versioning is
-- configured with MFA delete enabled.
deleteObject_mfa :: Lens.Lens' DeleteObject (Prelude.Maybe Prelude.Text)
deleteObject_mfa = Lens.lens (\DeleteObject' {mfa} -> mfa) (\s@DeleteObject' {} a -> s {mfa = a} :: DeleteObject)

-- | Undocumented member.
deleteObject_requestPayer :: Lens.Lens' DeleteObject (Prelude.Maybe RequestPayer)
deleteObject_requestPayer = Lens.lens (\DeleteObject' {requestPayer} -> requestPayer) (\s@DeleteObject' {} a -> s {requestPayer = a} :: DeleteObject)

-- | VersionId used to reference a specific version of the object.
deleteObject_versionId :: Lens.Lens' DeleteObject (Prelude.Maybe ObjectVersionId)
deleteObject_versionId = Lens.lens (\DeleteObject' {versionId} -> versionId) (\s@DeleteObject' {} a -> s {versionId = a} :: DeleteObject)

-- | The bucket name of the bucket containing the object.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
deleteObject_bucket :: Lens.Lens' DeleteObject BucketName
deleteObject_bucket = Lens.lens (\DeleteObject' {bucket} -> bucket) (\s@DeleteObject' {} a -> s {bucket = a} :: DeleteObject)

-- | Key name of the object to delete.
deleteObject_key :: Lens.Lens' DeleteObject ObjectKey
deleteObject_key = Lens.lens (\DeleteObject' {key} -> key) (\s@DeleteObject' {} a -> s {key = a} :: DeleteObject)

instance Core.AWSRequest DeleteObject where
  type AWSResponse DeleteObject = DeleteObjectResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteObjectResponse'
            Prelude.<$> (h Data..#? "x-amz-delete-marker")
            Prelude.<*> (h Data..#? "x-amz-request-charged")
            Prelude.<*> (h Data..#? "x-amz-version-id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteObject where
  hashWithSalt _salt DeleteObject' {..} =
    _salt
      `Prelude.hashWithSalt` bypassGovernanceRetention
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` mfa
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData DeleteObject where
  rnf DeleteObject' {..} =
    Prelude.rnf bypassGovernanceRetention `Prelude.seq`
      Prelude.rnf expectedBucketOwner `Prelude.seq`
        Prelude.rnf mfa `Prelude.seq`
          Prelude.rnf requestPayer `Prelude.seq`
            Prelude.rnf versionId `Prelude.seq`
              Prelude.rnf bucket `Prelude.seq`
                Prelude.rnf key

instance Data.ToHeaders DeleteObject where
  toHeaders DeleteObject' {..} =
    Prelude.mconcat
      [ "x-amz-bypass-governance-retention"
          Data.=# bypassGovernanceRetention,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-mfa" Data.=# mfa,
        "x-amz-request-payer" Data.=# requestPayer
      ]

instance Data.ToPath DeleteObject where
  toPath DeleteObject' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery DeleteObject where
  toQuery DeleteObject' {..} =
    Prelude.mconcat ["versionId" Data.=: versionId]

-- | /See:/ 'newDeleteObjectResponse' smart constructor.
data DeleteObjectResponse = DeleteObjectResponse'
  { -- | Specifies whether the versioned object that was permanently deleted was
    -- (true) or was not (false) a delete marker.
    deleteMarker :: Prelude.Maybe Prelude.Bool,
    requestCharged :: Prelude.Maybe RequestCharged,
    -- | Returns the version ID of the delete marker created as a result of the
    -- DELETE operation.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteMarker', 'deleteObjectResponse_deleteMarker' - Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker.
--
-- 'requestCharged', 'deleteObjectResponse_requestCharged' - Undocumented member.
--
-- 'versionId', 'deleteObjectResponse_versionId' - Returns the version ID of the delete marker created as a result of the
-- DELETE operation.
--
-- 'httpStatus', 'deleteObjectResponse_httpStatus' - The response's http status code.
newDeleteObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteObjectResponse
newDeleteObjectResponse pHttpStatus_ =
  DeleteObjectResponse'
    { deleteMarker =
        Prelude.Nothing,
      requestCharged = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker.
deleteObjectResponse_deleteMarker :: Lens.Lens' DeleteObjectResponse (Prelude.Maybe Prelude.Bool)
deleteObjectResponse_deleteMarker = Lens.lens (\DeleteObjectResponse' {deleteMarker} -> deleteMarker) (\s@DeleteObjectResponse' {} a -> s {deleteMarker = a} :: DeleteObjectResponse)

-- | Undocumented member.
deleteObjectResponse_requestCharged :: Lens.Lens' DeleteObjectResponse (Prelude.Maybe RequestCharged)
deleteObjectResponse_requestCharged = Lens.lens (\DeleteObjectResponse' {requestCharged} -> requestCharged) (\s@DeleteObjectResponse' {} a -> s {requestCharged = a} :: DeleteObjectResponse)

-- | Returns the version ID of the delete marker created as a result of the
-- DELETE operation.
deleteObjectResponse_versionId :: Lens.Lens' DeleteObjectResponse (Prelude.Maybe ObjectVersionId)
deleteObjectResponse_versionId = Lens.lens (\DeleteObjectResponse' {versionId} -> versionId) (\s@DeleteObjectResponse' {} a -> s {versionId = a} :: DeleteObjectResponse)

-- | The response's http status code.
deleteObjectResponse_httpStatus :: Lens.Lens' DeleteObjectResponse Prelude.Int
deleteObjectResponse_httpStatus = Lens.lens (\DeleteObjectResponse' {httpStatus} -> httpStatus) (\s@DeleteObjectResponse' {} a -> s {httpStatus = a} :: DeleteObjectResponse)

instance Prelude.NFData DeleteObjectResponse where
  rnf DeleteObjectResponse' {..} =
    Prelude.rnf deleteMarker `Prelude.seq`
      Prelude.rnf requestCharged `Prelude.seq`
        Prelude.rnf versionId `Prelude.seq`
          Prelude.rnf httpStatus
