{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.DeleteObjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation enables you to delete multiple objects from a bucket
-- using a single HTTP request. If you know the object keys that you want
-- to delete, then this operation provides a suitable alternative to
-- sending individual delete requests, reducing per-request overhead.
--
-- The request contains a list of up to 1000 keys that you want to delete.
-- In the XML, you provide the object key names, and optionally, version
-- IDs if you want to delete a specific version of the object from a
-- versioning-enabled bucket. For each key, Amazon S3 performs a delete
-- operation and returns the result of that delete, success, or failure, in
-- the response. Note that if the object specified in the request is not
-- found, Amazon S3 returns the result as deleted.
--
-- The operation supports two modes for the response: verbose and quiet. By
-- default, the operation uses verbose mode in which the response includes
-- the result of deletion of each key in your request. In quiet mode the
-- response includes only keys where the delete operation encountered an
-- error. For a successful deletion, the operation does not return any
-- information about the delete in the response body.
--
-- When performing this operation on an MFA Delete enabled bucket, that
-- attempts to delete any versioned objects, you must include an MFA token.
-- If you do not provide one, the entire request will fail, even if there
-- are non-versioned objects you are trying to delete. If you provide an
-- invalid token, whether there are versioned keys in the request or not,
-- the entire Multi-Object Delete request will fail. For information about
-- MFA Delete, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/Versioning.html#MultiFactorAuthenticationDelete MFA Delete>.
--
-- Finally, the Content-MD5 header is required for all Multi-Object Delete
-- requests. Amazon S3 uses the header value to ensure that your request
-- body has not been altered in transit.
--
-- The following operations are related to @DeleteObjects@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
module Network.AWS.S3.DeleteObjects
  ( -- * Creating a Request
    DeleteObjects (..),
    newDeleteObjects,

    -- * Request Lenses
    deleteObjects_expectedBucketOwner,
    deleteObjects_bypassGovernanceRetention,
    deleteObjects_requestPayer,
    deleteObjects_mfa,
    deleteObjects_bucket,
    deleteObjects_delete,

    -- * Destructuring the Response
    DeleteObjectsResponse (..),
    newDeleteObjectsResponse,

    -- * Response Lenses
    deleteObjectsResponse_requestCharged,
    deleteObjectsResponse_errors,
    deleteObjectsResponse_deleted,
    deleteObjectsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteObjects' smart constructor.
data DeleteObjects = DeleteObjects'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether you want to delete this object even if it has a
    -- Governance-type Object Lock in place. You must have sufficient
    -- permissions to perform this operation.
    bypassGovernanceRetention :: Prelude.Maybe Prelude.Bool,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | The concatenation of the authentication device\'s serial number, a
    -- space, and the value that is displayed on your authentication device.
    -- Required to permanently delete a versioned object if versioning is
    -- configured with MFA delete enabled.
    mfa :: Prelude.Maybe Prelude.Text,
    -- | The bucket name containing the objects to delete.
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
    -- | Container for the request.
    delete' :: Delete
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteObjects_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bypassGovernanceRetention', 'deleteObjects_bypassGovernanceRetention' - Specifies whether you want to delete this object even if it has a
-- Governance-type Object Lock in place. You must have sufficient
-- permissions to perform this operation.
--
-- 'requestPayer', 'deleteObjects_requestPayer' - Undocumented member.
--
-- 'mfa', 'deleteObjects_mfa' - The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
-- Required to permanently delete a versioned object if versioning is
-- configured with MFA delete enabled.
--
-- 'bucket', 'deleteObjects_bucket' - The bucket name containing the objects to delete.
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
-- 'delete'', 'deleteObjects_delete' - Container for the request.
newDeleteObjects ::
  -- | 'bucket'
  BucketName ->
  -- | 'delete''
  Delete ->
  DeleteObjects
newDeleteObjects pBucket_ pDelete_ =
  DeleteObjects'
    { expectedBucketOwner =
        Prelude.Nothing,
      bypassGovernanceRetention = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      mfa = Prelude.Nothing,
      bucket = pBucket_,
      delete' = pDelete_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteObjects_expectedBucketOwner :: Lens.Lens' DeleteObjects (Prelude.Maybe Prelude.Text)
deleteObjects_expectedBucketOwner = Lens.lens (\DeleteObjects' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteObjects' {} a -> s {expectedBucketOwner = a} :: DeleteObjects)

-- | Specifies whether you want to delete this object even if it has a
-- Governance-type Object Lock in place. You must have sufficient
-- permissions to perform this operation.
deleteObjects_bypassGovernanceRetention :: Lens.Lens' DeleteObjects (Prelude.Maybe Prelude.Bool)
deleteObjects_bypassGovernanceRetention = Lens.lens (\DeleteObjects' {bypassGovernanceRetention} -> bypassGovernanceRetention) (\s@DeleteObjects' {} a -> s {bypassGovernanceRetention = a} :: DeleteObjects)

-- | Undocumented member.
deleteObjects_requestPayer :: Lens.Lens' DeleteObjects (Prelude.Maybe RequestPayer)
deleteObjects_requestPayer = Lens.lens (\DeleteObjects' {requestPayer} -> requestPayer) (\s@DeleteObjects' {} a -> s {requestPayer = a} :: DeleteObjects)

-- | The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
-- Required to permanently delete a versioned object if versioning is
-- configured with MFA delete enabled.
deleteObjects_mfa :: Lens.Lens' DeleteObjects (Prelude.Maybe Prelude.Text)
deleteObjects_mfa = Lens.lens (\DeleteObjects' {mfa} -> mfa) (\s@DeleteObjects' {} a -> s {mfa = a} :: DeleteObjects)

-- | The bucket name containing the objects to delete.
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
deleteObjects_bucket :: Lens.Lens' DeleteObjects BucketName
deleteObjects_bucket = Lens.lens (\DeleteObjects' {bucket} -> bucket) (\s@DeleteObjects' {} a -> s {bucket = a} :: DeleteObjects)

-- | Container for the request.
deleteObjects_delete :: Lens.Lens' DeleteObjects Delete
deleteObjects_delete = Lens.lens (\DeleteObjects' {delete'} -> delete') (\s@DeleteObjects' {} a -> s {delete' = a} :: DeleteObjects)

instance Prelude.AWSRequest DeleteObjects where
  type Rs DeleteObjects = DeleteObjectsResponse
  request =
    Request.contentMD5Header
      Prelude.. Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteObjectsResponse'
            Prelude.<$> (h Prelude..#? "x-amz-request-charged")
            Prelude.<*> (Prelude.may (Prelude.parseXMLList "Error") x)
            Prelude.<*> (Prelude.may (Prelude.parseXMLList "Deleted") x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteObjects

instance Prelude.NFData DeleteObjects

instance Prelude.ToElement DeleteObjects where
  toElement DeleteObjects' {..} =
    Prelude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}Delete"
      delete'

instance Prelude.ToHeaders DeleteObjects where
  toHeaders DeleteObjects' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner,
        "x-amz-bypass-governance-retention"
          Prelude.=# bypassGovernanceRetention,
        "x-amz-request-payer" Prelude.=# requestPayer,
        "x-amz-mfa" Prelude.=# mfa
      ]

instance Prelude.ToPath DeleteObjects where
  toPath DeleteObjects' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery DeleteObjects where
  toQuery = Prelude.const (Prelude.mconcat ["delete"])

-- | /See:/ 'newDeleteObjectsResponse' smart constructor.
data DeleteObjectsResponse = DeleteObjectsResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | Container for a failed delete operation that describes the object that
    -- Amazon S3 attempted to delete and the error it encountered.
    errors :: Prelude.Maybe [S3ServiceError],
    -- | Container element for a successful delete. It identifies the object that
    -- was successfully deleted.
    deleted :: Prelude.Maybe [DeletedObject],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'deleteObjectsResponse_requestCharged' - Undocumented member.
--
-- 'errors', 'deleteObjectsResponse_errors' - Container for a failed delete operation that describes the object that
-- Amazon S3 attempted to delete and the error it encountered.
--
-- 'deleted', 'deleteObjectsResponse_deleted' - Container element for a successful delete. It identifies the object that
-- was successfully deleted.
--
-- 'httpStatus', 'deleteObjectsResponse_httpStatus' - The response's http status code.
newDeleteObjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteObjectsResponse
newDeleteObjectsResponse pHttpStatus_ =
  DeleteObjectsResponse'
    { requestCharged =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      deleted = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteObjectsResponse_requestCharged :: Lens.Lens' DeleteObjectsResponse (Prelude.Maybe RequestCharged)
deleteObjectsResponse_requestCharged = Lens.lens (\DeleteObjectsResponse' {requestCharged} -> requestCharged) (\s@DeleteObjectsResponse' {} a -> s {requestCharged = a} :: DeleteObjectsResponse)

-- | Container for a failed delete operation that describes the object that
-- Amazon S3 attempted to delete and the error it encountered.
deleteObjectsResponse_errors :: Lens.Lens' DeleteObjectsResponse (Prelude.Maybe [S3ServiceError])
deleteObjectsResponse_errors = Lens.lens (\DeleteObjectsResponse' {errors} -> errors) (\s@DeleteObjectsResponse' {} a -> s {errors = a} :: DeleteObjectsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Container element for a successful delete. It identifies the object that
-- was successfully deleted.
deleteObjectsResponse_deleted :: Lens.Lens' DeleteObjectsResponse (Prelude.Maybe [DeletedObject])
deleteObjectsResponse_deleted = Lens.lens (\DeleteObjectsResponse' {deleted} -> deleted) (\s@DeleteObjectsResponse' {} a -> s {deleted = a} :: DeleteObjectsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
deleteObjectsResponse_httpStatus :: Lens.Lens' DeleteObjectsResponse Prelude.Int
deleteObjectsResponse_httpStatus = Lens.lens (\DeleteObjectsResponse' {httpStatus} -> httpStatus) (\s@DeleteObjectsResponse' {} a -> s {httpStatus = a} :: DeleteObjectsResponse)

instance Prelude.NFData DeleteObjectsResponse
