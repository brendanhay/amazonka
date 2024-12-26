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
-- Module      : Amazonka.S3.DeleteObjects
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action enables you to delete multiple objects from a bucket using a
-- single HTTP request. If you know the object keys that you want to
-- delete, then this action provides a suitable alternative to sending
-- individual delete requests, reducing per-request overhead.
--
-- The request contains a list of up to 1000 keys that you want to delete.
-- In the XML, you provide the object key names, and optionally, version
-- IDs if you want to delete a specific version of the object from a
-- versioning-enabled bucket. For each key, Amazon S3 performs a delete
-- action and returns the result of that delete, success, or failure, in
-- the response. Note that if the object specified in the request is not
-- found, Amazon S3 returns the result as deleted.
--
-- The action supports two modes for the response: verbose and quiet. By
-- default, the action uses verbose mode in which the response includes the
-- result of deletion of each key in your request. In quiet mode the
-- response includes only keys where the delete action encountered an
-- error. For a successful deletion, the action does not return any
-- information about the delete in the response body.
--
-- When performing this action on an MFA Delete enabled bucket, that
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
module Amazonka.S3.DeleteObjects
  ( -- * Creating a Request
    DeleteObjects (..),
    newDeleteObjects,

    -- * Request Lenses
    deleteObjects_bypassGovernanceRetention,
    deleteObjects_checksumAlgorithm,
    deleteObjects_expectedBucketOwner,
    deleteObjects_mfa,
    deleteObjects_requestPayer,
    deleteObjects_bucket,
    deleteObjects_delete,

    -- * Destructuring the Response
    DeleteObjectsResponse (..),
    newDeleteObjectsResponse,

    -- * Response Lenses
    deleteObjectsResponse_deleted,
    deleteObjectsResponse_errors,
    deleteObjectsResponse_requestCharged,
    deleteObjectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeleteObjects' smart constructor.
data DeleteObjects = DeleteObjects'
  { -- | Specifies whether you want to delete this object even if it has a
    -- Governance-type Object Lock in place. To use this header, you must have
    -- the @s3:BypassGovernanceRetention@ permission.
    bypassGovernanceRetention :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the algorithm used to create the checksum for the object when
    -- using the SDK. This header will not provide any additional functionality
    -- if not using the SDK. When sending this header, there must be a
    -- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
    -- Otherwise, Amazon S3 fails the request with the HTTP status code
    -- @400 Bad Request@. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    --
    -- If you provide an individual checksum, Amazon S3 ignores any provided
    -- @ChecksumAlgorithm@ parameter.
    --
    -- This checksum algorithm must be the same for all parts and it match the
    -- checksum value supplied in the @CreateMultipartUpload@ request.
    checksumAlgorithm :: Prelude.Maybe ChecksumAlgorithm,
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
    -- | The bucket name containing the objects to delete.
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
    -- | Container for the request.
    delete' :: Delete
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bypassGovernanceRetention', 'deleteObjects_bypassGovernanceRetention' - Specifies whether you want to delete this object even if it has a
-- Governance-type Object Lock in place. To use this header, you must have
-- the @s3:BypassGovernanceRetention@ permission.
--
-- 'checksumAlgorithm', 'deleteObjects_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
-- using the SDK. This header will not provide any additional functionality
-- if not using the SDK. When sending this header, there must be a
-- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
-- Otherwise, Amazon S3 fails the request with the HTTP status code
-- @400 Bad Request@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- If you provide an individual checksum, Amazon S3 ignores any provided
-- @ChecksumAlgorithm@ parameter.
--
-- This checksum algorithm must be the same for all parts and it match the
-- checksum value supplied in the @CreateMultipartUpload@ request.
--
-- 'expectedBucketOwner', 'deleteObjects_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'mfa', 'deleteObjects_mfa' - The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
-- Required to permanently delete a versioned object if versioning is
-- configured with MFA delete enabled.
--
-- 'requestPayer', 'deleteObjects_requestPayer' - Undocumented member.
--
-- 'bucket', 'deleteObjects_bucket' - The bucket name containing the objects to delete.
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
-- 'delete'', 'deleteObjects_delete' - Container for the request.
newDeleteObjects ::
  -- | 'bucket'
  BucketName ->
  -- | 'delete''
  Delete ->
  DeleteObjects
newDeleteObjects pBucket_ pDelete_ =
  DeleteObjects'
    { bypassGovernanceRetention =
        Prelude.Nothing,
      checksumAlgorithm = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      mfa = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      bucket = pBucket_,
      delete' = pDelete_
    }

-- | Specifies whether you want to delete this object even if it has a
-- Governance-type Object Lock in place. To use this header, you must have
-- the @s3:BypassGovernanceRetention@ permission.
deleteObjects_bypassGovernanceRetention :: Lens.Lens' DeleteObjects (Prelude.Maybe Prelude.Bool)
deleteObjects_bypassGovernanceRetention = Lens.lens (\DeleteObjects' {bypassGovernanceRetention} -> bypassGovernanceRetention) (\s@DeleteObjects' {} a -> s {bypassGovernanceRetention = a} :: DeleteObjects)

-- | Indicates the algorithm used to create the checksum for the object when
-- using the SDK. This header will not provide any additional functionality
-- if not using the SDK. When sending this header, there must be a
-- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
-- Otherwise, Amazon S3 fails the request with the HTTP status code
-- @400 Bad Request@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- If you provide an individual checksum, Amazon S3 ignores any provided
-- @ChecksumAlgorithm@ parameter.
--
-- This checksum algorithm must be the same for all parts and it match the
-- checksum value supplied in the @CreateMultipartUpload@ request.
deleteObjects_checksumAlgorithm :: Lens.Lens' DeleteObjects (Prelude.Maybe ChecksumAlgorithm)
deleteObjects_checksumAlgorithm = Lens.lens (\DeleteObjects' {checksumAlgorithm} -> checksumAlgorithm) (\s@DeleteObjects' {} a -> s {checksumAlgorithm = a} :: DeleteObjects)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
deleteObjects_expectedBucketOwner :: Lens.Lens' DeleteObjects (Prelude.Maybe Prelude.Text)
deleteObjects_expectedBucketOwner = Lens.lens (\DeleteObjects' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteObjects' {} a -> s {expectedBucketOwner = a} :: DeleteObjects)

-- | The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
-- Required to permanently delete a versioned object if versioning is
-- configured with MFA delete enabled.
deleteObjects_mfa :: Lens.Lens' DeleteObjects (Prelude.Maybe Prelude.Text)
deleteObjects_mfa = Lens.lens (\DeleteObjects' {mfa} -> mfa) (\s@DeleteObjects' {} a -> s {mfa = a} :: DeleteObjects)

-- | Undocumented member.
deleteObjects_requestPayer :: Lens.Lens' DeleteObjects (Prelude.Maybe RequestPayer)
deleteObjects_requestPayer = Lens.lens (\DeleteObjects' {requestPayer} -> requestPayer) (\s@DeleteObjects' {} a -> s {requestPayer = a} :: DeleteObjects)

-- | The bucket name containing the objects to delete.
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
deleteObjects_bucket :: Lens.Lens' DeleteObjects BucketName
deleteObjects_bucket = Lens.lens (\DeleteObjects' {bucket} -> bucket) (\s@DeleteObjects' {} a -> s {bucket = a} :: DeleteObjects)

-- | Container for the request.
deleteObjects_delete :: Lens.Lens' DeleteObjects Delete
deleteObjects_delete = Lens.lens (\DeleteObjects' {delete'} -> delete') (\s@DeleteObjects' {} a -> s {delete' = a} :: DeleteObjects)

instance Core.AWSRequest DeleteObjects where
  type
    AWSResponse DeleteObjects =
      DeleteObjectsResponse
  request overrides =
    Request.contentMD5Header
      Prelude.. Request.s3vhost
      Prelude.. Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteObjectsResponse'
            Prelude.<$> (Core.may (Data.parseXMLList "Deleted") x)
            Prelude.<*> (Core.may (Data.parseXMLList "Error") x)
            Prelude.<*> (h Data..#? "x-amz-request-charged")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteObjects where
  hashWithSalt _salt DeleteObjects' {..} =
    _salt
      `Prelude.hashWithSalt` bypassGovernanceRetention
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` mfa
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` delete'

instance Prelude.NFData DeleteObjects where
  rnf DeleteObjects' {..} =
    Prelude.rnf bypassGovernanceRetention `Prelude.seq`
      Prelude.rnf checksumAlgorithm `Prelude.seq`
        Prelude.rnf expectedBucketOwner `Prelude.seq`
          Prelude.rnf mfa `Prelude.seq`
            Prelude.rnf requestPayer `Prelude.seq`
              Prelude.rnf bucket `Prelude.seq`
                Prelude.rnf delete'

instance Data.ToElement DeleteObjects where
  toElement DeleteObjects' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}Delete"
      delete'

instance Data.ToHeaders DeleteObjects where
  toHeaders DeleteObjects' {..} =
    Prelude.mconcat
      [ "x-amz-bypass-governance-retention"
          Data.=# bypassGovernanceRetention,
        "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-mfa" Data.=# mfa,
        "x-amz-request-payer" Data.=# requestPayer
      ]

instance Data.ToPath DeleteObjects where
  toPath DeleteObjects' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery DeleteObjects where
  toQuery = Prelude.const (Prelude.mconcat ["delete"])

-- | /See:/ 'newDeleteObjectsResponse' smart constructor.
data DeleteObjectsResponse = DeleteObjectsResponse'
  { -- | Container element for a successful delete. It identifies the object that
    -- was successfully deleted.
    deleted :: Prelude.Maybe [DeletedObject],
    -- | Container for a failed delete action that describes the object that
    -- Amazon S3 attempted to delete and the error it encountered.
    errors :: Prelude.Maybe [S3ServiceError],
    requestCharged :: Prelude.Maybe RequestCharged,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleted', 'deleteObjectsResponse_deleted' - Container element for a successful delete. It identifies the object that
-- was successfully deleted.
--
-- 'errors', 'deleteObjectsResponse_errors' - Container for a failed delete action that describes the object that
-- Amazon S3 attempted to delete and the error it encountered.
--
-- 'requestCharged', 'deleteObjectsResponse_requestCharged' - Undocumented member.
--
-- 'httpStatus', 'deleteObjectsResponse_httpStatus' - The response's http status code.
newDeleteObjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteObjectsResponse
newDeleteObjectsResponse pHttpStatus_ =
  DeleteObjectsResponse'
    { deleted = Prelude.Nothing,
      errors = Prelude.Nothing,
      requestCharged = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Container element for a successful delete. It identifies the object that
-- was successfully deleted.
deleteObjectsResponse_deleted :: Lens.Lens' DeleteObjectsResponse (Prelude.Maybe [DeletedObject])
deleteObjectsResponse_deleted = Lens.lens (\DeleteObjectsResponse' {deleted} -> deleted) (\s@DeleteObjectsResponse' {} a -> s {deleted = a} :: DeleteObjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Container for a failed delete action that describes the object that
-- Amazon S3 attempted to delete and the error it encountered.
deleteObjectsResponse_errors :: Lens.Lens' DeleteObjectsResponse (Prelude.Maybe [S3ServiceError])
deleteObjectsResponse_errors = Lens.lens (\DeleteObjectsResponse' {errors} -> errors) (\s@DeleteObjectsResponse' {} a -> s {errors = a} :: DeleteObjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
deleteObjectsResponse_requestCharged :: Lens.Lens' DeleteObjectsResponse (Prelude.Maybe RequestCharged)
deleteObjectsResponse_requestCharged = Lens.lens (\DeleteObjectsResponse' {requestCharged} -> requestCharged) (\s@DeleteObjectsResponse' {} a -> s {requestCharged = a} :: DeleteObjectsResponse)

-- | The response's http status code.
deleteObjectsResponse_httpStatus :: Lens.Lens' DeleteObjectsResponse Prelude.Int
deleteObjectsResponse_httpStatus = Lens.lens (\DeleteObjectsResponse' {httpStatus} -> httpStatus) (\s@DeleteObjectsResponse' {} a -> s {httpStatus = a} :: DeleteObjectsResponse)

instance Prelude.NFData DeleteObjectsResponse where
  rnf DeleteObjectsResponse' {..} =
    Prelude.rnf deleted `Prelude.seq`
      Prelude.rnf errors `Prelude.seq`
        Prelude.rnf requestCharged `Prelude.seq`
          Prelude.rnf httpStatus
