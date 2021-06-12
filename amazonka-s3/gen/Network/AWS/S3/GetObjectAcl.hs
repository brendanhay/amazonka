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
-- Module      : Network.AWS.S3.GetObjectAcl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the access control list (ACL) of an object. To use this
-- operation, you must have @READ_ACP@ access to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- __Versioning__
--
-- By default, GET returns ACL information about the current version of an
-- object. To return ACL information about a different version, use the
-- versionId subresource.
--
-- The following operations are related to @GetObjectAcl@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
module Network.AWS.S3.GetObjectAcl
  ( -- * Creating a Request
    GetObjectAcl (..),
    newGetObjectAcl,

    -- * Request Lenses
    getObjectAcl_expectedBucketOwner,
    getObjectAcl_versionId,
    getObjectAcl_requestPayer,
    getObjectAcl_bucket,
    getObjectAcl_key,

    -- * Destructuring the Response
    GetObjectAclResponse (..),
    newGetObjectAclResponse,

    -- * Response Lenses
    getObjectAclResponse_requestCharged,
    getObjectAclResponse_owner,
    getObjectAclResponse_grants,
    getObjectAclResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetObjectAcl' smart constructor.
data GetObjectAcl = GetObjectAcl'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Core.Maybe ObjectVersionId,
    requestPayer :: Core.Maybe RequestPayer,
    -- | The bucket name that contains the object for which to get the ACL
    -- information.
    --
    -- When using this API with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this operation with an access point through the AWS SDKs, you
    -- provide the access point ARN in place of the bucket name. For more
    -- information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    bucket :: BucketName,
    -- | The key of the object for which to get the ACL information.
    key :: ObjectKey
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetObjectAcl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getObjectAcl_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'versionId', 'getObjectAcl_versionId' - VersionId used to reference a specific version of the object.
--
-- 'requestPayer', 'getObjectAcl_requestPayer' - Undocumented member.
--
-- 'bucket', 'getObjectAcl_bucket' - The bucket name that contains the object for which to get the ACL
-- information.
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
-- 'key', 'getObjectAcl_key' - The key of the object for which to get the ACL information.
newGetObjectAcl ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectAcl
newGetObjectAcl pBucket_ pKey_ =
  GetObjectAcl'
    { expectedBucketOwner = Core.Nothing,
      versionId = Core.Nothing,
      requestPayer = Core.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getObjectAcl_expectedBucketOwner :: Lens.Lens' GetObjectAcl (Core.Maybe Core.Text)
getObjectAcl_expectedBucketOwner = Lens.lens (\GetObjectAcl' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObjectAcl' {} a -> s {expectedBucketOwner = a} :: GetObjectAcl)

-- | VersionId used to reference a specific version of the object.
getObjectAcl_versionId :: Lens.Lens' GetObjectAcl (Core.Maybe ObjectVersionId)
getObjectAcl_versionId = Lens.lens (\GetObjectAcl' {versionId} -> versionId) (\s@GetObjectAcl' {} a -> s {versionId = a} :: GetObjectAcl)

-- | Undocumented member.
getObjectAcl_requestPayer :: Lens.Lens' GetObjectAcl (Core.Maybe RequestPayer)
getObjectAcl_requestPayer = Lens.lens (\GetObjectAcl' {requestPayer} -> requestPayer) (\s@GetObjectAcl' {} a -> s {requestPayer = a} :: GetObjectAcl)

-- | The bucket name that contains the object for which to get the ACL
-- information.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
getObjectAcl_bucket :: Lens.Lens' GetObjectAcl BucketName
getObjectAcl_bucket = Lens.lens (\GetObjectAcl' {bucket} -> bucket) (\s@GetObjectAcl' {} a -> s {bucket = a} :: GetObjectAcl)

-- | The key of the object for which to get the ACL information.
getObjectAcl_key :: Lens.Lens' GetObjectAcl ObjectKey
getObjectAcl_key = Lens.lens (\GetObjectAcl' {key} -> key) (\s@GetObjectAcl' {} a -> s {key = a} :: GetObjectAcl)

instance Core.AWSRequest GetObjectAcl where
  type AWSResponse GetObjectAcl = GetObjectAclResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetObjectAclResponse'
            Core.<$> (h Core..#? "x-amz-request-charged")
            Core.<*> (x Core..@? "Owner")
            Core.<*> ( x Core..@? "AccessControlList" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "Grant")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetObjectAcl

instance Core.NFData GetObjectAcl

instance Core.ToHeaders GetObjectAcl where
  toHeaders GetObjectAcl' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-request-payer" Core.=# requestPayer
      ]

instance Core.ToPath GetObjectAcl where
  toPath GetObjectAcl' {..} =
    Core.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery GetObjectAcl where
  toQuery GetObjectAcl' {..} =
    Core.mconcat ["versionId" Core.=: versionId, "acl"]

-- | /See:/ 'newGetObjectAclResponse' smart constructor.
data GetObjectAclResponse = GetObjectAclResponse'
  { requestCharged :: Core.Maybe RequestCharged,
    -- | Container for the bucket owner\'s display name and ID.
    owner :: Core.Maybe Owner,
    -- | A list of grants.
    grants :: Core.Maybe [Grant],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetObjectAclResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'getObjectAclResponse_requestCharged' - Undocumented member.
--
-- 'owner', 'getObjectAclResponse_owner' - Container for the bucket owner\'s display name and ID.
--
-- 'grants', 'getObjectAclResponse_grants' - A list of grants.
--
-- 'httpStatus', 'getObjectAclResponse_httpStatus' - The response's http status code.
newGetObjectAclResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetObjectAclResponse
newGetObjectAclResponse pHttpStatus_ =
  GetObjectAclResponse'
    { requestCharged =
        Core.Nothing,
      owner = Core.Nothing,
      grants = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getObjectAclResponse_requestCharged :: Lens.Lens' GetObjectAclResponse (Core.Maybe RequestCharged)
getObjectAclResponse_requestCharged = Lens.lens (\GetObjectAclResponse' {requestCharged} -> requestCharged) (\s@GetObjectAclResponse' {} a -> s {requestCharged = a} :: GetObjectAclResponse)

-- | Container for the bucket owner\'s display name and ID.
getObjectAclResponse_owner :: Lens.Lens' GetObjectAclResponse (Core.Maybe Owner)
getObjectAclResponse_owner = Lens.lens (\GetObjectAclResponse' {owner} -> owner) (\s@GetObjectAclResponse' {} a -> s {owner = a} :: GetObjectAclResponse)

-- | A list of grants.
getObjectAclResponse_grants :: Lens.Lens' GetObjectAclResponse (Core.Maybe [Grant])
getObjectAclResponse_grants = Lens.lens (\GetObjectAclResponse' {grants} -> grants) (\s@GetObjectAclResponse' {} a -> s {grants = a} :: GetObjectAclResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getObjectAclResponse_httpStatus :: Lens.Lens' GetObjectAclResponse Core.Int
getObjectAclResponse_httpStatus = Lens.lens (\GetObjectAclResponse' {httpStatus} -> httpStatus) (\s@GetObjectAclResponse' {} a -> s {httpStatus = a} :: GetObjectAclResponse)

instance Core.NFData GetObjectAclResponse
