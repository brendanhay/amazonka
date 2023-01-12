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
-- Module      : Amazonka.S3.GetObjectAcl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the access control list (ACL) of an object. To use this
-- operation, you must have @s3:GetObjectAcl@ permissions or @READ_ACP@
-- access to the object. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/acl-overview.html#acl-access-policy-permission-mapping Mapping of ACL permissions and access policy permissions>
-- in the /Amazon S3 User Guide/
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- __Versioning__
--
-- By default, GET returns ACL information about the current version of an
-- object. To return ACL information about a different version, use the
-- versionId subresource.
--
-- If your bucket uses the bucket owner enforced setting for S3 Object
-- Ownership, requests to read ACLs are still supported and return the
-- @bucket-owner-full-control@ ACL with the owner being the account that
-- created the bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html Controlling object ownership and disabling ACLs>
-- in the /Amazon S3 User Guide/.
--
-- The following operations are related to @GetObjectAcl@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAttributes.html GetObjectAttributes>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
module Amazonka.S3.GetObjectAcl
  ( -- * Creating a Request
    GetObjectAcl (..),
    newGetObjectAcl,

    -- * Request Lenses
    getObjectAcl_expectedBucketOwner,
    getObjectAcl_requestPayer,
    getObjectAcl_versionId,
    getObjectAcl_bucket,
    getObjectAcl_key,

    -- * Destructuring the Response
    GetObjectAclResponse (..),
    newGetObjectAclResponse,

    -- * Response Lenses
    getObjectAclResponse_grants,
    getObjectAclResponse_owner,
    getObjectAclResponse_requestCharged,
    getObjectAclResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetObjectAcl' smart constructor.
data GetObjectAcl = GetObjectAcl'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The bucket name that contains the object for which to get the ACL
    -- information.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | The key of the object for which to get the ACL information.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectAcl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getObjectAcl_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'requestPayer', 'getObjectAcl_requestPayer' - Undocumented member.
--
-- 'versionId', 'getObjectAcl_versionId' - VersionId used to reference a specific version of the object.
--
-- 'bucket', 'getObjectAcl_bucket' - The bucket name that contains the object for which to get the ACL
-- information.
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
-- 'key', 'getObjectAcl_key' - The key of the object for which to get the ACL information.
newGetObjectAcl ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectAcl
newGetObjectAcl pBucket_ pKey_ =
  GetObjectAcl'
    { expectedBucketOwner =
        Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getObjectAcl_expectedBucketOwner :: Lens.Lens' GetObjectAcl (Prelude.Maybe Prelude.Text)
getObjectAcl_expectedBucketOwner = Lens.lens (\GetObjectAcl' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObjectAcl' {} a -> s {expectedBucketOwner = a} :: GetObjectAcl)

-- | Undocumented member.
getObjectAcl_requestPayer :: Lens.Lens' GetObjectAcl (Prelude.Maybe RequestPayer)
getObjectAcl_requestPayer = Lens.lens (\GetObjectAcl' {requestPayer} -> requestPayer) (\s@GetObjectAcl' {} a -> s {requestPayer = a} :: GetObjectAcl)

-- | VersionId used to reference a specific version of the object.
getObjectAcl_versionId :: Lens.Lens' GetObjectAcl (Prelude.Maybe ObjectVersionId)
getObjectAcl_versionId = Lens.lens (\GetObjectAcl' {versionId} -> versionId) (\s@GetObjectAcl' {} a -> s {versionId = a} :: GetObjectAcl)

-- | The bucket name that contains the object for which to get the ACL
-- information.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
getObjectAcl_bucket :: Lens.Lens' GetObjectAcl BucketName
getObjectAcl_bucket = Lens.lens (\GetObjectAcl' {bucket} -> bucket) (\s@GetObjectAcl' {} a -> s {bucket = a} :: GetObjectAcl)

-- | The key of the object for which to get the ACL information.
getObjectAcl_key :: Lens.Lens' GetObjectAcl ObjectKey
getObjectAcl_key = Lens.lens (\GetObjectAcl' {key} -> key) (\s@GetObjectAcl' {} a -> s {key = a} :: GetObjectAcl)

instance Core.AWSRequest GetObjectAcl where
  type AWSResponse GetObjectAcl = GetObjectAclResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetObjectAclResponse'
            Prelude.<$> ( x Data..@? "AccessControlList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "Grant")
                        )
            Prelude.<*> (x Data..@? "Owner")
            Prelude.<*> (h Data..#? "x-amz-request-charged")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetObjectAcl where
  hashWithSalt _salt GetObjectAcl' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData GetObjectAcl where
  rnf GetObjectAcl' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Data.ToHeaders GetObjectAcl where
  toHeaders GetObjectAcl' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-request-payer" Data.=# requestPayer
      ]

instance Data.ToPath GetObjectAcl where
  toPath GetObjectAcl' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery GetObjectAcl where
  toQuery GetObjectAcl' {..} =
    Prelude.mconcat
      ["versionId" Data.=: versionId, "acl"]

-- | /See:/ 'newGetObjectAclResponse' smart constructor.
data GetObjectAclResponse = GetObjectAclResponse'
  { -- | A list of grants.
    grants :: Prelude.Maybe [Grant],
    -- | Container for the bucket owner\'s display name and ID.
    owner :: Prelude.Maybe Owner,
    requestCharged :: Prelude.Maybe RequestCharged,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectAclResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grants', 'getObjectAclResponse_grants' - A list of grants.
--
-- 'owner', 'getObjectAclResponse_owner' - Container for the bucket owner\'s display name and ID.
--
-- 'requestCharged', 'getObjectAclResponse_requestCharged' - Undocumented member.
--
-- 'httpStatus', 'getObjectAclResponse_httpStatus' - The response's http status code.
newGetObjectAclResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetObjectAclResponse
newGetObjectAclResponse pHttpStatus_ =
  GetObjectAclResponse'
    { grants = Prelude.Nothing,
      owner = Prelude.Nothing,
      requestCharged = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of grants.
getObjectAclResponse_grants :: Lens.Lens' GetObjectAclResponse (Prelude.Maybe [Grant])
getObjectAclResponse_grants = Lens.lens (\GetObjectAclResponse' {grants} -> grants) (\s@GetObjectAclResponse' {} a -> s {grants = a} :: GetObjectAclResponse) Prelude.. Lens.mapping Lens.coerced

-- | Container for the bucket owner\'s display name and ID.
getObjectAclResponse_owner :: Lens.Lens' GetObjectAclResponse (Prelude.Maybe Owner)
getObjectAclResponse_owner = Lens.lens (\GetObjectAclResponse' {owner} -> owner) (\s@GetObjectAclResponse' {} a -> s {owner = a} :: GetObjectAclResponse)

-- | Undocumented member.
getObjectAclResponse_requestCharged :: Lens.Lens' GetObjectAclResponse (Prelude.Maybe RequestCharged)
getObjectAclResponse_requestCharged = Lens.lens (\GetObjectAclResponse' {requestCharged} -> requestCharged) (\s@GetObjectAclResponse' {} a -> s {requestCharged = a} :: GetObjectAclResponse)

-- | The response's http status code.
getObjectAclResponse_httpStatus :: Lens.Lens' GetObjectAclResponse Prelude.Int
getObjectAclResponse_httpStatus = Lens.lens (\GetObjectAclResponse' {httpStatus} -> httpStatus) (\s@GetObjectAclResponse' {} a -> s {httpStatus = a} :: GetObjectAclResponse)

instance Prelude.NFData GetObjectAclResponse where
  rnf GetObjectAclResponse' {..} =
    Prelude.rnf grants
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf httpStatus
