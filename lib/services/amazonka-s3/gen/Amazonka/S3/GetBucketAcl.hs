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
-- Module      : Amazonka.S3.GetBucketAcl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @GET@ action uses the @acl@ subresource to
-- return the access control list (ACL) of a bucket. To use @GET@ to return
-- the ACL of the bucket, you must have @READ_ACP@ access to the bucket. If
-- @READ_ACP@ permission is granted to the anonymous user, you can return
-- the ACL of the bucket without using an authorization header.
--
-- If your bucket uses the bucket owner enforced setting for S3 Object
-- Ownership, requests to read ACLs are still supported and return the
-- @bucket-owner-full-control@ ACL with the owner being the account that
-- created the bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html Controlling object ownership and disabling ACLs>
-- in the /Amazon S3 User Guide/.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjects.html ListObjects>
module Amazonka.S3.GetBucketAcl
  ( -- * Creating a Request
    GetBucketAcl (..),
    newGetBucketAcl,

    -- * Request Lenses
    getBucketAcl_expectedBucketOwner,
    getBucketAcl_bucket,

    -- * Destructuring the Response
    GetBucketAclResponse (..),
    newGetBucketAclResponse,

    -- * Response Lenses
    getBucketAclResponse_grants,
    getBucketAclResponse_owner,
    getBucketAclResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketAcl' smart constructor.
data GetBucketAcl = GetBucketAcl'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Specifies the S3 bucket whose ACL is being requested.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketAcl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketAcl_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getBucketAcl_bucket' - Specifies the S3 bucket whose ACL is being requested.
newGetBucketAcl ::
  -- | 'bucket'
  BucketName ->
  GetBucketAcl
newGetBucketAcl pBucket_ =
  GetBucketAcl'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getBucketAcl_expectedBucketOwner :: Lens.Lens' GetBucketAcl (Prelude.Maybe Prelude.Text)
getBucketAcl_expectedBucketOwner = Lens.lens (\GetBucketAcl' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketAcl' {} a -> s {expectedBucketOwner = a} :: GetBucketAcl)

-- | Specifies the S3 bucket whose ACL is being requested.
getBucketAcl_bucket :: Lens.Lens' GetBucketAcl BucketName
getBucketAcl_bucket = Lens.lens (\GetBucketAcl' {bucket} -> bucket) (\s@GetBucketAcl' {} a -> s {bucket = a} :: GetBucketAcl)

instance Core.AWSRequest GetBucketAcl where
  type AWSResponse GetBucketAcl = GetBucketAclResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketAclResponse'
            Prelude.<$> ( x
                            Data..@? "AccessControlList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "Grant")
                        )
            Prelude.<*> (x Data..@? "Owner")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketAcl where
  hashWithSalt _salt GetBucketAcl' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData GetBucketAcl where
  rnf GetBucketAcl' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders GetBucketAcl where
  toHeaders GetBucketAcl' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetBucketAcl where
  toPath GetBucketAcl' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetBucketAcl where
  toQuery = Prelude.const (Prelude.mconcat ["acl"])

-- | /See:/ 'newGetBucketAclResponse' smart constructor.
data GetBucketAclResponse = GetBucketAclResponse'
  { -- | A list of grants.
    grants :: Prelude.Maybe [Grant],
    -- | Container for the bucket owner\'s display name and ID.
    owner :: Prelude.Maybe Owner,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketAclResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grants', 'getBucketAclResponse_grants' - A list of grants.
--
-- 'owner', 'getBucketAclResponse_owner' - Container for the bucket owner\'s display name and ID.
--
-- 'httpStatus', 'getBucketAclResponse_httpStatus' - The response's http status code.
newGetBucketAclResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketAclResponse
newGetBucketAclResponse pHttpStatus_ =
  GetBucketAclResponse'
    { grants = Prelude.Nothing,
      owner = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of grants.
getBucketAclResponse_grants :: Lens.Lens' GetBucketAclResponse (Prelude.Maybe [Grant])
getBucketAclResponse_grants = Lens.lens (\GetBucketAclResponse' {grants} -> grants) (\s@GetBucketAclResponse' {} a -> s {grants = a} :: GetBucketAclResponse) Prelude.. Lens.mapping Lens.coerced

-- | Container for the bucket owner\'s display name and ID.
getBucketAclResponse_owner :: Lens.Lens' GetBucketAclResponse (Prelude.Maybe Owner)
getBucketAclResponse_owner = Lens.lens (\GetBucketAclResponse' {owner} -> owner) (\s@GetBucketAclResponse' {} a -> s {owner = a} :: GetBucketAclResponse)

-- | The response's http status code.
getBucketAclResponse_httpStatus :: Lens.Lens' GetBucketAclResponse Prelude.Int
getBucketAclResponse_httpStatus = Lens.lens (\GetBucketAclResponse' {httpStatus} -> httpStatus) (\s@GetBucketAclResponse' {} a -> s {httpStatus = a} :: GetBucketAclResponse)

instance Prelude.NFData GetBucketAclResponse where
  rnf GetBucketAclResponse' {..} =
    Prelude.rnf grants
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf httpStatus
