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
-- Module      : Network.AWS.S3.GetBucketAcl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @GET@ operation uses the @acl@ subresource to
-- return the access control list (ACL) of a bucket. To use @GET@ to return
-- the ACL of the bucket, you must have @READ_ACP@ access to the bucket. If
-- @READ_ACP@ permission is granted to the anonymous user, you can return
-- the ACL of the bucket without using an authorization header.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjects.html ListObjects>
module Network.AWS.S3.GetBucketAcl
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
    getBucketAclResponse_owner,
    getBucketAclResponse_grants,
    getBucketAclResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketAcl' smart constructor.
data GetBucketAcl = GetBucketAcl'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Specifies the S3 bucket whose ACL is being requested.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBucketAcl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketAcl_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
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

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketAcl_expectedBucketOwner :: Lens.Lens' GetBucketAcl (Prelude.Maybe Prelude.Text)
getBucketAcl_expectedBucketOwner = Lens.lens (\GetBucketAcl' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketAcl' {} a -> s {expectedBucketOwner = a} :: GetBucketAcl)

-- | Specifies the S3 bucket whose ACL is being requested.
getBucketAcl_bucket :: Lens.Lens' GetBucketAcl BucketName
getBucketAcl_bucket = Lens.lens (\GetBucketAcl' {bucket} -> bucket) (\s@GetBucketAcl' {} a -> s {bucket = a} :: GetBucketAcl)

instance Prelude.AWSRequest GetBucketAcl where
  type Rs GetBucketAcl = GetBucketAclResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketAclResponse'
            Prelude.<$> (x Prelude..@? "Owner")
            Prelude.<*> ( x Prelude..@? "AccessControlList"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "Grant")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketAcl

instance Prelude.NFData GetBucketAcl

instance Prelude.ToHeaders GetBucketAcl where
  toHeaders GetBucketAcl' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance Prelude.ToPath GetBucketAcl where
  toPath GetBucketAcl' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery GetBucketAcl where
  toQuery = Prelude.const (Prelude.mconcat ["acl"])

-- | /See:/ 'newGetBucketAclResponse' smart constructor.
data GetBucketAclResponse = GetBucketAclResponse'
  { -- | Container for the bucket owner\'s display name and ID.
    owner :: Prelude.Maybe Owner,
    -- | A list of grants.
    grants :: Prelude.Maybe [Grant],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBucketAclResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'owner', 'getBucketAclResponse_owner' - Container for the bucket owner\'s display name and ID.
--
-- 'grants', 'getBucketAclResponse_grants' - A list of grants.
--
-- 'httpStatus', 'getBucketAclResponse_httpStatus' - The response's http status code.
newGetBucketAclResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketAclResponse
newGetBucketAclResponse pHttpStatus_ =
  GetBucketAclResponse'
    { owner = Prelude.Nothing,
      grants = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Container for the bucket owner\'s display name and ID.
getBucketAclResponse_owner :: Lens.Lens' GetBucketAclResponse (Prelude.Maybe Owner)
getBucketAclResponse_owner = Lens.lens (\GetBucketAclResponse' {owner} -> owner) (\s@GetBucketAclResponse' {} a -> s {owner = a} :: GetBucketAclResponse)

-- | A list of grants.
getBucketAclResponse_grants :: Lens.Lens' GetBucketAclResponse (Prelude.Maybe [Grant])
getBucketAclResponse_grants = Lens.lens (\GetBucketAclResponse' {grants} -> grants) (\s@GetBucketAclResponse' {} a -> s {grants = a} :: GetBucketAclResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getBucketAclResponse_httpStatus :: Lens.Lens' GetBucketAclResponse Prelude.Int
getBucketAclResponse_httpStatus = Lens.lens (\GetBucketAclResponse' {httpStatus} -> httpStatus) (\s@GetBucketAclResponse' {} a -> s {httpStatus = a} :: GetBucketAclResponse)

instance Prelude.NFData GetBucketAclResponse
