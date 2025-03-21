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
-- Module      : Amazonka.S3.DeleteBucketPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the DELETE action uses the policy subresource to
-- delete the policy of a specified bucket. If you are using an identity
-- other than the root user of the Amazon Web Services account that owns
-- the bucket, the calling identity must have the @DeleteBucketPolicy@
-- permissions on the specified bucket and belong to the bucket owner\'s
-- account to use this operation.
--
-- If you don\'t have @DeleteBucketPolicy@ permissions, Amazon S3 returns a
-- @403 Access Denied@ error. If you have the correct permissions, but
-- you\'re not using an identity that belongs to the bucket owner\'s
-- account, Amazon S3 returns a @405 Method Not Allowed@ error.
--
-- As a security precaution, the root user of the Amazon Web Services
-- account that owns a bucket can always use this operation, even if the
-- policy explicitly denies the root user the ability to perform this
-- action.
--
-- For more information about bucket policies, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies and UserPolicies>.
--
-- The following operations are related to @DeleteBucketPolicy@
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Amazonka.S3.DeleteBucketPolicy
  ( -- * Creating a Request
    DeleteBucketPolicy (..),
    newDeleteBucketPolicy,

    -- * Request Lenses
    deleteBucketPolicy_expectedBucketOwner,
    deleteBucketPolicy_bucket,

    -- * Destructuring the Response
    DeleteBucketPolicyResponse (..),
    newDeleteBucketPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeleteBucketPolicy' smart constructor.
data DeleteBucketPolicy = DeleteBucketPolicy'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketPolicy_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'deleteBucketPolicy_bucket' - The bucket name.
newDeleteBucketPolicy ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketPolicy
newDeleteBucketPolicy pBucket_ =
  DeleteBucketPolicy'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
deleteBucketPolicy_expectedBucketOwner :: Lens.Lens' DeleteBucketPolicy (Prelude.Maybe Prelude.Text)
deleteBucketPolicy_expectedBucketOwner = Lens.lens (\DeleteBucketPolicy' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketPolicy' {} a -> s {expectedBucketOwner = a} :: DeleteBucketPolicy)

-- | The bucket name.
deleteBucketPolicy_bucket :: Lens.Lens' DeleteBucketPolicy BucketName
deleteBucketPolicy_bucket = Lens.lens (\DeleteBucketPolicy' {bucket} -> bucket) (\s@DeleteBucketPolicy' {} a -> s {bucket = a} :: DeleteBucketPolicy)

instance Core.AWSRequest DeleteBucketPolicy where
  type
    AWSResponse DeleteBucketPolicy =
      DeleteBucketPolicyResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteBucketPolicyResponse'

instance Prelude.Hashable DeleteBucketPolicy where
  hashWithSalt _salt DeleteBucketPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData DeleteBucketPolicy where
  rnf DeleteBucketPolicy' {..} =
    Prelude.rnf expectedBucketOwner `Prelude.seq`
      Prelude.rnf bucket

instance Data.ToHeaders DeleteBucketPolicy where
  toHeaders DeleteBucketPolicy' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath DeleteBucketPolicy where
  toPath DeleteBucketPolicy' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery DeleteBucketPolicy where
  toQuery = Prelude.const (Prelude.mconcat ["policy"])

-- | /See:/ 'newDeleteBucketPolicyResponse' smart constructor.
data DeleteBucketPolicyResponse = DeleteBucketPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketPolicyResponse ::
  DeleteBucketPolicyResponse
newDeleteBucketPolicyResponse =
  DeleteBucketPolicyResponse'

instance Prelude.NFData DeleteBucketPolicyResponse where
  rnf _ = ()
