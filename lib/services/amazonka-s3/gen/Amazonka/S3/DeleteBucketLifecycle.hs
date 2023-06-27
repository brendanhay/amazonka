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
-- Module      : Amazonka.S3.DeleteBucketLifecycle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the lifecycle configuration from the specified bucket. Amazon S3
-- removes all the lifecycle configuration rules in the lifecycle
-- subresource associated with the bucket. Your objects never expire, and
-- Amazon S3 no longer automatically deletes any objects on the basis of
-- rules contained in the deleted lifecycle configuration.
--
-- To use this operation, you must have permission to perform the
-- @s3:PutLifecycleConfiguration@ action. By default, the bucket owner has
-- this permission and the bucket owner can grant this permission to
-- others.
--
-- There is usually some time lag before lifecycle configuration deletion
-- is fully propagated to all the Amazon S3 systems.
--
-- For more information about the object expiration, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#intro-lifecycle-rules-actions Elements to Describe Lifecycle Actions>.
--
-- Related actions include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycleConfiguration.html GetBucketLifecycleConfiguration>
module Amazonka.S3.DeleteBucketLifecycle
  ( -- * Creating a Request
    DeleteBucketLifecycle (..),
    newDeleteBucketLifecycle,

    -- * Request Lenses
    deleteBucketLifecycle_expectedBucketOwner,
    deleteBucketLifecycle_bucket,

    -- * Destructuring the Response
    DeleteBucketLifecycleResponse (..),
    newDeleteBucketLifecycleResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeleteBucketLifecycle' smart constructor.
data DeleteBucketLifecycle = DeleteBucketLifecycle'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name of the lifecycle to delete.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketLifecycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketLifecycle_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'deleteBucketLifecycle_bucket' - The bucket name of the lifecycle to delete.
newDeleteBucketLifecycle ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketLifecycle
newDeleteBucketLifecycle pBucket_ =
  DeleteBucketLifecycle'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
deleteBucketLifecycle_expectedBucketOwner :: Lens.Lens' DeleteBucketLifecycle (Prelude.Maybe Prelude.Text)
deleteBucketLifecycle_expectedBucketOwner = Lens.lens (\DeleteBucketLifecycle' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketLifecycle' {} a -> s {expectedBucketOwner = a} :: DeleteBucketLifecycle)

-- | The bucket name of the lifecycle to delete.
deleteBucketLifecycle_bucket :: Lens.Lens' DeleteBucketLifecycle BucketName
deleteBucketLifecycle_bucket = Lens.lens (\DeleteBucketLifecycle' {bucket} -> bucket) (\s@DeleteBucketLifecycle' {} a -> s {bucket = a} :: DeleteBucketLifecycle)

instance Core.AWSRequest DeleteBucketLifecycle where
  type
    AWSResponse DeleteBucketLifecycle =
      DeleteBucketLifecycleResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteBucketLifecycleResponse'

instance Prelude.Hashable DeleteBucketLifecycle where
  hashWithSalt _salt DeleteBucketLifecycle' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData DeleteBucketLifecycle where
  rnf DeleteBucketLifecycle' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders DeleteBucketLifecycle where
  toHeaders DeleteBucketLifecycle' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath DeleteBucketLifecycle where
  toPath DeleteBucketLifecycle' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery DeleteBucketLifecycle where
  toQuery =
    Prelude.const (Prelude.mconcat ["lifecycle"])

-- | /See:/ 'newDeleteBucketLifecycleResponse' smart constructor.
data DeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketLifecycleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketLifecycleResponse ::
  DeleteBucketLifecycleResponse
newDeleteBucketLifecycleResponse =
  DeleteBucketLifecycleResponse'

instance Prelude.NFData DeleteBucketLifecycleResponse where
  rnf _ = ()
