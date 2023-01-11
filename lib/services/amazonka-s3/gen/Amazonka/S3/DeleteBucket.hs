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
-- Module      : Amazonka.S3.DeleteBucket
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the S3 bucket. All objects (including all object versions and
-- delete markers) in the bucket must be deleted before the bucket itself
-- can be deleted.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Amazonka.S3.DeleteBucket
  ( -- * Creating a Request
    DeleteBucket (..),
    newDeleteBucket,

    -- * Request Lenses
    deleteBucket_expectedBucketOwner,
    deleteBucket_bucket,

    -- * Destructuring the Response
    DeleteBucketResponse (..),
    newDeleteBucketResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeleteBucket' smart constructor.
data DeleteBucket = DeleteBucket'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Specifies the bucket being deleted.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucket_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'deleteBucket_bucket' - Specifies the bucket being deleted.
newDeleteBucket ::
  -- | 'bucket'
  BucketName ->
  DeleteBucket
newDeleteBucket pBucket_ =
  DeleteBucket'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
deleteBucket_expectedBucketOwner :: Lens.Lens' DeleteBucket (Prelude.Maybe Prelude.Text)
deleteBucket_expectedBucketOwner = Lens.lens (\DeleteBucket' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucket' {} a -> s {expectedBucketOwner = a} :: DeleteBucket)

-- | Specifies the bucket being deleted.
deleteBucket_bucket :: Lens.Lens' DeleteBucket BucketName
deleteBucket_bucket = Lens.lens (\DeleteBucket' {bucket} -> bucket) (\s@DeleteBucket' {} a -> s {bucket = a} :: DeleteBucket)

instance Core.AWSRequest DeleteBucket where
  type AWSResponse DeleteBucket = DeleteBucketResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteBucketResponse'

instance Prelude.Hashable DeleteBucket where
  hashWithSalt _salt DeleteBucket' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData DeleteBucket where
  rnf DeleteBucket' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders DeleteBucket where
  toHeaders DeleteBucket' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath DeleteBucket where
  toPath DeleteBucket' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery DeleteBucket where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBucketResponse' smart constructor.
data DeleteBucketResponse = DeleteBucketResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketResponse ::
  DeleteBucketResponse
newDeleteBucketResponse = DeleteBucketResponse'

instance Prelude.NFData DeleteBucketResponse where
  rnf _ = ()
