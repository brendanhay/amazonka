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
-- Module      : Amazonka.Lightsail.DeleteBucket
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Amazon Lightsail bucket.
--
-- When you delete your bucket, the bucket name is released and can be
-- reused for a new bucket in your account or another Amazon Web Services
-- account.
module Amazonka.Lightsail.DeleteBucket
  ( -- * Creating a Request
    DeleteBucket (..),
    newDeleteBucket,

    -- * Request Lenses
    deleteBucket_forceDelete,
    deleteBucket_bucketName,

    -- * Destructuring the Response
    DeleteBucketResponse (..),
    newDeleteBucketResponse,

    -- * Response Lenses
    deleteBucketResponse_operations,
    deleteBucketResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBucket' smart constructor.
data DeleteBucket = DeleteBucket'
  { -- | A Boolean value that indicates whether to force delete the bucket.
    --
    -- You must force delete the bucket if it has one of the following
    -- conditions:
    --
    -- -   The bucket is the origin of a distribution.
    --
    -- -   The bucket has instances that were granted access to it using the
    --     <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_SetResourceAccessForBucket.html SetResourceAccessForBucket>
    --     action.
    --
    -- -   The bucket has objects.
    --
    -- -   The bucket has access keys.
    --
    -- Force deleting a bucket might impact other resources that rely on the
    -- bucket, such as instances, distributions, or software that use the
    -- issued access keys.
    forceDelete :: Prelude.Maybe Prelude.Bool,
    -- | The name of the bucket to delete.
    --
    -- Use the
    -- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetBuckets.html GetBuckets>
    -- action to get a list of bucket names that you can specify.
    bucketName :: Prelude.Text
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
-- 'forceDelete', 'deleteBucket_forceDelete' - A Boolean value that indicates whether to force delete the bucket.
--
-- You must force delete the bucket if it has one of the following
-- conditions:
--
-- -   The bucket is the origin of a distribution.
--
-- -   The bucket has instances that were granted access to it using the
--     <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_SetResourceAccessForBucket.html SetResourceAccessForBucket>
--     action.
--
-- -   The bucket has objects.
--
-- -   The bucket has access keys.
--
-- Force deleting a bucket might impact other resources that rely on the
-- bucket, such as instances, distributions, or software that use the
-- issued access keys.
--
-- 'bucketName', 'deleteBucket_bucketName' - The name of the bucket to delete.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetBuckets.html GetBuckets>
-- action to get a list of bucket names that you can specify.
newDeleteBucket ::
  -- | 'bucketName'
  Prelude.Text ->
  DeleteBucket
newDeleteBucket pBucketName_ =
  DeleteBucket'
    { forceDelete = Prelude.Nothing,
      bucketName = pBucketName_
    }

-- | A Boolean value that indicates whether to force delete the bucket.
--
-- You must force delete the bucket if it has one of the following
-- conditions:
--
-- -   The bucket is the origin of a distribution.
--
-- -   The bucket has instances that were granted access to it using the
--     <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_SetResourceAccessForBucket.html SetResourceAccessForBucket>
--     action.
--
-- -   The bucket has objects.
--
-- -   The bucket has access keys.
--
-- Force deleting a bucket might impact other resources that rely on the
-- bucket, such as instances, distributions, or software that use the
-- issued access keys.
deleteBucket_forceDelete :: Lens.Lens' DeleteBucket (Prelude.Maybe Prelude.Bool)
deleteBucket_forceDelete = Lens.lens (\DeleteBucket' {forceDelete} -> forceDelete) (\s@DeleteBucket' {} a -> s {forceDelete = a} :: DeleteBucket)

-- | The name of the bucket to delete.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetBuckets.html GetBuckets>
-- action to get a list of bucket names that you can specify.
deleteBucket_bucketName :: Lens.Lens' DeleteBucket Prelude.Text
deleteBucket_bucketName = Lens.lens (\DeleteBucket' {bucketName} -> bucketName) (\s@DeleteBucket' {} a -> s {bucketName = a} :: DeleteBucket)

instance Core.AWSRequest DeleteBucket where
  type AWSResponse DeleteBucket = DeleteBucketResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBucketResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBucket where
  hashWithSalt _salt DeleteBucket' {..} =
    _salt `Prelude.hashWithSalt` forceDelete
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData DeleteBucket where
  rnf DeleteBucket' {..} =
    Prelude.rnf forceDelete
      `Prelude.seq` Prelude.rnf bucketName

instance Data.ToHeaders DeleteBucket where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DeleteBucket" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBucket where
  toJSON DeleteBucket' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("forceDelete" Data..=) Prelude.<$> forceDelete,
            Prelude.Just ("bucketName" Data..= bucketName)
          ]
      )

instance Data.ToPath DeleteBucket where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBucket where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBucketResponse' smart constructor.
data DeleteBucketResponse = DeleteBucketResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteBucketResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteBucketResponse_httpStatus' - The response's http status code.
newDeleteBucketResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBucketResponse
newDeleteBucketResponse pHttpStatus_ =
  DeleteBucketResponse'
    { operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteBucketResponse_operations :: Lens.Lens' DeleteBucketResponse (Prelude.Maybe [Operation])
deleteBucketResponse_operations = Lens.lens (\DeleteBucketResponse' {operations} -> operations) (\s@DeleteBucketResponse' {} a -> s {operations = a} :: DeleteBucketResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteBucketResponse_httpStatus :: Lens.Lens' DeleteBucketResponse Prelude.Int
deleteBucketResponse_httpStatus = Lens.lens (\DeleteBucketResponse' {httpStatus} -> httpStatus) (\s@DeleteBucketResponse' {} a -> s {httpStatus = a} :: DeleteBucketResponse)

instance Prelude.NFData DeleteBucketResponse where
  rnf DeleteBucketResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
