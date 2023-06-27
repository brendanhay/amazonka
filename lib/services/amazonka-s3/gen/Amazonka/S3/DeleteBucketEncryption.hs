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
-- Module      : Amazonka.S3.DeleteBucketEncryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the DELETE action resets the default encryption
-- for the bucket as server-side encryption with Amazon S3 managed keys
-- (SSE-S3). For information about the bucket default encryption feature,
-- see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Bucket Default Encryption>
-- in the /Amazon S3 User Guide/.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutEncryptionConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to your Amazon S3 Resources>
-- in the /Amazon S3 User Guide/.
--
-- The following operations are related to @DeleteBucketEncryption@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketEncryption.html PutBucketEncryption>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketEncryption.html GetBucketEncryption>
module Amazonka.S3.DeleteBucketEncryption
  ( -- * Creating a Request
    DeleteBucketEncryption (..),
    newDeleteBucketEncryption,

    -- * Request Lenses
    deleteBucketEncryption_expectedBucketOwner,
    deleteBucketEncryption_bucket,

    -- * Destructuring the Response
    DeleteBucketEncryptionResponse (..),
    newDeleteBucketEncryptionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeleteBucketEncryption' smart constructor.
data DeleteBucketEncryption = DeleteBucketEncryption'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket containing the server-side encryption
    -- configuration to delete.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketEncryption_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'deleteBucketEncryption_bucket' - The name of the bucket containing the server-side encryption
-- configuration to delete.
newDeleteBucketEncryption ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketEncryption
newDeleteBucketEncryption pBucket_ =
  DeleteBucketEncryption'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
deleteBucketEncryption_expectedBucketOwner :: Lens.Lens' DeleteBucketEncryption (Prelude.Maybe Prelude.Text)
deleteBucketEncryption_expectedBucketOwner = Lens.lens (\DeleteBucketEncryption' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketEncryption' {} a -> s {expectedBucketOwner = a} :: DeleteBucketEncryption)

-- | The name of the bucket containing the server-side encryption
-- configuration to delete.
deleteBucketEncryption_bucket :: Lens.Lens' DeleteBucketEncryption BucketName
deleteBucketEncryption_bucket = Lens.lens (\DeleteBucketEncryption' {bucket} -> bucket) (\s@DeleteBucketEncryption' {} a -> s {bucket = a} :: DeleteBucketEncryption)

instance Core.AWSRequest DeleteBucketEncryption where
  type
    AWSResponse DeleteBucketEncryption =
      DeleteBucketEncryptionResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteBucketEncryptionResponse'

instance Prelude.Hashable DeleteBucketEncryption where
  hashWithSalt _salt DeleteBucketEncryption' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData DeleteBucketEncryption where
  rnf DeleteBucketEncryption' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders DeleteBucketEncryption where
  toHeaders DeleteBucketEncryption' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath DeleteBucketEncryption where
  toPath DeleteBucketEncryption' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery DeleteBucketEncryption where
  toQuery =
    Prelude.const (Prelude.mconcat ["encryption"])

-- | /See:/ 'newDeleteBucketEncryptionResponse' smart constructor.
data DeleteBucketEncryptionResponse = DeleteBucketEncryptionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketEncryptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketEncryptionResponse ::
  DeleteBucketEncryptionResponse
newDeleteBucketEncryptionResponse =
  DeleteBucketEncryptionResponse'

instance
  Prelude.NFData
    DeleteBucketEncryptionResponse
  where
  rnf _ = ()
