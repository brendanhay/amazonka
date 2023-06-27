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
-- Module      : Amazonka.S3.DeleteBucketReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the replication configuration from the bucket.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutReplicationConfiguration@ action. The bucket owner has these
-- permissions by default and can grant it to others. For more information
-- about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- It can take a while for the deletion of a replication configuration to
-- fully propagate.
--
-- For information about replication configuration, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication>
-- in the /Amazon S3 User Guide/.
--
-- The following operations are related to @DeleteBucketReplication@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketReplication.html PutBucketReplication>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketReplication.html GetBucketReplication>
module Amazonka.S3.DeleteBucketReplication
  ( -- * Creating a Request
    DeleteBucketReplication (..),
    newDeleteBucketReplication,

    -- * Request Lenses
    deleteBucketReplication_expectedBucketOwner,
    deleteBucketReplication_bucket,

    -- * Destructuring the Response
    DeleteBucketReplicationResponse (..),
    newDeleteBucketReplicationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeleteBucketReplication' smart constructor.
data DeleteBucketReplication = DeleteBucketReplication'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketReplication_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'deleteBucketReplication_bucket' - The bucket name.
newDeleteBucketReplication ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketReplication
newDeleteBucketReplication pBucket_ =
  DeleteBucketReplication'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
deleteBucketReplication_expectedBucketOwner :: Lens.Lens' DeleteBucketReplication (Prelude.Maybe Prelude.Text)
deleteBucketReplication_expectedBucketOwner = Lens.lens (\DeleteBucketReplication' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketReplication' {} a -> s {expectedBucketOwner = a} :: DeleteBucketReplication)

-- | The bucket name.
deleteBucketReplication_bucket :: Lens.Lens' DeleteBucketReplication BucketName
deleteBucketReplication_bucket = Lens.lens (\DeleteBucketReplication' {bucket} -> bucket) (\s@DeleteBucketReplication' {} a -> s {bucket = a} :: DeleteBucketReplication)

instance Core.AWSRequest DeleteBucketReplication where
  type
    AWSResponse DeleteBucketReplication =
      DeleteBucketReplicationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteBucketReplicationResponse'

instance Prelude.Hashable DeleteBucketReplication where
  hashWithSalt _salt DeleteBucketReplication' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData DeleteBucketReplication where
  rnf DeleteBucketReplication' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders DeleteBucketReplication where
  toHeaders DeleteBucketReplication' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath DeleteBucketReplication where
  toPath DeleteBucketReplication' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery DeleteBucketReplication where
  toQuery =
    Prelude.const (Prelude.mconcat ["replication"])

-- | /See:/ 'newDeleteBucketReplicationResponse' smart constructor.
data DeleteBucketReplicationResponse = DeleteBucketReplicationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketReplicationResponse ::
  DeleteBucketReplicationResponse
newDeleteBucketReplicationResponse =
  DeleteBucketReplicationResponse'

instance
  Prelude.NFData
    DeleteBucketReplicationResponse
  where
  rnf _ = ()
