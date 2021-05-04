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
-- Module      : Network.AWS.S3.DeleteBucketReplication
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- It can take a while for the deletion of a replication configuration to
-- fully propagate.
--
-- For information about replication configuration, see
-- <%20https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication>
-- in the /Amazon S3 Developer Guide/.
--
-- The following operations are related to @DeleteBucketReplication@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketReplication.html PutBucketReplication>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketReplication.html GetBucketReplication>
module Network.AWS.S3.DeleteBucketReplication
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteBucketReplication' smart constructor.
data DeleteBucketReplication = DeleteBucketReplication'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketReplication_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
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

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteBucketReplication_expectedBucketOwner :: Lens.Lens' DeleteBucketReplication (Prelude.Maybe Prelude.Text)
deleteBucketReplication_expectedBucketOwner = Lens.lens (\DeleteBucketReplication' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketReplication' {} a -> s {expectedBucketOwner = a} :: DeleteBucketReplication)

-- | The bucket name.
deleteBucketReplication_bucket :: Lens.Lens' DeleteBucketReplication BucketName
deleteBucketReplication_bucket = Lens.lens (\DeleteBucketReplication' {bucket} -> bucket) (\s@DeleteBucketReplication' {} a -> s {bucket = a} :: DeleteBucketReplication)

instance Prelude.AWSRequest DeleteBucketReplication where
  type
    Rs DeleteBucketReplication =
      DeleteBucketReplicationResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteBucketReplicationResponse'

instance Prelude.Hashable DeleteBucketReplication

instance Prelude.NFData DeleteBucketReplication

instance Prelude.ToHeaders DeleteBucketReplication where
  toHeaders DeleteBucketReplication' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance Prelude.ToPath DeleteBucketReplication where
  toPath DeleteBucketReplication' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery DeleteBucketReplication where
  toQuery =
    Prelude.const (Prelude.mconcat ["replication"])

-- | /See:/ 'newDeleteBucketReplicationResponse' smart constructor.
data DeleteBucketReplicationResponse = DeleteBucketReplicationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
