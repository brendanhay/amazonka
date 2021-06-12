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
-- Module      : Network.AWS.S3.GetBucketReplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the replication configuration of a bucket.
--
-- It can take a while to propagate the put or delete a replication
-- configuration to all Amazon S3 systems. Therefore, a get request soon
-- after put or delete can return a wrong result.
--
-- For information about replication configuration, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- This operation requires permissions for the
-- @s3:GetReplicationConfiguration@ action. For more information about
-- permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies and User Policies>.
--
-- If you include the @Filter@ element in a replication configuration, you
-- must also include the @DeleteMarkerReplication@ and @Priority@ elements.
-- The response also returns those elements.
--
-- For information about @GetBucketReplication@ errors, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ReplicationErrorCodeList List of replication-related error codes>
--
-- The following operations are related to @GetBucketReplication@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketReplication.html PutBucketReplication>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketReplication.html DeleteBucketReplication>
module Network.AWS.S3.GetBucketReplication
  ( -- * Creating a Request
    GetBucketReplication (..),
    newGetBucketReplication,

    -- * Request Lenses
    getBucketReplication_expectedBucketOwner,
    getBucketReplication_bucket,

    -- * Destructuring the Response
    GetBucketReplicationResponse (..),
    newGetBucketReplicationResponse,

    -- * Response Lenses
    getBucketReplicationResponse_replicationConfiguration,
    getBucketReplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketReplication' smart constructor.
data GetBucketReplication = GetBucketReplication'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The bucket name for which to get the replication information.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketReplication_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketReplication_bucket' - The bucket name for which to get the replication information.
newGetBucketReplication ::
  -- | 'bucket'
  BucketName ->
  GetBucketReplication
newGetBucketReplication pBucket_ =
  GetBucketReplication'
    { expectedBucketOwner =
        Core.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketReplication_expectedBucketOwner :: Lens.Lens' GetBucketReplication (Core.Maybe Core.Text)
getBucketReplication_expectedBucketOwner = Lens.lens (\GetBucketReplication' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketReplication' {} a -> s {expectedBucketOwner = a} :: GetBucketReplication)

-- | The bucket name for which to get the replication information.
getBucketReplication_bucket :: Lens.Lens' GetBucketReplication BucketName
getBucketReplication_bucket = Lens.lens (\GetBucketReplication' {bucket} -> bucket) (\s@GetBucketReplication' {} a -> s {bucket = a} :: GetBucketReplication)

instance Core.AWSRequest GetBucketReplication where
  type
    AWSResponse GetBucketReplication =
      GetBucketReplicationResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketReplicationResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBucketReplication

instance Core.NFData GetBucketReplication

instance Core.ToHeaders GetBucketReplication where
  toHeaders GetBucketReplication' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath GetBucketReplication where
  toPath GetBucketReplication' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery GetBucketReplication where
  toQuery = Core.const (Core.mconcat ["replication"])

-- | /See:/ 'newGetBucketReplicationResponse' smart constructor.
data GetBucketReplicationResponse = GetBucketReplicationResponse'
  { replicationConfiguration :: Core.Maybe ReplicationConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationConfiguration', 'getBucketReplicationResponse_replicationConfiguration' - Undocumented member.
--
-- 'httpStatus', 'getBucketReplicationResponse_httpStatus' - The response's http status code.
newGetBucketReplicationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBucketReplicationResponse
newGetBucketReplicationResponse pHttpStatus_ =
  GetBucketReplicationResponse'
    { replicationConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getBucketReplicationResponse_replicationConfiguration :: Lens.Lens' GetBucketReplicationResponse (Core.Maybe ReplicationConfiguration)
getBucketReplicationResponse_replicationConfiguration = Lens.lens (\GetBucketReplicationResponse' {replicationConfiguration} -> replicationConfiguration) (\s@GetBucketReplicationResponse' {} a -> s {replicationConfiguration = a} :: GetBucketReplicationResponse)

-- | The response's http status code.
getBucketReplicationResponse_httpStatus :: Lens.Lens' GetBucketReplicationResponse Core.Int
getBucketReplicationResponse_httpStatus = Lens.lens (\GetBucketReplicationResponse' {httpStatus} -> httpStatus) (\s@GetBucketReplicationResponse' {} a -> s {httpStatus = a} :: GetBucketReplicationResponse)

instance Core.NFData GetBucketReplicationResponse
