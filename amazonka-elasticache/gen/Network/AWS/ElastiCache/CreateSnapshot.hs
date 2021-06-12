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
-- Module      : Network.AWS.ElastiCache.CreateSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an entire cluster or replication group at a specific
-- moment in time.
--
-- This operation is valid for Redis only.
module Network.AWS.ElastiCache.CreateSnapshot
  ( -- * Creating a Request
    CreateSnapshot (..),
    newCreateSnapshot,

    -- * Request Lenses
    createSnapshot_replicationGroupId,
    createSnapshot_cacheClusterId,
    createSnapshot_kmsKeyId,
    createSnapshot_snapshotName,

    -- * Destructuring the Response
    CreateSnapshotResponse (..),
    newCreateSnapshotResponse,

    -- * Response Lenses
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateSnapshot@ operation.
--
-- /See:/ 'newCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | The identifier of an existing replication group. The snapshot is created
    -- from this replication group.
    replicationGroupId :: Core.Maybe Core.Text,
    -- | The identifier of an existing cluster. The snapshot is created from this
    -- cluster.
    cacheClusterId :: Core.Maybe Core.Text,
    -- | The ID of the KMS key used to encrypt the snapshot.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | A name for the snapshot being created.
    snapshotName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroupId', 'createSnapshot_replicationGroupId' - The identifier of an existing replication group. The snapshot is created
-- from this replication group.
--
-- 'cacheClusterId', 'createSnapshot_cacheClusterId' - The identifier of an existing cluster. The snapshot is created from this
-- cluster.
--
-- 'kmsKeyId', 'createSnapshot_kmsKeyId' - The ID of the KMS key used to encrypt the snapshot.
--
-- 'snapshotName', 'createSnapshot_snapshotName' - A name for the snapshot being created.
newCreateSnapshot ::
  -- | 'snapshotName'
  Core.Text ->
  CreateSnapshot
newCreateSnapshot pSnapshotName_ =
  CreateSnapshot'
    { replicationGroupId = Core.Nothing,
      cacheClusterId = Core.Nothing,
      kmsKeyId = Core.Nothing,
      snapshotName = pSnapshotName_
    }

-- | The identifier of an existing replication group. The snapshot is created
-- from this replication group.
createSnapshot_replicationGroupId :: Lens.Lens' CreateSnapshot (Core.Maybe Core.Text)
createSnapshot_replicationGroupId = Lens.lens (\CreateSnapshot' {replicationGroupId} -> replicationGroupId) (\s@CreateSnapshot' {} a -> s {replicationGroupId = a} :: CreateSnapshot)

-- | The identifier of an existing cluster. The snapshot is created from this
-- cluster.
createSnapshot_cacheClusterId :: Lens.Lens' CreateSnapshot (Core.Maybe Core.Text)
createSnapshot_cacheClusterId = Lens.lens (\CreateSnapshot' {cacheClusterId} -> cacheClusterId) (\s@CreateSnapshot' {} a -> s {cacheClusterId = a} :: CreateSnapshot)

-- | The ID of the KMS key used to encrypt the snapshot.
createSnapshot_kmsKeyId :: Lens.Lens' CreateSnapshot (Core.Maybe Core.Text)
createSnapshot_kmsKeyId = Lens.lens (\CreateSnapshot' {kmsKeyId} -> kmsKeyId) (\s@CreateSnapshot' {} a -> s {kmsKeyId = a} :: CreateSnapshot)

-- | A name for the snapshot being created.
createSnapshot_snapshotName :: Lens.Lens' CreateSnapshot Core.Text
createSnapshot_snapshotName = Lens.lens (\CreateSnapshot' {snapshotName} -> snapshotName) (\s@CreateSnapshot' {} a -> s {snapshotName = a} :: CreateSnapshot)

instance Core.AWSRequest CreateSnapshot where
  type
    AWSResponse CreateSnapshot =
      CreateSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateSnapshotResult"
      ( \s h x ->
          CreateSnapshotResponse'
            Core.<$> (x Core..@? "Snapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSnapshot

instance Core.NFData CreateSnapshot

instance Core.ToHeaders CreateSnapshot where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery CreateSnapshot where
  toQuery CreateSnapshot' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateSnapshot" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "ReplicationGroupId" Core.=: replicationGroupId,
        "CacheClusterId" Core.=: cacheClusterId,
        "KmsKeyId" Core.=: kmsKeyId,
        "SnapshotName" Core.=: snapshotName
      ]

-- | /See:/ 'newCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { snapshot :: Core.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'createSnapshotResponse_snapshot' - Undocumented member.
--
-- 'httpStatus', 'createSnapshotResponse_httpStatus' - The response's http status code.
newCreateSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSnapshotResponse
newCreateSnapshotResponse pHttpStatus_ =
  CreateSnapshotResponse'
    { snapshot = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createSnapshotResponse_snapshot :: Lens.Lens' CreateSnapshotResponse (Core.Maybe Snapshot)
createSnapshotResponse_snapshot = Lens.lens (\CreateSnapshotResponse' {snapshot} -> snapshot) (\s@CreateSnapshotResponse' {} a -> s {snapshot = a} :: CreateSnapshotResponse)

-- | The response's http status code.
createSnapshotResponse_httpStatus :: Lens.Lens' CreateSnapshotResponse Core.Int
createSnapshotResponse_httpStatus = Lens.lens (\CreateSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotResponse' {} a -> s {httpStatus = a} :: CreateSnapshotResponse)

instance Core.NFData CreateSnapshotResponse
