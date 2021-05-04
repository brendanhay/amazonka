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

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateSnapshot@ operation.
--
-- /See:/ 'newCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | The identifier of an existing replication group. The snapshot is created
    -- from this replication group.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of an existing cluster. The snapshot is created from this
    -- cluster.
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS key used to encrypt the snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A name for the snapshot being created.
    snapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateSnapshot
newCreateSnapshot pSnapshotName_ =
  CreateSnapshot'
    { replicationGroupId =
        Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      snapshotName = pSnapshotName_
    }

-- | The identifier of an existing replication group. The snapshot is created
-- from this replication group.
createSnapshot_replicationGroupId :: Lens.Lens' CreateSnapshot (Prelude.Maybe Prelude.Text)
createSnapshot_replicationGroupId = Lens.lens (\CreateSnapshot' {replicationGroupId} -> replicationGroupId) (\s@CreateSnapshot' {} a -> s {replicationGroupId = a} :: CreateSnapshot)

-- | The identifier of an existing cluster. The snapshot is created from this
-- cluster.
createSnapshot_cacheClusterId :: Lens.Lens' CreateSnapshot (Prelude.Maybe Prelude.Text)
createSnapshot_cacheClusterId = Lens.lens (\CreateSnapshot' {cacheClusterId} -> cacheClusterId) (\s@CreateSnapshot' {} a -> s {cacheClusterId = a} :: CreateSnapshot)

-- | The ID of the KMS key used to encrypt the snapshot.
createSnapshot_kmsKeyId :: Lens.Lens' CreateSnapshot (Prelude.Maybe Prelude.Text)
createSnapshot_kmsKeyId = Lens.lens (\CreateSnapshot' {kmsKeyId} -> kmsKeyId) (\s@CreateSnapshot' {} a -> s {kmsKeyId = a} :: CreateSnapshot)

-- | A name for the snapshot being created.
createSnapshot_snapshotName :: Lens.Lens' CreateSnapshot Prelude.Text
createSnapshot_snapshotName = Lens.lens (\CreateSnapshot' {snapshotName} -> snapshotName) (\s@CreateSnapshot' {} a -> s {snapshotName = a} :: CreateSnapshot)

instance Prelude.AWSRequest CreateSnapshot where
  type Rs CreateSnapshot = CreateSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateSnapshotResult"
      ( \s h x ->
          CreateSnapshotResponse'
            Prelude.<$> (x Prelude..@? "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSnapshot

instance Prelude.NFData CreateSnapshot

instance Prelude.ToHeaders CreateSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateSnapshot where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateSnapshot where
  toQuery CreateSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateSnapshot" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "ReplicationGroupId" Prelude.=: replicationGroupId,
        "CacheClusterId" Prelude.=: cacheClusterId,
        "KmsKeyId" Prelude.=: kmsKeyId,
        "SnapshotName" Prelude.=: snapshotName
      ]

-- | /See:/ 'newCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateSnapshotResponse
newCreateSnapshotResponse pHttpStatus_ =
  CreateSnapshotResponse'
    { snapshot = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createSnapshotResponse_snapshot :: Lens.Lens' CreateSnapshotResponse (Prelude.Maybe Snapshot)
createSnapshotResponse_snapshot = Lens.lens (\CreateSnapshotResponse' {snapshot} -> snapshot) (\s@CreateSnapshotResponse' {} a -> s {snapshot = a} :: CreateSnapshotResponse)

-- | The response's http status code.
createSnapshotResponse_httpStatus :: Lens.Lens' CreateSnapshotResponse Prelude.Int
createSnapshotResponse_httpStatus = Lens.lens (\CreateSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotResponse' {} a -> s {httpStatus = a} :: CreateSnapshotResponse)

instance Prelude.NFData CreateSnapshotResponse
