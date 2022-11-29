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
-- Module      : Amazonka.ElastiCache.DeleteReplicationGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing replication group. By default, this operation
-- deletes the entire replication group, including the primary\/primaries
-- and all of the read replicas. If the replication group has only one
-- primary, you can optionally delete only the read replicas, while
-- retaining the primary by setting @RetainPrimaryCluster=true@.
--
-- When you receive a successful response from this operation, Amazon
-- ElastiCache immediately begins deleting the selected resources; you
-- cannot cancel or revert this operation.
--
-- This operation is valid for Redis only.
module Amazonka.ElastiCache.DeleteReplicationGroup
  ( -- * Creating a Request
    DeleteReplicationGroup (..),
    newDeleteReplicationGroup,

    -- * Request Lenses
    deleteReplicationGroup_retainPrimaryCluster,
    deleteReplicationGroup_finalSnapshotIdentifier,
    deleteReplicationGroup_replicationGroupId,

    -- * Destructuring the Response
    DeleteReplicationGroupResponse (..),
    newDeleteReplicationGroupResponse,

    -- * Response Lenses
    deleteReplicationGroupResponse_replicationGroup,
    deleteReplicationGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DeleteReplicationGroup@ operation.
--
-- /See:/ 'newDeleteReplicationGroup' smart constructor.
data DeleteReplicationGroup = DeleteReplicationGroup'
  { -- | If set to @true@, all of the read replicas are deleted, but the primary
    -- node is retained.
    retainPrimaryCluster :: Prelude.Maybe Prelude.Bool,
    -- | The name of a final node group (shard) snapshot. ElastiCache creates the
    -- snapshot from the primary node in the cluster, rather than one of the
    -- replicas; this is to ensure that it captures the freshest data. After
    -- the final snapshot is taken, the replication group is immediately
    -- deleted.
    finalSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the cluster to be deleted. This parameter is not case
    -- sensitive.
    replicationGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retainPrimaryCluster', 'deleteReplicationGroup_retainPrimaryCluster' - If set to @true@, all of the read replicas are deleted, but the primary
-- node is retained.
--
-- 'finalSnapshotIdentifier', 'deleteReplicationGroup_finalSnapshotIdentifier' - The name of a final node group (shard) snapshot. ElastiCache creates the
-- snapshot from the primary node in the cluster, rather than one of the
-- replicas; this is to ensure that it captures the freshest data. After
-- the final snapshot is taken, the replication group is immediately
-- deleted.
--
-- 'replicationGroupId', 'deleteReplicationGroup_replicationGroupId' - The identifier for the cluster to be deleted. This parameter is not case
-- sensitive.
newDeleteReplicationGroup ::
  -- | 'replicationGroupId'
  Prelude.Text ->
  DeleteReplicationGroup
newDeleteReplicationGroup pReplicationGroupId_ =
  DeleteReplicationGroup'
    { retainPrimaryCluster =
        Prelude.Nothing,
      finalSnapshotIdentifier = Prelude.Nothing,
      replicationGroupId = pReplicationGroupId_
    }

-- | If set to @true@, all of the read replicas are deleted, but the primary
-- node is retained.
deleteReplicationGroup_retainPrimaryCluster :: Lens.Lens' DeleteReplicationGroup (Prelude.Maybe Prelude.Bool)
deleteReplicationGroup_retainPrimaryCluster = Lens.lens (\DeleteReplicationGroup' {retainPrimaryCluster} -> retainPrimaryCluster) (\s@DeleteReplicationGroup' {} a -> s {retainPrimaryCluster = a} :: DeleteReplicationGroup)

-- | The name of a final node group (shard) snapshot. ElastiCache creates the
-- snapshot from the primary node in the cluster, rather than one of the
-- replicas; this is to ensure that it captures the freshest data. After
-- the final snapshot is taken, the replication group is immediately
-- deleted.
deleteReplicationGroup_finalSnapshotIdentifier :: Lens.Lens' DeleteReplicationGroup (Prelude.Maybe Prelude.Text)
deleteReplicationGroup_finalSnapshotIdentifier = Lens.lens (\DeleteReplicationGroup' {finalSnapshotIdentifier} -> finalSnapshotIdentifier) (\s@DeleteReplicationGroup' {} a -> s {finalSnapshotIdentifier = a} :: DeleteReplicationGroup)

-- | The identifier for the cluster to be deleted. This parameter is not case
-- sensitive.
deleteReplicationGroup_replicationGroupId :: Lens.Lens' DeleteReplicationGroup Prelude.Text
deleteReplicationGroup_replicationGroupId = Lens.lens (\DeleteReplicationGroup' {replicationGroupId} -> replicationGroupId) (\s@DeleteReplicationGroup' {} a -> s {replicationGroupId = a} :: DeleteReplicationGroup)

instance Core.AWSRequest DeleteReplicationGroup where
  type
    AWSResponse DeleteReplicationGroup =
      DeleteReplicationGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteReplicationGroupResult"
      ( \s h x ->
          DeleteReplicationGroupResponse'
            Prelude.<$> (x Core..@? "ReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReplicationGroup where
  hashWithSalt _salt DeleteReplicationGroup' {..} =
    _salt `Prelude.hashWithSalt` retainPrimaryCluster
      `Prelude.hashWithSalt` finalSnapshotIdentifier
      `Prelude.hashWithSalt` replicationGroupId

instance Prelude.NFData DeleteReplicationGroup where
  rnf DeleteReplicationGroup' {..} =
    Prelude.rnf retainPrimaryCluster
      `Prelude.seq` Prelude.rnf finalSnapshotIdentifier
      `Prelude.seq` Prelude.rnf replicationGroupId

instance Core.ToHeaders DeleteReplicationGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteReplicationGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteReplicationGroup where
  toQuery DeleteReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteReplicationGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "RetainPrimaryCluster" Core.=: retainPrimaryCluster,
        "FinalSnapshotIdentifier"
          Core.=: finalSnapshotIdentifier,
        "ReplicationGroupId" Core.=: replicationGroupId
      ]

-- | /See:/ 'newDeleteReplicationGroupResponse' smart constructor.
data DeleteReplicationGroupResponse = DeleteReplicationGroupResponse'
  { replicationGroup :: Prelude.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroup', 'deleteReplicationGroupResponse_replicationGroup' - Undocumented member.
--
-- 'httpStatus', 'deleteReplicationGroupResponse_httpStatus' - The response's http status code.
newDeleteReplicationGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReplicationGroupResponse
newDeleteReplicationGroupResponse pHttpStatus_ =
  DeleteReplicationGroupResponse'
    { replicationGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteReplicationGroupResponse_replicationGroup :: Lens.Lens' DeleteReplicationGroupResponse (Prelude.Maybe ReplicationGroup)
deleteReplicationGroupResponse_replicationGroup = Lens.lens (\DeleteReplicationGroupResponse' {replicationGroup} -> replicationGroup) (\s@DeleteReplicationGroupResponse' {} a -> s {replicationGroup = a} :: DeleteReplicationGroupResponse)

-- | The response's http status code.
deleteReplicationGroupResponse_httpStatus :: Lens.Lens' DeleteReplicationGroupResponse Prelude.Int
deleteReplicationGroupResponse_httpStatus = Lens.lens (\DeleteReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteReplicationGroupResponse' {} a -> s {httpStatus = a} :: DeleteReplicationGroupResponse)

instance
  Prelude.NFData
    DeleteReplicationGroupResponse
  where
  rnf DeleteReplicationGroupResponse' {..} =
    Prelude.rnf replicationGroup
      `Prelude.seq` Prelude.rnf httpStatus
