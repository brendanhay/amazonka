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
-- Module      : Network.AWS.SecretsManager.RemoveRegionsFromReplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove regions from replication.
module Network.AWS.SecretsManager.RemoveRegionsFromReplication
  ( -- * Creating a Request
    RemoveRegionsFromReplication (..),
    newRemoveRegionsFromReplication,

    -- * Request Lenses
    removeRegionsFromReplication_secretId,
    removeRegionsFromReplication_removeReplicaRegions,

    -- * Destructuring the Response
    RemoveRegionsFromReplicationResponse (..),
    newRemoveRegionsFromReplicationResponse,

    -- * Response Lenses
    removeRegionsFromReplicationResponse_replicationStatus,
    removeRegionsFromReplicationResponse_arn,
    removeRegionsFromReplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newRemoveRegionsFromReplication' smart constructor.
data RemoveRegionsFromReplication = RemoveRegionsFromReplication'
  { -- | Remove a secret by @SecretId@ from replica Regions.
    secretId :: Core.Text,
    -- | Remove replication from specific Regions.
    removeReplicaRegions :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveRegionsFromReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'removeRegionsFromReplication_secretId' - Remove a secret by @SecretId@ from replica Regions.
--
-- 'removeReplicaRegions', 'removeRegionsFromReplication_removeReplicaRegions' - Remove replication from specific Regions.
newRemoveRegionsFromReplication ::
  -- | 'secretId'
  Core.Text ->
  -- | 'removeReplicaRegions'
  Core.NonEmpty Core.Text ->
  RemoveRegionsFromReplication
newRemoveRegionsFromReplication
  pSecretId_
  pRemoveReplicaRegions_ =
    RemoveRegionsFromReplication'
      { secretId =
          pSecretId_,
        removeReplicaRegions =
          Lens._Coerce Lens.# pRemoveReplicaRegions_
      }

-- | Remove a secret by @SecretId@ from replica Regions.
removeRegionsFromReplication_secretId :: Lens.Lens' RemoveRegionsFromReplication Core.Text
removeRegionsFromReplication_secretId = Lens.lens (\RemoveRegionsFromReplication' {secretId} -> secretId) (\s@RemoveRegionsFromReplication' {} a -> s {secretId = a} :: RemoveRegionsFromReplication)

-- | Remove replication from specific Regions.
removeRegionsFromReplication_removeReplicaRegions :: Lens.Lens' RemoveRegionsFromReplication (Core.NonEmpty Core.Text)
removeRegionsFromReplication_removeReplicaRegions = Lens.lens (\RemoveRegionsFromReplication' {removeReplicaRegions} -> removeReplicaRegions) (\s@RemoveRegionsFromReplication' {} a -> s {removeReplicaRegions = a} :: RemoveRegionsFromReplication) Core.. Lens._Coerce

instance Core.AWSRequest RemoveRegionsFromReplication where
  type
    AWSResponse RemoveRegionsFromReplication =
      RemoveRegionsFromReplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveRegionsFromReplicationResponse'
            Core.<$> (x Core..?> "ReplicationStatus" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RemoveRegionsFromReplication

instance Core.NFData RemoveRegionsFromReplication

instance Core.ToHeaders RemoveRegionsFromReplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.RemoveRegionsFromReplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RemoveRegionsFromReplication where
  toJSON RemoveRegionsFromReplication' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SecretId" Core..= secretId),
            Core.Just
              ( "RemoveReplicaRegions"
                  Core..= removeReplicaRegions
              )
          ]
      )

instance Core.ToPath RemoveRegionsFromReplication where
  toPath = Core.const "/"

instance Core.ToQuery RemoveRegionsFromReplication where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRemoveRegionsFromReplicationResponse' smart constructor.
data RemoveRegionsFromReplicationResponse = RemoveRegionsFromReplicationResponse'
  { -- | Describes the remaining replication status after you remove regions from
    -- the replication list.
    replicationStatus :: Core.Maybe [ReplicationStatusType],
    -- | The secret @ARN@ removed from replication regions.
    arn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveRegionsFromReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationStatus', 'removeRegionsFromReplicationResponse_replicationStatus' - Describes the remaining replication status after you remove regions from
-- the replication list.
--
-- 'arn', 'removeRegionsFromReplicationResponse_arn' - The secret @ARN@ removed from replication regions.
--
-- 'httpStatus', 'removeRegionsFromReplicationResponse_httpStatus' - The response's http status code.
newRemoveRegionsFromReplicationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RemoveRegionsFromReplicationResponse
newRemoveRegionsFromReplicationResponse pHttpStatus_ =
  RemoveRegionsFromReplicationResponse'
    { replicationStatus =
        Core.Nothing,
      arn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the remaining replication status after you remove regions from
-- the replication list.
removeRegionsFromReplicationResponse_replicationStatus :: Lens.Lens' RemoveRegionsFromReplicationResponse (Core.Maybe [ReplicationStatusType])
removeRegionsFromReplicationResponse_replicationStatus = Lens.lens (\RemoveRegionsFromReplicationResponse' {replicationStatus} -> replicationStatus) (\s@RemoveRegionsFromReplicationResponse' {} a -> s {replicationStatus = a} :: RemoveRegionsFromReplicationResponse) Core.. Lens.mapping Lens._Coerce

-- | The secret @ARN@ removed from replication regions.
removeRegionsFromReplicationResponse_arn :: Lens.Lens' RemoveRegionsFromReplicationResponse (Core.Maybe Core.Text)
removeRegionsFromReplicationResponse_arn = Lens.lens (\RemoveRegionsFromReplicationResponse' {arn} -> arn) (\s@RemoveRegionsFromReplicationResponse' {} a -> s {arn = a} :: RemoveRegionsFromReplicationResponse)

-- | The response's http status code.
removeRegionsFromReplicationResponse_httpStatus :: Lens.Lens' RemoveRegionsFromReplicationResponse Core.Int
removeRegionsFromReplicationResponse_httpStatus = Lens.lens (\RemoveRegionsFromReplicationResponse' {httpStatus} -> httpStatus) (\s@RemoveRegionsFromReplicationResponse' {} a -> s {httpStatus = a} :: RemoveRegionsFromReplicationResponse)

instance
  Core.NFData
    RemoveRegionsFromReplicationResponse
