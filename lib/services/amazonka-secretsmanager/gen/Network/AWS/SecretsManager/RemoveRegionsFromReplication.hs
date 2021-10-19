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
    removeRegionsFromReplicationResponse_arn,
    removeRegionsFromReplicationResponse_replicationStatus,
    removeRegionsFromReplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newRemoveRegionsFromReplication' smart constructor.
data RemoveRegionsFromReplication = RemoveRegionsFromReplication'
  { -- | Remove a secret by @SecretId@ from replica Regions.
    secretId :: Prelude.Text,
    -- | Remove replication from specific Regions.
    removeReplicaRegions :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'removeReplicaRegions'
  Prelude.NonEmpty Prelude.Text ->
  RemoveRegionsFromReplication
newRemoveRegionsFromReplication
  pSecretId_
  pRemoveReplicaRegions_ =
    RemoveRegionsFromReplication'
      { secretId =
          pSecretId_,
        removeReplicaRegions =
          Lens.coerced Lens.# pRemoveReplicaRegions_
      }

-- | Remove a secret by @SecretId@ from replica Regions.
removeRegionsFromReplication_secretId :: Lens.Lens' RemoveRegionsFromReplication Prelude.Text
removeRegionsFromReplication_secretId = Lens.lens (\RemoveRegionsFromReplication' {secretId} -> secretId) (\s@RemoveRegionsFromReplication' {} a -> s {secretId = a} :: RemoveRegionsFromReplication)

-- | Remove replication from specific Regions.
removeRegionsFromReplication_removeReplicaRegions :: Lens.Lens' RemoveRegionsFromReplication (Prelude.NonEmpty Prelude.Text)
removeRegionsFromReplication_removeReplicaRegions = Lens.lens (\RemoveRegionsFromReplication' {removeReplicaRegions} -> removeReplicaRegions) (\s@RemoveRegionsFromReplication' {} a -> s {removeReplicaRegions = a} :: RemoveRegionsFromReplication) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveRegionsFromReplication where
  type
    AWSResponse RemoveRegionsFromReplication =
      RemoveRegionsFromReplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveRegionsFromReplicationResponse'
            Prelude.<$> (x Core..?> "ARN")
            Prelude.<*> ( x Core..?> "ReplicationStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RemoveRegionsFromReplication

instance Prelude.NFData RemoveRegionsFromReplication

instance Core.ToHeaders RemoveRegionsFromReplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.RemoveRegionsFromReplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveRegionsFromReplication where
  toJSON RemoveRegionsFromReplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SecretId" Core..= secretId),
            Prelude.Just
              ( "RemoveReplicaRegions"
                  Core..= removeReplicaRegions
              )
          ]
      )

instance Core.ToPath RemoveRegionsFromReplication where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveRegionsFromReplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveRegionsFromReplicationResponse' smart constructor.
data RemoveRegionsFromReplicationResponse = RemoveRegionsFromReplicationResponse'
  { -- | The secret @ARN@ removed from replication regions.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Describes the remaining replication status after you remove regions from
    -- the replication list.
    replicationStatus :: Prelude.Maybe [ReplicationStatusType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveRegionsFromReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'removeRegionsFromReplicationResponse_arn' - The secret @ARN@ removed from replication regions.
--
-- 'replicationStatus', 'removeRegionsFromReplicationResponse_replicationStatus' - Describes the remaining replication status after you remove regions from
-- the replication list.
--
-- 'httpStatus', 'removeRegionsFromReplicationResponse_httpStatus' - The response's http status code.
newRemoveRegionsFromReplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveRegionsFromReplicationResponse
newRemoveRegionsFromReplicationResponse pHttpStatus_ =
  RemoveRegionsFromReplicationResponse'
    { arn =
        Prelude.Nothing,
      replicationStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The secret @ARN@ removed from replication regions.
removeRegionsFromReplicationResponse_arn :: Lens.Lens' RemoveRegionsFromReplicationResponse (Prelude.Maybe Prelude.Text)
removeRegionsFromReplicationResponse_arn = Lens.lens (\RemoveRegionsFromReplicationResponse' {arn} -> arn) (\s@RemoveRegionsFromReplicationResponse' {} a -> s {arn = a} :: RemoveRegionsFromReplicationResponse)

-- | Describes the remaining replication status after you remove regions from
-- the replication list.
removeRegionsFromReplicationResponse_replicationStatus :: Lens.Lens' RemoveRegionsFromReplicationResponse (Prelude.Maybe [ReplicationStatusType])
removeRegionsFromReplicationResponse_replicationStatus = Lens.lens (\RemoveRegionsFromReplicationResponse' {replicationStatus} -> replicationStatus) (\s@RemoveRegionsFromReplicationResponse' {} a -> s {replicationStatus = a} :: RemoveRegionsFromReplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
removeRegionsFromReplicationResponse_httpStatus :: Lens.Lens' RemoveRegionsFromReplicationResponse Prelude.Int
removeRegionsFromReplicationResponse_httpStatus = Lens.lens (\RemoveRegionsFromReplicationResponse' {httpStatus} -> httpStatus) (\s@RemoveRegionsFromReplicationResponse' {} a -> s {httpStatus = a} :: RemoveRegionsFromReplicationResponse)

instance
  Prelude.NFData
    RemoveRegionsFromReplicationResponse
