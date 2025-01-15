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
-- Module      : Amazonka.CloudHSMV2.ModifyCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies AWS CloudHSM cluster.
module Amazonka.CloudHSMV2.ModifyCluster
  ( -- * Creating a Request
    ModifyCluster (..),
    newModifyCluster,

    -- * Request Lenses
    modifyCluster_backupRetentionPolicy,
    modifyCluster_clusterId,

    -- * Destructuring the Response
    ModifyClusterResponse (..),
    newModifyClusterResponse,

    -- * Response Lenses
    modifyClusterResponse_cluster,
    modifyClusterResponse_httpStatus,
  )
where

import Amazonka.CloudHSMV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { -- | A policy that defines how the service retains backups.
    backupRetentionPolicy :: BackupRetentionPolicy,
    -- | The identifier (ID) of the cluster that you want to modify. To find the
    -- cluster ID, use DescribeClusters.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupRetentionPolicy', 'modifyCluster_backupRetentionPolicy' - A policy that defines how the service retains backups.
--
-- 'clusterId', 'modifyCluster_clusterId' - The identifier (ID) of the cluster that you want to modify. To find the
-- cluster ID, use DescribeClusters.
newModifyCluster ::
  -- | 'backupRetentionPolicy'
  BackupRetentionPolicy ->
  -- | 'clusterId'
  Prelude.Text ->
  ModifyCluster
newModifyCluster pBackupRetentionPolicy_ pClusterId_ =
  ModifyCluster'
    { backupRetentionPolicy =
        pBackupRetentionPolicy_,
      clusterId = pClusterId_
    }

-- | A policy that defines how the service retains backups.
modifyCluster_backupRetentionPolicy :: Lens.Lens' ModifyCluster BackupRetentionPolicy
modifyCluster_backupRetentionPolicy = Lens.lens (\ModifyCluster' {backupRetentionPolicy} -> backupRetentionPolicy) (\s@ModifyCluster' {} a -> s {backupRetentionPolicy = a} :: ModifyCluster)

-- | The identifier (ID) of the cluster that you want to modify. To find the
-- cluster ID, use DescribeClusters.
modifyCluster_clusterId :: Lens.Lens' ModifyCluster Prelude.Text
modifyCluster_clusterId = Lens.lens (\ModifyCluster' {clusterId} -> clusterId) (\s@ModifyCluster' {} a -> s {clusterId = a} :: ModifyCluster)

instance Core.AWSRequest ModifyCluster where
  type
    AWSResponse ModifyCluster =
      ModifyClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyClusterResponse'
            Prelude.<$> (x Data..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyCluster where
  hashWithSalt _salt ModifyCluster' {..} =
    _salt
      `Prelude.hashWithSalt` backupRetentionPolicy
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData ModifyCluster where
  rnf ModifyCluster' {..} =
    Prelude.rnf backupRetentionPolicy `Prelude.seq`
      Prelude.rnf clusterId

instance Data.ToHeaders ModifyCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BaldrApiService.ModifyCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyCluster where
  toJSON ModifyCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "BackupRetentionPolicy"
                  Data..= backupRetentionPolicy
              ),
            Prelude.Just ("ClusterId" Data..= clusterId)
          ]
      )

instance Data.ToPath ModifyCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'modifyClusterResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'modifyClusterResponse_httpStatus' - The response's http status code.
newModifyClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyClusterResponse
newModifyClusterResponse pHttpStatus_ =
  ModifyClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterResponse_cluster :: Lens.Lens' ModifyClusterResponse (Prelude.Maybe Cluster)
modifyClusterResponse_cluster = Lens.lens (\ModifyClusterResponse' {cluster} -> cluster) (\s@ModifyClusterResponse' {} a -> s {cluster = a} :: ModifyClusterResponse)

-- | The response's http status code.
modifyClusterResponse_httpStatus :: Lens.Lens' ModifyClusterResponse Prelude.Int
modifyClusterResponse_httpStatus = Lens.lens (\ModifyClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterResponse' {} a -> s {httpStatus = a} :: ModifyClusterResponse)

instance Prelude.NFData ModifyClusterResponse where
  rnf ModifyClusterResponse' {..} =
    Prelude.rnf cluster `Prelude.seq`
      Prelude.rnf httpStatus
