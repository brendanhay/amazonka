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
-- Module      : Network.AWS.CloudHSMv2.ModifyCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies AWS CloudHSM cluster.
module Network.AWS.CloudHSMv2.ModifyCluster
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

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { -- | A policy that defines how the service retains backups.
    backupRetentionPolicy :: BackupRetentionPolicy,
    -- | The identifier (ID) of the cluster that you want to modify. To find the
    -- cluster ID, use DescribeClusters.
    clusterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
modifyCluster_clusterId :: Lens.Lens' ModifyCluster Core.Text
modifyCluster_clusterId = Lens.lens (\ModifyCluster' {clusterId} -> clusterId) (\s@ModifyCluster' {} a -> s {clusterId = a} :: ModifyCluster)

instance Core.AWSRequest ModifyCluster where
  type
    AWSResponse ModifyCluster =
      ModifyClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyClusterResponse'
            Core.<$> (x Core..?> "Cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyCluster

instance Core.NFData ModifyCluster

instance Core.ToHeaders ModifyCluster where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("BaldrApiService.ModifyCluster" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyCluster where
  toJSON ModifyCluster' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "BackupRetentionPolicy"
                  Core..= backupRetentionPolicy
              ),
            Core.Just ("ClusterId" Core..= clusterId)
          ]
      )

instance Core.ToPath ModifyCluster where
  toPath = Core.const "/"

instance Core.ToQuery ModifyCluster where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyClusterResponse
newModifyClusterResponse pHttpStatus_ =
  ModifyClusterResponse'
    { cluster = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterResponse_cluster :: Lens.Lens' ModifyClusterResponse (Core.Maybe Cluster)
modifyClusterResponse_cluster = Lens.lens (\ModifyClusterResponse' {cluster} -> cluster) (\s@ModifyClusterResponse' {} a -> s {cluster = a} :: ModifyClusterResponse)

-- | The response's http status code.
modifyClusterResponse_httpStatus :: Lens.Lens' ModifyClusterResponse Core.Int
modifyClusterResponse_httpStatus = Lens.lens (\ModifyClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterResponse' {} a -> s {httpStatus = a} :: ModifyClusterResponse)

instance Core.NFData ModifyClusterResponse
