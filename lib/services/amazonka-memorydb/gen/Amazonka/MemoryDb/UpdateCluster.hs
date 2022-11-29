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
-- Module      : Amazonka.MemoryDb.UpdateCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a cluster. You can use this operation to
-- change one or more cluster configuration settings by specifying the
-- settings and the new values.
module Amazonka.MemoryDb.UpdateCluster
  ( -- * Creating a Request
    UpdateCluster (..),
    newUpdateCluster,

    -- * Request Lenses
    updateCluster_shardConfiguration,
    updateCluster_parameterGroupName,
    updateCluster_aCLName,
    updateCluster_securityGroupIds,
    updateCluster_snsTopicStatus,
    updateCluster_description,
    updateCluster_nodeType,
    updateCluster_maintenanceWindow,
    updateCluster_snapshotWindow,
    updateCluster_snapshotRetentionLimit,
    updateCluster_snsTopicArn,
    updateCluster_replicaConfiguration,
    updateCluster_engineVersion,
    updateCluster_clusterName,

    -- * Destructuring the Response
    UpdateClusterResponse (..),
    newUpdateClusterResponse,

    -- * Response Lenses
    updateClusterResponse_cluster,
    updateClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { -- | The number of shards in the cluster
    shardConfiguration :: Prelude.Maybe ShardConfigurationRequest,
    -- | The name of the parameter group to update
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Access Control List that is associated with the cluster
    aCLName :: Prelude.Maybe Prelude.Text,
    -- | The SecurityGroupIds to update
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The status of the Amazon SNS notification topic. Notifications are sent
    -- only if the status is active.
    snsTopicStatus :: Prelude.Maybe Prelude.Text,
    -- | The description of the cluster to update
    description :: Prelude.Maybe Prelude.Text,
    -- | A valid node type that you want to scale this cluster up or down to.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which maintenance on the cluster
    -- is performed. It is specified as a range in the format
    -- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
    -- is a 60 minute period.
    --
    -- Valid values for @ddd@ are:
    --
    -- -   @sun@
    --
    -- -   @mon@
    --
    -- -   @tue@
    --
    -- -   @wed@
    --
    -- -   @thu@
    --
    -- -   @fri@
    --
    -- -   @sat@
    --
    -- Example: @sun:23:00-mon:01:30@
    maintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The daily time range (in UTC) during which MemoryDB begins taking a
    -- daily snapshot of your cluster.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which MemoryDB retains automatic cluster
    -- snapshots before deleting them. For example, if you set
    -- SnapshotRetentionLimit to 5, a snapshot that was taken today is retained
    -- for 5 days before being deleted.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The SNS topic ARN to update
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The number of replicas that will reside in each shard
    replicaConfiguration :: Prelude.Maybe ReplicaConfigurationRequest,
    -- | The upgraded version of the engine to be run on the nodes. You can
    -- upgrade to a newer engine version, but you cannot downgrade to an
    -- earlier engine version. If you want to use an earlier engine version,
    -- you must delete the existing cluster and create it anew with the earlier
    -- engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster to update
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shardConfiguration', 'updateCluster_shardConfiguration' - The number of shards in the cluster
--
-- 'parameterGroupName', 'updateCluster_parameterGroupName' - The name of the parameter group to update
--
-- 'aCLName', 'updateCluster_aCLName' - The Access Control List that is associated with the cluster
--
-- 'securityGroupIds', 'updateCluster_securityGroupIds' - The SecurityGroupIds to update
--
-- 'snsTopicStatus', 'updateCluster_snsTopicStatus' - The status of the Amazon SNS notification topic. Notifications are sent
-- only if the status is active.
--
-- 'description', 'updateCluster_description' - The description of the cluster to update
--
-- 'nodeType', 'updateCluster_nodeType' - A valid node type that you want to scale this cluster up or down to.
--
-- 'maintenanceWindow', 'updateCluster_maintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
--
-- Valid values for @ddd@ are:
--
-- -   @sun@
--
-- -   @mon@
--
-- -   @tue@
--
-- -   @wed@
--
-- -   @thu@
--
-- -   @fri@
--
-- -   @sat@
--
-- Example: @sun:23:00-mon:01:30@
--
-- 'snapshotWindow', 'updateCluster_snapshotWindow' - The daily time range (in UTC) during which MemoryDB begins taking a
-- daily snapshot of your cluster.
--
-- 'snapshotRetentionLimit', 'updateCluster_snapshotRetentionLimit' - The number of days for which MemoryDB retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, a snapshot that was taken today is retained
-- for 5 days before being deleted.
--
-- 'snsTopicArn', 'updateCluster_snsTopicArn' - The SNS topic ARN to update
--
-- 'replicaConfiguration', 'updateCluster_replicaConfiguration' - The number of replicas that will reside in each shard
--
-- 'engineVersion', 'updateCluster_engineVersion' - The upgraded version of the engine to be run on the nodes. You can
-- upgrade to a newer engine version, but you cannot downgrade to an
-- earlier engine version. If you want to use an earlier engine version,
-- you must delete the existing cluster and create it anew with the earlier
-- engine version.
--
-- 'clusterName', 'updateCluster_clusterName' - The name of the cluster to update
newUpdateCluster ::
  -- | 'clusterName'
  Prelude.Text ->
  UpdateCluster
newUpdateCluster pClusterName_ =
  UpdateCluster'
    { shardConfiguration =
        Prelude.Nothing,
      parameterGroupName = Prelude.Nothing,
      aCLName = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      snsTopicStatus = Prelude.Nothing,
      description = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      maintenanceWindow = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      replicaConfiguration = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      clusterName = pClusterName_
    }

-- | The number of shards in the cluster
updateCluster_shardConfiguration :: Lens.Lens' UpdateCluster (Prelude.Maybe ShardConfigurationRequest)
updateCluster_shardConfiguration = Lens.lens (\UpdateCluster' {shardConfiguration} -> shardConfiguration) (\s@UpdateCluster' {} a -> s {shardConfiguration = a} :: UpdateCluster)

-- | The name of the parameter group to update
updateCluster_parameterGroupName :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_parameterGroupName = Lens.lens (\UpdateCluster' {parameterGroupName} -> parameterGroupName) (\s@UpdateCluster' {} a -> s {parameterGroupName = a} :: UpdateCluster)

-- | The Access Control List that is associated with the cluster
updateCluster_aCLName :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_aCLName = Lens.lens (\UpdateCluster' {aCLName} -> aCLName) (\s@UpdateCluster' {} a -> s {aCLName = a} :: UpdateCluster)

-- | The SecurityGroupIds to update
updateCluster_securityGroupIds :: Lens.Lens' UpdateCluster (Prelude.Maybe [Prelude.Text])
updateCluster_securityGroupIds = Lens.lens (\UpdateCluster' {securityGroupIds} -> securityGroupIds) (\s@UpdateCluster' {} a -> s {securityGroupIds = a} :: UpdateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The status of the Amazon SNS notification topic. Notifications are sent
-- only if the status is active.
updateCluster_snsTopicStatus :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_snsTopicStatus = Lens.lens (\UpdateCluster' {snsTopicStatus} -> snsTopicStatus) (\s@UpdateCluster' {} a -> s {snsTopicStatus = a} :: UpdateCluster)

-- | The description of the cluster to update
updateCluster_description :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_description = Lens.lens (\UpdateCluster' {description} -> description) (\s@UpdateCluster' {} a -> s {description = a} :: UpdateCluster)

-- | A valid node type that you want to scale this cluster up or down to.
updateCluster_nodeType :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_nodeType = Lens.lens (\UpdateCluster' {nodeType} -> nodeType) (\s@UpdateCluster' {} a -> s {nodeType = a} :: UpdateCluster)

-- | Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
--
-- Valid values for @ddd@ are:
--
-- -   @sun@
--
-- -   @mon@
--
-- -   @tue@
--
-- -   @wed@
--
-- -   @thu@
--
-- -   @fri@
--
-- -   @sat@
--
-- Example: @sun:23:00-mon:01:30@
updateCluster_maintenanceWindow :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_maintenanceWindow = Lens.lens (\UpdateCluster' {maintenanceWindow} -> maintenanceWindow) (\s@UpdateCluster' {} a -> s {maintenanceWindow = a} :: UpdateCluster)

-- | The daily time range (in UTC) during which MemoryDB begins taking a
-- daily snapshot of your cluster.
updateCluster_snapshotWindow :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_snapshotWindow = Lens.lens (\UpdateCluster' {snapshotWindow} -> snapshotWindow) (\s@UpdateCluster' {} a -> s {snapshotWindow = a} :: UpdateCluster)

-- | The number of days for which MemoryDB retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, a snapshot that was taken today is retained
-- for 5 days before being deleted.
updateCluster_snapshotRetentionLimit :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Int)
updateCluster_snapshotRetentionLimit = Lens.lens (\UpdateCluster' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@UpdateCluster' {} a -> s {snapshotRetentionLimit = a} :: UpdateCluster)

-- | The SNS topic ARN to update
updateCluster_snsTopicArn :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_snsTopicArn = Lens.lens (\UpdateCluster' {snsTopicArn} -> snsTopicArn) (\s@UpdateCluster' {} a -> s {snsTopicArn = a} :: UpdateCluster)

-- | The number of replicas that will reside in each shard
updateCluster_replicaConfiguration :: Lens.Lens' UpdateCluster (Prelude.Maybe ReplicaConfigurationRequest)
updateCluster_replicaConfiguration = Lens.lens (\UpdateCluster' {replicaConfiguration} -> replicaConfiguration) (\s@UpdateCluster' {} a -> s {replicaConfiguration = a} :: UpdateCluster)

-- | The upgraded version of the engine to be run on the nodes. You can
-- upgrade to a newer engine version, but you cannot downgrade to an
-- earlier engine version. If you want to use an earlier engine version,
-- you must delete the existing cluster and create it anew with the earlier
-- engine version.
updateCluster_engineVersion :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_engineVersion = Lens.lens (\UpdateCluster' {engineVersion} -> engineVersion) (\s@UpdateCluster' {} a -> s {engineVersion = a} :: UpdateCluster)

-- | The name of the cluster to update
updateCluster_clusterName :: Lens.Lens' UpdateCluster Prelude.Text
updateCluster_clusterName = Lens.lens (\UpdateCluster' {clusterName} -> clusterName) (\s@UpdateCluster' {} a -> s {clusterName = a} :: UpdateCluster)

instance Core.AWSRequest UpdateCluster where
  type
    AWSResponse UpdateCluster =
      UpdateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateClusterResponse'
            Prelude.<$> (x Core..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCluster where
  hashWithSalt _salt UpdateCluster' {..} =
    _salt `Prelude.hashWithSalt` shardConfiguration
      `Prelude.hashWithSalt` parameterGroupName
      `Prelude.hashWithSalt` aCLName
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` snsTopicStatus
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` maintenanceWindow
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` replicaConfiguration
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData UpdateCluster where
  rnf UpdateCluster' {..} =
    Prelude.rnf shardConfiguration
      `Prelude.seq` Prelude.rnf parameterGroupName
      `Prelude.seq` Prelude.rnf aCLName
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf snsTopicStatus
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf maintenanceWindow
      `Prelude.seq` Prelude.rnf snapshotWindow
      `Prelude.seq` Prelude.rnf snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf replicaConfiguration
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf clusterName

instance Core.ToHeaders UpdateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonMemoryDB.UpdateCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateCluster where
  toJSON UpdateCluster' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ShardConfiguration" Core..=)
              Prelude.<$> shardConfiguration,
            ("ParameterGroupName" Core..=)
              Prelude.<$> parameterGroupName,
            ("ACLName" Core..=) Prelude.<$> aCLName,
            ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("SnsTopicStatus" Core..=)
              Prelude.<$> snsTopicStatus,
            ("Description" Core..=) Prelude.<$> description,
            ("NodeType" Core..=) Prelude.<$> nodeType,
            ("MaintenanceWindow" Core..=)
              Prelude.<$> maintenanceWindow,
            ("SnapshotWindow" Core..=)
              Prelude.<$> snapshotWindow,
            ("SnapshotRetentionLimit" Core..=)
              Prelude.<$> snapshotRetentionLimit,
            ("SnsTopicArn" Core..=) Prelude.<$> snsTopicArn,
            ("ReplicaConfiguration" Core..=)
              Prelude.<$> replicaConfiguration,
            ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            Prelude.Just ("ClusterName" Core..= clusterName)
          ]
      )

instance Core.ToPath UpdateCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClusterResponse' smart constructor.
data UpdateClusterResponse = UpdateClusterResponse'
  { -- | The updated cluster
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'updateClusterResponse_cluster' - The updated cluster
--
-- 'httpStatus', 'updateClusterResponse_httpStatus' - The response's http status code.
newUpdateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateClusterResponse
newUpdateClusterResponse pHttpStatus_ =
  UpdateClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated cluster
updateClusterResponse_cluster :: Lens.Lens' UpdateClusterResponse (Prelude.Maybe Cluster)
updateClusterResponse_cluster = Lens.lens (\UpdateClusterResponse' {cluster} -> cluster) (\s@UpdateClusterResponse' {} a -> s {cluster = a} :: UpdateClusterResponse)

-- | The response's http status code.
updateClusterResponse_httpStatus :: Lens.Lens' UpdateClusterResponse Prelude.Int
updateClusterResponse_httpStatus = Lens.lens (\UpdateClusterResponse' {httpStatus} -> httpStatus) (\s@UpdateClusterResponse' {} a -> s {httpStatus = a} :: UpdateClusterResponse)

instance Prelude.NFData UpdateClusterResponse where
  rnf UpdateClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
