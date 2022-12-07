{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MemoryDb.Types.ClusterConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ClusterConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.ShardDetail
import qualified Amazonka.Prelude as Prelude

-- | A list of cluster configuration options.
--
-- /See:/ 'newClusterConfiguration' smart constructor.
data ClusterConfiguration = ClusterConfiguration'
  { -- | The port used by the cluster
    port :: Prelude.Maybe Prelude.Int,
    -- | The name of the subnet group used by the cluster
    subnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of parameter group used by the cluster
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the SNS notification topic for the
    -- cluster
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the cluster configuration
    description :: Prelude.Maybe Prelude.Text,
    -- | The node type used for the cluster
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The specified maintenance window for the cluster
    maintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The snapshot window set by the cluster
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The snapshot retention limit set by the cluster
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The list of shards in the cluster
    shards :: Prelude.Maybe [ShardDetail],
    -- | The number of shards in the cluster
    numShards :: Prelude.Maybe Prelude.Int,
    -- | The ID of the VPC the cluster belongs to
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The Redis engine version used by the cluster
    engineVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'clusterConfiguration_port' - The port used by the cluster
--
-- 'subnetGroupName', 'clusterConfiguration_subnetGroupName' - The name of the subnet group used by the cluster
--
-- 'parameterGroupName', 'clusterConfiguration_parameterGroupName' - The name of parameter group used by the cluster
--
-- 'name', 'clusterConfiguration_name' - The name of the cluster
--
-- 'topicArn', 'clusterConfiguration_topicArn' - The Amazon Resource Name (ARN) of the SNS notification topic for the
-- cluster
--
-- 'description', 'clusterConfiguration_description' - The description of the cluster configuration
--
-- 'nodeType', 'clusterConfiguration_nodeType' - The node type used for the cluster
--
-- 'maintenanceWindow', 'clusterConfiguration_maintenanceWindow' - The specified maintenance window for the cluster
--
-- 'snapshotWindow', 'clusterConfiguration_snapshotWindow' - The snapshot window set by the cluster
--
-- 'snapshotRetentionLimit', 'clusterConfiguration_snapshotRetentionLimit' - The snapshot retention limit set by the cluster
--
-- 'shards', 'clusterConfiguration_shards' - The list of shards in the cluster
--
-- 'numShards', 'clusterConfiguration_numShards' - The number of shards in the cluster
--
-- 'vpcId', 'clusterConfiguration_vpcId' - The ID of the VPC the cluster belongs to
--
-- 'engineVersion', 'clusterConfiguration_engineVersion' - The Redis engine version used by the cluster
newClusterConfiguration ::
  ClusterConfiguration
newClusterConfiguration =
  ClusterConfiguration'
    { port = Prelude.Nothing,
      subnetGroupName = Prelude.Nothing,
      parameterGroupName = Prelude.Nothing,
      name = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      description = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      maintenanceWindow = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      shards = Prelude.Nothing,
      numShards = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      engineVersion = Prelude.Nothing
    }

-- | The port used by the cluster
clusterConfiguration_port :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Int)
clusterConfiguration_port = Lens.lens (\ClusterConfiguration' {port} -> port) (\s@ClusterConfiguration' {} a -> s {port = a} :: ClusterConfiguration)

-- | The name of the subnet group used by the cluster
clusterConfiguration_subnetGroupName :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Text)
clusterConfiguration_subnetGroupName = Lens.lens (\ClusterConfiguration' {subnetGroupName} -> subnetGroupName) (\s@ClusterConfiguration' {} a -> s {subnetGroupName = a} :: ClusterConfiguration)

-- | The name of parameter group used by the cluster
clusterConfiguration_parameterGroupName :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Text)
clusterConfiguration_parameterGroupName = Lens.lens (\ClusterConfiguration' {parameterGroupName} -> parameterGroupName) (\s@ClusterConfiguration' {} a -> s {parameterGroupName = a} :: ClusterConfiguration)

-- | The name of the cluster
clusterConfiguration_name :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Text)
clusterConfiguration_name = Lens.lens (\ClusterConfiguration' {name} -> name) (\s@ClusterConfiguration' {} a -> s {name = a} :: ClusterConfiguration)

-- | The Amazon Resource Name (ARN) of the SNS notification topic for the
-- cluster
clusterConfiguration_topicArn :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Text)
clusterConfiguration_topicArn = Lens.lens (\ClusterConfiguration' {topicArn} -> topicArn) (\s@ClusterConfiguration' {} a -> s {topicArn = a} :: ClusterConfiguration)

-- | The description of the cluster configuration
clusterConfiguration_description :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Text)
clusterConfiguration_description = Lens.lens (\ClusterConfiguration' {description} -> description) (\s@ClusterConfiguration' {} a -> s {description = a} :: ClusterConfiguration)

-- | The node type used for the cluster
clusterConfiguration_nodeType :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Text)
clusterConfiguration_nodeType = Lens.lens (\ClusterConfiguration' {nodeType} -> nodeType) (\s@ClusterConfiguration' {} a -> s {nodeType = a} :: ClusterConfiguration)

-- | The specified maintenance window for the cluster
clusterConfiguration_maintenanceWindow :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Text)
clusterConfiguration_maintenanceWindow = Lens.lens (\ClusterConfiguration' {maintenanceWindow} -> maintenanceWindow) (\s@ClusterConfiguration' {} a -> s {maintenanceWindow = a} :: ClusterConfiguration)

-- | The snapshot window set by the cluster
clusterConfiguration_snapshotWindow :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Text)
clusterConfiguration_snapshotWindow = Lens.lens (\ClusterConfiguration' {snapshotWindow} -> snapshotWindow) (\s@ClusterConfiguration' {} a -> s {snapshotWindow = a} :: ClusterConfiguration)

-- | The snapshot retention limit set by the cluster
clusterConfiguration_snapshotRetentionLimit :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Int)
clusterConfiguration_snapshotRetentionLimit = Lens.lens (\ClusterConfiguration' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@ClusterConfiguration' {} a -> s {snapshotRetentionLimit = a} :: ClusterConfiguration)

-- | The list of shards in the cluster
clusterConfiguration_shards :: Lens.Lens' ClusterConfiguration (Prelude.Maybe [ShardDetail])
clusterConfiguration_shards = Lens.lens (\ClusterConfiguration' {shards} -> shards) (\s@ClusterConfiguration' {} a -> s {shards = a} :: ClusterConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The number of shards in the cluster
clusterConfiguration_numShards :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Int)
clusterConfiguration_numShards = Lens.lens (\ClusterConfiguration' {numShards} -> numShards) (\s@ClusterConfiguration' {} a -> s {numShards = a} :: ClusterConfiguration)

-- | The ID of the VPC the cluster belongs to
clusterConfiguration_vpcId :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Text)
clusterConfiguration_vpcId = Lens.lens (\ClusterConfiguration' {vpcId} -> vpcId) (\s@ClusterConfiguration' {} a -> s {vpcId = a} :: ClusterConfiguration)

-- | The Redis engine version used by the cluster
clusterConfiguration_engineVersion :: Lens.Lens' ClusterConfiguration (Prelude.Maybe Prelude.Text)
clusterConfiguration_engineVersion = Lens.lens (\ClusterConfiguration' {engineVersion} -> engineVersion) (\s@ClusterConfiguration' {} a -> s {engineVersion = a} :: ClusterConfiguration)

instance Data.FromJSON ClusterConfiguration where
  parseJSON =
    Data.withObject
      "ClusterConfiguration"
      ( \x ->
          ClusterConfiguration'
            Prelude.<$> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "SubnetGroupName")
            Prelude.<*> (x Data..:? "ParameterGroupName")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "TopicArn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "NodeType")
            Prelude.<*> (x Data..:? "MaintenanceWindow")
            Prelude.<*> (x Data..:? "SnapshotWindow")
            Prelude.<*> (x Data..:? "SnapshotRetentionLimit")
            Prelude.<*> (x Data..:? "Shards" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NumShards")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> (x Data..:? "EngineVersion")
      )

instance Prelude.Hashable ClusterConfiguration where
  hashWithSalt _salt ClusterConfiguration' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` subnetGroupName
      `Prelude.hashWithSalt` parameterGroupName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` topicArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` maintenanceWindow
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` shards
      `Prelude.hashWithSalt` numShards
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData ClusterConfiguration where
  rnf ClusterConfiguration' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf subnetGroupName
      `Prelude.seq` Prelude.rnf parameterGroupName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf maintenanceWindow
      `Prelude.seq` Prelude.rnf snapshotWindow
      `Prelude.seq` Prelude.rnf snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf shards
      `Prelude.seq` Prelude.rnf numShards
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf engineVersion
