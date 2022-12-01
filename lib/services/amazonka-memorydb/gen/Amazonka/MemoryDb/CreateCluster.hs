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
-- Module      : Amazonka.MemoryDb.CreateCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cluster. All nodes in the cluster run the same
-- protocol-compliant engine software.
module Amazonka.MemoryDb.CreateCluster
  ( -- * Creating a Request
    CreateCluster (..),
    newCreateCluster,

    -- * Request Lenses
    createCluster_tags,
    createCluster_port,
    createCluster_subnetGroupName,
    createCluster_parameterGroupName,
    createCluster_numReplicasPerShard,
    createCluster_snapshotName,
    createCluster_securityGroupIds,
    createCluster_autoMinorVersionUpgrade,
    createCluster_tLSEnabled,
    createCluster_description,
    createCluster_snapshotArns,
    createCluster_maintenanceWindow,
    createCluster_snapshotWindow,
    createCluster_snapshotRetentionLimit,
    createCluster_snsTopicArn,
    createCluster_kmsKeyId,
    createCluster_numShards,
    createCluster_dataTiering,
    createCluster_engineVersion,
    createCluster_clusterName,
    createCluster_nodeType,
    createCluster_aCLName,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | A list of tags to be added to this resource. Tags are comma-separated
    -- key,value pairs (e.g. Key=myKey, Value=myKeyValue. You can include
    -- multiple tags as shown following: Key=myKey, Value=myKeyValue
    -- Key=mySecondKey, Value=mySecondKeyValue.
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which each of the nodes accepts connections.
    port :: Prelude.Maybe Prelude.Int,
    -- | The name of the subnet group to be used for the cluster.
    subnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter group associated with the cluster.
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The number of replicas to apply to each shard. The default value is 1.
    -- The maximum is 5.
    numReplicasPerShard :: Prelude.Maybe Prelude.Int,
    -- | The name of a snapshot from which to restore data into the new cluster.
    -- The snapshot status changes to restoring while the new cluster is being
    -- created.
    snapshotName :: Prelude.Maybe Prelude.Text,
    -- | A list of security group names to associate with this cluster.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | When set to true, the cluster will automatically receive minor engine
    -- version upgrades after launch.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | A flag to enable in-transit encryption on the cluster.
    tLSEnabled :: Prelude.Maybe Prelude.Bool,
    -- | An optional description of the cluster.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of Amazon Resource Names (ARN) that uniquely identify the RDB
    -- snapshot files stored in Amazon S3. The snapshot files are used to
    -- populate the new cluster. The Amazon S3 object name in the ARN cannot
    -- contain any commas.
    snapshotArns :: Prelude.Maybe [Prelude.Text],
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
    -- daily snapshot of your shard.
    --
    -- Example: 05:00-09:00
    --
    -- If you do not specify this parameter, MemoryDB automatically chooses an
    -- appropriate time range.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which MemoryDB retains automatic snapshots before
    -- deleting them. For example, if you set SnapshotRetentionLimit to 5, a
    -- snapshot that was taken today is retained for 5 days before being
    -- deleted.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
    -- (SNS) topic to which notifications are sent.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS key used to encrypt the cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The number of shards the cluster will contain. The default value is 1.
    numShards :: Prelude.Maybe Prelude.Int,
    -- | Enables data tiering. Data tiering is only supported for clusters using
    -- the r6gd node type. This parameter must be set when using r6gd nodes.
    -- For more information, see
    -- <https://docs.aws.amazon.com/memorydb/latest/devguide/data-tiering.html Data tiering>.
    dataTiering :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the Redis engine to be used for the cluster.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster. This value must be unique as it also serves as
    -- the cluster identifier.
    clusterName :: Prelude.Text,
    -- | The compute and memory capacity of the nodes in the cluster.
    nodeType :: Prelude.Text,
    -- | The name of the Access Control List to associate with the cluster.
    aCLName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCluster_tags' - A list of tags to be added to this resource. Tags are comma-separated
-- key,value pairs (e.g. Key=myKey, Value=myKeyValue. You can include
-- multiple tags as shown following: Key=myKey, Value=myKeyValue
-- Key=mySecondKey, Value=mySecondKeyValue.
--
-- 'port', 'createCluster_port' - The port number on which each of the nodes accepts connections.
--
-- 'subnetGroupName', 'createCluster_subnetGroupName' - The name of the subnet group to be used for the cluster.
--
-- 'parameterGroupName', 'createCluster_parameterGroupName' - The name of the parameter group associated with the cluster.
--
-- 'numReplicasPerShard', 'createCluster_numReplicasPerShard' - The number of replicas to apply to each shard. The default value is 1.
-- The maximum is 5.
--
-- 'snapshotName', 'createCluster_snapshotName' - The name of a snapshot from which to restore data into the new cluster.
-- The snapshot status changes to restoring while the new cluster is being
-- created.
--
-- 'securityGroupIds', 'createCluster_securityGroupIds' - A list of security group names to associate with this cluster.
--
-- 'autoMinorVersionUpgrade', 'createCluster_autoMinorVersionUpgrade' - When set to true, the cluster will automatically receive minor engine
-- version upgrades after launch.
--
-- 'tLSEnabled', 'createCluster_tLSEnabled' - A flag to enable in-transit encryption on the cluster.
--
-- 'description', 'createCluster_description' - An optional description of the cluster.
--
-- 'snapshotArns', 'createCluster_snapshotArns' - A list of Amazon Resource Names (ARN) that uniquely identify the RDB
-- snapshot files stored in Amazon S3. The snapshot files are used to
-- populate the new cluster. The Amazon S3 object name in the ARN cannot
-- contain any commas.
--
-- 'maintenanceWindow', 'createCluster_maintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster
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
-- 'snapshotWindow', 'createCluster_snapshotWindow' - The daily time range (in UTC) during which MemoryDB begins taking a
-- daily snapshot of your shard.
--
-- Example: 05:00-09:00
--
-- If you do not specify this parameter, MemoryDB automatically chooses an
-- appropriate time range.
--
-- 'snapshotRetentionLimit', 'createCluster_snapshotRetentionLimit' - The number of days for which MemoryDB retains automatic snapshots before
-- deleting them. For example, if you set SnapshotRetentionLimit to 5, a
-- snapshot that was taken today is retained for 5 days before being
-- deleted.
--
-- 'snsTopicArn', 'createCluster_snsTopicArn' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications are sent.
--
-- 'kmsKeyId', 'createCluster_kmsKeyId' - The ID of the KMS key used to encrypt the cluster.
--
-- 'numShards', 'createCluster_numShards' - The number of shards the cluster will contain. The default value is 1.
--
-- 'dataTiering', 'createCluster_dataTiering' - Enables data tiering. Data tiering is only supported for clusters using
-- the r6gd node type. This parameter must be set when using r6gd nodes.
-- For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/data-tiering.html Data tiering>.
--
-- 'engineVersion', 'createCluster_engineVersion' - The version number of the Redis engine to be used for the cluster.
--
-- 'clusterName', 'createCluster_clusterName' - The name of the cluster. This value must be unique as it also serves as
-- the cluster identifier.
--
-- 'nodeType', 'createCluster_nodeType' - The compute and memory capacity of the nodes in the cluster.
--
-- 'aCLName', 'createCluster_aCLName' - The name of the Access Control List to associate with the cluster.
newCreateCluster ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'nodeType'
  Prelude.Text ->
  -- | 'aCLName'
  Prelude.Text ->
  CreateCluster
newCreateCluster pClusterName_ pNodeType_ pACLName_ =
  CreateCluster'
    { tags = Prelude.Nothing,
      port = Prelude.Nothing,
      subnetGroupName = Prelude.Nothing,
      parameterGroupName = Prelude.Nothing,
      numReplicasPerShard = Prelude.Nothing,
      snapshotName = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      tLSEnabled = Prelude.Nothing,
      description = Prelude.Nothing,
      snapshotArns = Prelude.Nothing,
      maintenanceWindow = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      numShards = Prelude.Nothing,
      dataTiering = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      clusterName = pClusterName_,
      nodeType = pNodeType_,
      aCLName = pACLName_
    }

-- | A list of tags to be added to this resource. Tags are comma-separated
-- key,value pairs (e.g. Key=myKey, Value=myKeyValue. You can include
-- multiple tags as shown following: Key=myKey, Value=myKeyValue
-- Key=mySecondKey, Value=mySecondKeyValue.
createCluster_tags :: Lens.Lens' CreateCluster (Prelude.Maybe [Tag])
createCluster_tags = Lens.lens (\CreateCluster' {tags} -> tags) (\s@CreateCluster' {} a -> s {tags = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which each of the nodes accepts connections.
createCluster_port :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Int)
createCluster_port = Lens.lens (\CreateCluster' {port} -> port) (\s@CreateCluster' {} a -> s {port = a} :: CreateCluster)

-- | The name of the subnet group to be used for the cluster.
createCluster_subnetGroupName :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_subnetGroupName = Lens.lens (\CreateCluster' {subnetGroupName} -> subnetGroupName) (\s@CreateCluster' {} a -> s {subnetGroupName = a} :: CreateCluster)

-- | The name of the parameter group associated with the cluster.
createCluster_parameterGroupName :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_parameterGroupName = Lens.lens (\CreateCluster' {parameterGroupName} -> parameterGroupName) (\s@CreateCluster' {} a -> s {parameterGroupName = a} :: CreateCluster)

-- | The number of replicas to apply to each shard. The default value is 1.
-- The maximum is 5.
createCluster_numReplicasPerShard :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Int)
createCluster_numReplicasPerShard = Lens.lens (\CreateCluster' {numReplicasPerShard} -> numReplicasPerShard) (\s@CreateCluster' {} a -> s {numReplicasPerShard = a} :: CreateCluster)

-- | The name of a snapshot from which to restore data into the new cluster.
-- The snapshot status changes to restoring while the new cluster is being
-- created.
createCluster_snapshotName :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_snapshotName = Lens.lens (\CreateCluster' {snapshotName} -> snapshotName) (\s@CreateCluster' {} a -> s {snapshotName = a} :: CreateCluster)

-- | A list of security group names to associate with this cluster.
createCluster_securityGroupIds :: Lens.Lens' CreateCluster (Prelude.Maybe [Prelude.Text])
createCluster_securityGroupIds = Lens.lens (\CreateCluster' {securityGroupIds} -> securityGroupIds) (\s@CreateCluster' {} a -> s {securityGroupIds = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | When set to true, the cluster will automatically receive minor engine
-- version upgrades after launch.
createCluster_autoMinorVersionUpgrade :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Bool)
createCluster_autoMinorVersionUpgrade = Lens.lens (\CreateCluster' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateCluster' {} a -> s {autoMinorVersionUpgrade = a} :: CreateCluster)

-- | A flag to enable in-transit encryption on the cluster.
createCluster_tLSEnabled :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Bool)
createCluster_tLSEnabled = Lens.lens (\CreateCluster' {tLSEnabled} -> tLSEnabled) (\s@CreateCluster' {} a -> s {tLSEnabled = a} :: CreateCluster)

-- | An optional description of the cluster.
createCluster_description :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_description = Lens.lens (\CreateCluster' {description} -> description) (\s@CreateCluster' {} a -> s {description = a} :: CreateCluster)

-- | A list of Amazon Resource Names (ARN) that uniquely identify the RDB
-- snapshot files stored in Amazon S3. The snapshot files are used to
-- populate the new cluster. The Amazon S3 object name in the ARN cannot
-- contain any commas.
createCluster_snapshotArns :: Lens.Lens' CreateCluster (Prelude.Maybe [Prelude.Text])
createCluster_snapshotArns = Lens.lens (\CreateCluster' {snapshotArns} -> snapshotArns) (\s@CreateCluster' {} a -> s {snapshotArns = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

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
createCluster_maintenanceWindow :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_maintenanceWindow = Lens.lens (\CreateCluster' {maintenanceWindow} -> maintenanceWindow) (\s@CreateCluster' {} a -> s {maintenanceWindow = a} :: CreateCluster)

-- | The daily time range (in UTC) during which MemoryDB begins taking a
-- daily snapshot of your shard.
--
-- Example: 05:00-09:00
--
-- If you do not specify this parameter, MemoryDB automatically chooses an
-- appropriate time range.
createCluster_snapshotWindow :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_snapshotWindow = Lens.lens (\CreateCluster' {snapshotWindow} -> snapshotWindow) (\s@CreateCluster' {} a -> s {snapshotWindow = a} :: CreateCluster)

-- | The number of days for which MemoryDB retains automatic snapshots before
-- deleting them. For example, if you set SnapshotRetentionLimit to 5, a
-- snapshot that was taken today is retained for 5 days before being
-- deleted.
createCluster_snapshotRetentionLimit :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Int)
createCluster_snapshotRetentionLimit = Lens.lens (\CreateCluster' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@CreateCluster' {} a -> s {snapshotRetentionLimit = a} :: CreateCluster)

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications are sent.
createCluster_snsTopicArn :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_snsTopicArn = Lens.lens (\CreateCluster' {snsTopicArn} -> snsTopicArn) (\s@CreateCluster' {} a -> s {snsTopicArn = a} :: CreateCluster)

-- | The ID of the KMS key used to encrypt the cluster.
createCluster_kmsKeyId :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_kmsKeyId = Lens.lens (\CreateCluster' {kmsKeyId} -> kmsKeyId) (\s@CreateCluster' {} a -> s {kmsKeyId = a} :: CreateCluster)

-- | The number of shards the cluster will contain. The default value is 1.
createCluster_numShards :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Int)
createCluster_numShards = Lens.lens (\CreateCluster' {numShards} -> numShards) (\s@CreateCluster' {} a -> s {numShards = a} :: CreateCluster)

-- | Enables data tiering. Data tiering is only supported for clusters using
-- the r6gd node type. This parameter must be set when using r6gd nodes.
-- For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/data-tiering.html Data tiering>.
createCluster_dataTiering :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Bool)
createCluster_dataTiering = Lens.lens (\CreateCluster' {dataTiering} -> dataTiering) (\s@CreateCluster' {} a -> s {dataTiering = a} :: CreateCluster)

-- | The version number of the Redis engine to be used for the cluster.
createCluster_engineVersion :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_engineVersion = Lens.lens (\CreateCluster' {engineVersion} -> engineVersion) (\s@CreateCluster' {} a -> s {engineVersion = a} :: CreateCluster)

-- | The name of the cluster. This value must be unique as it also serves as
-- the cluster identifier.
createCluster_clusterName :: Lens.Lens' CreateCluster Prelude.Text
createCluster_clusterName = Lens.lens (\CreateCluster' {clusterName} -> clusterName) (\s@CreateCluster' {} a -> s {clusterName = a} :: CreateCluster)

-- | The compute and memory capacity of the nodes in the cluster.
createCluster_nodeType :: Lens.Lens' CreateCluster Prelude.Text
createCluster_nodeType = Lens.lens (\CreateCluster' {nodeType} -> nodeType) (\s@CreateCluster' {} a -> s {nodeType = a} :: CreateCluster)

-- | The name of the Access Control List to associate with the cluster.
createCluster_aCLName :: Lens.Lens' CreateCluster Prelude.Text
createCluster_aCLName = Lens.lens (\CreateCluster' {aCLName} -> aCLName) (\s@CreateCluster' {} a -> s {aCLName = a} :: CreateCluster)

instance Core.AWSRequest CreateCluster where
  type
    AWSResponse CreateCluster =
      CreateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Prelude.<$> (x Core..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCluster where
  hashWithSalt _salt CreateCluster' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` subnetGroupName
      `Prelude.hashWithSalt` parameterGroupName
      `Prelude.hashWithSalt` numReplicasPerShard
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` tLSEnabled
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` snapshotArns
      `Prelude.hashWithSalt` maintenanceWindow
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` numShards
      `Prelude.hashWithSalt` dataTiering
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` aCLName

instance Prelude.NFData CreateCluster where
  rnf CreateCluster' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf subnetGroupName
      `Prelude.seq` Prelude.rnf parameterGroupName
      `Prelude.seq` Prelude.rnf numReplicasPerShard
      `Prelude.seq` Prelude.rnf snapshotName
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf tLSEnabled
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf snapshotArns
      `Prelude.seq` Prelude.rnf maintenanceWindow
      `Prelude.seq` Prelude.rnf snapshotWindow
      `Prelude.seq` Prelude.rnf snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf numShards
      `Prelude.seq` Prelude.rnf dataTiering
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf aCLName

instance Core.ToHeaders CreateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonMemoryDB.CreateCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Port" Core..=) Prelude.<$> port,
            ("SubnetGroupName" Core..=)
              Prelude.<$> subnetGroupName,
            ("ParameterGroupName" Core..=)
              Prelude.<$> parameterGroupName,
            ("NumReplicasPerShard" Core..=)
              Prelude.<$> numReplicasPerShard,
            ("SnapshotName" Core..=) Prelude.<$> snapshotName,
            ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("AutoMinorVersionUpgrade" Core..=)
              Prelude.<$> autoMinorVersionUpgrade,
            ("TLSEnabled" Core..=) Prelude.<$> tLSEnabled,
            ("Description" Core..=) Prelude.<$> description,
            ("SnapshotArns" Core..=) Prelude.<$> snapshotArns,
            ("MaintenanceWindow" Core..=)
              Prelude.<$> maintenanceWindow,
            ("SnapshotWindow" Core..=)
              Prelude.<$> snapshotWindow,
            ("SnapshotRetentionLimit" Core..=)
              Prelude.<$> snapshotRetentionLimit,
            ("SnsTopicArn" Core..=) Prelude.<$> snsTopicArn,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("NumShards" Core..=) Prelude.<$> numShards,
            ("DataTiering" Core..=) Prelude.<$> dataTiering,
            ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            Prelude.Just ("ClusterName" Core..= clusterName),
            Prelude.Just ("NodeType" Core..= nodeType),
            Prelude.Just ("ACLName" Core..= aCLName)
          ]
      )

instance Core.ToPath CreateCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | The newly-created cluster.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'createClusterResponse_cluster' - The newly-created cluster.
--
-- 'httpStatus', 'createClusterResponse_httpStatus' - The response's http status code.
newCreateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClusterResponse
newCreateClusterResponse pHttpStatus_ =
  CreateClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly-created cluster.
createClusterResponse_cluster :: Lens.Lens' CreateClusterResponse (Prelude.Maybe Cluster)
createClusterResponse_cluster = Lens.lens (\CreateClusterResponse' {cluster} -> cluster) (\s@CreateClusterResponse' {} a -> s {cluster = a} :: CreateClusterResponse)

-- | The response's http status code.
createClusterResponse_httpStatus :: Lens.Lens' CreateClusterResponse Prelude.Int
createClusterResponse_httpStatus = Lens.lens (\CreateClusterResponse' {httpStatus} -> httpStatus) (\s@CreateClusterResponse' {} a -> s {httpStatus = a} :: CreateClusterResponse)

instance Prelude.NFData CreateClusterResponse where
  rnf CreateClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
