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
-- Module      : Amazonka.DocDbElastic.Types.Cluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocDbElastic.Types.Cluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types.Auth
import Amazonka.DocDbElastic.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a specific Elastic DocumentDB cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The name of the Elastic DocumentDB cluster administrator.
    adminUserName :: Prelude.Text,
    -- | The authentication type for the Elastic DocumentDB cluster.
    authType :: Auth,
    -- | The arn of the Elastic DocumentDB cluster.
    clusterArn :: Prelude.Text,
    -- | The URL used to connect to the Elastic DocumentDB cluster.
    clusterEndpoint :: Prelude.Text,
    -- | The name of the Elastic DocumentDB cluster.
    clusterName :: Prelude.Text,
    -- | The time when the Elastic DocumentDB cluster was created in Universal
    -- Coordinated Time (UTC).
    createTime :: Prelude.Text,
    -- | The KMS key identifier to use to encrypt the Elastic DocumentDB cluster.
    kmsKeyId :: Prelude.Text,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- /Format/: @ddd:hh24:mi-ddd:hh24:mi@
    preferredMaintenanceWindow :: Prelude.Text,
    -- | The capacity of each shard in the Elastic DocumentDB cluster.
    shardCapacity :: Prelude.Int,
    -- | The number of shards in the Elastic DocumentDB cluster.
    shardCount :: Prelude.Int,
    -- | The status of the Elastic DocumentDB cluster.
    status :: Status,
    -- | The Amazon EC2 subnet IDs for the Elastic DocumentDB cluster.
    subnetIds :: [Prelude.Text],
    -- | A list of EC2 VPC security groups associated with this cluster.
    vpcSecurityGroupIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminUserName', 'cluster_adminUserName' - The name of the Elastic DocumentDB cluster administrator.
--
-- 'authType', 'cluster_authType' - The authentication type for the Elastic DocumentDB cluster.
--
-- 'clusterArn', 'cluster_clusterArn' - The arn of the Elastic DocumentDB cluster.
--
-- 'clusterEndpoint', 'cluster_clusterEndpoint' - The URL used to connect to the Elastic DocumentDB cluster.
--
-- 'clusterName', 'cluster_clusterName' - The name of the Elastic DocumentDB cluster.
--
-- 'createTime', 'cluster_createTime' - The time when the Elastic DocumentDB cluster was created in Universal
-- Coordinated Time (UTC).
--
-- 'kmsKeyId', 'cluster_kmsKeyId' - The KMS key identifier to use to encrypt the Elastic DocumentDB cluster.
--
-- 'preferredMaintenanceWindow', 'cluster_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- /Format/: @ddd:hh24:mi-ddd:hh24:mi@
--
-- 'shardCapacity', 'cluster_shardCapacity' - The capacity of each shard in the Elastic DocumentDB cluster.
--
-- 'shardCount', 'cluster_shardCount' - The number of shards in the Elastic DocumentDB cluster.
--
-- 'status', 'cluster_status' - The status of the Elastic DocumentDB cluster.
--
-- 'subnetIds', 'cluster_subnetIds' - The Amazon EC2 subnet IDs for the Elastic DocumentDB cluster.
--
-- 'vpcSecurityGroupIds', 'cluster_vpcSecurityGroupIds' - A list of EC2 VPC security groups associated with this cluster.
newCluster ::
  -- | 'adminUserName'
  Prelude.Text ->
  -- | 'authType'
  Auth ->
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'clusterEndpoint'
  Prelude.Text ->
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.Text ->
  -- | 'kmsKeyId'
  Prelude.Text ->
  -- | 'preferredMaintenanceWindow'
  Prelude.Text ->
  -- | 'shardCapacity'
  Prelude.Int ->
  -- | 'shardCount'
  Prelude.Int ->
  -- | 'status'
  Status ->
  Cluster
newCluster
  pAdminUserName_
  pAuthType_
  pClusterArn_
  pClusterEndpoint_
  pClusterName_
  pCreateTime_
  pKmsKeyId_
  pPreferredMaintenanceWindow_
  pShardCapacity_
  pShardCount_
  pStatus_ =
    Cluster'
      { adminUserName = pAdminUserName_,
        authType = pAuthType_,
        clusterArn = pClusterArn_,
        clusterEndpoint = pClusterEndpoint_,
        clusterName = pClusterName_,
        createTime = pCreateTime_,
        kmsKeyId = pKmsKeyId_,
        preferredMaintenanceWindow =
          pPreferredMaintenanceWindow_,
        shardCapacity = pShardCapacity_,
        shardCount = pShardCount_,
        status = pStatus_,
        subnetIds = Prelude.mempty,
        vpcSecurityGroupIds = Prelude.mempty
      }

-- | The name of the Elastic DocumentDB cluster administrator.
cluster_adminUserName :: Lens.Lens' Cluster Prelude.Text
cluster_adminUserName = Lens.lens (\Cluster' {adminUserName} -> adminUserName) (\s@Cluster' {} a -> s {adminUserName = a} :: Cluster)

-- | The authentication type for the Elastic DocumentDB cluster.
cluster_authType :: Lens.Lens' Cluster Auth
cluster_authType = Lens.lens (\Cluster' {authType} -> authType) (\s@Cluster' {} a -> s {authType = a} :: Cluster)

-- | The arn of the Elastic DocumentDB cluster.
cluster_clusterArn :: Lens.Lens' Cluster Prelude.Text
cluster_clusterArn = Lens.lens (\Cluster' {clusterArn} -> clusterArn) (\s@Cluster' {} a -> s {clusterArn = a} :: Cluster)

-- | The URL used to connect to the Elastic DocumentDB cluster.
cluster_clusterEndpoint :: Lens.Lens' Cluster Prelude.Text
cluster_clusterEndpoint = Lens.lens (\Cluster' {clusterEndpoint} -> clusterEndpoint) (\s@Cluster' {} a -> s {clusterEndpoint = a} :: Cluster)

-- | The name of the Elastic DocumentDB cluster.
cluster_clusterName :: Lens.Lens' Cluster Prelude.Text
cluster_clusterName = Lens.lens (\Cluster' {clusterName} -> clusterName) (\s@Cluster' {} a -> s {clusterName = a} :: Cluster)

-- | The time when the Elastic DocumentDB cluster was created in Universal
-- Coordinated Time (UTC).
cluster_createTime :: Lens.Lens' Cluster Prelude.Text
cluster_createTime = Lens.lens (\Cluster' {createTime} -> createTime) (\s@Cluster' {} a -> s {createTime = a} :: Cluster)

-- | The KMS key identifier to use to encrypt the Elastic DocumentDB cluster.
cluster_kmsKeyId :: Lens.Lens' Cluster Prelude.Text
cluster_kmsKeyId = Lens.lens (\Cluster' {kmsKeyId} -> kmsKeyId) (\s@Cluster' {} a -> s {kmsKeyId = a} :: Cluster)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- /Format/: @ddd:hh24:mi-ddd:hh24:mi@
cluster_preferredMaintenanceWindow :: Lens.Lens' Cluster Prelude.Text
cluster_preferredMaintenanceWindow = Lens.lens (\Cluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@Cluster' {} a -> s {preferredMaintenanceWindow = a} :: Cluster)

-- | The capacity of each shard in the Elastic DocumentDB cluster.
cluster_shardCapacity :: Lens.Lens' Cluster Prelude.Int
cluster_shardCapacity = Lens.lens (\Cluster' {shardCapacity} -> shardCapacity) (\s@Cluster' {} a -> s {shardCapacity = a} :: Cluster)

-- | The number of shards in the Elastic DocumentDB cluster.
cluster_shardCount :: Lens.Lens' Cluster Prelude.Int
cluster_shardCount = Lens.lens (\Cluster' {shardCount} -> shardCount) (\s@Cluster' {} a -> s {shardCount = a} :: Cluster)

-- | The status of the Elastic DocumentDB cluster.
cluster_status :: Lens.Lens' Cluster Status
cluster_status = Lens.lens (\Cluster' {status} -> status) (\s@Cluster' {} a -> s {status = a} :: Cluster)

-- | The Amazon EC2 subnet IDs for the Elastic DocumentDB cluster.
cluster_subnetIds :: Lens.Lens' Cluster [Prelude.Text]
cluster_subnetIds = Lens.lens (\Cluster' {subnetIds} -> subnetIds) (\s@Cluster' {} a -> s {subnetIds = a} :: Cluster) Prelude.. Lens.coerced

-- | A list of EC2 VPC security groups associated with this cluster.
cluster_vpcSecurityGroupIds :: Lens.Lens' Cluster [Prelude.Text]
cluster_vpcSecurityGroupIds = Lens.lens (\Cluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@Cluster' {} a -> s {vpcSecurityGroupIds = a} :: Cluster) Prelude.. Lens.coerced

instance Data.FromJSON Cluster where
  parseJSON =
    Data.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Data..: "adminUserName")
            Prelude.<*> (x Data..: "authType")
            Prelude.<*> (x Data..: "clusterArn")
            Prelude.<*> (x Data..: "clusterEndpoint")
            Prelude.<*> (x Data..: "clusterName")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "kmsKeyId")
            Prelude.<*> (x Data..: "preferredMaintenanceWindow")
            Prelude.<*> (x Data..: "shardCapacity")
            Prelude.<*> (x Data..: "shardCount")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..:? "subnetIds" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "vpcSecurityGroupIds"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Cluster where
  hashWithSalt _salt Cluster' {..} =
    _salt `Prelude.hashWithSalt` adminUserName
      `Prelude.hashWithSalt` authType
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` clusterEndpoint
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` shardCapacity
      `Prelude.hashWithSalt` shardCount
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcSecurityGroupIds

instance Prelude.NFData Cluster where
  rnf Cluster' {..} =
    Prelude.rnf adminUserName
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterEndpoint
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf shardCapacity
      `Prelude.seq` Prelude.rnf shardCount
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
