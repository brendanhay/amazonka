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
-- Module      : Network.AWS.CloudHSMv2.Types.Cluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.Cluster where

import Network.AWS.CloudHSMv2.Types.BackupPolicy
import Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy
import Network.AWS.CloudHSMv2.Types.Certificates
import Network.AWS.CloudHSMv2.Types.ClusterState
import Network.AWS.CloudHSMv2.Types.Hsm
import Network.AWS.CloudHSMv2.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about an AWS CloudHSM cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The cluster\'s identifier (ID).
    clusterId :: Core.Maybe Core.Text,
    -- | A description of the cluster\'s state.
    stateMessage :: Core.Maybe Core.Text,
    -- | The cluster\'s backup policy.
    backupPolicy :: Core.Maybe BackupPolicy,
    -- | The date and time when the cluster was created.
    createTimestamp :: Core.Maybe Core.POSIX,
    -- | A map from availability zone to the cluster’s subnet in that
    -- availability zone.
    subnetMapping :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The cluster\'s state.
    state :: Core.Maybe ClusterState,
    -- | The default password for the cluster\'s Pre-Crypto Officer (PRECO) user.
    preCoPassword :: Core.Maybe Core.Text,
    -- | The identifier (ID) of the cluster\'s security group.
    securityGroup :: Core.Maybe Core.Text,
    -- | The type of HSM that the cluster contains.
    hsmType :: Core.Maybe Core.Text,
    -- | The identifier (ID) of the backup used to create the cluster. This value
    -- exists only when the cluster was created from a backup.
    sourceBackupId :: Core.Maybe Core.Text,
    -- | Contains one or more certificates or a certificate signing request
    -- (CSR).
    certificates :: Core.Maybe Certificates,
    -- | The list of tags for the cluster.
    tagList :: Core.Maybe [Tag],
    -- | The identifier (ID) of the virtual private cloud (VPC) that contains the
    -- cluster.
    vpcId :: Core.Maybe Core.Text,
    -- | Contains information about the HSMs in the cluster.
    hsms :: Core.Maybe [Hsm],
    -- | A policy that defines how the service retains backups.
    backupRetentionPolicy :: Core.Maybe BackupRetentionPolicy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Cluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'cluster_clusterId' - The cluster\'s identifier (ID).
--
-- 'stateMessage', 'cluster_stateMessage' - A description of the cluster\'s state.
--
-- 'backupPolicy', 'cluster_backupPolicy' - The cluster\'s backup policy.
--
-- 'createTimestamp', 'cluster_createTimestamp' - The date and time when the cluster was created.
--
-- 'subnetMapping', 'cluster_subnetMapping' - A map from availability zone to the cluster’s subnet in that
-- availability zone.
--
-- 'state', 'cluster_state' - The cluster\'s state.
--
-- 'preCoPassword', 'cluster_preCoPassword' - The default password for the cluster\'s Pre-Crypto Officer (PRECO) user.
--
-- 'securityGroup', 'cluster_securityGroup' - The identifier (ID) of the cluster\'s security group.
--
-- 'hsmType', 'cluster_hsmType' - The type of HSM that the cluster contains.
--
-- 'sourceBackupId', 'cluster_sourceBackupId' - The identifier (ID) of the backup used to create the cluster. This value
-- exists only when the cluster was created from a backup.
--
-- 'certificates', 'cluster_certificates' - Contains one or more certificates or a certificate signing request
-- (CSR).
--
-- 'tagList', 'cluster_tagList' - The list of tags for the cluster.
--
-- 'vpcId', 'cluster_vpcId' - The identifier (ID) of the virtual private cloud (VPC) that contains the
-- cluster.
--
-- 'hsms', 'cluster_hsms' - Contains information about the HSMs in the cluster.
--
-- 'backupRetentionPolicy', 'cluster_backupRetentionPolicy' - A policy that defines how the service retains backups.
newCluster ::
  Cluster
newCluster =
  Cluster'
    { clusterId = Core.Nothing,
      stateMessage = Core.Nothing,
      backupPolicy = Core.Nothing,
      createTimestamp = Core.Nothing,
      subnetMapping = Core.Nothing,
      state = Core.Nothing,
      preCoPassword = Core.Nothing,
      securityGroup = Core.Nothing,
      hsmType = Core.Nothing,
      sourceBackupId = Core.Nothing,
      certificates = Core.Nothing,
      tagList = Core.Nothing,
      vpcId = Core.Nothing,
      hsms = Core.Nothing,
      backupRetentionPolicy = Core.Nothing
    }

-- | The cluster\'s identifier (ID).
cluster_clusterId :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_clusterId = Lens.lens (\Cluster' {clusterId} -> clusterId) (\s@Cluster' {} a -> s {clusterId = a} :: Cluster)

-- | A description of the cluster\'s state.
cluster_stateMessage :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_stateMessage = Lens.lens (\Cluster' {stateMessage} -> stateMessage) (\s@Cluster' {} a -> s {stateMessage = a} :: Cluster)

-- | The cluster\'s backup policy.
cluster_backupPolicy :: Lens.Lens' Cluster (Core.Maybe BackupPolicy)
cluster_backupPolicy = Lens.lens (\Cluster' {backupPolicy} -> backupPolicy) (\s@Cluster' {} a -> s {backupPolicy = a} :: Cluster)

-- | The date and time when the cluster was created.
cluster_createTimestamp :: Lens.Lens' Cluster (Core.Maybe Core.UTCTime)
cluster_createTimestamp = Lens.lens (\Cluster' {createTimestamp} -> createTimestamp) (\s@Cluster' {} a -> s {createTimestamp = a} :: Cluster) Core.. Lens.mapping Core._Time

-- | A map from availability zone to the cluster’s subnet in that
-- availability zone.
cluster_subnetMapping :: Lens.Lens' Cluster (Core.Maybe (Core.HashMap Core.Text Core.Text))
cluster_subnetMapping = Lens.lens (\Cluster' {subnetMapping} -> subnetMapping) (\s@Cluster' {} a -> s {subnetMapping = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | The cluster\'s state.
cluster_state :: Lens.Lens' Cluster (Core.Maybe ClusterState)
cluster_state = Lens.lens (\Cluster' {state} -> state) (\s@Cluster' {} a -> s {state = a} :: Cluster)

-- | The default password for the cluster\'s Pre-Crypto Officer (PRECO) user.
cluster_preCoPassword :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_preCoPassword = Lens.lens (\Cluster' {preCoPassword} -> preCoPassword) (\s@Cluster' {} a -> s {preCoPassword = a} :: Cluster)

-- | The identifier (ID) of the cluster\'s security group.
cluster_securityGroup :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_securityGroup = Lens.lens (\Cluster' {securityGroup} -> securityGroup) (\s@Cluster' {} a -> s {securityGroup = a} :: Cluster)

-- | The type of HSM that the cluster contains.
cluster_hsmType :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_hsmType = Lens.lens (\Cluster' {hsmType} -> hsmType) (\s@Cluster' {} a -> s {hsmType = a} :: Cluster)

-- | The identifier (ID) of the backup used to create the cluster. This value
-- exists only when the cluster was created from a backup.
cluster_sourceBackupId :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_sourceBackupId = Lens.lens (\Cluster' {sourceBackupId} -> sourceBackupId) (\s@Cluster' {} a -> s {sourceBackupId = a} :: Cluster)

-- | Contains one or more certificates or a certificate signing request
-- (CSR).
cluster_certificates :: Lens.Lens' Cluster (Core.Maybe Certificates)
cluster_certificates = Lens.lens (\Cluster' {certificates} -> certificates) (\s@Cluster' {} a -> s {certificates = a} :: Cluster)

-- | The list of tags for the cluster.
cluster_tagList :: Lens.Lens' Cluster (Core.Maybe [Tag])
cluster_tagList = Lens.lens (\Cluster' {tagList} -> tagList) (\s@Cluster' {} a -> s {tagList = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | The identifier (ID) of the virtual private cloud (VPC) that contains the
-- cluster.
cluster_vpcId :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cluster_vpcId = Lens.lens (\Cluster' {vpcId} -> vpcId) (\s@Cluster' {} a -> s {vpcId = a} :: Cluster)

-- | Contains information about the HSMs in the cluster.
cluster_hsms :: Lens.Lens' Cluster (Core.Maybe [Hsm])
cluster_hsms = Lens.lens (\Cluster' {hsms} -> hsms) (\s@Cluster' {} a -> s {hsms = a} :: Cluster) Core.. Lens.mapping Lens._Coerce

-- | A policy that defines how the service retains backups.
cluster_backupRetentionPolicy :: Lens.Lens' Cluster (Core.Maybe BackupRetentionPolicy)
cluster_backupRetentionPolicy = Lens.lens (\Cluster' {backupRetentionPolicy} -> backupRetentionPolicy) (\s@Cluster' {} a -> s {backupRetentionPolicy = a} :: Cluster)

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Core.<$> (x Core..:? "ClusterId")
            Core.<*> (x Core..:? "StateMessage")
            Core.<*> (x Core..:? "BackupPolicy")
            Core.<*> (x Core..:? "CreateTimestamp")
            Core.<*> (x Core..:? "SubnetMapping" Core..!= Core.mempty)
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "PreCoPassword")
            Core.<*> (x Core..:? "SecurityGroup")
            Core.<*> (x Core..:? "HsmType")
            Core.<*> (x Core..:? "SourceBackupId")
            Core.<*> (x Core..:? "Certificates")
            Core.<*> (x Core..:? "TagList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "VpcId")
            Core.<*> (x Core..:? "Hsms" Core..!= Core.mempty)
            Core.<*> (x Core..:? "BackupRetentionPolicy")
      )

instance Core.Hashable Cluster

instance Core.NFData Cluster
