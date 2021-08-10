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
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about an AWS CloudHSM cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The cluster\'s identifier (ID).
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | A description of the cluster\'s state.
    stateMessage :: Prelude.Maybe Prelude.Text,
    -- | The cluster\'s backup policy.
    backupPolicy :: Prelude.Maybe BackupPolicy,
    -- | The date and time when the cluster was created.
    createTimestamp :: Prelude.Maybe Core.POSIX,
    -- | A map from availability zone to the cluster’s subnet in that
    -- availability zone.
    subnetMapping :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The cluster\'s state.
    state :: Prelude.Maybe ClusterState,
    -- | The default password for the cluster\'s Pre-Crypto Officer (PRECO) user.
    preCoPassword :: Prelude.Maybe Prelude.Text,
    -- | The identifier (ID) of the cluster\'s security group.
    securityGroup :: Prelude.Maybe Prelude.Text,
    -- | The type of HSM that the cluster contains.
    hsmType :: Prelude.Maybe Prelude.Text,
    -- | The identifier (ID) of the backup used to create the cluster. This value
    -- exists only when the cluster was created from a backup.
    sourceBackupId :: Prelude.Maybe Prelude.Text,
    -- | Contains one or more certificates or a certificate signing request
    -- (CSR).
    certificates :: Prelude.Maybe Certificates,
    -- | The list of tags for the cluster.
    tagList :: Prelude.Maybe [Tag],
    -- | The identifier (ID) of the virtual private cloud (VPC) that contains the
    -- cluster.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the HSMs in the cluster.
    hsms :: Prelude.Maybe [Hsm],
    -- | A policy that defines how the service retains backups.
    backupRetentionPolicy :: Prelude.Maybe BackupRetentionPolicy
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
    { clusterId = Prelude.Nothing,
      stateMessage = Prelude.Nothing,
      backupPolicy = Prelude.Nothing,
      createTimestamp = Prelude.Nothing,
      subnetMapping = Prelude.Nothing,
      state = Prelude.Nothing,
      preCoPassword = Prelude.Nothing,
      securityGroup = Prelude.Nothing,
      hsmType = Prelude.Nothing,
      sourceBackupId = Prelude.Nothing,
      certificates = Prelude.Nothing,
      tagList = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      hsms = Prelude.Nothing,
      backupRetentionPolicy = Prelude.Nothing
    }

-- | The cluster\'s identifier (ID).
cluster_clusterId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_clusterId = Lens.lens (\Cluster' {clusterId} -> clusterId) (\s@Cluster' {} a -> s {clusterId = a} :: Cluster)

-- | A description of the cluster\'s state.
cluster_stateMessage :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_stateMessage = Lens.lens (\Cluster' {stateMessage} -> stateMessage) (\s@Cluster' {} a -> s {stateMessage = a} :: Cluster)

-- | The cluster\'s backup policy.
cluster_backupPolicy :: Lens.Lens' Cluster (Prelude.Maybe BackupPolicy)
cluster_backupPolicy = Lens.lens (\Cluster' {backupPolicy} -> backupPolicy) (\s@Cluster' {} a -> s {backupPolicy = a} :: Cluster)

-- | The date and time when the cluster was created.
cluster_createTimestamp :: Lens.Lens' Cluster (Prelude.Maybe Prelude.UTCTime)
cluster_createTimestamp = Lens.lens (\Cluster' {createTimestamp} -> createTimestamp) (\s@Cluster' {} a -> s {createTimestamp = a} :: Cluster) Prelude.. Lens.mapping Core._Time

-- | A map from availability zone to the cluster’s subnet in that
-- availability zone.
cluster_subnetMapping :: Lens.Lens' Cluster (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
cluster_subnetMapping = Lens.lens (\Cluster' {subnetMapping} -> subnetMapping) (\s@Cluster' {} a -> s {subnetMapping = a} :: Cluster) Prelude.. Lens.mapping Lens._Coerce

-- | The cluster\'s state.
cluster_state :: Lens.Lens' Cluster (Prelude.Maybe ClusterState)
cluster_state = Lens.lens (\Cluster' {state} -> state) (\s@Cluster' {} a -> s {state = a} :: Cluster)

-- | The default password for the cluster\'s Pre-Crypto Officer (PRECO) user.
cluster_preCoPassword :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_preCoPassword = Lens.lens (\Cluster' {preCoPassword} -> preCoPassword) (\s@Cluster' {} a -> s {preCoPassword = a} :: Cluster)

-- | The identifier (ID) of the cluster\'s security group.
cluster_securityGroup :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_securityGroup = Lens.lens (\Cluster' {securityGroup} -> securityGroup) (\s@Cluster' {} a -> s {securityGroup = a} :: Cluster)

-- | The type of HSM that the cluster contains.
cluster_hsmType :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_hsmType = Lens.lens (\Cluster' {hsmType} -> hsmType) (\s@Cluster' {} a -> s {hsmType = a} :: Cluster)

-- | The identifier (ID) of the backup used to create the cluster. This value
-- exists only when the cluster was created from a backup.
cluster_sourceBackupId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_sourceBackupId = Lens.lens (\Cluster' {sourceBackupId} -> sourceBackupId) (\s@Cluster' {} a -> s {sourceBackupId = a} :: Cluster)

-- | Contains one or more certificates or a certificate signing request
-- (CSR).
cluster_certificates :: Lens.Lens' Cluster (Prelude.Maybe Certificates)
cluster_certificates = Lens.lens (\Cluster' {certificates} -> certificates) (\s@Cluster' {} a -> s {certificates = a} :: Cluster)

-- | The list of tags for the cluster.
cluster_tagList :: Lens.Lens' Cluster (Prelude.Maybe [Tag])
cluster_tagList = Lens.lens (\Cluster' {tagList} -> tagList) (\s@Cluster' {} a -> s {tagList = a} :: Cluster) Prelude.. Lens.mapping Lens._Coerce

-- | The identifier (ID) of the virtual private cloud (VPC) that contains the
-- cluster.
cluster_vpcId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_vpcId = Lens.lens (\Cluster' {vpcId} -> vpcId) (\s@Cluster' {} a -> s {vpcId = a} :: Cluster)

-- | Contains information about the HSMs in the cluster.
cluster_hsms :: Lens.Lens' Cluster (Prelude.Maybe [Hsm])
cluster_hsms = Lens.lens (\Cluster' {hsms} -> hsms) (\s@Cluster' {} a -> s {hsms = a} :: Cluster) Prelude.. Lens.mapping Lens._Coerce

-- | A policy that defines how the service retains backups.
cluster_backupRetentionPolicy :: Lens.Lens' Cluster (Prelude.Maybe BackupRetentionPolicy)
cluster_backupRetentionPolicy = Lens.lens (\Cluster' {backupRetentionPolicy} -> backupRetentionPolicy) (\s@Cluster' {} a -> s {backupRetentionPolicy = a} :: Cluster)

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Core..:? "ClusterId")
            Prelude.<*> (x Core..:? "StateMessage")
            Prelude.<*> (x Core..:? "BackupPolicy")
            Prelude.<*> (x Core..:? "CreateTimestamp")
            Prelude.<*> (x Core..:? "SubnetMapping" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "PreCoPassword")
            Prelude.<*> (x Core..:? "SecurityGroup")
            Prelude.<*> (x Core..:? "HsmType")
            Prelude.<*> (x Core..:? "SourceBackupId")
            Prelude.<*> (x Core..:? "Certificates")
            Prelude.<*> (x Core..:? "TagList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "Hsms" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "BackupRetentionPolicy")
      )

instance Prelude.Hashable Cluster

instance Prelude.NFData Cluster
