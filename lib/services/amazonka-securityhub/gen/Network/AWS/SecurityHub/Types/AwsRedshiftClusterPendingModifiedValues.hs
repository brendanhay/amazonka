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
-- Module      : Network.AWS.SecurityHub.Types.AwsRedshiftClusterPendingModifiedValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsRedshiftClusterPendingModifiedValues where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Changes to the Amazon Redshift cluster that are currently pending.
--
-- /See:/ 'newAwsRedshiftClusterPendingModifiedValues' smart constructor.
data AwsRedshiftClusterPendingModifiedValues = AwsRedshiftClusterPendingModifiedValues'
  { -- | The encryption type for a cluster.
    encryptionType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to create the cluster with enhanced VPC routing
    -- enabled.
    enhancedVpcRouting :: Prelude.Maybe Prelude.Bool,
    -- | The pending or in-progress change to the master user password for the
    -- cluster.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change to whether the cluster can be
    -- connected to from the public network.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The name of the maintenance track that the cluster changes to during the
    -- next maintenance window.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change to the automated snapshot retention
    -- period.
    automatedSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The pending or in-progress change to the identifier for the cluster.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change to the number of nodes in the cluster.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The pending or in-progress change to the cluster type.
    clusterType :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change to the service version.
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change to the cluster\'s node type.
    nodeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterPendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 'awsRedshiftClusterPendingModifiedValues_encryptionType' - The encryption type for a cluster.
--
-- 'enhancedVpcRouting', 'awsRedshiftClusterPendingModifiedValues_enhancedVpcRouting' - Indicates whether to create the cluster with enhanced VPC routing
-- enabled.
--
-- 'masterUserPassword', 'awsRedshiftClusterPendingModifiedValues_masterUserPassword' - The pending or in-progress change to the master user password for the
-- cluster.
--
-- 'publiclyAccessible', 'awsRedshiftClusterPendingModifiedValues_publiclyAccessible' - The pending or in-progress change to whether the cluster can be
-- connected to from the public network.
--
-- 'maintenanceTrackName', 'awsRedshiftClusterPendingModifiedValues_maintenanceTrackName' - The name of the maintenance track that the cluster changes to during the
-- next maintenance window.
--
-- 'automatedSnapshotRetentionPeriod', 'awsRedshiftClusterPendingModifiedValues_automatedSnapshotRetentionPeriod' - The pending or in-progress change to the automated snapshot retention
-- period.
--
-- 'clusterIdentifier', 'awsRedshiftClusterPendingModifiedValues_clusterIdentifier' - The pending or in-progress change to the identifier for the cluster.
--
-- 'numberOfNodes', 'awsRedshiftClusterPendingModifiedValues_numberOfNodes' - The pending or in-progress change to the number of nodes in the cluster.
--
-- 'clusterType', 'awsRedshiftClusterPendingModifiedValues_clusterType' - The pending or in-progress change to the cluster type.
--
-- 'clusterVersion', 'awsRedshiftClusterPendingModifiedValues_clusterVersion' - The pending or in-progress change to the service version.
--
-- 'nodeType', 'awsRedshiftClusterPendingModifiedValues_nodeType' - The pending or in-progress change to the cluster\'s node type.
newAwsRedshiftClusterPendingModifiedValues ::
  AwsRedshiftClusterPendingModifiedValues
newAwsRedshiftClusterPendingModifiedValues =
  AwsRedshiftClusterPendingModifiedValues'
    { encryptionType =
        Prelude.Nothing,
      enhancedVpcRouting =
        Prelude.Nothing,
      masterUserPassword =
        Prelude.Nothing,
      publiclyAccessible =
        Prelude.Nothing,
      maintenanceTrackName =
        Prelude.Nothing,
      automatedSnapshotRetentionPeriod =
        Prelude.Nothing,
      clusterIdentifier =
        Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      clusterType = Prelude.Nothing,
      clusterVersion = Prelude.Nothing,
      nodeType = Prelude.Nothing
    }

-- | The encryption type for a cluster.
awsRedshiftClusterPendingModifiedValues_encryptionType :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_encryptionType = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {encryptionType} -> encryptionType) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {encryptionType = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | Indicates whether to create the cluster with enhanced VPC routing
-- enabled.
awsRedshiftClusterPendingModifiedValues_enhancedVpcRouting :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterPendingModifiedValues_enhancedVpcRouting = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {enhancedVpcRouting = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the master user password for the
-- cluster.
awsRedshiftClusterPendingModifiedValues_masterUserPassword :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_masterUserPassword = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {masterUserPassword = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to whether the cluster can be
-- connected to from the public network.
awsRedshiftClusterPendingModifiedValues_publiclyAccessible :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterPendingModifiedValues_publiclyAccessible = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {publiclyAccessible} -> publiclyAccessible) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {publiclyAccessible = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The name of the maintenance track that the cluster changes to during the
-- next maintenance window.
awsRedshiftClusterPendingModifiedValues_maintenanceTrackName :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_maintenanceTrackName = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {maintenanceTrackName} -> maintenanceTrackName) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {maintenanceTrackName = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the automated snapshot retention
-- period.
awsRedshiftClusterPendingModifiedValues_automatedSnapshotRetentionPeriod :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRedshiftClusterPendingModifiedValues_automatedSnapshotRetentionPeriod = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {automatedSnapshotRetentionPeriod} -> automatedSnapshotRetentionPeriod) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {automatedSnapshotRetentionPeriod = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the identifier for the cluster.
awsRedshiftClusterPendingModifiedValues_clusterIdentifier :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_clusterIdentifier = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {clusterIdentifier} -> clusterIdentifier) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {clusterIdentifier = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the number of nodes in the cluster.
awsRedshiftClusterPendingModifiedValues_numberOfNodes :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRedshiftClusterPendingModifiedValues_numberOfNodes = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {numberOfNodes} -> numberOfNodes) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {numberOfNodes = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the cluster type.
awsRedshiftClusterPendingModifiedValues_clusterType :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_clusterType = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {clusterType} -> clusterType) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {clusterType = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the service version.
awsRedshiftClusterPendingModifiedValues_clusterVersion :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_clusterVersion = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {clusterVersion} -> clusterVersion) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {clusterVersion = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the cluster\'s node type.
awsRedshiftClusterPendingModifiedValues_nodeType :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_nodeType = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {nodeType} -> nodeType) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {nodeType = a} :: AwsRedshiftClusterPendingModifiedValues)

instance
  Core.FromJSON
    AwsRedshiftClusterPendingModifiedValues
  where
  parseJSON =
    Core.withObject
      "AwsRedshiftClusterPendingModifiedValues"
      ( \x ->
          AwsRedshiftClusterPendingModifiedValues'
            Prelude.<$> (x Core..:? "EncryptionType")
            Prelude.<*> (x Core..:? "EnhancedVpcRouting")
            Prelude.<*> (x Core..:? "MasterUserPassword")
            Prelude.<*> (x Core..:? "PubliclyAccessible")
            Prelude.<*> (x Core..:? "MaintenanceTrackName")
            Prelude.<*> (x Core..:? "AutomatedSnapshotRetentionPeriod")
            Prelude.<*> (x Core..:? "ClusterIdentifier")
            Prelude.<*> (x Core..:? "NumberOfNodes")
            Prelude.<*> (x Core..:? "ClusterType")
            Prelude.<*> (x Core..:? "ClusterVersion")
            Prelude.<*> (x Core..:? "NodeType")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterPendingModifiedValues

instance
  Prelude.NFData
    AwsRedshiftClusterPendingModifiedValues

instance
  Core.ToJSON
    AwsRedshiftClusterPendingModifiedValues
  where
  toJSON AwsRedshiftClusterPendingModifiedValues' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EncryptionType" Core..=)
              Prelude.<$> encryptionType,
            ("EnhancedVpcRouting" Core..=)
              Prelude.<$> enhancedVpcRouting,
            ("MasterUserPassword" Core..=)
              Prelude.<$> masterUserPassword,
            ("PubliclyAccessible" Core..=)
              Prelude.<$> publiclyAccessible,
            ("MaintenanceTrackName" Core..=)
              Prelude.<$> maintenanceTrackName,
            ("AutomatedSnapshotRetentionPeriod" Core..=)
              Prelude.<$> automatedSnapshotRetentionPeriod,
            ("ClusterIdentifier" Core..=)
              Prelude.<$> clusterIdentifier,
            ("NumberOfNodes" Core..=) Prelude.<$> numberOfNodes,
            ("ClusterType" Core..=) Prelude.<$> clusterType,
            ("ClusterVersion" Core..=)
              Prelude.<$> clusterVersion,
            ("NodeType" Core..=) Prelude.<$> nodeType
          ]
      )
