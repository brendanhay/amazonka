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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterPendingModifiedValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterPendingModifiedValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Changes to the Amazon Redshift cluster that are currently pending.
--
-- /See:/ 'newAwsRedshiftClusterPendingModifiedValues' smart constructor.
data AwsRedshiftClusterPendingModifiedValues = AwsRedshiftClusterPendingModifiedValues'
  { -- | The pending or in-progress change to the automated snapshot retention
    -- period.
    automatedSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The pending or in-progress change to the identifier for the cluster.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change to the cluster type.
    clusterType :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change to the service version.
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | The encryption type for a cluster.
    encryptionType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to create the cluster with enhanced VPC routing
    -- enabled.
    enhancedVpcRouting :: Prelude.Maybe Prelude.Bool,
    -- | The name of the maintenance track that the cluster changes to during the
    -- next maintenance window.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change to the master user password for the
    -- cluster.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change to the cluster\'s node type.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change to the number of nodes in the cluster.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The pending or in-progress change to whether the cluster can be
    -- connected to from the public network.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool
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
-- 'automatedSnapshotRetentionPeriod', 'awsRedshiftClusterPendingModifiedValues_automatedSnapshotRetentionPeriod' - The pending or in-progress change to the automated snapshot retention
-- period.
--
-- 'clusterIdentifier', 'awsRedshiftClusterPendingModifiedValues_clusterIdentifier' - The pending or in-progress change to the identifier for the cluster.
--
-- 'clusterType', 'awsRedshiftClusterPendingModifiedValues_clusterType' - The pending or in-progress change to the cluster type.
--
-- 'clusterVersion', 'awsRedshiftClusterPendingModifiedValues_clusterVersion' - The pending or in-progress change to the service version.
--
-- 'encryptionType', 'awsRedshiftClusterPendingModifiedValues_encryptionType' - The encryption type for a cluster.
--
-- 'enhancedVpcRouting', 'awsRedshiftClusterPendingModifiedValues_enhancedVpcRouting' - Indicates whether to create the cluster with enhanced VPC routing
-- enabled.
--
-- 'maintenanceTrackName', 'awsRedshiftClusterPendingModifiedValues_maintenanceTrackName' - The name of the maintenance track that the cluster changes to during the
-- next maintenance window.
--
-- 'masterUserPassword', 'awsRedshiftClusterPendingModifiedValues_masterUserPassword' - The pending or in-progress change to the master user password for the
-- cluster.
--
-- 'nodeType', 'awsRedshiftClusterPendingModifiedValues_nodeType' - The pending or in-progress change to the cluster\'s node type.
--
-- 'numberOfNodes', 'awsRedshiftClusterPendingModifiedValues_numberOfNodes' - The pending or in-progress change to the number of nodes in the cluster.
--
-- 'publiclyAccessible', 'awsRedshiftClusterPendingModifiedValues_publiclyAccessible' - The pending or in-progress change to whether the cluster can be
-- connected to from the public network.
newAwsRedshiftClusterPendingModifiedValues ::
  AwsRedshiftClusterPendingModifiedValues
newAwsRedshiftClusterPendingModifiedValues =
  AwsRedshiftClusterPendingModifiedValues'
    { automatedSnapshotRetentionPeriod =
        Prelude.Nothing,
      clusterIdentifier =
        Prelude.Nothing,
      clusterType = Prelude.Nothing,
      clusterVersion = Prelude.Nothing,
      encryptionType = Prelude.Nothing,
      enhancedVpcRouting =
        Prelude.Nothing,
      maintenanceTrackName =
        Prelude.Nothing,
      masterUserPassword =
        Prelude.Nothing,
      nodeType = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      publiclyAccessible =
        Prelude.Nothing
    }

-- | The pending or in-progress change to the automated snapshot retention
-- period.
awsRedshiftClusterPendingModifiedValues_automatedSnapshotRetentionPeriod :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRedshiftClusterPendingModifiedValues_automatedSnapshotRetentionPeriod = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {automatedSnapshotRetentionPeriod} -> automatedSnapshotRetentionPeriod) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {automatedSnapshotRetentionPeriod = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the identifier for the cluster.
awsRedshiftClusterPendingModifiedValues_clusterIdentifier :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_clusterIdentifier = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {clusterIdentifier} -> clusterIdentifier) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {clusterIdentifier = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the cluster type.
awsRedshiftClusterPendingModifiedValues_clusterType :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_clusterType = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {clusterType} -> clusterType) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {clusterType = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the service version.
awsRedshiftClusterPendingModifiedValues_clusterVersion :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_clusterVersion = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {clusterVersion} -> clusterVersion) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {clusterVersion = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The encryption type for a cluster.
awsRedshiftClusterPendingModifiedValues_encryptionType :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_encryptionType = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {encryptionType} -> encryptionType) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {encryptionType = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | Indicates whether to create the cluster with enhanced VPC routing
-- enabled.
awsRedshiftClusterPendingModifiedValues_enhancedVpcRouting :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterPendingModifiedValues_enhancedVpcRouting = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {enhancedVpcRouting = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The name of the maintenance track that the cluster changes to during the
-- next maintenance window.
awsRedshiftClusterPendingModifiedValues_maintenanceTrackName :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_maintenanceTrackName = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {maintenanceTrackName} -> maintenanceTrackName) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {maintenanceTrackName = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the master user password for the
-- cluster.
awsRedshiftClusterPendingModifiedValues_masterUserPassword :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_masterUserPassword = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {masterUserPassword = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the cluster\'s node type.
awsRedshiftClusterPendingModifiedValues_nodeType :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRedshiftClusterPendingModifiedValues_nodeType = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {nodeType} -> nodeType) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {nodeType = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to the number of nodes in the cluster.
awsRedshiftClusterPendingModifiedValues_numberOfNodes :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRedshiftClusterPendingModifiedValues_numberOfNodes = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {numberOfNodes} -> numberOfNodes) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {numberOfNodes = a} :: AwsRedshiftClusterPendingModifiedValues)

-- | The pending or in-progress change to whether the cluster can be
-- connected to from the public network.
awsRedshiftClusterPendingModifiedValues_publiclyAccessible :: Lens.Lens' AwsRedshiftClusterPendingModifiedValues (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterPendingModifiedValues_publiclyAccessible = Lens.lens (\AwsRedshiftClusterPendingModifiedValues' {publiclyAccessible} -> publiclyAccessible) (\s@AwsRedshiftClusterPendingModifiedValues' {} a -> s {publiclyAccessible = a} :: AwsRedshiftClusterPendingModifiedValues)

instance
  Data.FromJSON
    AwsRedshiftClusterPendingModifiedValues
  where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterPendingModifiedValues"
      ( \x ->
          AwsRedshiftClusterPendingModifiedValues'
            Prelude.<$> (x Data..:? "AutomatedSnapshotRetentionPeriod")
            Prelude.<*> (x Data..:? "ClusterIdentifier")
            Prelude.<*> (x Data..:? "ClusterType")
            Prelude.<*> (x Data..:? "ClusterVersion")
            Prelude.<*> (x Data..:? "EncryptionType")
            Prelude.<*> (x Data..:? "EnhancedVpcRouting")
            Prelude.<*> (x Data..:? "MaintenanceTrackName")
            Prelude.<*> (x Data..:? "MasterUserPassword")
            Prelude.<*> (x Data..:? "NodeType")
            Prelude.<*> (x Data..:? "NumberOfNodes")
            Prelude.<*> (x Data..:? "PubliclyAccessible")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterPendingModifiedValues
  where
  hashWithSalt
    _salt
    AwsRedshiftClusterPendingModifiedValues' {..} =
      _salt
        `Prelude.hashWithSalt` automatedSnapshotRetentionPeriod
        `Prelude.hashWithSalt` clusterIdentifier
        `Prelude.hashWithSalt` clusterType
        `Prelude.hashWithSalt` clusterVersion
        `Prelude.hashWithSalt` encryptionType
        `Prelude.hashWithSalt` enhancedVpcRouting
        `Prelude.hashWithSalt` maintenanceTrackName
        `Prelude.hashWithSalt` masterUserPassword
        `Prelude.hashWithSalt` nodeType
        `Prelude.hashWithSalt` numberOfNodes
        `Prelude.hashWithSalt` publiclyAccessible

instance
  Prelude.NFData
    AwsRedshiftClusterPendingModifiedValues
  where
  rnf AwsRedshiftClusterPendingModifiedValues' {..} =
    Prelude.rnf automatedSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf clusterType
      `Prelude.seq` Prelude.rnf clusterVersion
      `Prelude.seq` Prelude.rnf encryptionType
      `Prelude.seq` Prelude.rnf enhancedVpcRouting
      `Prelude.seq` Prelude.rnf maintenanceTrackName
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf numberOfNodes
      `Prelude.seq` Prelude.rnf publiclyAccessible

instance
  Data.ToJSON
    AwsRedshiftClusterPendingModifiedValues
  where
  toJSON AwsRedshiftClusterPendingModifiedValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutomatedSnapshotRetentionPeriod" Data..=)
              Prelude.<$> automatedSnapshotRetentionPeriod,
            ("ClusterIdentifier" Data..=)
              Prelude.<$> clusterIdentifier,
            ("ClusterType" Data..=) Prelude.<$> clusterType,
            ("ClusterVersion" Data..=)
              Prelude.<$> clusterVersion,
            ("EncryptionType" Data..=)
              Prelude.<$> encryptionType,
            ("EnhancedVpcRouting" Data..=)
              Prelude.<$> enhancedVpcRouting,
            ("MaintenanceTrackName" Data..=)
              Prelude.<$> maintenanceTrackName,
            ("MasterUserPassword" Data..=)
              Prelude.<$> masterUserPassword,
            ("NodeType" Data..=) Prelude.<$> nodeType,
            ("NumberOfNodes" Data..=) Prelude.<$> numberOfNodes,
            ("PubliclyAccessible" Data..=)
              Prelude.<$> publiclyAccessible
          ]
      )
