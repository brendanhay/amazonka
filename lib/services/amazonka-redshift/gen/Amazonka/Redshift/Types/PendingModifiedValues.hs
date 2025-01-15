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
-- Module      : Amazonka.Redshift.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.PendingModifiedValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes cluster attributes that are in a pending state. A change to
-- one or more the attributes was requested and is in progress or will be
-- applied.
--
-- /See:/ 'newPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { -- | The pending or in-progress change of the automated snapshot retention
    -- period.
    automatedSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The pending or in-progress change of the new identifier for the cluster.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change of the cluster type.
    clusterType :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change of the service version.
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | The encryption type for a cluster. Possible values are: KMS and None.
    encryptionType :: Prelude.Maybe Prelude.Text,
    -- | An option that specifies whether to create the cluster with enhanced VPC
    -- routing enabled. To create a cluster that uses enhanced VPC routing, the
    -- cluster must be in a VPC. For more information, see
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
    -- in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@, enhanced VPC routing is enabled.
    --
    -- Default: false
    enhancedVpcRouting :: Prelude.Maybe Prelude.Bool,
    -- | The name of the maintenance track that the cluster will change to during
    -- the next maintenance window.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change of the admin user password for the
    -- cluster.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change of the cluster\'s node type.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The pending or in-progress change of the number of nodes in the cluster.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The pending or in-progress change of the ability to connect to the
    -- cluster from the public network.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automatedSnapshotRetentionPeriod', 'pendingModifiedValues_automatedSnapshotRetentionPeriod' - The pending or in-progress change of the automated snapshot retention
-- period.
--
-- 'clusterIdentifier', 'pendingModifiedValues_clusterIdentifier' - The pending or in-progress change of the new identifier for the cluster.
--
-- 'clusterType', 'pendingModifiedValues_clusterType' - The pending or in-progress change of the cluster type.
--
-- 'clusterVersion', 'pendingModifiedValues_clusterVersion' - The pending or in-progress change of the service version.
--
-- 'encryptionType', 'pendingModifiedValues_encryptionType' - The encryption type for a cluster. Possible values are: KMS and None.
--
-- 'enhancedVpcRouting', 'pendingModifiedValues_enhancedVpcRouting' - An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
--
-- 'maintenanceTrackName', 'pendingModifiedValues_maintenanceTrackName' - The name of the maintenance track that the cluster will change to during
-- the next maintenance window.
--
-- 'masterUserPassword', 'pendingModifiedValues_masterUserPassword' - The pending or in-progress change of the admin user password for the
-- cluster.
--
-- 'nodeType', 'pendingModifiedValues_nodeType' - The pending or in-progress change of the cluster\'s node type.
--
-- 'numberOfNodes', 'pendingModifiedValues_numberOfNodes' - The pending or in-progress change of the number of nodes in the cluster.
--
-- 'publiclyAccessible', 'pendingModifiedValues_publiclyAccessible' - The pending or in-progress change of the ability to connect to the
-- cluster from the public network.
newPendingModifiedValues ::
  PendingModifiedValues
newPendingModifiedValues =
  PendingModifiedValues'
    { automatedSnapshotRetentionPeriod =
        Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      clusterType = Prelude.Nothing,
      clusterVersion = Prelude.Nothing,
      encryptionType = Prelude.Nothing,
      enhancedVpcRouting = Prelude.Nothing,
      maintenanceTrackName = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing
    }

-- | The pending or in-progress change of the automated snapshot retention
-- period.
pendingModifiedValues_automatedSnapshotRetentionPeriod :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_automatedSnapshotRetentionPeriod = Lens.lens (\PendingModifiedValues' {automatedSnapshotRetentionPeriod} -> automatedSnapshotRetentionPeriod) (\s@PendingModifiedValues' {} a -> s {automatedSnapshotRetentionPeriod = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the new identifier for the cluster.
pendingModifiedValues_clusterIdentifier :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_clusterIdentifier = Lens.lens (\PendingModifiedValues' {clusterIdentifier} -> clusterIdentifier) (\s@PendingModifiedValues' {} a -> s {clusterIdentifier = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the cluster type.
pendingModifiedValues_clusterType :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_clusterType = Lens.lens (\PendingModifiedValues' {clusterType} -> clusterType) (\s@PendingModifiedValues' {} a -> s {clusterType = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the service version.
pendingModifiedValues_clusterVersion :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_clusterVersion = Lens.lens (\PendingModifiedValues' {clusterVersion} -> clusterVersion) (\s@PendingModifiedValues' {} a -> s {clusterVersion = a} :: PendingModifiedValues)

-- | The encryption type for a cluster. Possible values are: KMS and None.
pendingModifiedValues_encryptionType :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_encryptionType = Lens.lens (\PendingModifiedValues' {encryptionType} -> encryptionType) (\s@PendingModifiedValues' {} a -> s {encryptionType = a} :: PendingModifiedValues)

-- | An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
pendingModifiedValues_enhancedVpcRouting :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Bool)
pendingModifiedValues_enhancedVpcRouting = Lens.lens (\PendingModifiedValues' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@PendingModifiedValues' {} a -> s {enhancedVpcRouting = a} :: PendingModifiedValues)

-- | The name of the maintenance track that the cluster will change to during
-- the next maintenance window.
pendingModifiedValues_maintenanceTrackName :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_maintenanceTrackName = Lens.lens (\PendingModifiedValues' {maintenanceTrackName} -> maintenanceTrackName) (\s@PendingModifiedValues' {} a -> s {maintenanceTrackName = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the admin user password for the
-- cluster.
pendingModifiedValues_masterUserPassword :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_masterUserPassword = Lens.lens (\PendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@PendingModifiedValues' {} a -> s {masterUserPassword = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the cluster\'s node type.
pendingModifiedValues_nodeType :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_nodeType = Lens.lens (\PendingModifiedValues' {nodeType} -> nodeType) (\s@PendingModifiedValues' {} a -> s {nodeType = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the number of nodes in the cluster.
pendingModifiedValues_numberOfNodes :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_numberOfNodes = Lens.lens (\PendingModifiedValues' {numberOfNodes} -> numberOfNodes) (\s@PendingModifiedValues' {} a -> s {numberOfNodes = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the ability to connect to the
-- cluster from the public network.
pendingModifiedValues_publiclyAccessible :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Bool)
pendingModifiedValues_publiclyAccessible = Lens.lens (\PendingModifiedValues' {publiclyAccessible} -> publiclyAccessible) (\s@PendingModifiedValues' {} a -> s {publiclyAccessible = a} :: PendingModifiedValues)

instance Data.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Prelude.<$> (x Data..@? "AutomatedSnapshotRetentionPeriod")
      Prelude.<*> (x Data..@? "ClusterIdentifier")
      Prelude.<*> (x Data..@? "ClusterType")
      Prelude.<*> (x Data..@? "ClusterVersion")
      Prelude.<*> (x Data..@? "EncryptionType")
      Prelude.<*> (x Data..@? "EnhancedVpcRouting")
      Prelude.<*> (x Data..@? "MaintenanceTrackName")
      Prelude.<*> (x Data..@? "MasterUserPassword")
      Prelude.<*> (x Data..@? "NodeType")
      Prelude.<*> (x Data..@? "NumberOfNodes")
      Prelude.<*> (x Data..@? "PubliclyAccessible")

instance Prelude.Hashable PendingModifiedValues where
  hashWithSalt _salt PendingModifiedValues' {..} =
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

instance Prelude.NFData PendingModifiedValues where
  rnf PendingModifiedValues' {..} =
    Prelude.rnf automatedSnapshotRetentionPeriod `Prelude.seq`
      Prelude.rnf clusterIdentifier `Prelude.seq`
        Prelude.rnf clusterType `Prelude.seq`
          Prelude.rnf clusterVersion `Prelude.seq`
            Prelude.rnf encryptionType `Prelude.seq`
              Prelude.rnf enhancedVpcRouting `Prelude.seq`
                Prelude.rnf maintenanceTrackName `Prelude.seq`
                  Prelude.rnf masterUserPassword `Prelude.seq`
                    Prelude.rnf nodeType `Prelude.seq`
                      Prelude.rnf numberOfNodes `Prelude.seq`
                        Prelude.rnf publiclyAccessible
