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
-- Module      : Network.AWS.Redshift.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.PendingModifiedValues where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Describes cluster attributes that are in a pending state. A change to
-- one or more the attributes was requested and is in progress or will be
-- applied.
--
-- /See:/ 'newPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { -- | The encryption type for a cluster. Possible values are: KMS and None.
    encryptionType :: Core.Maybe Core.Text,
    -- | An option that specifies whether to create the cluster with enhanced VPC
    -- routing enabled. To create a cluster that uses enhanced VPC routing, the
    -- cluster must be in a VPC. For more information, see
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
    -- in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@, enhanced VPC routing is enabled.
    --
    -- Default: false
    enhancedVpcRouting :: Core.Maybe Core.Bool,
    -- | The pending or in-progress change of the automated snapshot retention
    -- period.
    automatedSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The pending or in-progress change of the master user password for the
    -- cluster.
    masterUserPassword :: Core.Maybe Core.Text,
    -- | The pending or in-progress change of the ability to connect to the
    -- cluster from the public network.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | The pending or in-progress change of the cluster type.
    clusterType :: Core.Maybe Core.Text,
    -- | The pending or in-progress change of the new identifier for the cluster.
    clusterIdentifier :: Core.Maybe Core.Text,
    -- | The pending or in-progress change of the number of nodes in the cluster.
    numberOfNodes :: Core.Maybe Core.Int,
    -- | The pending or in-progress change of the cluster\'s node type.
    nodeType :: Core.Maybe Core.Text,
    -- | The pending or in-progress change of the service version.
    clusterVersion :: Core.Maybe Core.Text,
    -- | The name of the maintenance track that the cluster will change to during
    -- the next maintenance window.
    maintenanceTrackName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'automatedSnapshotRetentionPeriod', 'pendingModifiedValues_automatedSnapshotRetentionPeriod' - The pending or in-progress change of the automated snapshot retention
-- period.
--
-- 'masterUserPassword', 'pendingModifiedValues_masterUserPassword' - The pending or in-progress change of the master user password for the
-- cluster.
--
-- 'publiclyAccessible', 'pendingModifiedValues_publiclyAccessible' - The pending or in-progress change of the ability to connect to the
-- cluster from the public network.
--
-- 'clusterType', 'pendingModifiedValues_clusterType' - The pending or in-progress change of the cluster type.
--
-- 'clusterIdentifier', 'pendingModifiedValues_clusterIdentifier' - The pending or in-progress change of the new identifier for the cluster.
--
-- 'numberOfNodes', 'pendingModifiedValues_numberOfNodes' - The pending or in-progress change of the number of nodes in the cluster.
--
-- 'nodeType', 'pendingModifiedValues_nodeType' - The pending or in-progress change of the cluster\'s node type.
--
-- 'clusterVersion', 'pendingModifiedValues_clusterVersion' - The pending or in-progress change of the service version.
--
-- 'maintenanceTrackName', 'pendingModifiedValues_maintenanceTrackName' - The name of the maintenance track that the cluster will change to during
-- the next maintenance window.
newPendingModifiedValues ::
  PendingModifiedValues
newPendingModifiedValues =
  PendingModifiedValues'
    { encryptionType =
        Core.Nothing,
      enhancedVpcRouting = Core.Nothing,
      automatedSnapshotRetentionPeriod = Core.Nothing,
      masterUserPassword = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      clusterType = Core.Nothing,
      clusterIdentifier = Core.Nothing,
      numberOfNodes = Core.Nothing,
      nodeType = Core.Nothing,
      clusterVersion = Core.Nothing,
      maintenanceTrackName = Core.Nothing
    }

-- | The encryption type for a cluster. Possible values are: KMS and None.
pendingModifiedValues_encryptionType :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
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
pendingModifiedValues_enhancedVpcRouting :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Bool)
pendingModifiedValues_enhancedVpcRouting = Lens.lens (\PendingModifiedValues' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@PendingModifiedValues' {} a -> s {enhancedVpcRouting = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the automated snapshot retention
-- period.
pendingModifiedValues_automatedSnapshotRetentionPeriod :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Int)
pendingModifiedValues_automatedSnapshotRetentionPeriod = Lens.lens (\PendingModifiedValues' {automatedSnapshotRetentionPeriod} -> automatedSnapshotRetentionPeriod) (\s@PendingModifiedValues' {} a -> s {automatedSnapshotRetentionPeriod = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the master user password for the
-- cluster.
pendingModifiedValues_masterUserPassword :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pendingModifiedValues_masterUserPassword = Lens.lens (\PendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@PendingModifiedValues' {} a -> s {masterUserPassword = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the ability to connect to the
-- cluster from the public network.
pendingModifiedValues_publiclyAccessible :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Bool)
pendingModifiedValues_publiclyAccessible = Lens.lens (\PendingModifiedValues' {publiclyAccessible} -> publiclyAccessible) (\s@PendingModifiedValues' {} a -> s {publiclyAccessible = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the cluster type.
pendingModifiedValues_clusterType :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pendingModifiedValues_clusterType = Lens.lens (\PendingModifiedValues' {clusterType} -> clusterType) (\s@PendingModifiedValues' {} a -> s {clusterType = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the new identifier for the cluster.
pendingModifiedValues_clusterIdentifier :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pendingModifiedValues_clusterIdentifier = Lens.lens (\PendingModifiedValues' {clusterIdentifier} -> clusterIdentifier) (\s@PendingModifiedValues' {} a -> s {clusterIdentifier = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the number of nodes in the cluster.
pendingModifiedValues_numberOfNodes :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Int)
pendingModifiedValues_numberOfNodes = Lens.lens (\PendingModifiedValues' {numberOfNodes} -> numberOfNodes) (\s@PendingModifiedValues' {} a -> s {numberOfNodes = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the cluster\'s node type.
pendingModifiedValues_nodeType :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pendingModifiedValues_nodeType = Lens.lens (\PendingModifiedValues' {nodeType} -> nodeType) (\s@PendingModifiedValues' {} a -> s {nodeType = a} :: PendingModifiedValues)

-- | The pending or in-progress change of the service version.
pendingModifiedValues_clusterVersion :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pendingModifiedValues_clusterVersion = Lens.lens (\PendingModifiedValues' {clusterVersion} -> clusterVersion) (\s@PendingModifiedValues' {} a -> s {clusterVersion = a} :: PendingModifiedValues)

-- | The name of the maintenance track that the cluster will change to during
-- the next maintenance window.
pendingModifiedValues_maintenanceTrackName :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pendingModifiedValues_maintenanceTrackName = Lens.lens (\PendingModifiedValues' {maintenanceTrackName} -> maintenanceTrackName) (\s@PendingModifiedValues' {} a -> s {maintenanceTrackName = a} :: PendingModifiedValues)

instance Core.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Core.<$> (x Core..@? "EncryptionType")
      Core.<*> (x Core..@? "EnhancedVpcRouting")
      Core.<*> (x Core..@? "AutomatedSnapshotRetentionPeriod")
      Core.<*> (x Core..@? "MasterUserPassword")
      Core.<*> (x Core..@? "PubliclyAccessible")
      Core.<*> (x Core..@? "ClusterType")
      Core.<*> (x Core..@? "ClusterIdentifier")
      Core.<*> (x Core..@? "NumberOfNodes")
      Core.<*> (x Core..@? "NodeType")
      Core.<*> (x Core..@? "ClusterVersion")
      Core.<*> (x Core..@? "MaintenanceTrackName")

instance Core.Hashable PendingModifiedValues

instance Core.NFData PendingModifiedValues
