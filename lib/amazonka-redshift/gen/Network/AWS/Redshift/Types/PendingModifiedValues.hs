{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.PendingModifiedValues
  ( PendingModifiedValues (..),

    -- * Smart constructor
    mkPendingModifiedValues,

    -- * Lenses
    pmvEncryptionType,
    pmvEnhancedVPCRouting,
    pmvMasterUserPassword,
    pmvPubliclyAccessible,
    pmvMaintenanceTrackName,
    pmvAutomatedSnapshotRetentionPeriod,
    pmvClusterIdentifier,
    pmvNumberOfNodes,
    pmvClusterType,
    pmvClusterVersion,
    pmvNodeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes cluster attributes that are in a pending state. A change to one or more the attributes was requested and is in progress or will be applied.
--
-- /See:/ 'mkPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { -- | The encryption type for a cluster. Possible values are: KMS and None.
    encryptionType :: Lude.Maybe Lude.Text,
    -- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@ , enhanced VPC routing is enabled.
    -- Default: false
    enhancedVPCRouting :: Lude.Maybe Lude.Bool,
    -- | The pending or in-progress change of the master user password for the cluster.
    masterUserPassword :: Lude.Maybe Lude.Text,
    -- | The pending or in-progress change of the ability to connect to the cluster from the public network.
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    -- | The name of the maintenance track that the cluster will change to during the next maintenance window.
    maintenanceTrackName :: Lude.Maybe Lude.Text,
    -- | The pending or in-progress change of the automated snapshot retention period.
    automatedSnapshotRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | The pending or in-progress change of the new identifier for the cluster.
    clusterIdentifier :: Lude.Maybe Lude.Text,
    -- | The pending or in-progress change of the number of nodes in the cluster.
    numberOfNodes :: Lude.Maybe Lude.Int,
    -- | The pending or in-progress change of the cluster type.
    clusterType :: Lude.Maybe Lude.Text,
    -- | The pending or in-progress change of the service version.
    clusterVersion :: Lude.Maybe Lude.Text,
    -- | The pending or in-progress change of the cluster's node type.
    nodeType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PendingModifiedValues' with the minimum fields required to make a request.
--
-- * 'encryptionType' - The encryption type for a cluster. Possible values are: KMS and None.
-- * 'enhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
-- * 'masterUserPassword' - The pending or in-progress change of the master user password for the cluster.
-- * 'publiclyAccessible' - The pending or in-progress change of the ability to connect to the cluster from the public network.
-- * 'maintenanceTrackName' - The name of the maintenance track that the cluster will change to during the next maintenance window.
-- * 'automatedSnapshotRetentionPeriod' - The pending or in-progress change of the automated snapshot retention period.
-- * 'clusterIdentifier' - The pending or in-progress change of the new identifier for the cluster.
-- * 'numberOfNodes' - The pending or in-progress change of the number of nodes in the cluster.
-- * 'clusterType' - The pending or in-progress change of the cluster type.
-- * 'clusterVersion' - The pending or in-progress change of the service version.
-- * 'nodeType' - The pending or in-progress change of the cluster's node type.
mkPendingModifiedValues ::
  PendingModifiedValues
mkPendingModifiedValues =
  PendingModifiedValues'
    { encryptionType = Lude.Nothing,
      enhancedVPCRouting = Lude.Nothing,
      masterUserPassword = Lude.Nothing,
      publiclyAccessible = Lude.Nothing,
      maintenanceTrackName = Lude.Nothing,
      automatedSnapshotRetentionPeriod = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      numberOfNodes = Lude.Nothing,
      clusterType = Lude.Nothing,
      clusterVersion = Lude.Nothing,
      nodeType = Lude.Nothing
    }

-- | The encryption type for a cluster. Possible values are: KMS and None.
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvEncryptionType :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvEncryptionType = Lens.lens (encryptionType :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {encryptionType = a} :: PendingModifiedValues)
{-# DEPRECATED pmvEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
--
-- /Note:/ Consider using 'enhancedVPCRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvEnhancedVPCRouting :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Bool)
pmvEnhancedVPCRouting = Lens.lens (enhancedVPCRouting :: PendingModifiedValues -> Lude.Maybe Lude.Bool) (\s a -> s {enhancedVPCRouting = a} :: PendingModifiedValues)
{-# DEPRECATED pmvEnhancedVPCRouting "Use generic-lens or generic-optics with 'enhancedVPCRouting' instead." #-}

-- | The pending or in-progress change of the master user password for the cluster.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvMasterUserPassword :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvMasterUserPassword = Lens.lens (masterUserPassword :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {masterUserPassword = a} :: PendingModifiedValues)
{-# DEPRECATED pmvMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The pending or in-progress change of the ability to connect to the cluster from the public network.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvPubliclyAccessible :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Bool)
pmvPubliclyAccessible = Lens.lens (publiclyAccessible :: PendingModifiedValues -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: PendingModifiedValues)
{-# DEPRECATED pmvPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | The name of the maintenance track that the cluster will change to during the next maintenance window.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvMaintenanceTrackName :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvMaintenanceTrackName = Lens.lens (maintenanceTrackName :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {maintenanceTrackName = a} :: PendingModifiedValues)
{-# DEPRECATED pmvMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | The pending or in-progress change of the automated snapshot retention period.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvAutomatedSnapshotRetentionPeriod :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Int)
pmvAutomatedSnapshotRetentionPeriod = Lens.lens (automatedSnapshotRetentionPeriod :: PendingModifiedValues -> Lude.Maybe Lude.Int) (\s a -> s {automatedSnapshotRetentionPeriod = a} :: PendingModifiedValues)
{-# DEPRECATED pmvAutomatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead." #-}

-- | The pending or in-progress change of the new identifier for the cluster.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvClusterIdentifier :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvClusterIdentifier = Lens.lens (clusterIdentifier :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: PendingModifiedValues)
{-# DEPRECATED pmvClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The pending or in-progress change of the number of nodes in the cluster.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvNumberOfNodes :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Int)
pmvNumberOfNodes = Lens.lens (numberOfNodes :: PendingModifiedValues -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: PendingModifiedValues)
{-# DEPRECATED pmvNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The pending or in-progress change of the cluster type.
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvClusterType :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvClusterType = Lens.lens (clusterType :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {clusterType = a} :: PendingModifiedValues)
{-# DEPRECATED pmvClusterType "Use generic-lens or generic-optics with 'clusterType' instead." #-}

-- | The pending or in-progress change of the service version.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvClusterVersion :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvClusterVersion = Lens.lens (clusterVersion :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {clusterVersion = a} :: PendingModifiedValues)
{-# DEPRECATED pmvClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | The pending or in-progress change of the cluster's node type.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvNodeType :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvNodeType = Lens.lens (nodeType :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: PendingModifiedValues)
{-# DEPRECATED pmvNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

instance Lude.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Lude.<$> (x Lude..@? "EncryptionType")
      Lude.<*> (x Lude..@? "EnhancedVpcRouting")
      Lude.<*> (x Lude..@? "MasterUserPassword")
      Lude.<*> (x Lude..@? "PubliclyAccessible")
      Lude.<*> (x Lude..@? "MaintenanceTrackName")
      Lude.<*> (x Lude..@? "AutomatedSnapshotRetentionPeriod")
      Lude.<*> (x Lude..@? "ClusterIdentifier")
      Lude.<*> (x Lude..@? "NumberOfNodes")
      Lude.<*> (x Lude..@? "ClusterType")
      Lude.<*> (x Lude..@? "ClusterVersion")
      Lude.<*> (x Lude..@? "NodeType")
