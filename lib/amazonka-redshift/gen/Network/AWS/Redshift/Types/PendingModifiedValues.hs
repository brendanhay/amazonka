{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.PendingModifiedValues
  ( PendingModifiedValues (..)
  -- * Smart constructor
  , mkPendingModifiedValues
  -- * Lenses
  , pmvAutomatedSnapshotRetentionPeriod
  , pmvClusterIdentifier
  , pmvClusterType
  , pmvClusterVersion
  , pmvEncryptionType
  , pmvEnhancedVpcRouting
  , pmvMaintenanceTrackName
  , pmvMasterUserPassword
  , pmvNodeType
  , pmvNumberOfNodes
  , pmvPubliclyAccessible
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes cluster attributes that are in a pending state. A change to one or more the attributes was requested and is in progress or will be applied.
--
-- /See:/ 'mkPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { automatedSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The pending or in-progress change of the automated snapshot retention period.
  , clusterIdentifier :: Core.Maybe Core.Text
    -- ^ The pending or in-progress change of the new identifier for the cluster.
  , clusterType :: Core.Maybe Core.Text
    -- ^ The pending or in-progress change of the cluster type.
  , clusterVersion :: Core.Maybe Core.Text
    -- ^ The pending or in-progress change of the service version.
  , encryptionType :: Core.Maybe Core.Text
    -- ^ The encryption type for a cluster. Possible values are: KMS and None. 
  , enhancedVpcRouting :: Core.Maybe Core.Bool
    -- ^ An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
  , maintenanceTrackName :: Core.Maybe Core.Text
    -- ^ The name of the maintenance track that the cluster will change to during the next maintenance window.
  , masterUserPassword :: Core.Maybe Core.Text
    -- ^ The pending or in-progress change of the master user password for the cluster.
  , nodeType :: Core.Maybe Core.Text
    -- ^ The pending or in-progress change of the cluster's node type.
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The pending or in-progress change of the number of nodes in the cluster.
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ The pending or in-progress change of the ability to connect to the cluster from the public network.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PendingModifiedValues' value with any optional fields omitted.
mkPendingModifiedValues
    :: PendingModifiedValues
mkPendingModifiedValues
  = PendingModifiedValues'{automatedSnapshotRetentionPeriod =
                             Core.Nothing,
                           clusterIdentifier = Core.Nothing, clusterType = Core.Nothing,
                           clusterVersion = Core.Nothing, encryptionType = Core.Nothing,
                           enhancedVpcRouting = Core.Nothing,
                           maintenanceTrackName = Core.Nothing,
                           masterUserPassword = Core.Nothing, nodeType = Core.Nothing,
                           numberOfNodes = Core.Nothing, publiclyAccessible = Core.Nothing}

-- | The pending or in-progress change of the automated snapshot retention period.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvAutomatedSnapshotRetentionPeriod :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Int)
pmvAutomatedSnapshotRetentionPeriod = Lens.field @"automatedSnapshotRetentionPeriod"
{-# INLINEABLE pmvAutomatedSnapshotRetentionPeriod #-}
{-# DEPRECATED automatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead"  #-}

-- | The pending or in-progress change of the new identifier for the cluster.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvClusterIdentifier :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pmvClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE pmvClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | The pending or in-progress change of the cluster type.
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvClusterType :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pmvClusterType = Lens.field @"clusterType"
{-# INLINEABLE pmvClusterType #-}
{-# DEPRECATED clusterType "Use generic-lens or generic-optics with 'clusterType' instead"  #-}

-- | The pending or in-progress change of the service version.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvClusterVersion :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pmvClusterVersion = Lens.field @"clusterVersion"
{-# INLINEABLE pmvClusterVersion #-}
{-# DEPRECATED clusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead"  #-}

-- | The encryption type for a cluster. Possible values are: KMS and None. 
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvEncryptionType :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pmvEncryptionType = Lens.field @"encryptionType"
{-# INLINEABLE pmvEncryptionType #-}
{-# DEPRECATED encryptionType "Use generic-lens or generic-optics with 'encryptionType' instead"  #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
--
-- /Note:/ Consider using 'enhancedVpcRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvEnhancedVpcRouting :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Bool)
pmvEnhancedVpcRouting = Lens.field @"enhancedVpcRouting"
{-# INLINEABLE pmvEnhancedVpcRouting #-}
{-# DEPRECATED enhancedVpcRouting "Use generic-lens or generic-optics with 'enhancedVpcRouting' instead"  #-}

-- | The name of the maintenance track that the cluster will change to during the next maintenance window.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvMaintenanceTrackName :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pmvMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# INLINEABLE pmvMaintenanceTrackName #-}
{-# DEPRECATED maintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead"  #-}

-- | The pending or in-progress change of the master user password for the cluster.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvMasterUserPassword :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pmvMasterUserPassword = Lens.field @"masterUserPassword"
{-# INLINEABLE pmvMasterUserPassword #-}
{-# DEPRECATED masterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead"  #-}

-- | The pending or in-progress change of the cluster's node type.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvNodeType :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pmvNodeType = Lens.field @"nodeType"
{-# INLINEABLE pmvNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

-- | The pending or in-progress change of the number of nodes in the cluster.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvNumberOfNodes :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Int)
pmvNumberOfNodes = Lens.field @"numberOfNodes"
{-# INLINEABLE pmvNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

-- | The pending or in-progress change of the ability to connect to the cluster from the public network.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvPubliclyAccessible :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Bool)
pmvPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE pmvPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

instance Core.FromXML PendingModifiedValues where
        parseXML x
          = PendingModifiedValues' Core.<$>
              (x Core..@? "AutomatedSnapshotRetentionPeriod") Core.<*>
                x Core..@? "ClusterIdentifier"
                Core.<*> x Core..@? "ClusterType"
                Core.<*> x Core..@? "ClusterVersion"
                Core.<*> x Core..@? "EncryptionType"
                Core.<*> x Core..@? "EnhancedVpcRouting"
                Core.<*> x Core..@? "MaintenanceTrackName"
                Core.<*> x Core..@? "MasterUserPassword"
                Core.<*> x Core..@? "NodeType"
                Core.<*> x Core..@? "NumberOfNodes"
                Core.<*> x Core..@? "PubliclyAccessible"
