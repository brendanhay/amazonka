{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Cluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.Cluster
  ( Cluster (..)
  -- * Smart constructor
  , mkCluster
  -- * Lenses
  , cAllowVersionUpgrade
  , cAutomatedSnapshotRetentionPeriod
  , cAvailabilityZone
  , cClusterAvailabilityStatus
  , cClusterCreateTime
  , cClusterIdentifier
  , cClusterNamespaceArn
  , cClusterNodes
  , cClusterParameterGroups
  , cClusterPublicKey
  , cClusterRevisionNumber
  , cClusterSecurityGroups
  , cClusterSnapshotCopyStatus
  , cClusterStatus
  , cClusterSubnetGroupName
  , cClusterVersion
  , cDBName
  , cDataTransferProgress
  , cDeferredMaintenanceWindows
  , cElasticIpStatus
  , cElasticResizeNumberOfNodeOptions
  , cEncrypted
  , cEndpoint
  , cEnhancedVpcRouting
  , cExpectedNextSnapshotScheduleTime
  , cExpectedNextSnapshotScheduleTimeStatus
  , cHsmStatus
  , cIamRoles
  , cKmsKeyId
  , cMaintenanceTrackName
  , cManualSnapshotRetentionPeriod
  , cMasterUsername
  , cModifyStatus
  , cNextMaintenanceWindowStartTime
  , cNodeType
  , cNumberOfNodes
  , cPendingActions
  , cPendingModifiedValues
  , cPreferredMaintenanceWindow
  , cPubliclyAccessible
  , cResizeInfo
  , cRestoreStatus
  , cSnapshotScheduleIdentifier
  , cSnapshotScheduleState
  , cTags
  , cVpcId
  , cVpcSecurityGroups
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.ClusterIamRole as Types
import qualified Network.AWS.Redshift.Types.ClusterNode as Types
import qualified Network.AWS.Redshift.Types.ClusterParameterGroupStatus as Types
import qualified Network.AWS.Redshift.Types.ClusterSecurityGroupMembership as Types
import qualified Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus as Types
import qualified Network.AWS.Redshift.Types.DataTransferProgress as Types
import qualified Network.AWS.Redshift.Types.DeferredMaintenanceWindow as Types
import qualified Network.AWS.Redshift.Types.ElasticIpStatus as Types
import qualified Network.AWS.Redshift.Types.Endpoint as Types
import qualified Network.AWS.Redshift.Types.HsmStatus as Types
import qualified Network.AWS.Redshift.Types.PendingModifiedValues as Types
import qualified Network.AWS.Redshift.Types.ResizeInfo as Types
import qualified Network.AWS.Redshift.Types.RestoreStatus as Types
import qualified Network.AWS.Redshift.Types.ScheduleState as Types
import qualified Network.AWS.Redshift.Types.Tag as Types
import qualified Network.AWS.Redshift.Types.VpcSecurityGroupMembership as Types

-- | Describes a cluster.
--
-- /See:/ 'mkCluster' smart constructor.
data Cluster = Cluster'
  { allowVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ A boolean value that, if @true@ , indicates that major version upgrades will be applied automatically to the cluster during the maintenance window. 
  , automatedSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days that automatic cluster snapshots are retained.
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The name of the Availability Zone in which the cluster is located.
  , clusterAvailabilityStatus :: Core.Maybe Core.Text
    -- ^ The availability status of the cluster for queries. Possible values are the following:
--
--
--     * Available - The cluster is available for queries. 
--
--
--     * Unavailable - The cluster is not available for queries.
--
--
--     * Maintenance - The cluster is intermittently available for queries due to maintenance activities.
--
--
--     * Modifying - The cluster is intermittently available for queries due to changes that modify the cluster.
--
--
--     * Failed - The cluster failed and is not available for queries.
--
--
  , clusterCreateTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time that the cluster was created.
  , clusterIdentifier :: Core.Maybe Core.Text
    -- ^ The unique identifier of the cluster.
  , clusterNamespaceArn :: Core.Maybe Core.Text
    -- ^ The namespace Amazon Resource Name (ARN) of the cluster.
  , clusterNodes :: Core.Maybe [Types.ClusterNode]
    -- ^ The nodes in the cluster.
  , clusterParameterGroups :: Core.Maybe [Types.ClusterParameterGroupStatus]
    -- ^ The list of cluster parameter groups that are associated with this cluster. Each parameter group in the list is returned with its status.
  , clusterPublicKey :: Core.Maybe Core.Text
    -- ^ The public key for the cluster.
  , clusterRevisionNumber :: Core.Maybe Core.Text
    -- ^ The specific revision number of the database in the cluster.
  , clusterSecurityGroups :: Core.Maybe [Types.ClusterSecurityGroupMembership]
    -- ^ A list of cluster security group that are associated with the cluster. Each security group is represented by an element that contains @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@ subelements. 
--
-- Cluster security groups are used when the cluster is not created in an Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC use VPC security groups, which are listed by the __VpcSecurityGroups__ parameter. 
  , clusterSnapshotCopyStatus :: Core.Maybe Types.ClusterSnapshotCopyStatus
    -- ^ A value that returns the destination region and retention period that are configured for cross-region snapshot copy.
  , clusterStatus :: Core.Maybe Core.Text
    -- ^ The current state of the cluster. Possible values are the following:
--
--
--     * @available@ 
--
--
--     * @available, prep-for-resize@ 
--
--
--     * @available, resize-cleanup@ 
--
--
--     * @cancelling-resize@ 
--
--
--     * @creating@ 
--
--
--     * @deleting@ 
--
--
--     * @final-snapshot@ 
--
--
--     * @hardware-failure@ 
--
--
--     * @incompatible-hsm@ 
--
--
--     * @incompatible-network@ 
--
--
--     * @incompatible-parameters@ 
--
--
--     * @incompatible-restore@ 
--
--
--     * @modifying@ 
--
--
--     * @paused@ 
--
--
--     * @rebooting@ 
--
--
--     * @renaming@ 
--
--
--     * @resizing@ 
--
--
--     * @rotating-keys@ 
--
--
--     * @storage-full@ 
--
--
--     * @updating-hsm@ 
--
--
  , clusterSubnetGroupName :: Core.Maybe Core.Text
    -- ^ The name of the subnet group that is associated with the cluster. This parameter is valid only when the cluster is in a VPC.
  , clusterVersion :: Core.Maybe Core.Text
    -- ^ The version ID of the Amazon Redshift engine that is running on the cluster.
  , dBName :: Core.Maybe Core.Text
    -- ^ The name of the initial database that was created when the cluster was created. This same name is returned for the life of the cluster. If an initial database was not specified, a database named @dev@ dev was created by default. 
  , dataTransferProgress :: Core.Maybe Types.DataTransferProgress
    -- ^ 
  , deferredMaintenanceWindows :: Core.Maybe [Types.DeferredMaintenanceWindow]
    -- ^ Describes a group of @DeferredMaintenanceWindow@ objects.
  , elasticIpStatus :: Core.Maybe Types.ElasticIpStatus
    -- ^ The status of the elastic IP (EIP) address.
  , elasticResizeNumberOfNodeOptions :: Core.Maybe Core.Text
    -- ^ The number of nodes that you can resize the cluster to with the elastic resize method. 
  , encrypted :: Core.Maybe Core.Bool
    -- ^ A boolean value that, if @true@ , indicates that data in the cluster is encrypted at rest.
  , endpoint :: Core.Maybe Types.Endpoint
    -- ^ The connection endpoint.
  , enhancedVpcRouting :: Core.Maybe Core.Bool
    -- ^ An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
  , expectedNextSnapshotScheduleTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time when the next snapshot is expected to be taken for clusters with a valid snapshot schedule and backups enabled. 
  , expectedNextSnapshotScheduleTimeStatus :: Core.Maybe Core.Text
    -- ^ The status of next expected snapshot for clusters having a valid snapshot schedule and backups enabled. Possible values are the following:
--
--
--     * OnTrack - The next snapshot is expected to be taken on time. 
--
--
--     * Pending - The next snapshot is pending to be taken. 
--
--
  , hsmStatus :: Core.Maybe Types.HsmStatus
    -- ^ A value that reports whether the Amazon Redshift cluster has finished applying any hardware security module (HSM) settings changes specified in a modify cluster command.
--
-- Values: active, applying
  , iamRoles :: Core.Maybe [Types.ClusterIamRole]
    -- ^ A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS Key Management Service (AWS KMS) key ID of the encryption key used to encrypt data in the cluster.
  , maintenanceTrackName :: Core.Maybe Core.Text
    -- ^ The name of the maintenance track for the cluster.
  , manualSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
  , masterUsername :: Core.Maybe Core.Text
    -- ^ The master user name for the cluster. This name is used to connect to the database that is specified in the __DBName__ parameter. 
  , modifyStatus :: Core.Maybe Core.Text
    -- ^ The status of a modify operation, if any, initiated for the cluster.
  , nextMaintenanceWindowStartTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time in UTC when system maintenance can begin.
  , nodeType :: Core.Maybe Core.Text
    -- ^ The node type for the nodes in the cluster.
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The number of compute nodes in the cluster.
  , pendingActions :: Core.Maybe [Core.Text]
    -- ^ Cluster operations that are waiting to be started.
  , pendingModifiedValues :: Core.Maybe Types.PendingModifiedValues
    -- ^ A value that, if present, indicates that changes to the cluster are pending. Specific pending changes are identified by subelements.
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ The weekly time range, in Universal Coordinated Time (UTC), during which system maintenance can occur.
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ A boolean value that, if @true@ , indicates that the cluster can be accessed from a public network.
  , resizeInfo :: Core.Maybe Types.ResizeInfo
    -- ^ Returns the following:
--
--
--     * AllowCancelResize: a boolean value indicating if the resize operation can be cancelled.
--
--
--     * ResizeType: Returns ClassicResize
--
--
  , restoreStatus :: Core.Maybe Types.RestoreStatus
    -- ^ A value that describes the status of a cluster restore action. This parameter returns null if the cluster was not created by restoring a snapshot.
  , snapshotScheduleIdentifier :: Core.Maybe Core.Text
    -- ^ A unique identifier for the cluster snapshot schedule.
  , snapshotScheduleState :: Core.Maybe Types.ScheduleState
    -- ^ The current state of the cluster snapshot schedule.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags for the cluster.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The identifier of the VPC the cluster is in, if the cluster is in a VPC.
  , vpcSecurityGroups :: Core.Maybe [Types.VpcSecurityGroupMembership]
    -- ^ A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that are associated with the cluster. This parameter is returned only if the cluster is in a VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Cluster' value with any optional fields omitted.
mkCluster
    :: Cluster
mkCluster
  = Cluster'{allowVersionUpgrade = Core.Nothing,
             automatedSnapshotRetentionPeriod = Core.Nothing,
             availabilityZone = Core.Nothing,
             clusterAvailabilityStatus = Core.Nothing,
             clusterCreateTime = Core.Nothing, clusterIdentifier = Core.Nothing,
             clusterNamespaceArn = Core.Nothing, clusterNodes = Core.Nothing,
             clusterParameterGroups = Core.Nothing,
             clusterPublicKey = Core.Nothing,
             clusterRevisionNumber = Core.Nothing,
             clusterSecurityGroups = Core.Nothing,
             clusterSnapshotCopyStatus = Core.Nothing,
             clusterStatus = Core.Nothing,
             clusterSubnetGroupName = Core.Nothing,
             clusterVersion = Core.Nothing, dBName = Core.Nothing,
             dataTransferProgress = Core.Nothing,
             deferredMaintenanceWindows = Core.Nothing,
             elasticIpStatus = Core.Nothing,
             elasticResizeNumberOfNodeOptions = Core.Nothing,
             encrypted = Core.Nothing, endpoint = Core.Nothing,
             enhancedVpcRouting = Core.Nothing,
             expectedNextSnapshotScheduleTime = Core.Nothing,
             expectedNextSnapshotScheduleTimeStatus = Core.Nothing,
             hsmStatus = Core.Nothing, iamRoles = Core.Nothing,
             kmsKeyId = Core.Nothing, maintenanceTrackName = Core.Nothing,
             manualSnapshotRetentionPeriod = Core.Nothing,
             masterUsername = Core.Nothing, modifyStatus = Core.Nothing,
             nextMaintenanceWindowStartTime = Core.Nothing,
             nodeType = Core.Nothing, numberOfNodes = Core.Nothing,
             pendingActions = Core.Nothing,
             pendingModifiedValues = Core.Nothing,
             preferredMaintenanceWindow = Core.Nothing,
             publiclyAccessible = Core.Nothing, resizeInfo = Core.Nothing,
             restoreStatus = Core.Nothing,
             snapshotScheduleIdentifier = Core.Nothing,
             snapshotScheduleState = Core.Nothing, tags = Core.Nothing,
             vpcId = Core.Nothing, vpcSecurityGroups = Core.Nothing}

-- | A boolean value that, if @true@ , indicates that major version upgrades will be applied automatically to the cluster during the maintenance window. 
--
-- /Note:/ Consider using 'allowVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAllowVersionUpgrade :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cAllowVersionUpgrade = Lens.field @"allowVersionUpgrade"
{-# INLINEABLE cAllowVersionUpgrade #-}
{-# DEPRECATED allowVersionUpgrade "Use generic-lens or generic-optics with 'allowVersionUpgrade' instead"  #-}

-- | The number of days that automatic cluster snapshots are retained.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAutomatedSnapshotRetentionPeriod :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cAutomatedSnapshotRetentionPeriod = Lens.field @"automatedSnapshotRetentionPeriod"
{-# INLINEABLE cAutomatedSnapshotRetentionPeriod #-}
{-# DEPRECATED automatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead"  #-}

-- | The name of the Availability Zone in which the cluster is located.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAvailabilityZone :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE cAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The availability status of the cluster for queries. Possible values are the following:
--
--
--     * Available - The cluster is available for queries. 
--
--
--     * Unavailable - The cluster is not available for queries.
--
--
--     * Maintenance - The cluster is intermittently available for queries due to maintenance activities.
--
--
--     * Modifying - The cluster is intermittently available for queries due to changes that modify the cluster.
--
--
--     * Failed - The cluster failed and is not available for queries.
--
--
--
-- /Note:/ Consider using 'clusterAvailabilityStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterAvailabilityStatus :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cClusterAvailabilityStatus = Lens.field @"clusterAvailabilityStatus"
{-# INLINEABLE cClusterAvailabilityStatus #-}
{-# DEPRECATED clusterAvailabilityStatus "Use generic-lens or generic-optics with 'clusterAvailabilityStatus' instead"  #-}

-- | The date and time that the cluster was created.
--
-- /Note:/ Consider using 'clusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterCreateTime :: Lens.Lens' Cluster (Core.Maybe Core.UTCTime)
cClusterCreateTime = Lens.field @"clusterCreateTime"
{-# INLINEABLE cClusterCreateTime #-}
{-# DEPRECATED clusterCreateTime "Use generic-lens or generic-optics with 'clusterCreateTime' instead"  #-}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterIdentifier :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE cClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | The namespace Amazon Resource Name (ARN) of the cluster.
--
-- /Note:/ Consider using 'clusterNamespaceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterNamespaceArn :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cClusterNamespaceArn = Lens.field @"clusterNamespaceArn"
{-# INLINEABLE cClusterNamespaceArn #-}
{-# DEPRECATED clusterNamespaceArn "Use generic-lens or generic-optics with 'clusterNamespaceArn' instead"  #-}

-- | The nodes in the cluster.
--
-- /Note:/ Consider using 'clusterNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterNodes :: Lens.Lens' Cluster (Core.Maybe [Types.ClusterNode])
cClusterNodes = Lens.field @"clusterNodes"
{-# INLINEABLE cClusterNodes #-}
{-# DEPRECATED clusterNodes "Use generic-lens or generic-optics with 'clusterNodes' instead"  #-}

-- | The list of cluster parameter groups that are associated with this cluster. Each parameter group in the list is returned with its status.
--
-- /Note:/ Consider using 'clusterParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterParameterGroups :: Lens.Lens' Cluster (Core.Maybe [Types.ClusterParameterGroupStatus])
cClusterParameterGroups = Lens.field @"clusterParameterGroups"
{-# INLINEABLE cClusterParameterGroups #-}
{-# DEPRECATED clusterParameterGroups "Use generic-lens or generic-optics with 'clusterParameterGroups' instead"  #-}

-- | The public key for the cluster.
--
-- /Note:/ Consider using 'clusterPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterPublicKey :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cClusterPublicKey = Lens.field @"clusterPublicKey"
{-# INLINEABLE cClusterPublicKey #-}
{-# DEPRECATED clusterPublicKey "Use generic-lens or generic-optics with 'clusterPublicKey' instead"  #-}

-- | The specific revision number of the database in the cluster.
--
-- /Note:/ Consider using 'clusterRevisionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterRevisionNumber :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cClusterRevisionNumber = Lens.field @"clusterRevisionNumber"
{-# INLINEABLE cClusterRevisionNumber #-}
{-# DEPRECATED clusterRevisionNumber "Use generic-lens or generic-optics with 'clusterRevisionNumber' instead"  #-}

-- | A list of cluster security group that are associated with the cluster. Each security group is represented by an element that contains @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@ subelements. 
--
-- Cluster security groups are used when the cluster is not created in an Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC use VPC security groups, which are listed by the __VpcSecurityGroups__ parameter. 
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterSecurityGroups :: Lens.Lens' Cluster (Core.Maybe [Types.ClusterSecurityGroupMembership])
cClusterSecurityGroups = Lens.field @"clusterSecurityGroups"
{-# INLINEABLE cClusterSecurityGroups #-}
{-# DEPRECATED clusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead"  #-}

-- | A value that returns the destination region and retention period that are configured for cross-region snapshot copy.
--
-- /Note:/ Consider using 'clusterSnapshotCopyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterSnapshotCopyStatus :: Lens.Lens' Cluster (Core.Maybe Types.ClusterSnapshotCopyStatus)
cClusterSnapshotCopyStatus = Lens.field @"clusterSnapshotCopyStatus"
{-# INLINEABLE cClusterSnapshotCopyStatus #-}
{-# DEPRECATED clusterSnapshotCopyStatus "Use generic-lens or generic-optics with 'clusterSnapshotCopyStatus' instead"  #-}

-- | The current state of the cluster. Possible values are the following:
--
--
--     * @available@ 
--
--
--     * @available, prep-for-resize@ 
--
--
--     * @available, resize-cleanup@ 
--
--
--     * @cancelling-resize@ 
--
--
--     * @creating@ 
--
--
--     * @deleting@ 
--
--
--     * @final-snapshot@ 
--
--
--     * @hardware-failure@ 
--
--
--     * @incompatible-hsm@ 
--
--
--     * @incompatible-network@ 
--
--
--     * @incompatible-parameters@ 
--
--
--     * @incompatible-restore@ 
--
--
--     * @modifying@ 
--
--
--     * @paused@ 
--
--
--     * @rebooting@ 
--
--
--     * @renaming@ 
--
--
--     * @resizing@ 
--
--
--     * @rotating-keys@ 
--
--
--     * @storage-full@ 
--
--
--     * @updating-hsm@ 
--
--
--
-- /Note:/ Consider using 'clusterStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterStatus :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cClusterStatus = Lens.field @"clusterStatus"
{-# INLINEABLE cClusterStatus #-}
{-# DEPRECATED clusterStatus "Use generic-lens or generic-optics with 'clusterStatus' instead"  #-}

-- | The name of the subnet group that is associated with the cluster. This parameter is valid only when the cluster is in a VPC.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterSubnetGroupName :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# INLINEABLE cClusterSubnetGroupName #-}
{-# DEPRECATED clusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead"  #-}

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterVersion :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cClusterVersion = Lens.field @"clusterVersion"
{-# INLINEABLE cClusterVersion #-}
{-# DEPRECATED clusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead"  #-}

-- | The name of the initial database that was created when the cluster was created. This same name is returned for the life of the cluster. If an initial database was not specified, a database named @dev@ dev was created by default. 
--
-- /Note:/ Consider using 'dBName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDBName :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cDBName = Lens.field @"dBName"
{-# INLINEABLE cDBName #-}
{-# DEPRECATED dBName "Use generic-lens or generic-optics with 'dBName' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'dataTransferProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDataTransferProgress :: Lens.Lens' Cluster (Core.Maybe Types.DataTransferProgress)
cDataTransferProgress = Lens.field @"dataTransferProgress"
{-# INLINEABLE cDataTransferProgress #-}
{-# DEPRECATED dataTransferProgress "Use generic-lens or generic-optics with 'dataTransferProgress' instead"  #-}

-- | Describes a group of @DeferredMaintenanceWindow@ objects.
--
-- /Note:/ Consider using 'deferredMaintenanceWindows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDeferredMaintenanceWindows :: Lens.Lens' Cluster (Core.Maybe [Types.DeferredMaintenanceWindow])
cDeferredMaintenanceWindows = Lens.field @"deferredMaintenanceWindows"
{-# INLINEABLE cDeferredMaintenanceWindows #-}
{-# DEPRECATED deferredMaintenanceWindows "Use generic-lens or generic-optics with 'deferredMaintenanceWindows' instead"  #-}

-- | The status of the elastic IP (EIP) address.
--
-- /Note:/ Consider using 'elasticIpStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cElasticIpStatus :: Lens.Lens' Cluster (Core.Maybe Types.ElasticIpStatus)
cElasticIpStatus = Lens.field @"elasticIpStatus"
{-# INLINEABLE cElasticIpStatus #-}
{-# DEPRECATED elasticIpStatus "Use generic-lens or generic-optics with 'elasticIpStatus' instead"  #-}

-- | The number of nodes that you can resize the cluster to with the elastic resize method. 
--
-- /Note:/ Consider using 'elasticResizeNumberOfNodeOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cElasticResizeNumberOfNodeOptions :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cElasticResizeNumberOfNodeOptions = Lens.field @"elasticResizeNumberOfNodeOptions"
{-# INLINEABLE cElasticResizeNumberOfNodeOptions #-}
{-# DEPRECATED elasticResizeNumberOfNodeOptions "Use generic-lens or generic-optics with 'elasticResizeNumberOfNodeOptions' instead"  #-}

-- | A boolean value that, if @true@ , indicates that data in the cluster is encrypted at rest.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEncrypted :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cEncrypted = Lens.field @"encrypted"
{-# INLINEABLE cEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The connection endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEndpoint :: Lens.Lens' Cluster (Core.Maybe Types.Endpoint)
cEndpoint = Lens.field @"endpoint"
{-# INLINEABLE cEndpoint #-}
{-# DEPRECATED endpoint "Use generic-lens or generic-optics with 'endpoint' instead"  #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
--
-- /Note:/ Consider using 'enhancedVpcRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEnhancedVpcRouting :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cEnhancedVpcRouting = Lens.field @"enhancedVpcRouting"
{-# INLINEABLE cEnhancedVpcRouting #-}
{-# DEPRECATED enhancedVpcRouting "Use generic-lens or generic-optics with 'enhancedVpcRouting' instead"  #-}

-- | The date and time when the next snapshot is expected to be taken for clusters with a valid snapshot schedule and backups enabled. 
--
-- /Note:/ Consider using 'expectedNextSnapshotScheduleTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpectedNextSnapshotScheduleTime :: Lens.Lens' Cluster (Core.Maybe Core.UTCTime)
cExpectedNextSnapshotScheduleTime = Lens.field @"expectedNextSnapshotScheduleTime"
{-# INLINEABLE cExpectedNextSnapshotScheduleTime #-}
{-# DEPRECATED expectedNextSnapshotScheduleTime "Use generic-lens or generic-optics with 'expectedNextSnapshotScheduleTime' instead"  #-}

-- | The status of next expected snapshot for clusters having a valid snapshot schedule and backups enabled. Possible values are the following:
--
--
--     * OnTrack - The next snapshot is expected to be taken on time. 
--
--
--     * Pending - The next snapshot is pending to be taken. 
--
--
--
-- /Note:/ Consider using 'expectedNextSnapshotScheduleTimeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpectedNextSnapshotScheduleTimeStatus :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cExpectedNextSnapshotScheduleTimeStatus = Lens.field @"expectedNextSnapshotScheduleTimeStatus"
{-# INLINEABLE cExpectedNextSnapshotScheduleTimeStatus #-}
{-# DEPRECATED expectedNextSnapshotScheduleTimeStatus "Use generic-lens or generic-optics with 'expectedNextSnapshotScheduleTimeStatus' instead"  #-}

-- | A value that reports whether the Amazon Redshift cluster has finished applying any hardware security module (HSM) settings changes specified in a modify cluster command.
--
-- Values: active, applying
--
-- /Note:/ Consider using 'hsmStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHsmStatus :: Lens.Lens' Cluster (Core.Maybe Types.HsmStatus)
cHsmStatus = Lens.field @"hsmStatus"
{-# INLINEABLE cHsmStatus #-}
{-# DEPRECATED hsmStatus "Use generic-lens or generic-optics with 'hsmStatus' instead"  #-}

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
--
-- /Note:/ Consider using 'iamRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIamRoles :: Lens.Lens' Cluster (Core.Maybe [Types.ClusterIamRole])
cIamRoles = Lens.field @"iamRoles"
{-# INLINEABLE cIamRoles #-}
{-# DEPRECATED iamRoles "Use generic-lens or generic-optics with 'iamRoles' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) key ID of the encryption key used to encrypt data in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKmsKeyId :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE cKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The name of the maintenance track for the cluster.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMaintenanceTrackName :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# INLINEABLE cMaintenanceTrackName #-}
{-# DEPRECATED maintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead"  #-}

-- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cManualSnapshotRetentionPeriod :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# INLINEABLE cManualSnapshotRetentionPeriod #-}
{-# DEPRECATED manualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead"  #-}

-- | The master user name for the cluster. This name is used to connect to the database that is specified in the __DBName__ parameter. 
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMasterUsername :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cMasterUsername = Lens.field @"masterUsername"
{-# INLINEABLE cMasterUsername #-}
{-# DEPRECATED masterUsername "Use generic-lens or generic-optics with 'masterUsername' instead"  #-}

-- | The status of a modify operation, if any, initiated for the cluster.
--
-- /Note:/ Consider using 'modifyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cModifyStatus :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cModifyStatus = Lens.field @"modifyStatus"
{-# INLINEABLE cModifyStatus #-}
{-# DEPRECATED modifyStatus "Use generic-lens or generic-optics with 'modifyStatus' instead"  #-}

-- | The date and time in UTC when system maintenance can begin.
--
-- /Note:/ Consider using 'nextMaintenanceWindowStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNextMaintenanceWindowStartTime :: Lens.Lens' Cluster (Core.Maybe Core.UTCTime)
cNextMaintenanceWindowStartTime = Lens.field @"nextMaintenanceWindowStartTime"
{-# INLINEABLE cNextMaintenanceWindowStartTime #-}
{-# DEPRECATED nextMaintenanceWindowStartTime "Use generic-lens or generic-optics with 'nextMaintenanceWindowStartTime' instead"  #-}

-- | The node type for the nodes in the cluster.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNodeType :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cNodeType = Lens.field @"nodeType"
{-# INLINEABLE cNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

-- | The number of compute nodes in the cluster.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNumberOfNodes :: Lens.Lens' Cluster (Core.Maybe Core.Int)
cNumberOfNodes = Lens.field @"numberOfNodes"
{-# INLINEABLE cNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

-- | Cluster operations that are waiting to be started.
--
-- /Note:/ Consider using 'pendingActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPendingActions :: Lens.Lens' Cluster (Core.Maybe [Core.Text])
cPendingActions = Lens.field @"pendingActions"
{-# INLINEABLE cPendingActions #-}
{-# DEPRECATED pendingActions "Use generic-lens or generic-optics with 'pendingActions' instead"  #-}

-- | A value that, if present, indicates that changes to the cluster are pending. Specific pending changes are identified by subelements.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPendingModifiedValues :: Lens.Lens' Cluster (Core.Maybe Types.PendingModifiedValues)
cPendingModifiedValues = Lens.field @"pendingModifiedValues"
{-# INLINEABLE cPendingModifiedValues #-}
{-# DEPRECATED pendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead"  #-}

-- | The weekly time range, in Universal Coordinated Time (UTC), during which system maintenance can occur.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPreferredMaintenanceWindow :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE cPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | A boolean value that, if @true@ , indicates that the cluster can be accessed from a public network.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPubliclyAccessible :: Lens.Lens' Cluster (Core.Maybe Core.Bool)
cPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE cPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

-- | Returns the following:
--
--
--     * AllowCancelResize: a boolean value indicating if the resize operation can be cancelled.
--
--
--     * ResizeType: Returns ClassicResize
--
--
--
-- /Note:/ Consider using 'resizeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cResizeInfo :: Lens.Lens' Cluster (Core.Maybe Types.ResizeInfo)
cResizeInfo = Lens.field @"resizeInfo"
{-# INLINEABLE cResizeInfo #-}
{-# DEPRECATED resizeInfo "Use generic-lens or generic-optics with 'resizeInfo' instead"  #-}

-- | A value that describes the status of a cluster restore action. This parameter returns null if the cluster was not created by restoring a snapshot.
--
-- /Note:/ Consider using 'restoreStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRestoreStatus :: Lens.Lens' Cluster (Core.Maybe Types.RestoreStatus)
cRestoreStatus = Lens.field @"restoreStatus"
{-# INLINEABLE cRestoreStatus #-}
{-# DEPRECATED restoreStatus "Use generic-lens or generic-optics with 'restoreStatus' instead"  #-}

-- | A unique identifier for the cluster snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSnapshotScheduleIdentifier :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cSnapshotScheduleIdentifier = Lens.field @"snapshotScheduleIdentifier"
{-# INLINEABLE cSnapshotScheduleIdentifier #-}
{-# DEPRECATED snapshotScheduleIdentifier "Use generic-lens or generic-optics with 'snapshotScheduleIdentifier' instead"  #-}

-- | The current state of the cluster snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSnapshotScheduleState :: Lens.Lens' Cluster (Core.Maybe Types.ScheduleState)
cSnapshotScheduleState = Lens.field @"snapshotScheduleState"
{-# INLINEABLE cSnapshotScheduleState #-}
{-# DEPRECATED snapshotScheduleState "Use generic-lens or generic-optics with 'snapshotScheduleState' instead"  #-}

-- | The list of tags for the cluster.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' Cluster (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# INLINEABLE cTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVpcId :: Lens.Lens' Cluster (Core.Maybe Core.Text)
cVpcId = Lens.field @"vpcId"
{-# INLINEABLE cVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that are associated with the cluster. This parameter is returned only if the cluster is in a VPC.
--
-- /Note:/ Consider using 'vpcSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVpcSecurityGroups :: Lens.Lens' Cluster (Core.Maybe [Types.VpcSecurityGroupMembership])
cVpcSecurityGroups = Lens.field @"vpcSecurityGroups"
{-# INLINEABLE cVpcSecurityGroups #-}
{-# DEPRECATED vpcSecurityGroups "Use generic-lens or generic-optics with 'vpcSecurityGroups' instead"  #-}

instance Core.FromXML Cluster where
        parseXML x
          = Cluster' Core.<$>
              (x Core..@? "AllowVersionUpgrade") Core.<*>
                x Core..@? "AutomatedSnapshotRetentionPeriod"
                Core.<*> x Core..@? "AvailabilityZone"
                Core.<*> x Core..@? "ClusterAvailabilityStatus"
                Core.<*> x Core..@? "ClusterCreateTime"
                Core.<*> x Core..@? "ClusterIdentifier"
                Core.<*> x Core..@? "ClusterNamespaceArn"
                Core.<*>
                x Core..@? "ClusterNodes" Core..<@> Core.parseXMLList "member"
                Core.<*>
                x Core..@? "ClusterParameterGroups" Core..<@>
                  Core.parseXMLList "ClusterParameterGroup"
                Core.<*> x Core..@? "ClusterPublicKey"
                Core.<*> x Core..@? "ClusterRevisionNumber"
                Core.<*>
                x Core..@? "ClusterSecurityGroups" Core..<@>
                  Core.parseXMLList "ClusterSecurityGroup"
                Core.<*> x Core..@? "ClusterSnapshotCopyStatus"
                Core.<*> x Core..@? "ClusterStatus"
                Core.<*> x Core..@? "ClusterSubnetGroupName"
                Core.<*> x Core..@? "ClusterVersion"
                Core.<*> x Core..@? "DBName"
                Core.<*> x Core..@? "DataTransferProgress"
                Core.<*>
                x Core..@? "DeferredMaintenanceWindows" Core..<@>
                  Core.parseXMLList "DeferredMaintenanceWindow"
                Core.<*> x Core..@? "ElasticIpStatus"
                Core.<*> x Core..@? "ElasticResizeNumberOfNodeOptions"
                Core.<*> x Core..@? "Encrypted"
                Core.<*> x Core..@? "Endpoint"
                Core.<*> x Core..@? "EnhancedVpcRouting"
                Core.<*> x Core..@? "ExpectedNextSnapshotScheduleTime"
                Core.<*> x Core..@? "ExpectedNextSnapshotScheduleTimeStatus"
                Core.<*> x Core..@? "HsmStatus"
                Core.<*>
                x Core..@? "IamRoles" Core..<@> Core.parseXMLList "ClusterIamRole"
                Core.<*> x Core..@? "KmsKeyId"
                Core.<*> x Core..@? "MaintenanceTrackName"
                Core.<*> x Core..@? "ManualSnapshotRetentionPeriod"
                Core.<*> x Core..@? "MasterUsername"
                Core.<*> x Core..@? "ModifyStatus"
                Core.<*> x Core..@? "NextMaintenanceWindowStartTime"
                Core.<*> x Core..@? "NodeType"
                Core.<*> x Core..@? "NumberOfNodes"
                Core.<*>
                x Core..@? "PendingActions" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "PendingModifiedValues"
                Core.<*> x Core..@? "PreferredMaintenanceWindow"
                Core.<*> x Core..@? "PubliclyAccessible"
                Core.<*> x Core..@? "ResizeInfo"
                Core.<*> x Core..@? "RestoreStatus"
                Core.<*> x Core..@? "SnapshotScheduleIdentifier"
                Core.<*> x Core..@? "SnapshotScheduleState"
                Core.<*> x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag"
                Core.<*> x Core..@? "VpcId"
                Core.<*>
                x Core..@? "VpcSecurityGroups" Core..<@>
                  Core.parseXMLList "VpcSecurityGroup"
