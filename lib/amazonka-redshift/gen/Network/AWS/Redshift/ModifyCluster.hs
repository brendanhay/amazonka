{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a cluster.
--
-- You can also change node type and the number of nodes to scale up or down the cluster. When resizing a cluster, you must specify both the number of nodes and the node type even if one of the parameters does not change.
-- You can add another security or parameter group, or change the master user password. Resetting a cluster password or modifying the security groups associated with a cluster do not need a reboot. However, modifying a parameter group requires a reboot for parameters to take effect. For more information about managing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.ModifyCluster
    (
    -- * Creating a request
      ModifyCluster (..)
    , mkModifyCluster
    -- ** Request lenses
    , mcClusterIdentifier
    , mcAllowVersionUpgrade
    , mcAutomatedSnapshotRetentionPeriod
    , mcClusterParameterGroupName
    , mcClusterSecurityGroups
    , mcClusterType
    , mcClusterVersion
    , mcElasticIp
    , mcEncrypted
    , mcEnhancedVpcRouting
    , mcHsmClientCertificateIdentifier
    , mcHsmConfigurationIdentifier
    , mcKmsKeyId
    , mcMaintenanceTrackName
    , mcManualSnapshotRetentionPeriod
    , mcMasterUserPassword
    , mcNewClusterIdentifier
    , mcNodeType
    , mcNumberOfNodes
    , mcPreferredMaintenanceWindow
    , mcPubliclyAccessible
    , mcVpcSecurityGroupIds

    -- * Destructuring the response
    , ModifyClusterResponse (..)
    , mkModifyClusterResponse
    -- ** Response lenses
    , mcrrsCluster
    , mcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { clusterIdentifier :: Core.Text
    -- ^ The unique identifier of the cluster to be modified.
--
-- Example: @examplecluster@ 
  , allowVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ If @true@ , major version upgrades will be applied automatically to the cluster during the maintenance window. 
--
-- Default: @false@ 
  , automatedSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' . 
--
-- If you decrease the automated snapshot retention period from its current value, existing automated snapshots that fall outside of the new retention period will be immediately deleted.
-- Default: Uses existing setting.
-- Constraints: Must be a value from 0 to 35.
  , clusterParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the cluster parameter group to apply to this cluster. This change is applied only after the cluster is rebooted. To reboot a cluster use 'RebootCluster' . 
--
-- Default: Uses existing setting.
-- Constraints: The cluster parameter group must be in the same parameter group family that matches the cluster version.
  , clusterSecurityGroups :: Core.Maybe [Core.Text]
    -- ^ A list of cluster security groups to be authorized on this cluster. This change is asynchronously applied as soon as possible.
--
-- Security groups currently associated with the cluster, and not in the list of groups to apply, will be revoked from the cluster.
-- Constraints:
--
--     * Must be 1 to 255 alphanumeric characters or hyphens
--
--
--     * First character must be a letter
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens
--
--
  , clusterType :: Core.Maybe Core.Text
    -- ^ The new cluster type.
--
-- When you submit your cluster resize request, your existing cluster goes into a read-only mode. After Amazon Redshift provisions a new cluster based on your resize requirements, there will be outage for a period while the old cluster is deleted and your connection is switched to the new cluster. You can use 'DescribeResize' to track the progress of the resize request. 
-- Valid Values: @multi-node | single-node @ 
  , clusterVersion :: Core.Maybe Core.Text
    -- ^ The new version number of the Amazon Redshift engine to upgrade to.
--
-- For major version upgrades, if a non-default cluster parameter group is currently in use, a new cluster parameter group in the cluster parameter group family for the new version must be specified. The new cluster parameter group can be the default for that cluster parameter group family. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
-- Example: @1.0@ 
  , elasticIp :: Core.Maybe Core.Text
    -- ^ The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the cluster is encrypted. If the value is encrypted (true) and you provide a value for the @KmsKeyId@ parameter, we encrypt the cluster with the provided @KmsKeyId@ . If you don't provide a @KmsKeyId@ , we encrypt with the default key. 
--
-- If the value is not encrypted (false), then the cluster is decrypted. 
  , enhancedVpcRouting :: Core.Maybe Core.Bool
    -- ^ An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
  , hsmClientCertificateIdentifier :: Core.Maybe Core.Text
    -- ^ Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
  , hsmConfigurationIdentifier :: Core.Maybe Core.Text
    -- ^ Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
  , maintenanceTrackName :: Core.Maybe Core.Text
    -- ^ The name for the maintenance track that you want to assign for the cluster. This name change is asynchronous. The new track name stays in the @PendingModifiedValues@ for the cluster until the next maintenance window. When the maintenance track changes, the cluster is switched to the latest cluster release available for the maintenance track. At this point, the maintenance track name is applied.
  , manualSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The default for number of days that a newly created manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. This value doesn't retroactively change the retention periods of existing manual snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
  , masterUserPassword :: Core.Maybe Core.Text
    -- ^ The new password for the cluster master user. This change is asynchronously applied as soon as possible. Between the time of the request and the completion of the request, the @MasterUserPassword@ element exists in the @PendingModifiedValues@ element of the operation response. 
--
-- Default: Uses existing setting.
-- Constraints:
--
--     * Must be between 8 and 64 characters in length.
--
--
--     * Must contain at least one uppercase letter.
--
--
--     * Must contain at least one lowercase letter.
--
--
--     * Must contain one number.
--
--
--     * Can be any printable ASCII character (ASCII code 33 to 126) except ' (single quote), " (double quote), \, /, @, or space.
--
--
  , newClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The new identifier for the cluster.
--
-- Constraints:
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * Alphabetic characters must be lowercase.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique for all clusters within an AWS account.
--
--
-- Example: @examplecluster@ 
  , nodeType :: Core.Maybe Core.Text
    -- ^ The new node type of the cluster. If you specify a new node type, you must also specify the number of nodes parameter.
--
-- For more information about resizing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@ | @ra3.4xlarge@ | @ra3.16xlarge@ 
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The new number of nodes of the cluster. If you specify a new number of nodes, you must also specify the node type parameter.
--
-- For more information about resizing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
-- Valid Values: Integer greater than @0@ .
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ The weekly time range (in UTC) during which system maintenance can occur, if necessary. If system maintenance is necessary during the window, it may result in an outage.
--
-- This maintenance window change is made immediately. If the new maintenance window indicates the current time, there must be at least 120 minutes between the current time and end of the window in order to ensure that pending changes are applied.
-- Default: Uses existing setting.
-- Format: ddd:hh24:mi-ddd:hh24:mi, for example @wed:07:30-wed:08:00@ .
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes.
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ If @true@ , the cluster can be accessed from a public network. Only clusters in VPCs can be set to be publicly available.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of virtual private cloud (VPC) security groups to be associated with the cluster. This change is asynchronously applied as soon as possible.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCluster' value with any optional fields omitted.
mkModifyCluster
    :: Core.Text -- ^ 'clusterIdentifier'
    -> ModifyCluster
mkModifyCluster clusterIdentifier
  = ModifyCluster'{clusterIdentifier,
                   allowVersionUpgrade = Core.Nothing,
                   automatedSnapshotRetentionPeriod = Core.Nothing,
                   clusterParameterGroupName = Core.Nothing,
                   clusterSecurityGroups = Core.Nothing, clusterType = Core.Nothing,
                   clusterVersion = Core.Nothing, elasticIp = Core.Nothing,
                   encrypted = Core.Nothing, enhancedVpcRouting = Core.Nothing,
                   hsmClientCertificateIdentifier = Core.Nothing,
                   hsmConfigurationIdentifier = Core.Nothing, kmsKeyId = Core.Nothing,
                   maintenanceTrackName = Core.Nothing,
                   manualSnapshotRetentionPeriod = Core.Nothing,
                   masterUserPassword = Core.Nothing,
                   newClusterIdentifier = Core.Nothing, nodeType = Core.Nothing,
                   numberOfNodes = Core.Nothing,
                   preferredMaintenanceWindow = Core.Nothing,
                   publiclyAccessible = Core.Nothing,
                   vpcSecurityGroupIds = Core.Nothing}

-- | The unique identifier of the cluster to be modified.
--
-- Example: @examplecluster@ 
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterIdentifier :: Lens.Lens' ModifyCluster Core.Text
mcClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE mcClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | If @true@ , major version upgrades will be applied automatically to the cluster during the maintenance window. 
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'allowVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAllowVersionUpgrade :: Lens.Lens' ModifyCluster (Core.Maybe Core.Bool)
mcAllowVersionUpgrade = Lens.field @"allowVersionUpgrade"
{-# INLINEABLE mcAllowVersionUpgrade #-}
{-# DEPRECATED allowVersionUpgrade "Use generic-lens or generic-optics with 'allowVersionUpgrade' instead"  #-}

-- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' . 
--
-- If you decrease the automated snapshot retention period from its current value, existing automated snapshots that fall outside of the new retention period will be immediately deleted.
-- Default: Uses existing setting.
-- Constraints: Must be a value from 0 to 35.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAutomatedSnapshotRetentionPeriod :: Lens.Lens' ModifyCluster (Core.Maybe Core.Int)
mcAutomatedSnapshotRetentionPeriod = Lens.field @"automatedSnapshotRetentionPeriod"
{-# INLINEABLE mcAutomatedSnapshotRetentionPeriod #-}
{-# DEPRECATED automatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead"  #-}

-- | The name of the cluster parameter group to apply to this cluster. This change is applied only after the cluster is rebooted. To reboot a cluster use 'RebootCluster' . 
--
-- Default: Uses existing setting.
-- Constraints: The cluster parameter group must be in the same parameter group family that matches the cluster version.
--
-- /Note:/ Consider using 'clusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterParameterGroupName :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcClusterParameterGroupName = Lens.field @"clusterParameterGroupName"
{-# INLINEABLE mcClusterParameterGroupName #-}
{-# DEPRECATED clusterParameterGroupName "Use generic-lens or generic-optics with 'clusterParameterGroupName' instead"  #-}

-- | A list of cluster security groups to be authorized on this cluster. This change is asynchronously applied as soon as possible.
--
-- Security groups currently associated with the cluster, and not in the list of groups to apply, will be revoked from the cluster.
-- Constraints:
--
--     * Must be 1 to 255 alphanumeric characters or hyphens
--
--
--     * First character must be a letter
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterSecurityGroups :: Lens.Lens' ModifyCluster (Core.Maybe [Core.Text])
mcClusterSecurityGroups = Lens.field @"clusterSecurityGroups"
{-# INLINEABLE mcClusterSecurityGroups #-}
{-# DEPRECATED clusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead"  #-}

-- | The new cluster type.
--
-- When you submit your cluster resize request, your existing cluster goes into a read-only mode. After Amazon Redshift provisions a new cluster based on your resize requirements, there will be outage for a period while the old cluster is deleted and your connection is switched to the new cluster. You can use 'DescribeResize' to track the progress of the resize request. 
-- Valid Values: @multi-node | single-node @ 
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterType :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcClusterType = Lens.field @"clusterType"
{-# INLINEABLE mcClusterType #-}
{-# DEPRECATED clusterType "Use generic-lens or generic-optics with 'clusterType' instead"  #-}

-- | The new version number of the Amazon Redshift engine to upgrade to.
--
-- For major version upgrades, if a non-default cluster parameter group is currently in use, a new cluster parameter group in the cluster parameter group family for the new version must be specified. The new cluster parameter group can be the default for that cluster parameter group family. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
-- Example: @1.0@ 
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterVersion :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcClusterVersion = Lens.field @"clusterVersion"
{-# INLINEABLE mcClusterVersion #-}
{-# DEPRECATED clusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead"  #-}

-- | The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcElasticIp :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcElasticIp = Lens.field @"elasticIp"
{-# INLINEABLE mcElasticIp #-}
{-# DEPRECATED elasticIp "Use generic-lens or generic-optics with 'elasticIp' instead"  #-}

-- | Indicates whether the cluster is encrypted. If the value is encrypted (true) and you provide a value for the @KmsKeyId@ parameter, we encrypt the cluster with the provided @KmsKeyId@ . If you don't provide a @KmsKeyId@ , we encrypt with the default key. 
--
-- If the value is not encrypted (false), then the cluster is decrypted. 
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcEncrypted :: Lens.Lens' ModifyCluster (Core.Maybe Core.Bool)
mcEncrypted = Lens.field @"encrypted"
{-# INLINEABLE mcEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
--
-- /Note:/ Consider using 'enhancedVpcRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcEnhancedVpcRouting :: Lens.Lens' ModifyCluster (Core.Maybe Core.Bool)
mcEnhancedVpcRouting = Lens.field @"enhancedVpcRouting"
{-# INLINEABLE mcEnhancedVpcRouting #-}
{-# DEPRECATED enhancedVpcRouting "Use generic-lens or generic-optics with 'enhancedVpcRouting' instead"  #-}

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcHsmClientCertificateIdentifier :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcHsmClientCertificateIdentifier = Lens.field @"hsmClientCertificateIdentifier"
{-# INLINEABLE mcHsmClientCertificateIdentifier #-}
{-# DEPRECATED hsmClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead"  #-}

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcHsmConfigurationIdentifier :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcHsmConfigurationIdentifier = Lens.field @"hsmConfigurationIdentifier"
{-# INLINEABLE mcHsmConfigurationIdentifier #-}
{-# DEPRECATED hsmConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead"  #-}

-- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcKmsKeyId :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE mcKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The name for the maintenance track that you want to assign for the cluster. This name change is asynchronous. The new track name stays in the @PendingModifiedValues@ for the cluster until the next maintenance window. When the maintenance track changes, the cluster is switched to the latest cluster release available for the maintenance track. At this point, the maintenance track name is applied.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcMaintenanceTrackName :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# INLINEABLE mcMaintenanceTrackName #-}
{-# DEPRECATED maintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead"  #-}

-- | The default for number of days that a newly created manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. This value doesn't retroactively change the retention periods of existing manual snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcManualSnapshotRetentionPeriod :: Lens.Lens' ModifyCluster (Core.Maybe Core.Int)
mcManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# INLINEABLE mcManualSnapshotRetentionPeriod #-}
{-# DEPRECATED manualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead"  #-}

-- | The new password for the cluster master user. This change is asynchronously applied as soon as possible. Between the time of the request and the completion of the request, the @MasterUserPassword@ element exists in the @PendingModifiedValues@ element of the operation response. 
--
-- Default: Uses existing setting.
-- Constraints:
--
--     * Must be between 8 and 64 characters in length.
--
--
--     * Must contain at least one uppercase letter.
--
--
--     * Must contain at least one lowercase letter.
--
--
--     * Must contain one number.
--
--
--     * Can be any printable ASCII character (ASCII code 33 to 126) except ' (single quote), " (double quote), \, /, @, or space.
--
--
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcMasterUserPassword :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcMasterUserPassword = Lens.field @"masterUserPassword"
{-# INLINEABLE mcMasterUserPassword #-}
{-# DEPRECATED masterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead"  #-}

-- | The new identifier for the cluster.
--
-- Constraints:
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * Alphabetic characters must be lowercase.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique for all clusters within an AWS account.
--
--
-- Example: @examplecluster@ 
--
-- /Note:/ Consider using 'newClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcNewClusterIdentifier :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcNewClusterIdentifier = Lens.field @"newClusterIdentifier"
{-# INLINEABLE mcNewClusterIdentifier #-}
{-# DEPRECATED newClusterIdentifier "Use generic-lens or generic-optics with 'newClusterIdentifier' instead"  #-}

-- | The new node type of the cluster. If you specify a new node type, you must also specify the number of nodes parameter.
--
-- For more information about resizing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@ | @ra3.4xlarge@ | @ra3.16xlarge@ 
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcNodeType :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcNodeType = Lens.field @"nodeType"
{-# INLINEABLE mcNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

-- | The new number of nodes of the cluster. If you specify a new number of nodes, you must also specify the node type parameter.
--
-- For more information about resizing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
-- Valid Values: Integer greater than @0@ .
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcNumberOfNodes :: Lens.Lens' ModifyCluster (Core.Maybe Core.Int)
mcNumberOfNodes = Lens.field @"numberOfNodes"
{-# INLINEABLE mcNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

-- | The weekly time range (in UTC) during which system maintenance can occur, if necessary. If system maintenance is necessary during the window, it may result in an outage.
--
-- This maintenance window change is made immediately. If the new maintenance window indicates the current time, there must be at least 120 minutes between the current time and end of the window in order to ensure that pending changes are applied.
-- Default: Uses existing setting.
-- Format: ddd:hh24:mi-ddd:hh24:mi, for example @wed:07:30-wed:08:00@ .
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcPreferredMaintenanceWindow :: Lens.Lens' ModifyCluster (Core.Maybe Core.Text)
mcPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE mcPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | If @true@ , the cluster can be accessed from a public network. Only clusters in VPCs can be set to be publicly available.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcPubliclyAccessible :: Lens.Lens' ModifyCluster (Core.Maybe Core.Bool)
mcPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE mcPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

-- | A list of virtual private cloud (VPC) security groups to be associated with the cluster. This change is asynchronously applied as soon as possible.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcVpcSecurityGroupIds :: Lens.Lens' ModifyCluster (Core.Maybe [Core.Text])
mcVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE mcVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery ModifyCluster where
        toQuery ModifyCluster{..}
          = Core.toQueryPair "Action" ("ModifyCluster" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AllowVersionUpgrade")
                allowVersionUpgrade
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AutomatedSnapshotRetentionPeriod")
                automatedSnapshotRetentionPeriod
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ClusterParameterGroupName")
                clusterParameterGroupName
              Core.<>
              Core.toQueryPair "ClusterSecurityGroups"
                (Core.maybe Core.mempty
                   (Core.toQueryList "ClusterSecurityGroupName")
                   clusterSecurityGroups)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterType") clusterType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterVersion")
                clusterVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ElasticIp") elasticIp
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Encrypted") encrypted
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnhancedVpcRouting")
                enhancedVpcRouting
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "HsmClientCertificateIdentifier")
                hsmClientCertificateIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "HsmConfigurationIdentifier")
                hsmConfigurationIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaintenanceTrackName")
                maintenanceTrackName
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ManualSnapshotRetentionPeriod")
                manualSnapshotRetentionPeriod
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MasterUserPassword")
                masterUserPassword
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NewClusterIdentifier")
                newClusterIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NodeType") nodeType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NumberOfNodes")
                numberOfNodes
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PreferredMaintenanceWindow")
                preferredMaintenanceWindow
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PubliclyAccessible")
                publiclyAccessible
              Core.<>
              Core.toQueryPair "VpcSecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "VpcSecurityGroupId")
                   vpcSecurityGroupIds)

instance Core.ToHeaders ModifyCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyCluster where
        type Rs ModifyCluster = ModifyClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyClusterResult"
              (\ s h x ->
                 ModifyClusterResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyClusterResponse' value with any optional fields omitted.
mkModifyClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyClusterResponse
mkModifyClusterResponse responseStatus
  = ModifyClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrrsCluster :: Lens.Lens' ModifyClusterResponse (Core.Maybe Types.Cluster)
mcrrsCluster = Lens.field @"cluster"
{-# INLINEABLE mcrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrrsResponseStatus :: Lens.Lens' ModifyClusterResponse Core.Int
mcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
