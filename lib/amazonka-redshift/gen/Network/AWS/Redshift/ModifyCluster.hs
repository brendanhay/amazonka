{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyCluster (..),
    mkModifyCluster,

    -- ** Request lenses
    mcManualSnapshotRetentionPeriod,
    mcEnhancedVPCRouting,
    mcMasterUserPassword,
    mcPubliclyAccessible,
    mcMaintenanceTrackName,
    mcHSMConfigurationIdentifier,
    mcClusterSecurityGroups,
    mcAutomatedSnapshotRetentionPeriod,
    mcEncrypted,
    mcClusterIdentifier,
    mcHSMClientCertificateIdentifier,
    mcNumberOfNodes,
    mcElasticIP,
    mcPreferredMaintenanceWindow,
    mcKMSKeyId,
    mcVPCSecurityGroupIds,
    mcClusterType,
    mcNewClusterIdentifier,
    mcClusterVersion,
    mcNodeType,
    mcAllowVersionUpgrade,
    mcClusterParameterGroupName,

    -- * Destructuring the response
    ModifyClusterResponse (..),
    mkModifyClusterResponse,

    -- ** Response lenses
    mcrsCluster,
    mcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { -- | The default for number of days that a newly created manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. This value doesn't retroactively change the retention periods of existing manual snapshots.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    -- The default value is -1.
    manualSnapshotRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@ , enhanced VPC routing is enabled.
    -- Default: false
    enhancedVPCRouting :: Lude.Maybe Lude.Bool,
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
    masterUserPassword :: Lude.Maybe Lude.Text,
    -- | If @true@ , the cluster can be accessed from a public network. Only clusters in VPCs can be set to be publicly available.
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    -- | The name for the maintenance track that you want to assign for the cluster. This name change is asynchronous. The new track name stays in the @PendingModifiedValues@ for the cluster until the next maintenance window. When the maintenance track changes, the cluster is switched to the latest cluster release available for the maintenance track. At this point, the maintenance track name is applied.
    maintenanceTrackName :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
    hsmConfigurationIdentifier :: Lude.Maybe Lude.Text,
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
    clusterSecurityGroups :: Lude.Maybe [Lude.Text],
    -- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .
    --
    -- If you decrease the automated snapshot retention period from its current value, existing automated snapshots that fall outside of the new retention period will be immediately deleted.
    -- Default: Uses existing setting.
    -- Constraints: Must be a value from 0 to 35.
    automatedSnapshotRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | Indicates whether the cluster is encrypted. If the value is encrypted (true) and you provide a value for the @KmsKeyId@ parameter, we encrypt the cluster with the provided @KmsKeyId@ . If you don't provide a @KmsKeyId@ , we encrypt with the default key.
    --
    -- If the value is not encrypted (false), then the cluster is decrypted.
    encrypted :: Lude.Maybe Lude.Bool,
    -- | The unique identifier of the cluster to be modified.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Lude.Text,
    -- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
    hsmClientCertificateIdentifier :: Lude.Maybe Lude.Text,
    -- | The new number of nodes of the cluster. If you specify a new number of nodes, you must also specify the node type parameter.
    --
    -- For more information about resizing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
    -- Valid Values: Integer greater than @0@ .
    numberOfNodes :: Lude.Maybe Lude.Int,
    -- | The Elastic IP (EIP) address for the cluster.
    --
    -- Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
    elasticIP :: Lude.Maybe Lude.Text,
    -- | The weekly time range (in UTC) during which system maintenance can occur, if necessary. If system maintenance is necessary during the window, it may result in an outage.
    --
    -- This maintenance window change is made immediately. If the new maintenance window indicates the current time, there must be at least 120 minutes between the current time and end of the window in order to ensure that pending changes are applied.
    -- Default: Uses existing setting.
    -- Format: ddd:hh24:mi-ddd:hh24:mi, for example @wed:07:30-wed:08:00@ .
    -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
    -- Constraints: Must be at least 30 minutes.
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    -- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | A list of virtual private cloud (VPC) security groups to be associated with the cluster. This change is asynchronously applied as soon as possible.
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    -- | The new cluster type.
    --
    -- When you submit your cluster resize request, your existing cluster goes into a read-only mode. After Amazon Redshift provisions a new cluster based on your resize requirements, there will be outage for a period while the old cluster is deleted and your connection is switched to the new cluster. You can use 'DescribeResize' to track the progress of the resize request.
    -- Valid Values: @multi-node | single-node @
    clusterType :: Lude.Maybe Lude.Text,
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
    newClusterIdentifier :: Lude.Maybe Lude.Text,
    -- | The new version number of the Amazon Redshift engine to upgrade to.
    --
    -- For major version upgrades, if a non-default cluster parameter group is currently in use, a new cluster parameter group in the cluster parameter group family for the new version must be specified. The new cluster parameter group can be the default for that cluster parameter group family. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
    -- Example: @1.0@
    clusterVersion :: Lude.Maybe Lude.Text,
    -- | The new node type of the cluster. If you specify a new node type, you must also specify the number of nodes parameter.
    --
    -- For more information about resizing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
    -- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@ | @ra3.4xlarge@ | @ra3.16xlarge@
    nodeType :: Lude.Maybe Lude.Text,
    -- | If @true@ , major version upgrades will be applied automatically to the cluster during the maintenance window.
    --
    -- Default: @false@
    allowVersionUpgrade :: Lude.Maybe Lude.Bool,
    -- | The name of the cluster parameter group to apply to this cluster. This change is applied only after the cluster is rebooted. To reboot a cluster use 'RebootCluster' .
    --
    -- Default: Uses existing setting.
    -- Constraints: The cluster parameter group must be in the same parameter group family that matches the cluster version.
    clusterParameterGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyCluster' with the minimum fields required to make a request.
--
-- * 'manualSnapshotRetentionPeriod' - The default for number of days that a newly created manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. This value doesn't retroactively change the retention periods of existing manual snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
-- * 'enhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
-- * 'masterUserPassword' - The new password for the cluster master user. This change is asynchronously applied as soon as possible. Between the time of the request and the completion of the request, the @MasterUserPassword@ element exists in the @PendingModifiedValues@ element of the operation response.
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
-- * 'publiclyAccessible' - If @true@ , the cluster can be accessed from a public network. Only clusters in VPCs can be set to be publicly available.
-- * 'maintenanceTrackName' - The name for the maintenance track that you want to assign for the cluster. This name change is asynchronous. The new track name stays in the @PendingModifiedValues@ for the cluster until the next maintenance window. When the maintenance track changes, the cluster is switched to the latest cluster release available for the maintenance track. At this point, the maintenance track name is applied.
-- * 'hsmConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
-- * 'clusterSecurityGroups' - A list of cluster security groups to be authorized on this cluster. This change is asynchronously applied as soon as possible.
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
-- * 'automatedSnapshotRetentionPeriod' - The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .
--
-- If you decrease the automated snapshot retention period from its current value, existing automated snapshots that fall outside of the new retention period will be immediately deleted.
-- Default: Uses existing setting.
-- Constraints: Must be a value from 0 to 35.
-- * 'encrypted' - Indicates whether the cluster is encrypted. If the value is encrypted (true) and you provide a value for the @KmsKeyId@ parameter, we encrypt the cluster with the provided @KmsKeyId@ . If you don't provide a @KmsKeyId@ , we encrypt with the default key.
--
-- If the value is not encrypted (false), then the cluster is decrypted.
-- * 'clusterIdentifier' - The unique identifier of the cluster to be modified.
--
-- Example: @examplecluster@
-- * 'hsmClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
-- * 'numberOfNodes' - The new number of nodes of the cluster. If you specify a new number of nodes, you must also specify the node type parameter.
--
-- For more information about resizing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
-- Valid Values: Integer greater than @0@ .
-- * 'elasticIP' - The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
-- * 'preferredMaintenanceWindow' - The weekly time range (in UTC) during which system maintenance can occur, if necessary. If system maintenance is necessary during the window, it may result in an outage.
--
-- This maintenance window change is made immediately. If the new maintenance window indicates the current time, there must be at least 120 minutes between the current time and end of the window in order to ensure that pending changes are applied.
-- Default: Uses existing setting.
-- Format: ddd:hh24:mi-ddd:hh24:mi, for example @wed:07:30-wed:08:00@ .
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes.
-- * 'kmsKeyId' - The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
-- * 'vpcSecurityGroupIds' - A list of virtual private cloud (VPC) security groups to be associated with the cluster. This change is asynchronously applied as soon as possible.
-- * 'clusterType' - The new cluster type.
--
-- When you submit your cluster resize request, your existing cluster goes into a read-only mode. After Amazon Redshift provisions a new cluster based on your resize requirements, there will be outage for a period while the old cluster is deleted and your connection is switched to the new cluster. You can use 'DescribeResize' to track the progress of the resize request.
-- Valid Values: @multi-node | single-node @
-- * 'newClusterIdentifier' - The new identifier for the cluster.
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
-- * 'clusterVersion' - The new version number of the Amazon Redshift engine to upgrade to.
--
-- For major version upgrades, if a non-default cluster parameter group is currently in use, a new cluster parameter group in the cluster parameter group family for the new version must be specified. The new cluster parameter group can be the default for that cluster parameter group family. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
-- Example: @1.0@
-- * 'nodeType' - The new node type of the cluster. If you specify a new node type, you must also specify the number of nodes parameter.
--
-- For more information about resizing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@ | @ra3.4xlarge@ | @ra3.16xlarge@
-- * 'allowVersionUpgrade' - If @true@ , major version upgrades will be applied automatically to the cluster during the maintenance window.
--
-- Default: @false@
-- * 'clusterParameterGroupName' - The name of the cluster parameter group to apply to this cluster. This change is applied only after the cluster is rebooted. To reboot a cluster use 'RebootCluster' .
--
-- Default: Uses existing setting.
-- Constraints: The cluster parameter group must be in the same parameter group family that matches the cluster version.
mkModifyCluster ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  ModifyCluster
mkModifyCluster pClusterIdentifier_ =
  ModifyCluster'
    { manualSnapshotRetentionPeriod = Lude.Nothing,
      enhancedVPCRouting = Lude.Nothing,
      masterUserPassword = Lude.Nothing,
      publiclyAccessible = Lude.Nothing,
      maintenanceTrackName = Lude.Nothing,
      hsmConfigurationIdentifier = Lude.Nothing,
      clusterSecurityGroups = Lude.Nothing,
      automatedSnapshotRetentionPeriod = Lude.Nothing,
      encrypted = Lude.Nothing,
      clusterIdentifier = pClusterIdentifier_,
      hsmClientCertificateIdentifier = Lude.Nothing,
      numberOfNodes = Lude.Nothing,
      elasticIP = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      vpcSecurityGroupIds = Lude.Nothing,
      clusterType = Lude.Nothing,
      newClusterIdentifier = Lude.Nothing,
      clusterVersion = Lude.Nothing,
      nodeType = Lude.Nothing,
      allowVersionUpgrade = Lude.Nothing,
      clusterParameterGroupName = Lude.Nothing
    }

-- | The default for number of days that a newly created manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. This value doesn't retroactively change the retention periods of existing manual snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcManualSnapshotRetentionPeriod :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Int)
mcManualSnapshotRetentionPeriod = Lens.lens (manualSnapshotRetentionPeriod :: ModifyCluster -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRetentionPeriod = a} :: ModifyCluster)
{-# DEPRECATED mcManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
--
-- /Note:/ Consider using 'enhancedVPCRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcEnhancedVPCRouting :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Bool)
mcEnhancedVPCRouting = Lens.lens (enhancedVPCRouting :: ModifyCluster -> Lude.Maybe Lude.Bool) (\s a -> s {enhancedVPCRouting = a} :: ModifyCluster)
{-# DEPRECATED mcEnhancedVPCRouting "Use generic-lens or generic-optics with 'enhancedVPCRouting' instead." #-}

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
mcMasterUserPassword :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcMasterUserPassword = Lens.lens (masterUserPassword :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {masterUserPassword = a} :: ModifyCluster)
{-# DEPRECATED mcMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | If @true@ , the cluster can be accessed from a public network. Only clusters in VPCs can be set to be publicly available.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcPubliclyAccessible :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Bool)
mcPubliclyAccessible = Lens.lens (publiclyAccessible :: ModifyCluster -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: ModifyCluster)
{-# DEPRECATED mcPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | The name for the maintenance track that you want to assign for the cluster. This name change is asynchronous. The new track name stays in the @PendingModifiedValues@ for the cluster until the next maintenance window. When the maintenance track changes, the cluster is switched to the latest cluster release available for the maintenance track. At this point, the maintenance track name is applied.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcMaintenanceTrackName :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcMaintenanceTrackName = Lens.lens (maintenanceTrackName :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {maintenanceTrackName = a} :: ModifyCluster)
{-# DEPRECATED mcMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcHSMConfigurationIdentifier :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcHSMConfigurationIdentifier = Lens.lens (hsmConfigurationIdentifier :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {hsmConfigurationIdentifier = a} :: ModifyCluster)
{-# DEPRECATED mcHSMConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead." #-}

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
mcClusterSecurityGroups :: Lens.Lens' ModifyCluster (Lude.Maybe [Lude.Text])
mcClusterSecurityGroups = Lens.lens (clusterSecurityGroups :: ModifyCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {clusterSecurityGroups = a} :: ModifyCluster)
{-# DEPRECATED mcClusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead." #-}

-- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .
--
-- If you decrease the automated snapshot retention period from its current value, existing automated snapshots that fall outside of the new retention period will be immediately deleted.
-- Default: Uses existing setting.
-- Constraints: Must be a value from 0 to 35.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAutomatedSnapshotRetentionPeriod :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Int)
mcAutomatedSnapshotRetentionPeriod = Lens.lens (automatedSnapshotRetentionPeriod :: ModifyCluster -> Lude.Maybe Lude.Int) (\s a -> s {automatedSnapshotRetentionPeriod = a} :: ModifyCluster)
{-# DEPRECATED mcAutomatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead." #-}

-- | Indicates whether the cluster is encrypted. If the value is encrypted (true) and you provide a value for the @KmsKeyId@ parameter, we encrypt the cluster with the provided @KmsKeyId@ . If you don't provide a @KmsKeyId@ , we encrypt with the default key.
--
-- If the value is not encrypted (false), then the cluster is decrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcEncrypted :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Bool)
mcEncrypted = Lens.lens (encrypted :: ModifyCluster -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: ModifyCluster)
{-# DEPRECATED mcEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The unique identifier of the cluster to be modified.
--
-- Example: @examplecluster@
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterIdentifier :: Lens.Lens' ModifyCluster Lude.Text
mcClusterIdentifier = Lens.lens (clusterIdentifier :: ModifyCluster -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: ModifyCluster)
{-# DEPRECATED mcClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcHSMClientCertificateIdentifier :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcHSMClientCertificateIdentifier = Lens.lens (hsmClientCertificateIdentifier :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {hsmClientCertificateIdentifier = a} :: ModifyCluster)
{-# DEPRECATED mcHSMClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead." #-}

-- | The new number of nodes of the cluster. If you specify a new number of nodes, you must also specify the node type parameter.
--
-- For more information about resizing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
-- Valid Values: Integer greater than @0@ .
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcNumberOfNodes :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Int)
mcNumberOfNodes = Lens.lens (numberOfNodes :: ModifyCluster -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: ModifyCluster)
{-# DEPRECATED mcNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
--
-- /Note:/ Consider using 'elasticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcElasticIP :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcElasticIP = Lens.lens (elasticIP :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {elasticIP = a} :: ModifyCluster)
{-# DEPRECATED mcElasticIP "Use generic-lens or generic-optics with 'elasticIP' instead." #-}

-- | The weekly time range (in UTC) during which system maintenance can occur, if necessary. If system maintenance is necessary during the window, it may result in an outage.
--
-- This maintenance window change is made immediately. If the new maintenance window indicates the current time, there must be at least 120 minutes between the current time and end of the window in order to ensure that pending changes are applied.
-- Default: Uses existing setting.
-- Format: ddd:hh24:mi-ddd:hh24:mi, for example @wed:07:30-wed:08:00@ .
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcPreferredMaintenanceWindow :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: ModifyCluster)
{-# DEPRECATED mcPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcKMSKeyId :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcKMSKeyId = Lens.lens (kmsKeyId :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ModifyCluster)
{-# DEPRECATED mcKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A list of virtual private cloud (VPC) security groups to be associated with the cluster. This change is asynchronously applied as soon as possible.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcVPCSecurityGroupIds :: Lens.Lens' ModifyCluster (Lude.Maybe [Lude.Text])
mcVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: ModifyCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: ModifyCluster)
{-# DEPRECATED mcVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | The new cluster type.
--
-- When you submit your cluster resize request, your existing cluster goes into a read-only mode. After Amazon Redshift provisions a new cluster based on your resize requirements, there will be outage for a period while the old cluster is deleted and your connection is switched to the new cluster. You can use 'DescribeResize' to track the progress of the resize request.
-- Valid Values: @multi-node | single-node @
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterType :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcClusterType = Lens.lens (clusterType :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterType = a} :: ModifyCluster)
{-# DEPRECATED mcClusterType "Use generic-lens or generic-optics with 'clusterType' instead." #-}

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
mcNewClusterIdentifier :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcNewClusterIdentifier = Lens.lens (newClusterIdentifier :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {newClusterIdentifier = a} :: ModifyCluster)
{-# DEPRECATED mcNewClusterIdentifier "Use generic-lens or generic-optics with 'newClusterIdentifier' instead." #-}

-- | The new version number of the Amazon Redshift engine to upgrade to.
--
-- For major version upgrades, if a non-default cluster parameter group is currently in use, a new cluster parameter group in the cluster parameter group family for the new version must be specified. The new cluster parameter group can be the default for that cluster parameter group family. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
-- Example: @1.0@
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterVersion :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcClusterVersion = Lens.lens (clusterVersion :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterVersion = a} :: ModifyCluster)
{-# DEPRECATED mcClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | The new node type of the cluster. If you specify a new node type, you must also specify the number of nodes parameter.
--
-- For more information about resizing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@ | @ra3.4xlarge@ | @ra3.16xlarge@
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcNodeType :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcNodeType = Lens.lens (nodeType :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: ModifyCluster)
{-# DEPRECATED mcNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | If @true@ , major version upgrades will be applied automatically to the cluster during the maintenance window.
--
-- Default: @false@
--
-- /Note:/ Consider using 'allowVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcAllowVersionUpgrade :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Bool)
mcAllowVersionUpgrade = Lens.lens (allowVersionUpgrade :: ModifyCluster -> Lude.Maybe Lude.Bool) (\s a -> s {allowVersionUpgrade = a} :: ModifyCluster)
{-# DEPRECATED mcAllowVersionUpgrade "Use generic-lens or generic-optics with 'allowVersionUpgrade' instead." #-}

-- | The name of the cluster parameter group to apply to this cluster. This change is applied only after the cluster is rebooted. To reboot a cluster use 'RebootCluster' .
--
-- Default: Uses existing setting.
-- Constraints: The cluster parameter group must be in the same parameter group family that matches the cluster version.
--
-- /Note:/ Consider using 'clusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterParameterGroupName :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Text)
mcClusterParameterGroupName = Lens.lens (clusterParameterGroupName :: ModifyCluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterParameterGroupName = a} :: ModifyCluster)
{-# DEPRECATED mcClusterParameterGroupName "Use generic-lens or generic-optics with 'clusterParameterGroupName' instead." #-}

instance Lude.AWSRequest ModifyCluster where
  type Rs ModifyCluster = ModifyClusterResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ModifyClusterResult"
      ( \s h x ->
          ModifyClusterResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyCluster where
  toQuery ModifyCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Lude.=: manualSnapshotRetentionPeriod,
        "EnhancedVpcRouting" Lude.=: enhancedVPCRouting,
        "MasterUserPassword" Lude.=: masterUserPassword,
        "PubliclyAccessible" Lude.=: publiclyAccessible,
        "MaintenanceTrackName" Lude.=: maintenanceTrackName,
        "HsmConfigurationIdentifier" Lude.=: hsmConfigurationIdentifier,
        "ClusterSecurityGroups"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "ClusterSecurityGroupName"
                Lude.<$> clusterSecurityGroups
            ),
        "AutomatedSnapshotRetentionPeriod"
          Lude.=: automatedSnapshotRetentionPeriod,
        "Encrypted" Lude.=: encrypted,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "HsmClientCertificateIdentifier"
          Lude.=: hsmClientCertificateIdentifier,
        "NumberOfNodes" Lude.=: numberOfNodes,
        "ElasticIp" Lude.=: elasticIP,
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "KmsKeyId" Lude.=: kmsKeyId,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "ClusterType" Lude.=: clusterType,
        "NewClusterIdentifier" Lude.=: newClusterIdentifier,
        "ClusterVersion" Lude.=: clusterVersion,
        "NodeType" Lude.=: nodeType,
        "AllowVersionUpgrade" Lude.=: allowVersionUpgrade,
        "ClusterParameterGroupName" Lude.=: clusterParameterGroupName
      ]

-- | /See:/ 'mkModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' -
-- * 'responseStatus' - The response status code.
mkModifyClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyClusterResponse
mkModifyClusterResponse pResponseStatus_ =
  ModifyClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrsCluster :: Lens.Lens' ModifyClusterResponse (Lude.Maybe Cluster)
mcrsCluster = Lens.lens (cluster :: ModifyClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: ModifyClusterResponse)
{-# DEPRECATED mcrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrsResponseStatus :: Lens.Lens' ModifyClusterResponse Lude.Int
mcrsResponseStatus = Lens.lens (responseStatus :: ModifyClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyClusterResponse)
{-# DEPRECATED mcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
