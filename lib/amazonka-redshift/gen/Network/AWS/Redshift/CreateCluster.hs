{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cluster with the specified parameters.
--
-- To create a cluster in Virtual Private Cloud (VPC), you must provide a cluster subnet group name. The cluster subnet group identifies the subnets of your VPC that Amazon Redshift uses when creating the cluster. For more information about managing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CreateCluster
  ( -- * Creating a request
    CreateCluster (..),
    mkCreateCluster,

    -- ** Request lenses
    ccManualSnapshotRetentionPeriod,
    ccEnhancedVPCRouting,
    ccAdditionalInfo,
    ccSnapshotScheduleIdentifier,
    ccPubliclyAccessible,
    ccMaintenanceTrackName,
    ccHSMConfigurationIdentifier,
    ccClusterSecurityGroups,
    ccAutomatedSnapshotRetentionPeriod,
    ccEncrypted,
    ccClusterSubnetGroupName,
    ccHSMClientCertificateIdentifier,
    ccNumberOfNodes,
    ccElasticIP,
    ccPreferredMaintenanceWindow,
    ccKMSKeyId,
    ccAvailabilityZone,
    ccVPCSecurityGroupIds,
    ccIAMRoles,
    ccClusterType,
    ccClusterVersion,
    ccAllowVersionUpgrade,
    ccClusterParameterGroupName,
    ccTags,
    ccPort,
    ccDBName,
    ccClusterIdentifier,
    ccNodeType,
    ccMasterUsername,
    ccMasterUserPassword,

    -- * Destructuring the response
    CreateClusterResponse (..),
    mkCreateClusterResponse,

    -- ** Response lenses
    ccrsCluster,
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { manualSnapshotRetentionPeriod ::
      Lude.Maybe Lude.Int,
    enhancedVPCRouting :: Lude.Maybe Lude.Bool,
    additionalInfo :: Lude.Maybe Lude.Text,
    snapshotScheduleIdentifier :: Lude.Maybe Lude.Text,
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    maintenanceTrackName :: Lude.Maybe Lude.Text,
    hsmConfigurationIdentifier :: Lude.Maybe Lude.Text,
    clusterSecurityGroups :: Lude.Maybe [Lude.Text],
    automatedSnapshotRetentionPeriod :: Lude.Maybe Lude.Int,
    encrypted :: Lude.Maybe Lude.Bool,
    clusterSubnetGroupName :: Lude.Maybe Lude.Text,
    hsmClientCertificateIdentifier :: Lude.Maybe Lude.Text,
    numberOfNodes :: Lude.Maybe Lude.Int,
    elasticIP :: Lude.Maybe Lude.Text,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    iamRoles :: Lude.Maybe [Lude.Text],
    clusterType :: Lude.Maybe Lude.Text,
    clusterVersion :: Lude.Maybe Lude.Text,
    allowVersionUpgrade :: Lude.Maybe Lude.Bool,
    clusterParameterGroupName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    port :: Lude.Maybe Lude.Int,
    dbName :: Lude.Maybe Lude.Text,
    clusterIdentifier :: Lude.Text,
    nodeType :: Lude.Text,
    masterUsername :: Lude.Text,
    masterUserPassword :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCluster' with the minimum fields required to make a request.
--
-- * 'additionalInfo' - Reserved.
-- * 'allowVersionUpgrade' - If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster.
--
-- When a new major version of the Amazon Redshift engine is released, you can request that the service automatically apply upgrades during the maintenance window to the Amazon Redshift engine that is running on your cluster.
-- Default: @true@
-- * 'automatedSnapshotRetentionPeriod' - The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .
--
-- Default: @1@
-- Constraints: Must be a value from 0 to 35.
-- * 'availabilityZone' - The EC2 Availability Zone (AZ) in which you want Amazon Redshift to provision the cluster. For example, if you have several EC2 instances running in a specific Availability Zone, then you might want the cluster to be provisioned in the same zone in order to decrease network latency.
--
-- Default: A random, system-chosen Availability Zone in the region that is specified by the endpoint.
-- Example: @us-east-2d@
-- Constraint: The specified Availability Zone must be in the same region as the current endpoint.
-- * 'clusterIdentifier' - A unique identifier for the cluster. You use this identifier to refer to the cluster for any subsequent cluster operations such as deleting or modifying. The identifier also appears in the Amazon Redshift console.
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
-- Example: @myexamplecluster@
-- * 'clusterParameterGroupName' - The name of the parameter group to be associated with this cluster.
--
-- Default: The default Amazon Redshift cluster parameter group. For information about the default parameter group, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>
-- Constraints:
--
--     * Must be 1 to 255 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
-- * 'clusterSecurityGroups' - A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
-- * 'clusterSubnetGroupName' - The name of a cluster subnet group to be associated with this cluster.
--
-- If this parameter is not provided the resulting cluster will be deployed outside virtual private cloud (VPC).
-- * 'clusterType' - The type of the cluster. When cluster type is specified as
--
--
--     * @single-node@ , the __NumberOfNodes__ parameter is not required.
--
--
--     * @multi-node@ , the __NumberOfNodes__ parameter is required.
--
--
-- Valid Values: @multi-node@ | @single-node@
-- Default: @multi-node@
-- * 'clusterVersion' - The version of the Amazon Redshift engine software that you want to deploy on the cluster.
--
-- The version selected runs on all the nodes in the cluster.
-- Constraints: Only version 1.0 is currently available.
-- Example: @1.0@
-- * 'dbName' - The name of the first database to be created when the cluster is created.
--
-- To create additional databases after the cluster is created, connect to the cluster with a SQL client and use SQL commands to create a database. For more information, go to <https://docs.aws.amazon.com/redshift/latest/dg/t_creating_database.html Create a Database> in the Amazon Redshift Database Developer Guide.
-- Default: @dev@
-- Constraints:
--
--     * Must contain 1 to 64 alphanumeric characters.
--
--
--     * Must contain only lowercase letters.
--
--
--     * Cannot be a word that is reserved by the service. A list of reserved words can be found in <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
--
-- * 'elasticIP' - The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
-- * 'encrypted' - If @true@ , the data in the cluster is encrypted at rest.
--
-- Default: false
-- * 'enhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
-- * 'hsmClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
-- * 'hsmConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
-- * 'iamRoles' - A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
--
-- A cluster can have up to 10 IAM roles associated with it at any time.
-- * 'kmsKeyId' - The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
-- * 'maintenanceTrackName' - An optional parameter for the name of the maintenance track for the cluster. If you don't provide a maintenance track name, the cluster is assigned to the @current@ track.
-- * 'manualSnapshotRetentionPeriod' - The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- * 'masterUserPassword' - The password associated with the master user account for the cluster that is being created.
--
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
-- * 'masterUsername' - The user name associated with the master user account for the cluster that is being created.
--
-- Constraints:
--
--     * Must be 1 - 128 alphanumeric characters. The user name can't be @PUBLIC@ .
--
--
--     * First character must be a letter.
--
--
--     * Cannot be a reserved word. A list of reserved words can be found in <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
--
-- * 'nodeType' - The node type to be provisioned for the cluster. For information about node types, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@ | @ra3.4xlarge@ | @ra3.16xlarge@
-- * 'numberOfNodes' - The number of compute nodes in the cluster. This parameter is required when the __ClusterType__ parameter is specified as @multi-node@ .
--
-- For information about determining how many nodes you need, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .
-- If you don't specify this parameter, you get a single-node cluster. When requesting a multi-node cluster, you must specify the number of nodes that you want in the cluster.
-- Default: @1@
-- Constraints: Value must be at least 1 and no more than 100.
-- * 'port' - The port number on which the cluster accepts incoming connections.
--
-- The cluster is accessible only via the JDBC and ODBC connection strings. Part of the connection string requires the port on which the cluster will listen for incoming connections.
-- Default: @5439@
-- Valid Values: @1150-65535@
-- * 'preferredMaintenanceWindow' - The weekly time range (in UTC) during which automated cluster maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- Default: A 30-minute window selected at random from an 8-hour block of time per region, occurring on a random day of the week. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide.
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Minimum 30-minute window.
-- * 'publiclyAccessible' - If @true@ , the cluster can be accessed from a public network.
-- * 'snapshotScheduleIdentifier' - A unique identifier for the snapshot schedule.
-- * 'tags' - A list of tag instances.
-- * 'vpcSecurityGroupIds' - A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
mkCreateCluster ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  -- | 'nodeType'
  Lude.Text ->
  -- | 'masterUsername'
  Lude.Text ->
  -- | 'masterUserPassword'
  Lude.Text ->
  CreateCluster
mkCreateCluster
  pClusterIdentifier_
  pNodeType_
  pMasterUsername_
  pMasterUserPassword_ =
    CreateCluster'
      { manualSnapshotRetentionPeriod = Lude.Nothing,
        enhancedVPCRouting = Lude.Nothing,
        additionalInfo = Lude.Nothing,
        snapshotScheduleIdentifier = Lude.Nothing,
        publiclyAccessible = Lude.Nothing,
        maintenanceTrackName = Lude.Nothing,
        hsmConfigurationIdentifier = Lude.Nothing,
        clusterSecurityGroups = Lude.Nothing,
        automatedSnapshotRetentionPeriod = Lude.Nothing,
        encrypted = Lude.Nothing,
        clusterSubnetGroupName = Lude.Nothing,
        hsmClientCertificateIdentifier = Lude.Nothing,
        numberOfNodes = Lude.Nothing,
        elasticIP = Lude.Nothing,
        preferredMaintenanceWindow = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        availabilityZone = Lude.Nothing,
        vpcSecurityGroupIds = Lude.Nothing,
        iamRoles = Lude.Nothing,
        clusterType = Lude.Nothing,
        clusterVersion = Lude.Nothing,
        allowVersionUpgrade = Lude.Nothing,
        clusterParameterGroupName = Lude.Nothing,
        tags = Lude.Nothing,
        port = Lude.Nothing,
        dbName = Lude.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        nodeType = pNodeType_,
        masterUsername = pMasterUsername_,
        masterUserPassword = pMasterUserPassword_
      }

-- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccManualSnapshotRetentionPeriod :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Int)
ccManualSnapshotRetentionPeriod = Lens.lens (manualSnapshotRetentionPeriod :: CreateCluster -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRetentionPeriod = a} :: CreateCluster)
{-# DEPRECATED ccManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
--
-- /Note:/ Consider using 'enhancedVPCRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEnhancedVPCRouting :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Bool)
ccEnhancedVPCRouting = Lens.lens (enhancedVPCRouting :: CreateCluster -> Lude.Maybe Lude.Bool) (\s a -> s {enhancedVPCRouting = a} :: CreateCluster)
{-# DEPRECATED ccEnhancedVPCRouting "Use generic-lens or generic-optics with 'enhancedVPCRouting' instead." #-}

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAdditionalInfo :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccAdditionalInfo = Lens.lens (additionalInfo :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {additionalInfo = a} :: CreateCluster)
{-# DEPRECATED ccAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | A unique identifier for the snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSnapshotScheduleIdentifier :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccSnapshotScheduleIdentifier = Lens.lens (snapshotScheduleIdentifier :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {snapshotScheduleIdentifier = a} :: CreateCluster)
{-# DEPRECATED ccSnapshotScheduleIdentifier "Use generic-lens or generic-optics with 'snapshotScheduleIdentifier' instead." #-}

-- | If @true@ , the cluster can be accessed from a public network.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPubliclyAccessible :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Bool)
ccPubliclyAccessible = Lens.lens (publiclyAccessible :: CreateCluster -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: CreateCluster)
{-# DEPRECATED ccPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | An optional parameter for the name of the maintenance track for the cluster. If you don't provide a maintenance track name, the cluster is assigned to the @current@ track.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccMaintenanceTrackName :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccMaintenanceTrackName = Lens.lens (maintenanceTrackName :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {maintenanceTrackName = a} :: CreateCluster)
{-# DEPRECATED ccMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHSMConfigurationIdentifier :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccHSMConfigurationIdentifier = Lens.lens (hsmConfigurationIdentifier :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {hsmConfigurationIdentifier = a} :: CreateCluster)
{-# DEPRECATED ccHSMConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead." #-}

-- | A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterSecurityGroups :: Lens.Lens' CreateCluster (Lude.Maybe [Lude.Text])
ccClusterSecurityGroups = Lens.lens (clusterSecurityGroups :: CreateCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {clusterSecurityGroups = a} :: CreateCluster)
{-# DEPRECATED ccClusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead." #-}

-- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .
--
-- Default: @1@
-- Constraints: Must be a value from 0 to 35.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAutomatedSnapshotRetentionPeriod :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Int)
ccAutomatedSnapshotRetentionPeriod = Lens.lens (automatedSnapshotRetentionPeriod :: CreateCluster -> Lude.Maybe Lude.Int) (\s a -> s {automatedSnapshotRetentionPeriod = a} :: CreateCluster)
{-# DEPRECATED ccAutomatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead." #-}

-- | If @true@ , the data in the cluster is encrypted at rest.
--
-- Default: false
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEncrypted :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Bool)
ccEncrypted = Lens.lens (encrypted :: CreateCluster -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: CreateCluster)
{-# DEPRECATED ccEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The name of a cluster subnet group to be associated with this cluster.
--
-- If this parameter is not provided the resulting cluster will be deployed outside virtual private cloud (VPC).
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterSubnetGroupName :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccClusterSubnetGroupName = Lens.lens (clusterSubnetGroupName :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterSubnetGroupName = a} :: CreateCluster)
{-# DEPRECATED ccClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHSMClientCertificateIdentifier :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccHSMClientCertificateIdentifier = Lens.lens (hsmClientCertificateIdentifier :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {hsmClientCertificateIdentifier = a} :: CreateCluster)
{-# DEPRECATED ccHSMClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead." #-}

-- | The number of compute nodes in the cluster. This parameter is required when the __ClusterType__ parameter is specified as @multi-node@ .
--
-- For information about determining how many nodes you need, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .
-- If you don't specify this parameter, you get a single-node cluster. When requesting a multi-node cluster, you must specify the number of nodes that you want in the cluster.
-- Default: @1@
-- Constraints: Value must be at least 1 and no more than 100.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNumberOfNodes :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Int)
ccNumberOfNodes = Lens.lens (numberOfNodes :: CreateCluster -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: CreateCluster)
{-# DEPRECATED ccNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
--
-- /Note:/ Consider using 'elasticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccElasticIP :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccElasticIP = Lens.lens (elasticIP :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {elasticIP = a} :: CreateCluster)
{-# DEPRECATED ccElasticIP "Use generic-lens or generic-optics with 'elasticIP' instead." #-}

-- | The weekly time range (in UTC) during which automated cluster maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- Default: A 30-minute window selected at random from an 8-hour block of time per region, occurring on a random day of the week. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide.
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPreferredMaintenanceWindow :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: CreateCluster)
{-# DEPRECATED ccPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccKMSKeyId :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccKMSKeyId = Lens.lens (kmsKeyId :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateCluster)
{-# DEPRECATED ccKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The EC2 Availability Zone (AZ) in which you want Amazon Redshift to provision the cluster. For example, if you have several EC2 instances running in a specific Availability Zone, then you might want the cluster to be provisioned in the same zone in order to decrease network latency.
--
-- Default: A random, system-chosen Availability Zone in the region that is specified by the endpoint.
-- Example: @us-east-2d@
-- Constraint: The specified Availability Zone must be in the same region as the current endpoint.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAvailabilityZone :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccAvailabilityZone = Lens.lens (availabilityZone :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateCluster)
{-# DEPRECATED ccAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVPCSecurityGroupIds :: Lens.Lens' CreateCluster (Lude.Maybe [Lude.Text])
ccVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: CreateCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: CreateCluster)
{-# DEPRECATED ccVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
--
-- A cluster can have up to 10 IAM roles associated with it at any time.
--
-- /Note:/ Consider using 'iamRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccIAMRoles :: Lens.Lens' CreateCluster (Lude.Maybe [Lude.Text])
ccIAMRoles = Lens.lens (iamRoles :: CreateCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {iamRoles = a} :: CreateCluster)
{-# DEPRECATED ccIAMRoles "Use generic-lens or generic-optics with 'iamRoles' instead." #-}

-- | The type of the cluster. When cluster type is specified as
--
--
--     * @single-node@ , the __NumberOfNodes__ parameter is not required.
--
--
--     * @multi-node@ , the __NumberOfNodes__ parameter is required.
--
--
-- Valid Values: @multi-node@ | @single-node@
-- Default: @multi-node@
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterType :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccClusterType = Lens.lens (clusterType :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterType = a} :: CreateCluster)
{-# DEPRECATED ccClusterType "Use generic-lens or generic-optics with 'clusterType' instead." #-}

-- | The version of the Amazon Redshift engine software that you want to deploy on the cluster.
--
-- The version selected runs on all the nodes in the cluster.
-- Constraints: Only version 1.0 is currently available.
-- Example: @1.0@
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterVersion :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccClusterVersion = Lens.lens (clusterVersion :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterVersion = a} :: CreateCluster)
{-# DEPRECATED ccClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster.
--
-- When a new major version of the Amazon Redshift engine is released, you can request that the service automatically apply upgrades during the maintenance window to the Amazon Redshift engine that is running on your cluster.
-- Default: @true@
--
-- /Note:/ Consider using 'allowVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAllowVersionUpgrade :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Bool)
ccAllowVersionUpgrade = Lens.lens (allowVersionUpgrade :: CreateCluster -> Lude.Maybe Lude.Bool) (\s a -> s {allowVersionUpgrade = a} :: CreateCluster)
{-# DEPRECATED ccAllowVersionUpgrade "Use generic-lens or generic-optics with 'allowVersionUpgrade' instead." #-}

-- | The name of the parameter group to be associated with this cluster.
--
-- Default: The default Amazon Redshift cluster parameter group. For information about the default parameter group, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>
-- Constraints:
--
--     * Must be 1 to 255 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'clusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterParameterGroupName :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccClusterParameterGroupName = Lens.lens (clusterParameterGroupName :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterParameterGroupName = a} :: CreateCluster)
{-# DEPRECATED ccClusterParameterGroupName "Use generic-lens or generic-optics with 'clusterParameterGroupName' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateCluster (Lude.Maybe [Tag])
ccTags = Lens.lens (tags :: CreateCluster -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCluster)
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which the cluster accepts incoming connections.
--
-- The cluster is accessible only via the JDBC and ODBC connection strings. Part of the connection string requires the port on which the cluster will listen for incoming connections.
-- Default: @5439@
-- Valid Values: @1150-65535@
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPort :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Int)
ccPort = Lens.lens (port :: CreateCluster -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: CreateCluster)
{-# DEPRECATED ccPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The name of the first database to be created when the cluster is created.
--
-- To create additional databases after the cluster is created, connect to the cluster with a SQL client and use SQL commands to create a database. For more information, go to <https://docs.aws.amazon.com/redshift/latest/dg/t_creating_database.html Create a Database> in the Amazon Redshift Database Developer Guide.
-- Default: @dev@
-- Constraints:
--
--     * Must contain 1 to 64 alphanumeric characters.
--
--
--     * Must contain only lowercase letters.
--
--
--     * Cannot be a word that is reserved by the service. A list of reserved words can be found in <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
--
--
-- /Note:/ Consider using 'dbName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDBName :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccDBName = Lens.lens (dbName :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {dbName = a} :: CreateCluster)
{-# DEPRECATED ccDBName "Use generic-lens or generic-optics with 'dbName' instead." #-}

-- | A unique identifier for the cluster. You use this identifier to refer to the cluster for any subsequent cluster operations such as deleting or modifying. The identifier also appears in the Amazon Redshift console.
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
-- Example: @myexamplecluster@
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterIdentifier :: Lens.Lens' CreateCluster Lude.Text
ccClusterIdentifier = Lens.lens (clusterIdentifier :: CreateCluster -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: CreateCluster)
{-# DEPRECATED ccClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The node type to be provisioned for the cluster. For information about node types, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@ | @ra3.4xlarge@ | @ra3.16xlarge@
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNodeType :: Lens.Lens' CreateCluster Lude.Text
ccNodeType = Lens.lens (nodeType :: CreateCluster -> Lude.Text) (\s a -> s {nodeType = a} :: CreateCluster)
{-# DEPRECATED ccNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The user name associated with the master user account for the cluster that is being created.
--
-- Constraints:
--
--     * Must be 1 - 128 alphanumeric characters. The user name can't be @PUBLIC@ .
--
--
--     * First character must be a letter.
--
--
--     * Cannot be a reserved word. A list of reserved words can be found in <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
--
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccMasterUsername :: Lens.Lens' CreateCluster Lude.Text
ccMasterUsername = Lens.lens (masterUsername :: CreateCluster -> Lude.Text) (\s a -> s {masterUsername = a} :: CreateCluster)
{-# DEPRECATED ccMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The password associated with the master user account for the cluster that is being created.
--
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
ccMasterUserPassword :: Lens.Lens' CreateCluster Lude.Text
ccMasterUserPassword = Lens.lens (masterUserPassword :: CreateCluster -> Lude.Text) (\s a -> s {masterUserPassword = a} :: CreateCluster)
{-# DEPRECATED ccMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

instance Lude.AWSRequest CreateCluster where
  type Rs CreateCluster = CreateClusterResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CreateClusterResult"
      ( \s h x ->
          CreateClusterResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCluster where
  toQuery CreateCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Lude.=: manualSnapshotRetentionPeriod,
        "EnhancedVpcRouting" Lude.=: enhancedVPCRouting,
        "AdditionalInfo" Lude.=: additionalInfo,
        "SnapshotScheduleIdentifier" Lude.=: snapshotScheduleIdentifier,
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
        "ClusterSubnetGroupName" Lude.=: clusterSubnetGroupName,
        "HsmClientCertificateIdentifier"
          Lude.=: hsmClientCertificateIdentifier,
        "NumberOfNodes" Lude.=: numberOfNodes,
        "ElasticIp" Lude.=: elasticIP,
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "KmsKeyId" Lude.=: kmsKeyId,
        "AvailabilityZone" Lude.=: availabilityZone,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "IamRoles"
          Lude.=: Lude.toQuery (Lude.toQueryList "IamRoleArn" Lude.<$> iamRoles),
        "ClusterType" Lude.=: clusterType,
        "ClusterVersion" Lude.=: clusterVersion,
        "AllowVersionUpgrade" Lude.=: allowVersionUpgrade,
        "ClusterParameterGroupName" Lude.=: clusterParameterGroupName,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "Port" Lude.=: port,
        "DBName" Lude.=: dbName,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "NodeType" Lude.=: nodeType,
        "MasterUsername" Lude.=: masterUsername,
        "MasterUserPassword" Lude.=: masterUserPassword
      ]

-- | /See:/ 'mkCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { cluster ::
      Lude.Maybe Cluster,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateClusterResponse
mkCreateClusterResponse pResponseStatus_ =
  CreateClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsCluster :: Lens.Lens' CreateClusterResponse (Lude.Maybe Cluster)
ccrsCluster = Lens.lens (cluster :: CreateClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: CreateClusterResponse)
{-# DEPRECATED ccrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateClusterResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateClusterResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
