{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ccClusterIdentifier,
    ccNodeType,
    ccMasterUsername,
    ccMasterUserPassword,
    ccAdditionalInfo,
    ccAllowVersionUpgrade,
    ccAutomatedSnapshotRetentionPeriod,
    ccAvailabilityZone,
    ccClusterParameterGroupName,
    ccClusterSecurityGroups,
    ccClusterSubnetGroupName,
    ccClusterType,
    ccClusterVersion,
    ccDBName,
    ccElasticIp,
    ccEncrypted,
    ccEnhancedVpcRouting,
    ccHsmClientCertificateIdentifier,
    ccHsmConfigurationIdentifier,
    ccIamRoles,
    ccKmsKeyId,
    ccMaintenanceTrackName,
    ccManualSnapshotRetentionPeriod,
    ccNumberOfNodes,
    ccPort,
    ccPreferredMaintenanceWindow,
    ccPubliclyAccessible,
    ccSnapshotScheduleIdentifier,
    ccTags,
    ccVpcSecurityGroupIds,

    -- * Destructuring the response
    CreateClusterResponse (..),
    mkCreateClusterResponse,

    -- ** Response lenses
    ccrrsCluster,
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | A unique identifier for the cluster. You use this identifier to refer to the cluster for any subsequent cluster operations such as deleting or modifying. The identifier also appears in the Amazon Redshift console.
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
    clusterIdentifier :: Types.String,
    -- | The node type to be provisioned for the cluster. For information about node types, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .
    --
    -- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@ | @ra3.4xlarge@ | @ra3.16xlarge@
    nodeType :: Types.String,
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
    masterUsername :: Types.String,
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
    masterUserPassword :: Types.String,
    -- | Reserved.
    additionalInfo :: Core.Maybe Types.String,
    -- | If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster.
    --
    -- When a new major version of the Amazon Redshift engine is released, you can request that the service automatically apply upgrades during the maintenance window to the Amazon Redshift engine that is running on your cluster.
    -- Default: @true@
    allowVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .
    --
    -- Default: @1@
    -- Constraints: Must be a value from 0 to 35.
    automatedSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The EC2 Availability Zone (AZ) in which you want Amazon Redshift to provision the cluster. For example, if you have several EC2 instances running in a specific Availability Zone, then you might want the cluster to be provisioned in the same zone in order to decrease network latency.
    --
    -- Default: A random, system-chosen Availability Zone in the region that is specified by the endpoint.
    -- Example: @us-east-2d@
    -- Constraint: The specified Availability Zone must be in the same region as the current endpoint.
    availabilityZone :: Core.Maybe Types.String,
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
    clusterParameterGroupName :: Core.Maybe Types.String,
    -- | A list of security groups to be associated with this cluster.
    --
    -- Default: The default cluster security group for Amazon Redshift.
    clusterSecurityGroups :: Core.Maybe [Types.String],
    -- | The name of a cluster subnet group to be associated with this cluster.
    --
    -- If this parameter is not provided the resulting cluster will be deployed outside virtual private cloud (VPC).
    clusterSubnetGroupName :: Core.Maybe Types.String,
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
    clusterType :: Core.Maybe Types.String,
    -- | The version of the Amazon Redshift engine software that you want to deploy on the cluster.
    --
    -- The version selected runs on all the nodes in the cluster.
    -- Constraints: Only version 1.0 is currently available.
    -- Example: @1.0@
    clusterVersion :: Core.Maybe Types.String,
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
    dBName :: Core.Maybe Types.String,
    -- | The Elastic IP (EIP) address for the cluster.
    --
    -- Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
    elasticIp :: Core.Maybe Types.String,
    -- | If @true@ , the data in the cluster is encrypted at rest.
    --
    -- Default: false
    encrypted :: Core.Maybe Core.Bool,
    -- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@ , enhanced VPC routing is enabled.
    -- Default: false
    enhancedVpcRouting :: Core.Maybe Core.Bool,
    -- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
    hsmClientCertificateIdentifier :: Core.Maybe Types.String,
    -- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
    hsmConfigurationIdentifier :: Core.Maybe Types.String,
    -- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
    --
    -- A cluster can have up to 10 IAM roles associated with it at any time.
    iamRoles :: Core.Maybe [Types.String],
    -- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
    kmsKeyId :: Core.Maybe Types.String,
    -- | An optional parameter for the name of the maintenance track for the cluster. If you don't provide a maintenance track name, the cluster is assigned to the @current@ track.
    maintenanceTrackName :: Core.Maybe Types.String,
    -- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The number of compute nodes in the cluster. This parameter is required when the __ClusterType__ parameter is specified as @multi-node@ .
    --
    -- For information about determining how many nodes you need, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .
    -- If you don't specify this parameter, you get a single-node cluster. When requesting a multi-node cluster, you must specify the number of nodes that you want in the cluster.
    -- Default: @1@
    -- Constraints: Value must be at least 1 and no more than 100.
    numberOfNodes :: Core.Maybe Core.Int,
    -- | The port number on which the cluster accepts incoming connections.
    --
    -- The cluster is accessible only via the JDBC and ODBC connection strings. Part of the connection string requires the port on which the cluster will listen for incoming connections.
    -- Default: @5439@
    -- Valid Values: @1150-65535@
    port :: Core.Maybe Core.Int,
    -- | The weekly time range (in UTC) during which automated cluster maintenance can occur.
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    -- Default: A 30-minute window selected at random from an 8-hour block of time per region, occurring on a random day of the week. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide.
    -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Core.Maybe Types.String,
    -- | If @true@ , the cluster can be accessed from a public network.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | A unique identifier for the snapshot schedule.
    snapshotScheduleIdentifier :: Core.Maybe Types.String,
    -- | A list of tag instances.
    tags :: Core.Maybe [Types.Tag],
    -- | A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
    --
    -- Default: The default VPC security group is associated with the cluster.
    vpcSecurityGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCluster' value with any optional fields omitted.
mkCreateCluster ::
  -- | 'clusterIdentifier'
  Types.String ->
  -- | 'nodeType'
  Types.String ->
  -- | 'masterUsername'
  Types.String ->
  -- | 'masterUserPassword'
  Types.String ->
  CreateCluster
mkCreateCluster
  clusterIdentifier
  nodeType
  masterUsername
  masterUserPassword =
    CreateCluster'
      { clusterIdentifier,
        nodeType,
        masterUsername,
        masterUserPassword,
        additionalInfo = Core.Nothing,
        allowVersionUpgrade = Core.Nothing,
        automatedSnapshotRetentionPeriod = Core.Nothing,
        availabilityZone = Core.Nothing,
        clusterParameterGroupName = Core.Nothing,
        clusterSecurityGroups = Core.Nothing,
        clusterSubnetGroupName = Core.Nothing,
        clusterType = Core.Nothing,
        clusterVersion = Core.Nothing,
        dBName = Core.Nothing,
        elasticIp = Core.Nothing,
        encrypted = Core.Nothing,
        enhancedVpcRouting = Core.Nothing,
        hsmClientCertificateIdentifier = Core.Nothing,
        hsmConfigurationIdentifier = Core.Nothing,
        iamRoles = Core.Nothing,
        kmsKeyId = Core.Nothing,
        maintenanceTrackName = Core.Nothing,
        manualSnapshotRetentionPeriod = Core.Nothing,
        numberOfNodes = Core.Nothing,
        port = Core.Nothing,
        preferredMaintenanceWindow = Core.Nothing,
        publiclyAccessible = Core.Nothing,
        snapshotScheduleIdentifier = Core.Nothing,
        tags = Core.Nothing,
        vpcSecurityGroupIds = Core.Nothing
      }

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
ccClusterIdentifier :: Lens.Lens' CreateCluster Types.String
ccClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED ccClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The node type to be provisioned for the cluster. For information about node types, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@ | @ra3.4xlarge@ | @ra3.16xlarge@
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNodeType :: Lens.Lens' CreateCluster Types.String
ccNodeType = Lens.field @"nodeType"
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
ccMasterUsername :: Lens.Lens' CreateCluster Types.String
ccMasterUsername = Lens.field @"masterUsername"
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
ccMasterUserPassword :: Lens.Lens' CreateCluster Types.String
ccMasterUserPassword = Lens.field @"masterUserPassword"
{-# DEPRECATED ccMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAdditionalInfo :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccAdditionalInfo = Lens.field @"additionalInfo"
{-# DEPRECATED ccAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster.
--
-- When a new major version of the Amazon Redshift engine is released, you can request that the service automatically apply upgrades during the maintenance window to the Amazon Redshift engine that is running on your cluster.
-- Default: @true@
--
-- /Note:/ Consider using 'allowVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAllowVersionUpgrade :: Lens.Lens' CreateCluster (Core.Maybe Core.Bool)
ccAllowVersionUpgrade = Lens.field @"allowVersionUpgrade"
{-# DEPRECATED ccAllowVersionUpgrade "Use generic-lens or generic-optics with 'allowVersionUpgrade' instead." #-}

-- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' .
--
-- Default: @1@
-- Constraints: Must be a value from 0 to 35.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAutomatedSnapshotRetentionPeriod :: Lens.Lens' CreateCluster (Core.Maybe Core.Int)
ccAutomatedSnapshotRetentionPeriod = Lens.field @"automatedSnapshotRetentionPeriod"
{-# DEPRECATED ccAutomatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead." #-}

-- | The EC2 Availability Zone (AZ) in which you want Amazon Redshift to provision the cluster. For example, if you have several EC2 instances running in a specific Availability Zone, then you might want the cluster to be provisioned in the same zone in order to decrease network latency.
--
-- Default: A random, system-chosen Availability Zone in the region that is specified by the endpoint.
-- Example: @us-east-2d@
-- Constraint: The specified Availability Zone must be in the same region as the current endpoint.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAvailabilityZone :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED ccAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

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
ccClusterParameterGroupName :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccClusterParameterGroupName = Lens.field @"clusterParameterGroupName"
{-# DEPRECATED ccClusterParameterGroupName "Use generic-lens or generic-optics with 'clusterParameterGroupName' instead." #-}

-- | A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterSecurityGroups :: Lens.Lens' CreateCluster (Core.Maybe [Types.String])
ccClusterSecurityGroups = Lens.field @"clusterSecurityGroups"
{-# DEPRECATED ccClusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead." #-}

-- | The name of a cluster subnet group to be associated with this cluster.
--
-- If this parameter is not provided the resulting cluster will be deployed outside virtual private cloud (VPC).
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterSubnetGroupName :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# DEPRECATED ccClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

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
ccClusterType :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccClusterType = Lens.field @"clusterType"
{-# DEPRECATED ccClusterType "Use generic-lens or generic-optics with 'clusterType' instead." #-}

-- | The version of the Amazon Redshift engine software that you want to deploy on the cluster.
--
-- The version selected runs on all the nodes in the cluster.
-- Constraints: Only version 1.0 is currently available.
-- Example: @1.0@
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterVersion :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccClusterVersion = Lens.field @"clusterVersion"
{-# DEPRECATED ccClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

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
-- /Note:/ Consider using 'dBName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDBName :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccDBName = Lens.field @"dBName"
{-# DEPRECATED ccDBName "Use generic-lens or generic-optics with 'dBName' instead." #-}

-- | The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccElasticIp :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccElasticIp = Lens.field @"elasticIp"
{-# DEPRECATED ccElasticIp "Use generic-lens or generic-optics with 'elasticIp' instead." #-}

-- | If @true@ , the data in the cluster is encrypted at rest.
--
-- Default: false
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEncrypted :: Lens.Lens' CreateCluster (Core.Maybe Core.Bool)
ccEncrypted = Lens.field @"encrypted"
{-# DEPRECATED ccEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
--
-- /Note:/ Consider using 'enhancedVpcRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEnhancedVpcRouting :: Lens.Lens' CreateCluster (Core.Maybe Core.Bool)
ccEnhancedVpcRouting = Lens.field @"enhancedVpcRouting"
{-# DEPRECATED ccEnhancedVpcRouting "Use generic-lens or generic-optics with 'enhancedVpcRouting' instead." #-}

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHsmClientCertificateIdentifier :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccHsmClientCertificateIdentifier = Lens.field @"hsmClientCertificateIdentifier"
{-# DEPRECATED ccHsmClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead." #-}

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHsmConfigurationIdentifier :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccHsmConfigurationIdentifier = Lens.field @"hsmConfigurationIdentifier"
{-# DEPRECATED ccHsmConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead." #-}

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
--
-- A cluster can have up to 10 IAM roles associated with it at any time.
--
-- /Note:/ Consider using 'iamRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccIamRoles :: Lens.Lens' CreateCluster (Core.Maybe [Types.String])
ccIamRoles = Lens.field @"iamRoles"
{-# DEPRECATED ccIamRoles "Use generic-lens or generic-optics with 'iamRoles' instead." #-}

-- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccKmsKeyId :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED ccKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | An optional parameter for the name of the maintenance track for the cluster. If you don't provide a maintenance track name, the cluster is assigned to the @current@ track.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccMaintenanceTrackName :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# DEPRECATED ccMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccManualSnapshotRetentionPeriod :: Lens.Lens' CreateCluster (Core.Maybe Core.Int)
ccManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# DEPRECATED ccManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | The number of compute nodes in the cluster. This parameter is required when the __ClusterType__ parameter is specified as @multi-node@ .
--
-- For information about determining how many nodes you need, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ .
-- If you don't specify this parameter, you get a single-node cluster. When requesting a multi-node cluster, you must specify the number of nodes that you want in the cluster.
-- Default: @1@
-- Constraints: Value must be at least 1 and no more than 100.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNumberOfNodes :: Lens.Lens' CreateCluster (Core.Maybe Core.Int)
ccNumberOfNodes = Lens.field @"numberOfNodes"
{-# DEPRECATED ccNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The port number on which the cluster accepts incoming connections.
--
-- The cluster is accessible only via the JDBC and ODBC connection strings. Part of the connection string requires the port on which the cluster will listen for incoming connections.
-- Default: @5439@
-- Valid Values: @1150-65535@
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPort :: Lens.Lens' CreateCluster (Core.Maybe Core.Int)
ccPort = Lens.field @"port"
{-# DEPRECATED ccPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The weekly time range (in UTC) during which automated cluster maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- Default: A 30-minute window selected at random from an 8-hour block of time per region, occurring on a random day of the week. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide.
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPreferredMaintenanceWindow :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED ccPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | If @true@ , the cluster can be accessed from a public network.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPubliclyAccessible :: Lens.Lens' CreateCluster (Core.Maybe Core.Bool)
ccPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# DEPRECATED ccPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | A unique identifier for the snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSnapshotScheduleIdentifier :: Lens.Lens' CreateCluster (Core.Maybe Types.String)
ccSnapshotScheduleIdentifier = Lens.field @"snapshotScheduleIdentifier"
{-# DEPRECATED ccSnapshotScheduleIdentifier "Use generic-lens or generic-optics with 'snapshotScheduleIdentifier' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateCluster (Core.Maybe [Types.Tag])
ccTags = Lens.field @"tags"
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVpcSecurityGroupIds :: Lens.Lens' CreateCluster (Core.Maybe [Types.String])
ccVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# DEPRECATED ccVpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

instance Core.AWSRequest CreateCluster where
  type Rs CreateCluster = CreateClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateCluster")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
                Core.<> (Core.toQueryValue "NodeType" nodeType)
                Core.<> (Core.toQueryValue "MasterUsername" masterUsername)
                Core.<> (Core.toQueryValue "MasterUserPassword" masterUserPassword)
                Core.<> (Core.toQueryValue "AdditionalInfo" Core.<$> additionalInfo)
                Core.<> ( Core.toQueryValue "AllowVersionUpgrade"
                            Core.<$> allowVersionUpgrade
                        )
                Core.<> ( Core.toQueryValue "AutomatedSnapshotRetentionPeriod"
                            Core.<$> automatedSnapshotRetentionPeriod
                        )
                Core.<> (Core.toQueryValue "AvailabilityZone" Core.<$> availabilityZone)
                Core.<> ( Core.toQueryValue "ClusterParameterGroupName"
                            Core.<$> clusterParameterGroupName
                        )
                Core.<> ( Core.toQueryValue
                            "ClusterSecurityGroups"
                            ( Core.toQueryList "ClusterSecurityGroupName"
                                Core.<$> clusterSecurityGroups
                            )
                        )
                Core.<> ( Core.toQueryValue "ClusterSubnetGroupName"
                            Core.<$> clusterSubnetGroupName
                        )
                Core.<> (Core.toQueryValue "ClusterType" Core.<$> clusterType)
                Core.<> (Core.toQueryValue "ClusterVersion" Core.<$> clusterVersion)
                Core.<> (Core.toQueryValue "DBName" Core.<$> dBName)
                Core.<> (Core.toQueryValue "ElasticIp" Core.<$> elasticIp)
                Core.<> (Core.toQueryValue "Encrypted" Core.<$> encrypted)
                Core.<> ( Core.toQueryValue "EnhancedVpcRouting"
                            Core.<$> enhancedVpcRouting
                        )
                Core.<> ( Core.toQueryValue "HsmClientCertificateIdentifier"
                            Core.<$> hsmClientCertificateIdentifier
                        )
                Core.<> ( Core.toQueryValue "HsmConfigurationIdentifier"
                            Core.<$> hsmConfigurationIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "IamRoles"
                            (Core.toQueryList "IamRoleArn" Core.<$> iamRoles)
                        )
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> ( Core.toQueryValue "MaintenanceTrackName"
                            Core.<$> maintenanceTrackName
                        )
                Core.<> ( Core.toQueryValue "ManualSnapshotRetentionPeriod"
                            Core.<$> manualSnapshotRetentionPeriod
                        )
                Core.<> (Core.toQueryValue "NumberOfNodes" Core.<$> numberOfNodes)
                Core.<> (Core.toQueryValue "Port" Core.<$> port)
                Core.<> ( Core.toQueryValue "PreferredMaintenanceWindow"
                            Core.<$> preferredMaintenanceWindow
                        )
                Core.<> ( Core.toQueryValue "PubliclyAccessible"
                            Core.<$> publiclyAccessible
                        )
                Core.<> ( Core.toQueryValue "SnapshotScheduleIdentifier"
                            Core.<$> snapshotScheduleIdentifier
                        )
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
                Core.<> ( Core.toQueryValue
                            "VpcSecurityGroupIds"
                            ( Core.toQueryList "VpcSecurityGroupId"
                                Core.<$> vpcSecurityGroupIds
                            )
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateClusterResult"
      ( \s h x ->
          CreateClusterResponse'
            Core.<$> (x Core..@? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateClusterResponse' value with any optional fields omitted.
mkCreateClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateClusterResponse
mkCreateClusterResponse responseStatus =
  CreateClusterResponse' {cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCluster :: Lens.Lens' CreateClusterResponse (Core.Maybe Types.Cluster)
ccrrsCluster = Lens.field @"cluster"
{-# DEPRECATED ccrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateClusterResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
