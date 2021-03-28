{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateCluster (..)
    , mkCreateCluster
    -- ** Request lenses
    , ccClusterIdentifier
    , ccNodeType
    , ccMasterUsername
    , ccMasterUserPassword
    , ccAdditionalInfo
    , ccAllowVersionUpgrade
    , ccAutomatedSnapshotRetentionPeriod
    , ccAvailabilityZone
    , ccClusterParameterGroupName
    , ccClusterSecurityGroups
    , ccClusterSubnetGroupName
    , ccClusterType
    , ccClusterVersion
    , ccDBName
    , ccElasticIp
    , ccEncrypted
    , ccEnhancedVpcRouting
    , ccHsmClientCertificateIdentifier
    , ccHsmConfigurationIdentifier
    , ccIamRoles
    , ccKmsKeyId
    , ccMaintenanceTrackName
    , ccManualSnapshotRetentionPeriod
    , ccNumberOfNodes
    , ccPort
    , ccPreferredMaintenanceWindow
    , ccPubliclyAccessible
    , ccSnapshotScheduleIdentifier
    , ccTags
    , ccVpcSecurityGroupIds

    -- * Destructuring the response
    , CreateClusterResponse (..)
    , mkCreateClusterResponse
    -- ** Response lenses
    , ccrrsCluster
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { clusterIdentifier :: Core.Text
    -- ^ A unique identifier for the cluster. You use this identifier to refer to the cluster for any subsequent cluster operations such as deleting or modifying. The identifier also appears in the Amazon Redshift console.
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
  , nodeType :: Core.Text
    -- ^ The node type to be provisioned for the cluster. For information about node types, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ . 
--
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@ | @ra3.4xlarge@ | @ra3.16xlarge@ 
  , masterUsername :: Core.Text
    -- ^ The user name associated with the master user account for the cluster that is being created.
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
  , masterUserPassword :: Core.Text
    -- ^ The password associated with the master user account for the cluster that is being created.
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
  , additionalInfo :: Core.Maybe Core.Text
    -- ^ Reserved.
  , allowVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster.
--
-- When a new major version of the Amazon Redshift engine is released, you can request that the service automatically apply upgrades during the maintenance window to the Amazon Redshift engine that is running on your cluster.
-- Default: @true@ 
  , automatedSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' . 
--
-- Default: @1@ 
-- Constraints: Must be a value from 0 to 35.
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The EC2 Availability Zone (AZ) in which you want Amazon Redshift to provision the cluster. For example, if you have several EC2 instances running in a specific Availability Zone, then you might want the cluster to be provisioned in the same zone in order to decrease network latency.
--
-- Default: A random, system-chosen Availability Zone in the region that is specified by the endpoint.
-- Example: @us-east-2d@ 
-- Constraint: The specified Availability Zone must be in the same region as the current endpoint.
  , clusterParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the parameter group to be associated with this cluster.
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
  , clusterSecurityGroups :: Core.Maybe [Core.Text]
    -- ^ A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
  , clusterSubnetGroupName :: Core.Maybe Core.Text
    -- ^ The name of a cluster subnet group to be associated with this cluster.
--
-- If this parameter is not provided the resulting cluster will be deployed outside virtual private cloud (VPC).
  , clusterType :: Core.Maybe Core.Text
    -- ^ The type of the cluster. When cluster type is specified as
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
  , clusterVersion :: Core.Maybe Core.Text
    -- ^ The version of the Amazon Redshift engine software that you want to deploy on the cluster.
--
-- The version selected runs on all the nodes in the cluster.
-- Constraints: Only version 1.0 is currently available.
-- Example: @1.0@ 
  , dBName :: Core.Maybe Core.Text
    -- ^ The name of the first database to be created when the cluster is created.
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
  , elasticIp :: Core.Maybe Core.Text
    -- ^ The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ If @true@ , the data in the cluster is encrypted at rest. 
--
-- Default: false
  , enhancedVpcRouting :: Core.Maybe Core.Bool
    -- ^ An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
  , hsmClientCertificateIdentifier :: Core.Maybe Core.Text
    -- ^ Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
  , hsmConfigurationIdentifier :: Core.Maybe Core.Text
    -- ^ Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
  , iamRoles :: Core.Maybe [Core.Text]
    -- ^ A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
--
-- A cluster can have up to 10 IAM roles associated with it at any time.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
  , maintenanceTrackName :: Core.Maybe Core.Text
    -- ^ An optional parameter for the name of the maintenance track for the cluster. If you don't provide a maintenance track name, the cluster is assigned to the @current@ track.
  , manualSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The number of compute nodes in the cluster. This parameter is required when the __ClusterType__ parameter is specified as @multi-node@ . 
--
-- For information about determining how many nodes you need, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ . 
-- If you don't specify this parameter, you get a single-node cluster. When requesting a multi-node cluster, you must specify the number of nodes that you want in the cluster.
-- Default: @1@ 
-- Constraints: Value must be at least 1 and no more than 100.
  , port :: Core.Maybe Core.Int
    -- ^ The port number on which the cluster accepts incoming connections.
--
-- The cluster is accessible only via the JDBC and ODBC connection strings. Part of the connection string requires the port on which the cluster will listen for incoming connections.
-- Default: @5439@ 
-- Valid Values: @1150-65535@ 
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ The weekly time range (in UTC) during which automated cluster maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@ 
-- Default: A 30-minute window selected at random from an 8-hour block of time per region, occurring on a random day of the week. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide.
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Minimum 30-minute window.
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ If @true@ , the cluster can be accessed from a public network. 
  , snapshotScheduleIdentifier :: Core.Maybe Core.Text
    -- ^ A unique identifier for the snapshot schedule.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tag instances.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCluster' value with any optional fields omitted.
mkCreateCluster
    :: Core.Text -- ^ 'clusterIdentifier'
    -> Core.Text -- ^ 'nodeType'
    -> Core.Text -- ^ 'masterUsername'
    -> Core.Text -- ^ 'masterUserPassword'
    -> CreateCluster
mkCreateCluster clusterIdentifier nodeType masterUsername
  masterUserPassword
  = CreateCluster'{clusterIdentifier, nodeType, masterUsername,
                   masterUserPassword, additionalInfo = Core.Nothing,
                   allowVersionUpgrade = Core.Nothing,
                   automatedSnapshotRetentionPeriod = Core.Nothing,
                   availabilityZone = Core.Nothing,
                   clusterParameterGroupName = Core.Nothing,
                   clusterSecurityGroups = Core.Nothing,
                   clusterSubnetGroupName = Core.Nothing, clusterType = Core.Nothing,
                   clusterVersion = Core.Nothing, dBName = Core.Nothing,
                   elasticIp = Core.Nothing, encrypted = Core.Nothing,
                   enhancedVpcRouting = Core.Nothing,
                   hsmClientCertificateIdentifier = Core.Nothing,
                   hsmConfigurationIdentifier = Core.Nothing, iamRoles = Core.Nothing,
                   kmsKeyId = Core.Nothing, maintenanceTrackName = Core.Nothing,
                   manualSnapshotRetentionPeriod = Core.Nothing,
                   numberOfNodes = Core.Nothing, port = Core.Nothing,
                   preferredMaintenanceWindow = Core.Nothing,
                   publiclyAccessible = Core.Nothing,
                   snapshotScheduleIdentifier = Core.Nothing, tags = Core.Nothing,
                   vpcSecurityGroupIds = Core.Nothing}

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
ccClusterIdentifier :: Lens.Lens' CreateCluster Core.Text
ccClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE ccClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | The node type to be provisioned for the cluster. For information about node types, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters> in the /Amazon Redshift Cluster Management Guide/ . 
--
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@ | @dc2.large@ | @dc2.8xlarge@ | @ra3.4xlarge@ | @ra3.16xlarge@ 
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNodeType :: Lens.Lens' CreateCluster Core.Text
ccNodeType = Lens.field @"nodeType"
{-# INLINEABLE ccNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

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
ccMasterUsername :: Lens.Lens' CreateCluster Core.Text
ccMasterUsername = Lens.field @"masterUsername"
{-# INLINEABLE ccMasterUsername #-}
{-# DEPRECATED masterUsername "Use generic-lens or generic-optics with 'masterUsername' instead"  #-}

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
ccMasterUserPassword :: Lens.Lens' CreateCluster Core.Text
ccMasterUserPassword = Lens.field @"masterUserPassword"
{-# INLINEABLE ccMasterUserPassword #-}
{-# DEPRECATED masterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead"  #-}

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAdditionalInfo :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccAdditionalInfo = Lens.field @"additionalInfo"
{-# INLINEABLE ccAdditionalInfo #-}
{-# DEPRECATED additionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead"  #-}

-- | If @true@ , major version upgrades can be applied during the maintenance window to the Amazon Redshift engine that is running on the cluster.
--
-- When a new major version of the Amazon Redshift engine is released, you can request that the service automatically apply upgrades during the maintenance window to the Amazon Redshift engine that is running on your cluster.
-- Default: @true@ 
--
-- /Note:/ Consider using 'allowVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAllowVersionUpgrade :: Lens.Lens' CreateCluster (Core.Maybe Core.Bool)
ccAllowVersionUpgrade = Lens.field @"allowVersionUpgrade"
{-# INLINEABLE ccAllowVersionUpgrade #-}
{-# DEPRECATED allowVersionUpgrade "Use generic-lens or generic-optics with 'allowVersionUpgrade' instead"  #-}

-- | The number of days that automated snapshots are retained. If the value is 0, automated snapshots are disabled. Even if automated snapshots are disabled, you can still create manual snapshots when you want with 'CreateClusterSnapshot' . 
--
-- Default: @1@ 
-- Constraints: Must be a value from 0 to 35.
--
-- /Note:/ Consider using 'automatedSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAutomatedSnapshotRetentionPeriod :: Lens.Lens' CreateCluster (Core.Maybe Core.Int)
ccAutomatedSnapshotRetentionPeriod = Lens.field @"automatedSnapshotRetentionPeriod"
{-# INLINEABLE ccAutomatedSnapshotRetentionPeriod #-}
{-# DEPRECATED automatedSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'automatedSnapshotRetentionPeriod' instead"  #-}

-- | The EC2 Availability Zone (AZ) in which you want Amazon Redshift to provision the cluster. For example, if you have several EC2 instances running in a specific Availability Zone, then you might want the cluster to be provisioned in the same zone in order to decrease network latency.
--
-- Default: A random, system-chosen Availability Zone in the region that is specified by the endpoint.
-- Example: @us-east-2d@ 
-- Constraint: The specified Availability Zone must be in the same region as the current endpoint.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAvailabilityZone :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE ccAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

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
ccClusterParameterGroupName :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccClusterParameterGroupName = Lens.field @"clusterParameterGroupName"
{-# INLINEABLE ccClusterParameterGroupName #-}
{-# DEPRECATED clusterParameterGroupName "Use generic-lens or generic-optics with 'clusterParameterGroupName' instead"  #-}

-- | A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
--
-- /Note:/ Consider using 'clusterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterSecurityGroups :: Lens.Lens' CreateCluster (Core.Maybe [Core.Text])
ccClusterSecurityGroups = Lens.field @"clusterSecurityGroups"
{-# INLINEABLE ccClusterSecurityGroups #-}
{-# DEPRECATED clusterSecurityGroups "Use generic-lens or generic-optics with 'clusterSecurityGroups' instead"  #-}

-- | The name of a cluster subnet group to be associated with this cluster.
--
-- If this parameter is not provided the resulting cluster will be deployed outside virtual private cloud (VPC).
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterSubnetGroupName :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# INLINEABLE ccClusterSubnetGroupName #-}
{-# DEPRECATED clusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead"  #-}

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
ccClusterType :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccClusterType = Lens.field @"clusterType"
{-# INLINEABLE ccClusterType #-}
{-# DEPRECATED clusterType "Use generic-lens or generic-optics with 'clusterType' instead"  #-}

-- | The version of the Amazon Redshift engine software that you want to deploy on the cluster.
--
-- The version selected runs on all the nodes in the cluster.
-- Constraints: Only version 1.0 is currently available.
-- Example: @1.0@ 
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterVersion :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccClusterVersion = Lens.field @"clusterVersion"
{-# INLINEABLE ccClusterVersion #-}
{-# DEPRECATED clusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead"  #-}

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
ccDBName :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccDBName = Lens.field @"dBName"
{-# INLINEABLE ccDBName #-}
{-# DEPRECATED dBName "Use generic-lens or generic-optics with 'dBName' instead"  #-}

-- | The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and publicly-accessible through an Internet gateway. For more information about provisioning clusters in EC2-VPC, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster> in the Amazon Redshift Cluster Management Guide.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccElasticIp :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccElasticIp = Lens.field @"elasticIp"
{-# INLINEABLE ccElasticIp #-}
{-# DEPRECATED elasticIp "Use generic-lens or generic-optics with 'elasticIp' instead"  #-}

-- | If @true@ , the data in the cluster is encrypted at rest. 
--
-- Default: false
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEncrypted :: Lens.Lens' CreateCluster (Core.Maybe Core.Bool)
ccEncrypted = Lens.field @"encrypted"
{-# INLINEABLE ccEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
--
-- /Note:/ Consider using 'enhancedVpcRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEnhancedVpcRouting :: Lens.Lens' CreateCluster (Core.Maybe Core.Bool)
ccEnhancedVpcRouting = Lens.field @"enhancedVpcRouting"
{-# INLINEABLE ccEnhancedVpcRouting #-}
{-# DEPRECATED enhancedVpcRouting "Use generic-lens or generic-optics with 'enhancedVpcRouting' instead"  #-}

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHsmClientCertificateIdentifier :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccHsmClientCertificateIdentifier = Lens.field @"hsmClientCertificateIdentifier"
{-# INLINEABLE ccHsmClientCertificateIdentifier #-}
{-# DEPRECATED hsmClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead"  #-}

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHsmConfigurationIdentifier :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccHsmConfigurationIdentifier = Lens.field @"hsmConfigurationIdentifier"
{-# INLINEABLE ccHsmConfigurationIdentifier #-}
{-# DEPRECATED hsmConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead"  #-}

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services. You must supply the IAM roles in their Amazon Resource Name (ARN) format. You can supply up to 10 IAM roles in a single request.
--
-- A cluster can have up to 10 IAM roles associated with it at any time.
--
-- /Note:/ Consider using 'iamRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccIamRoles :: Lens.Lens' CreateCluster (Core.Maybe [Core.Text])
ccIamRoles = Lens.field @"iamRoles"
{-# INLINEABLE ccIamRoles #-}
{-# DEPRECATED iamRoles "Use generic-lens or generic-optics with 'iamRoles' instead"  #-}

-- | The AWS Key Management Service (KMS) key ID of the encryption key that you want to use to encrypt data in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccKmsKeyId :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE ccKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | An optional parameter for the name of the maintenance track for the cluster. If you don't provide a maintenance track name, the cluster is assigned to the @current@ track.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccMaintenanceTrackName :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# INLINEABLE ccMaintenanceTrackName #-}
{-# DEPRECATED maintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead"  #-}

-- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccManualSnapshotRetentionPeriod :: Lens.Lens' CreateCluster (Core.Maybe Core.Int)
ccManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# INLINEABLE ccManualSnapshotRetentionPeriod #-}
{-# DEPRECATED manualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead"  #-}

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
{-# INLINEABLE ccNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

-- | The port number on which the cluster accepts incoming connections.
--
-- The cluster is accessible only via the JDBC and ODBC connection strings. Part of the connection string requires the port on which the cluster will listen for incoming connections.
-- Default: @5439@ 
-- Valid Values: @1150-65535@ 
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPort :: Lens.Lens' CreateCluster (Core.Maybe Core.Int)
ccPort = Lens.field @"port"
{-# INLINEABLE ccPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The weekly time range (in UTC) during which automated cluster maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@ 
-- Default: A 30-minute window selected at random from an 8-hour block of time per region, occurring on a random day of the week. For more information about the time blocks for each region, see <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows> in Amazon Redshift Cluster Management Guide.
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPreferredMaintenanceWindow :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE ccPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | If @true@ , the cluster can be accessed from a public network. 
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPubliclyAccessible :: Lens.Lens' CreateCluster (Core.Maybe Core.Bool)
ccPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE ccPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

-- | A unique identifier for the snapshot schedule.
--
-- /Note:/ Consider using 'snapshotScheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSnapshotScheduleIdentifier :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccSnapshotScheduleIdentifier = Lens.field @"snapshotScheduleIdentifier"
{-# INLINEABLE ccSnapshotScheduleIdentifier #-}
{-# DEPRECATED snapshotScheduleIdentifier "Use generic-lens or generic-optics with 'snapshotScheduleIdentifier' instead"  #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateCluster (Core.Maybe [Types.Tag])
ccTags = Lens.field @"tags"
{-# INLINEABLE ccTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A list of Virtual Private Cloud (VPC) security groups to be associated with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVpcSecurityGroupIds :: Lens.Lens' CreateCluster (Core.Maybe [Core.Text])
ccVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE ccVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery CreateCluster where
        toQuery CreateCluster{..}
          = Core.toQueryPair "Action" ("CreateCluster" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<> Core.toQueryPair "NodeType" nodeType
              Core.<> Core.toQueryPair "MasterUsername" masterUsername
              Core.<> Core.toQueryPair "MasterUserPassword" masterUserPassword
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AdditionalInfo")
                additionalInfo
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AllowVersionUpgrade")
                allowVersionUpgrade
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AutomatedSnapshotRetentionPeriod")
                automatedSnapshotRetentionPeriod
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZone")
                availabilityZone
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
              Core.maybe Core.mempty (Core.toQueryPair "ClusterSubnetGroupName")
                clusterSubnetGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterType") clusterType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterVersion")
                clusterVersion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DBName") dBName
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
              Core.toQueryPair "IamRoles"
                (Core.maybe Core.mempty (Core.toQueryList "IamRoleArn") iamRoles)
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
              Core.maybe Core.mempty (Core.toQueryPair "NumberOfNodes")
                numberOfNodes
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PreferredMaintenanceWindow")
                preferredMaintenanceWindow
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PubliclyAccessible")
                publiclyAccessible
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SnapshotScheduleIdentifier")
                snapshotScheduleIdentifier
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)
              Core.<>
              Core.toQueryPair "VpcSecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "VpcSecurityGroupId")
                   vpcSecurityGroupIds)

instance Core.ToHeaders CreateCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateCluster where
        type Rs CreateCluster = CreateClusterResponse
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
          = Response.receiveXMLWrapper "CreateClusterResult"
              (\ s h x ->
                 CreateClusterResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateClusterResponse' value with any optional fields omitted.
mkCreateClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateClusterResponse
mkCreateClusterResponse responseStatus
  = CreateClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCluster :: Lens.Lens' CreateClusterResponse (Core.Maybe Types.Cluster)
ccrrsCluster = Lens.field @"cluster"
{-# INLINEABLE ccrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateClusterResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
