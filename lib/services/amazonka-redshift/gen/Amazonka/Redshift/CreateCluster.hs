{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Redshift.CreateCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cluster with the specified parameters.
--
-- To create a cluster in Virtual Private Cloud (VPC), you must provide a
-- cluster subnet group name. The cluster subnet group identifies the
-- subnets of your VPC that Amazon Redshift uses when creating the cluster.
-- For more information about managing clusters, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
module Amazonka.Redshift.CreateCluster
  ( -- * Creating a Request
    CreateCluster (..),
    newCreateCluster,

    -- * Request Lenses
    createCluster_tags,
    createCluster_port,
    createCluster_vpcSecurityGroupIds,
    createCluster_elasticIp,
    createCluster_manualSnapshotRetentionPeriod,
    createCluster_additionalInfo,
    createCluster_allowVersionUpgrade,
    createCluster_clusterSubnetGroupName,
    createCluster_snapshotScheduleIdentifier,
    createCluster_aquaConfigurationStatus,
    createCluster_clusterVersion,
    createCluster_loadSampleData,
    createCluster_maintenanceTrackName,
    createCluster_iamRoles,
    createCluster_hsmClientCertificateIdentifier,
    createCluster_availabilityZone,
    createCluster_publiclyAccessible,
    createCluster_clusterParameterGroupName,
    createCluster_encrypted,
    createCluster_numberOfNodes,
    createCluster_kmsKeyId,
    createCluster_availabilityZoneRelocation,
    createCluster_defaultIamRoleArn,
    createCluster_enhancedVpcRouting,
    createCluster_preferredMaintenanceWindow,
    createCluster_clusterType,
    createCluster_clusterSecurityGroups,
    createCluster_automatedSnapshotRetentionPeriod,
    createCluster_hsmConfigurationIdentifier,
    createCluster_dbName,
    createCluster_clusterIdentifier,
    createCluster_nodeType,
    createCluster_masterUsername,
    createCluster_masterUserPassword,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | A list of tag instances.
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which the cluster accepts incoming connections.
    --
    -- The cluster is accessible only via the JDBC and ODBC connection strings.
    -- Part of the connection string requires the port on which the cluster
    -- will listen for incoming connections.
    --
    -- Default: @5439@
    --
    -- Valid Values: @1150-65535@
    port :: Prelude.Maybe Prelude.Int,
    -- | A list of Virtual Private Cloud (VPC) security groups to be associated
    -- with the cluster.
    --
    -- Default: The default VPC security group is associated with the cluster.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The Elastic IP (EIP) address for the cluster.
    --
    -- Constraints: The cluster must be provisioned in EC2-VPC and
    -- publicly-accessible through an Internet gateway. Don\'t specify the
    -- Elastic IP address for a publicly accessible cluster with availability
    -- zone relocation turned on. For more information about provisioning
    -- clusters in EC2-VPC, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster>
    -- in the Amazon Redshift Cluster Management Guide.
    elasticIp :: Prelude.Maybe Prelude.Text,
    -- | The default number of days to retain a manual snapshot. If the value is
    -- -1, the snapshot is retained indefinitely. This setting doesn\'t change
    -- the retention period of existing snapshots.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Reserved.
    additionalInfo :: Prelude.Maybe Prelude.Text,
    -- | If @true@, major version upgrades can be applied during the maintenance
    -- window to the Amazon Redshift engine that is running on the cluster.
    --
    -- When a new major version of the Amazon Redshift engine is released, you
    -- can request that the service automatically apply upgrades during the
    -- maintenance window to the Amazon Redshift engine that is running on your
    -- cluster.
    --
    -- Default: @true@
    allowVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The name of a cluster subnet group to be associated with this cluster.
    --
    -- If this parameter is not provided the resulting cluster will be deployed
    -- outside virtual private cloud (VPC).
    clusterSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the snapshot schedule.
    snapshotScheduleIdentifier :: Prelude.Maybe Prelude.Text,
    -- | This parameter is retired. It does not set the AQUA configuration
    -- status. Amazon Redshift automatically determines whether to use AQUA
    -- (Advanced Query Accelerator).
    aquaConfigurationStatus :: Prelude.Maybe AquaConfigurationStatus,
    -- | The version of the Amazon Redshift engine software that you want to
    -- deploy on the cluster.
    --
    -- The version selected runs on all the nodes in the cluster.
    --
    -- Constraints: Only version 1.0 is currently available.
    --
    -- Example: @1.0@
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | A flag that specifies whether to load sample data once the cluster is
    -- created.
    loadSampleData :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter for the name of the maintenance track for the
    -- cluster. If you don\'t provide a maintenance track name, the cluster is
    -- assigned to the @current@ track.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | A list of Identity and Access Management (IAM) roles that can be used by
    -- the cluster to access other Amazon Web Services services. You must
    -- supply the IAM roles in their Amazon Resource Name (ARN) format.
    --
    -- The maximum number of IAM roles that you can associate is subject to a
    -- quota. For more information, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Quotas and limits>
    -- in the /Amazon Redshift Cluster Management Guide/.
    iamRoles :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the name of the HSM client certificate the Amazon Redshift
    -- cluster uses to retrieve the data encryption keys stored in an HSM.
    hsmClientCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The EC2 Availability Zone (AZ) in which you want Amazon Redshift to
    -- provision the cluster. For example, if you have several EC2 instances
    -- running in a specific Availability Zone, then you might want the cluster
    -- to be provisioned in the same zone in order to decrease network latency.
    --
    -- Default: A random, system-chosen Availability Zone in the region that is
    -- specified by the endpoint.
    --
    -- Example: @us-east-2d@
    --
    -- Constraint: The specified Availability Zone must be in the same region
    -- as the current endpoint.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | If @true@, the cluster can be accessed from a public network.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The name of the parameter group to be associated with this cluster.
    --
    -- Default: The default Amazon Redshift cluster parameter group. For
    -- information about the default parameter group, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 alphanumeric characters or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    clusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | If @true@, the data in the cluster is encrypted at rest.
    --
    -- Default: false
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The number of compute nodes in the cluster. This parameter is required
    -- when the __ClusterType__ parameter is specified as @multi-node@.
    --
    -- For information about determining how many nodes you need, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters>
    -- in the /Amazon Redshift Cluster Management Guide/.
    --
    -- If you don\'t specify this parameter, you get a single-node cluster.
    -- When requesting a multi-node cluster, you must specify the number of
    -- nodes that you want in the cluster.
    --
    -- Default: @1@
    --
    -- Constraints: Value must be at least 1 and no more than 100.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The Key Management Service (KMS) key ID of the encryption key that you
    -- want to use to encrypt data in the cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The option to enable relocation for an Amazon Redshift cluster between
    -- Availability Zones after the cluster is created.
    availabilityZoneRelocation :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the IAM role that was set as default
    -- for the cluster when the cluster was created.
    defaultIamRoleArn :: Prelude.Maybe Prelude.Text,
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
    -- | The weekly time range (in UTC) during which automated cluster
    -- maintenance can occur.
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- Default: A 30-minute window selected at random from an 8-hour block of
    -- time per region, occurring on a random day of the week. For more
    -- information about the time blocks for each region, see
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows>
    -- in Amazon Redshift Cluster Management Guide.
    --
    -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The type of the cluster. When cluster type is specified as
    --
    -- -   @single-node@, the __NumberOfNodes__ parameter is not required.
    --
    -- -   @multi-node@, the __NumberOfNodes__ parameter is required.
    --
    -- Valid Values: @multi-node@ | @single-node@
    --
    -- Default: @multi-node@
    clusterType :: Prelude.Maybe Prelude.Text,
    -- | A list of security groups to be associated with this cluster.
    --
    -- Default: The default cluster security group for Amazon Redshift.
    clusterSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The number of days that automated snapshots are retained. If the value
    -- is 0, automated snapshots are disabled. Even if automated snapshots are
    -- disabled, you can still create manual snapshots when you want with
    -- CreateClusterSnapshot.
    --
    -- You can\'t disable automated snapshots for RA3 node types. Set the
    -- automated retention period from 1-35 days.
    --
    -- Default: @1@
    --
    -- Constraints: Must be a value from 0 to 35.
    automatedSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Specifies the name of the HSM configuration that contains the
    -- information the Amazon Redshift cluster can use to retrieve and store
    -- keys in an HSM.
    hsmConfigurationIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the first database to be created when the cluster is
    -- created.
    --
    -- To create additional databases after the cluster is created, connect to
    -- the cluster with a SQL client and use SQL commands to create a database.
    -- For more information, go to
    -- <https://docs.aws.amazon.com/redshift/latest/dg/t_creating_database.html Create a Database>
    -- in the Amazon Redshift Database Developer Guide.
    --
    -- Default: @dev@
    --
    -- Constraints:
    --
    -- -   Must contain 1 to 64 alphanumeric characters.
    --
    -- -   Must contain only lowercase letters.
    --
    -- -   Cannot be a word that is reserved by the service. A list of reserved
    --     words can be found in
    --     <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
    --     in the Amazon Redshift Database Developer Guide.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the cluster. You use this identifier to refer to
    -- the cluster for any subsequent cluster operations such as deleting or
    -- modifying. The identifier also appears in the Amazon Redshift console.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
    --
    -- -   Alphabetic characters must be lowercase.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- -   Must be unique for all clusters within an Amazon Web Services
    --     account.
    --
    -- Example: @myexamplecluster@
    clusterIdentifier :: Prelude.Text,
    -- | The node type to be provisioned for the cluster. For information about
    -- node types, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters>
    -- in the /Amazon Redshift Cluster Management Guide/.
    --
    -- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@
    -- | @dc2.large@ | @dc2.8xlarge@ | @ra3.xlplus@ | @ra3.4xlarge@ |
    -- @ra3.16xlarge@
    nodeType :: Prelude.Text,
    -- | The user name associated with the admin user account for the cluster
    -- that is being created.
    --
    -- Constraints:
    --
    -- -   Must be 1 - 128 alphanumeric characters. The user name can\'t be
    --     @PUBLIC@.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot be a reserved word. A list of reserved words can be found in
    --     <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
    --     in the Amazon Redshift Database Developer Guide.
    masterUsername :: Prelude.Text,
    -- | The password associated with the admin user account for the cluster that
    -- is being created.
    --
    -- Constraints:
    --
    -- -   Must be between 8 and 64 characters in length.
    --
    -- -   Must contain at least one uppercase letter.
    --
    -- -   Must contain at least one lowercase letter.
    --
    -- -   Must contain one number.
    --
    -- -   Can be any printable ASCII character (ASCII code 33-126) except @\'@
    --     (single quote), @\"@ (double quote), @\\@, @\/@, or @\@@.
    masterUserPassword :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCluster_tags' - A list of tag instances.
--
-- 'port', 'createCluster_port' - The port number on which the cluster accepts incoming connections.
--
-- The cluster is accessible only via the JDBC and ODBC connection strings.
-- Part of the connection string requires the port on which the cluster
-- will listen for incoming connections.
--
-- Default: @5439@
--
-- Valid Values: @1150-65535@
--
-- 'vpcSecurityGroupIds', 'createCluster_vpcSecurityGroupIds' - A list of Virtual Private Cloud (VPC) security groups to be associated
-- with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
--
-- 'elasticIp', 'createCluster_elasticIp' - The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and
-- publicly-accessible through an Internet gateway. Don\'t specify the
-- Elastic IP address for a publicly accessible cluster with availability
-- zone relocation turned on. For more information about provisioning
-- clusters in EC2-VPC, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster>
-- in the Amazon Redshift Cluster Management Guide.
--
-- 'manualSnapshotRetentionPeriod', 'createCluster_manualSnapshotRetentionPeriod' - The default number of days to retain a manual snapshot. If the value is
-- -1, the snapshot is retained indefinitely. This setting doesn\'t change
-- the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- 'additionalInfo', 'createCluster_additionalInfo' - Reserved.
--
-- 'allowVersionUpgrade', 'createCluster_allowVersionUpgrade' - If @true@, major version upgrades can be applied during the maintenance
-- window to the Amazon Redshift engine that is running on the cluster.
--
-- When a new major version of the Amazon Redshift engine is released, you
-- can request that the service automatically apply upgrades during the
-- maintenance window to the Amazon Redshift engine that is running on your
-- cluster.
--
-- Default: @true@
--
-- 'clusterSubnetGroupName', 'createCluster_clusterSubnetGroupName' - The name of a cluster subnet group to be associated with this cluster.
--
-- If this parameter is not provided the resulting cluster will be deployed
-- outside virtual private cloud (VPC).
--
-- 'snapshotScheduleIdentifier', 'createCluster_snapshotScheduleIdentifier' - A unique identifier for the snapshot schedule.
--
-- 'aquaConfigurationStatus', 'createCluster_aquaConfigurationStatus' - This parameter is retired. It does not set the AQUA configuration
-- status. Amazon Redshift automatically determines whether to use AQUA
-- (Advanced Query Accelerator).
--
-- 'clusterVersion', 'createCluster_clusterVersion' - The version of the Amazon Redshift engine software that you want to
-- deploy on the cluster.
--
-- The version selected runs on all the nodes in the cluster.
--
-- Constraints: Only version 1.0 is currently available.
--
-- Example: @1.0@
--
-- 'loadSampleData', 'createCluster_loadSampleData' - A flag that specifies whether to load sample data once the cluster is
-- created.
--
-- 'maintenanceTrackName', 'createCluster_maintenanceTrackName' - An optional parameter for the name of the maintenance track for the
-- cluster. If you don\'t provide a maintenance track name, the cluster is
-- assigned to the @current@ track.
--
-- 'iamRoles', 'createCluster_iamRoles' - A list of Identity and Access Management (IAM) roles that can be used by
-- the cluster to access other Amazon Web Services services. You must
-- supply the IAM roles in their Amazon Resource Name (ARN) format.
--
-- The maximum number of IAM roles that you can associate is subject to a
-- quota. For more information, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Quotas and limits>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- 'hsmClientCertificateIdentifier', 'createCluster_hsmClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- 'availabilityZone', 'createCluster_availabilityZone' - The EC2 Availability Zone (AZ) in which you want Amazon Redshift to
-- provision the cluster. For example, if you have several EC2 instances
-- running in a specific Availability Zone, then you might want the cluster
-- to be provisioned in the same zone in order to decrease network latency.
--
-- Default: A random, system-chosen Availability Zone in the region that is
-- specified by the endpoint.
--
-- Example: @us-east-2d@
--
-- Constraint: The specified Availability Zone must be in the same region
-- as the current endpoint.
--
-- 'publiclyAccessible', 'createCluster_publiclyAccessible' - If @true@, the cluster can be accessed from a public network.
--
-- 'clusterParameterGroupName', 'createCluster_clusterParameterGroupName' - The name of the parameter group to be associated with this cluster.
--
-- Default: The default Amazon Redshift cluster parameter group. For
-- information about the default parameter group, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- 'encrypted', 'createCluster_encrypted' - If @true@, the data in the cluster is encrypted at rest.
--
-- Default: false
--
-- 'numberOfNodes', 'createCluster_numberOfNodes' - The number of compute nodes in the cluster. This parameter is required
-- when the __ClusterType__ parameter is specified as @multi-node@.
--
-- For information about determining how many nodes you need, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- If you don\'t specify this parameter, you get a single-node cluster.
-- When requesting a multi-node cluster, you must specify the number of
-- nodes that you want in the cluster.
--
-- Default: @1@
--
-- Constraints: Value must be at least 1 and no more than 100.
--
-- 'kmsKeyId', 'createCluster_kmsKeyId' - The Key Management Service (KMS) key ID of the encryption key that you
-- want to use to encrypt data in the cluster.
--
-- 'availabilityZoneRelocation', 'createCluster_availabilityZoneRelocation' - The option to enable relocation for an Amazon Redshift cluster between
-- Availability Zones after the cluster is created.
--
-- 'defaultIamRoleArn', 'createCluster_defaultIamRoleArn' - The Amazon Resource Name (ARN) for the IAM role that was set as default
-- for the cluster when the cluster was created.
--
-- 'enhancedVpcRouting', 'createCluster_enhancedVpcRouting' - An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
--
-- 'preferredMaintenanceWindow', 'createCluster_preferredMaintenanceWindow' - The weekly time range (in UTC) during which automated cluster
-- maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region, occurring on a random day of the week. For more
-- information about the time blocks for each region, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows>
-- in Amazon Redshift Cluster Management Guide.
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Minimum 30-minute window.
--
-- 'clusterType', 'createCluster_clusterType' - The type of the cluster. When cluster type is specified as
--
-- -   @single-node@, the __NumberOfNodes__ parameter is not required.
--
-- -   @multi-node@, the __NumberOfNodes__ parameter is required.
--
-- Valid Values: @multi-node@ | @single-node@
--
-- Default: @multi-node@
--
-- 'clusterSecurityGroups', 'createCluster_clusterSecurityGroups' - A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
--
-- 'automatedSnapshotRetentionPeriod', 'createCluster_automatedSnapshotRetentionPeriod' - The number of days that automated snapshots are retained. If the value
-- is 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot.
--
-- You can\'t disable automated snapshots for RA3 node types. Set the
-- automated retention period from 1-35 days.
--
-- Default: @1@
--
-- Constraints: Must be a value from 0 to 35.
--
-- 'hsmConfigurationIdentifier', 'createCluster_hsmConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
--
-- 'dbName', 'createCluster_dbName' - The name of the first database to be created when the cluster is
-- created.
--
-- To create additional databases after the cluster is created, connect to
-- the cluster with a SQL client and use SQL commands to create a database.
-- For more information, go to
-- <https://docs.aws.amazon.com/redshift/latest/dg/t_creating_database.html Create a Database>
-- in the Amazon Redshift Database Developer Guide.
--
-- Default: @dev@
--
-- Constraints:
--
-- -   Must contain 1 to 64 alphanumeric characters.
--
-- -   Must contain only lowercase letters.
--
-- -   Cannot be a word that is reserved by the service. A list of reserved
--     words can be found in
--     <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
--
-- 'clusterIdentifier', 'createCluster_clusterIdentifier' - A unique identifier for the cluster. You use this identifier to refer to
-- the cluster for any subsequent cluster operations such as deleting or
-- modifying. The identifier also appears in the Amazon Redshift console.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
--
-- -   Alphabetic characters must be lowercase.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- -   Must be unique for all clusters within an Amazon Web Services
--     account.
--
-- Example: @myexamplecluster@
--
-- 'nodeType', 'createCluster_nodeType' - The node type to be provisioned for the cluster. For information about
-- node types, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@
-- | @dc2.large@ | @dc2.8xlarge@ | @ra3.xlplus@ | @ra3.4xlarge@ |
-- @ra3.16xlarge@
--
-- 'masterUsername', 'createCluster_masterUsername' - The user name associated with the admin user account for the cluster
-- that is being created.
--
-- Constraints:
--
-- -   Must be 1 - 128 alphanumeric characters. The user name can\'t be
--     @PUBLIC@.
--
-- -   First character must be a letter.
--
-- -   Cannot be a reserved word. A list of reserved words can be found in
--     <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
--
-- 'masterUserPassword', 'createCluster_masterUserPassword' - The password associated with the admin user account for the cluster that
-- is being created.
--
-- Constraints:
--
-- -   Must be between 8 and 64 characters in length.
--
-- -   Must contain at least one uppercase letter.
--
-- -   Must contain at least one lowercase letter.
--
-- -   Must contain one number.
--
-- -   Can be any printable ASCII character (ASCII code 33-126) except @\'@
--     (single quote), @\"@ (double quote), @\\@, @\/@, or @\@@.
newCreateCluster ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  -- | 'nodeType'
  Prelude.Text ->
  -- | 'masterUsername'
  Prelude.Text ->
  -- | 'masterUserPassword'
  Prelude.Text ->
  CreateCluster
newCreateCluster
  pClusterIdentifier_
  pNodeType_
  pMasterUsername_
  pMasterUserPassword_ =
    CreateCluster'
      { tags = Prelude.Nothing,
        port = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        elasticIp = Prelude.Nothing,
        manualSnapshotRetentionPeriod = Prelude.Nothing,
        additionalInfo = Prelude.Nothing,
        allowVersionUpgrade = Prelude.Nothing,
        clusterSubnetGroupName = Prelude.Nothing,
        snapshotScheduleIdentifier = Prelude.Nothing,
        aquaConfigurationStatus = Prelude.Nothing,
        clusterVersion = Prelude.Nothing,
        loadSampleData = Prelude.Nothing,
        maintenanceTrackName = Prelude.Nothing,
        iamRoles = Prelude.Nothing,
        hsmClientCertificateIdentifier = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        clusterParameterGroupName = Prelude.Nothing,
        encrypted = Prelude.Nothing,
        numberOfNodes = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        availabilityZoneRelocation = Prelude.Nothing,
        defaultIamRoleArn = Prelude.Nothing,
        enhancedVpcRouting = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        clusterType = Prelude.Nothing,
        clusterSecurityGroups = Prelude.Nothing,
        automatedSnapshotRetentionPeriod = Prelude.Nothing,
        hsmConfigurationIdentifier = Prelude.Nothing,
        dbName = Prelude.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        nodeType = pNodeType_,
        masterUsername = pMasterUsername_,
        masterUserPassword = pMasterUserPassword_
      }

-- | A list of tag instances.
createCluster_tags :: Lens.Lens' CreateCluster (Prelude.Maybe [Tag])
createCluster_tags = Lens.lens (\CreateCluster' {tags} -> tags) (\s@CreateCluster' {} a -> s {tags = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which the cluster accepts incoming connections.
--
-- The cluster is accessible only via the JDBC and ODBC connection strings.
-- Part of the connection string requires the port on which the cluster
-- will listen for incoming connections.
--
-- Default: @5439@
--
-- Valid Values: @1150-65535@
createCluster_port :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Int)
createCluster_port = Lens.lens (\CreateCluster' {port} -> port) (\s@CreateCluster' {} a -> s {port = a} :: CreateCluster)

-- | A list of Virtual Private Cloud (VPC) security groups to be associated
-- with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
createCluster_vpcSecurityGroupIds :: Lens.Lens' CreateCluster (Prelude.Maybe [Prelude.Text])
createCluster_vpcSecurityGroupIds = Lens.lens (\CreateCluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateCluster' {} a -> s {vpcSecurityGroupIds = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and
-- publicly-accessible through an Internet gateway. Don\'t specify the
-- Elastic IP address for a publicly accessible cluster with availability
-- zone relocation turned on. For more information about provisioning
-- clusters in EC2-VPC, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster>
-- in the Amazon Redshift Cluster Management Guide.
createCluster_elasticIp :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_elasticIp = Lens.lens (\CreateCluster' {elasticIp} -> elasticIp) (\s@CreateCluster' {} a -> s {elasticIp = a} :: CreateCluster)

-- | The default number of days to retain a manual snapshot. If the value is
-- -1, the snapshot is retained indefinitely. This setting doesn\'t change
-- the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
createCluster_manualSnapshotRetentionPeriod :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Int)
createCluster_manualSnapshotRetentionPeriod = Lens.lens (\CreateCluster' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@CreateCluster' {} a -> s {manualSnapshotRetentionPeriod = a} :: CreateCluster)

-- | Reserved.
createCluster_additionalInfo :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_additionalInfo = Lens.lens (\CreateCluster' {additionalInfo} -> additionalInfo) (\s@CreateCluster' {} a -> s {additionalInfo = a} :: CreateCluster)

-- | If @true@, major version upgrades can be applied during the maintenance
-- window to the Amazon Redshift engine that is running on the cluster.
--
-- When a new major version of the Amazon Redshift engine is released, you
-- can request that the service automatically apply upgrades during the
-- maintenance window to the Amazon Redshift engine that is running on your
-- cluster.
--
-- Default: @true@
createCluster_allowVersionUpgrade :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Bool)
createCluster_allowVersionUpgrade = Lens.lens (\CreateCluster' {allowVersionUpgrade} -> allowVersionUpgrade) (\s@CreateCluster' {} a -> s {allowVersionUpgrade = a} :: CreateCluster)

-- | The name of a cluster subnet group to be associated with this cluster.
--
-- If this parameter is not provided the resulting cluster will be deployed
-- outside virtual private cloud (VPC).
createCluster_clusterSubnetGroupName :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_clusterSubnetGroupName = Lens.lens (\CreateCluster' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@CreateCluster' {} a -> s {clusterSubnetGroupName = a} :: CreateCluster)

-- | A unique identifier for the snapshot schedule.
createCluster_snapshotScheduleIdentifier :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_snapshotScheduleIdentifier = Lens.lens (\CreateCluster' {snapshotScheduleIdentifier} -> snapshotScheduleIdentifier) (\s@CreateCluster' {} a -> s {snapshotScheduleIdentifier = a} :: CreateCluster)

-- | This parameter is retired. It does not set the AQUA configuration
-- status. Amazon Redshift automatically determines whether to use AQUA
-- (Advanced Query Accelerator).
createCluster_aquaConfigurationStatus :: Lens.Lens' CreateCluster (Prelude.Maybe AquaConfigurationStatus)
createCluster_aquaConfigurationStatus = Lens.lens (\CreateCluster' {aquaConfigurationStatus} -> aquaConfigurationStatus) (\s@CreateCluster' {} a -> s {aquaConfigurationStatus = a} :: CreateCluster)

-- | The version of the Amazon Redshift engine software that you want to
-- deploy on the cluster.
--
-- The version selected runs on all the nodes in the cluster.
--
-- Constraints: Only version 1.0 is currently available.
--
-- Example: @1.0@
createCluster_clusterVersion :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_clusterVersion = Lens.lens (\CreateCluster' {clusterVersion} -> clusterVersion) (\s@CreateCluster' {} a -> s {clusterVersion = a} :: CreateCluster)

-- | A flag that specifies whether to load sample data once the cluster is
-- created.
createCluster_loadSampleData :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_loadSampleData = Lens.lens (\CreateCluster' {loadSampleData} -> loadSampleData) (\s@CreateCluster' {} a -> s {loadSampleData = a} :: CreateCluster)

-- | An optional parameter for the name of the maintenance track for the
-- cluster. If you don\'t provide a maintenance track name, the cluster is
-- assigned to the @current@ track.
createCluster_maintenanceTrackName :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_maintenanceTrackName = Lens.lens (\CreateCluster' {maintenanceTrackName} -> maintenanceTrackName) (\s@CreateCluster' {} a -> s {maintenanceTrackName = a} :: CreateCluster)

-- | A list of Identity and Access Management (IAM) roles that can be used by
-- the cluster to access other Amazon Web Services services. You must
-- supply the IAM roles in their Amazon Resource Name (ARN) format.
--
-- The maximum number of IAM roles that you can associate is subject to a
-- quota. For more information, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Quotas and limits>
-- in the /Amazon Redshift Cluster Management Guide/.
createCluster_iamRoles :: Lens.Lens' CreateCluster (Prelude.Maybe [Prelude.Text])
createCluster_iamRoles = Lens.lens (\CreateCluster' {iamRoles} -> iamRoles) (\s@CreateCluster' {} a -> s {iamRoles = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
createCluster_hsmClientCertificateIdentifier :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_hsmClientCertificateIdentifier = Lens.lens (\CreateCluster' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@CreateCluster' {} a -> s {hsmClientCertificateIdentifier = a} :: CreateCluster)

-- | The EC2 Availability Zone (AZ) in which you want Amazon Redshift to
-- provision the cluster. For example, if you have several EC2 instances
-- running in a specific Availability Zone, then you might want the cluster
-- to be provisioned in the same zone in order to decrease network latency.
--
-- Default: A random, system-chosen Availability Zone in the region that is
-- specified by the endpoint.
--
-- Example: @us-east-2d@
--
-- Constraint: The specified Availability Zone must be in the same region
-- as the current endpoint.
createCluster_availabilityZone :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_availabilityZone = Lens.lens (\CreateCluster' {availabilityZone} -> availabilityZone) (\s@CreateCluster' {} a -> s {availabilityZone = a} :: CreateCluster)

-- | If @true@, the cluster can be accessed from a public network.
createCluster_publiclyAccessible :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Bool)
createCluster_publiclyAccessible = Lens.lens (\CreateCluster' {publiclyAccessible} -> publiclyAccessible) (\s@CreateCluster' {} a -> s {publiclyAccessible = a} :: CreateCluster)

-- | The name of the parameter group to be associated with this cluster.
--
-- Default: The default Amazon Redshift cluster parameter group. For
-- information about the default parameter group, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
createCluster_clusterParameterGroupName :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_clusterParameterGroupName = Lens.lens (\CreateCluster' {clusterParameterGroupName} -> clusterParameterGroupName) (\s@CreateCluster' {} a -> s {clusterParameterGroupName = a} :: CreateCluster)

-- | If @true@, the data in the cluster is encrypted at rest.
--
-- Default: false
createCluster_encrypted :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Bool)
createCluster_encrypted = Lens.lens (\CreateCluster' {encrypted} -> encrypted) (\s@CreateCluster' {} a -> s {encrypted = a} :: CreateCluster)

-- | The number of compute nodes in the cluster. This parameter is required
-- when the __ClusterType__ parameter is specified as @multi-node@.
--
-- For information about determining how many nodes you need, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- If you don\'t specify this parameter, you get a single-node cluster.
-- When requesting a multi-node cluster, you must specify the number of
-- nodes that you want in the cluster.
--
-- Default: @1@
--
-- Constraints: Value must be at least 1 and no more than 100.
createCluster_numberOfNodes :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Int)
createCluster_numberOfNodes = Lens.lens (\CreateCluster' {numberOfNodes} -> numberOfNodes) (\s@CreateCluster' {} a -> s {numberOfNodes = a} :: CreateCluster)

-- | The Key Management Service (KMS) key ID of the encryption key that you
-- want to use to encrypt data in the cluster.
createCluster_kmsKeyId :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_kmsKeyId = Lens.lens (\CreateCluster' {kmsKeyId} -> kmsKeyId) (\s@CreateCluster' {} a -> s {kmsKeyId = a} :: CreateCluster)

-- | The option to enable relocation for an Amazon Redshift cluster between
-- Availability Zones after the cluster is created.
createCluster_availabilityZoneRelocation :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Bool)
createCluster_availabilityZoneRelocation = Lens.lens (\CreateCluster' {availabilityZoneRelocation} -> availabilityZoneRelocation) (\s@CreateCluster' {} a -> s {availabilityZoneRelocation = a} :: CreateCluster)

-- | The Amazon Resource Name (ARN) for the IAM role that was set as default
-- for the cluster when the cluster was created.
createCluster_defaultIamRoleArn :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_defaultIamRoleArn = Lens.lens (\CreateCluster' {defaultIamRoleArn} -> defaultIamRoleArn) (\s@CreateCluster' {} a -> s {defaultIamRoleArn = a} :: CreateCluster)

-- | An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
createCluster_enhancedVpcRouting :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Bool)
createCluster_enhancedVpcRouting = Lens.lens (\CreateCluster' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@CreateCluster' {} a -> s {enhancedVpcRouting = a} :: CreateCluster)

-- | The weekly time range (in UTC) during which automated cluster
-- maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region, occurring on a random day of the week. For more
-- information about the time blocks for each region, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows>
-- in Amazon Redshift Cluster Management Guide.
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Minimum 30-minute window.
createCluster_preferredMaintenanceWindow :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_preferredMaintenanceWindow = Lens.lens (\CreateCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateCluster' {} a -> s {preferredMaintenanceWindow = a} :: CreateCluster)

-- | The type of the cluster. When cluster type is specified as
--
-- -   @single-node@, the __NumberOfNodes__ parameter is not required.
--
-- -   @multi-node@, the __NumberOfNodes__ parameter is required.
--
-- Valid Values: @multi-node@ | @single-node@
--
-- Default: @multi-node@
createCluster_clusterType :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_clusterType = Lens.lens (\CreateCluster' {clusterType} -> clusterType) (\s@CreateCluster' {} a -> s {clusterType = a} :: CreateCluster)

-- | A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
createCluster_clusterSecurityGroups :: Lens.Lens' CreateCluster (Prelude.Maybe [Prelude.Text])
createCluster_clusterSecurityGroups = Lens.lens (\CreateCluster' {clusterSecurityGroups} -> clusterSecurityGroups) (\s@CreateCluster' {} a -> s {clusterSecurityGroups = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The number of days that automated snapshots are retained. If the value
-- is 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot.
--
-- You can\'t disable automated snapshots for RA3 node types. Set the
-- automated retention period from 1-35 days.
--
-- Default: @1@
--
-- Constraints: Must be a value from 0 to 35.
createCluster_automatedSnapshotRetentionPeriod :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Int)
createCluster_automatedSnapshotRetentionPeriod = Lens.lens (\CreateCluster' {automatedSnapshotRetentionPeriod} -> automatedSnapshotRetentionPeriod) (\s@CreateCluster' {} a -> s {automatedSnapshotRetentionPeriod = a} :: CreateCluster)

-- | Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
createCluster_hsmConfigurationIdentifier :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_hsmConfigurationIdentifier = Lens.lens (\CreateCluster' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@CreateCluster' {} a -> s {hsmConfigurationIdentifier = a} :: CreateCluster)

-- | The name of the first database to be created when the cluster is
-- created.
--
-- To create additional databases after the cluster is created, connect to
-- the cluster with a SQL client and use SQL commands to create a database.
-- For more information, go to
-- <https://docs.aws.amazon.com/redshift/latest/dg/t_creating_database.html Create a Database>
-- in the Amazon Redshift Database Developer Guide.
--
-- Default: @dev@
--
-- Constraints:
--
-- -   Must contain 1 to 64 alphanumeric characters.
--
-- -   Must contain only lowercase letters.
--
-- -   Cannot be a word that is reserved by the service. A list of reserved
--     words can be found in
--     <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
createCluster_dbName :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_dbName = Lens.lens (\CreateCluster' {dbName} -> dbName) (\s@CreateCluster' {} a -> s {dbName = a} :: CreateCluster)

-- | A unique identifier for the cluster. You use this identifier to refer to
-- the cluster for any subsequent cluster operations such as deleting or
-- modifying. The identifier also appears in the Amazon Redshift console.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
--
-- -   Alphabetic characters must be lowercase.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- -   Must be unique for all clusters within an Amazon Web Services
--     account.
--
-- Example: @myexamplecluster@
createCluster_clusterIdentifier :: Lens.Lens' CreateCluster Prelude.Text
createCluster_clusterIdentifier = Lens.lens (\CreateCluster' {clusterIdentifier} -> clusterIdentifier) (\s@CreateCluster' {} a -> s {clusterIdentifier = a} :: CreateCluster)

-- | The node type to be provisioned for the cluster. For information about
-- node types, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#how-many-nodes Working with Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@
-- | @dc2.large@ | @dc2.8xlarge@ | @ra3.xlplus@ | @ra3.4xlarge@ |
-- @ra3.16xlarge@
createCluster_nodeType :: Lens.Lens' CreateCluster Prelude.Text
createCluster_nodeType = Lens.lens (\CreateCluster' {nodeType} -> nodeType) (\s@CreateCluster' {} a -> s {nodeType = a} :: CreateCluster)

-- | The user name associated with the admin user account for the cluster
-- that is being created.
--
-- Constraints:
--
-- -   Must be 1 - 128 alphanumeric characters. The user name can\'t be
--     @PUBLIC@.
--
-- -   First character must be a letter.
--
-- -   Cannot be a reserved word. A list of reserved words can be found in
--     <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
createCluster_masterUsername :: Lens.Lens' CreateCluster Prelude.Text
createCluster_masterUsername = Lens.lens (\CreateCluster' {masterUsername} -> masterUsername) (\s@CreateCluster' {} a -> s {masterUsername = a} :: CreateCluster)

-- | The password associated with the admin user account for the cluster that
-- is being created.
--
-- Constraints:
--
-- -   Must be between 8 and 64 characters in length.
--
-- -   Must contain at least one uppercase letter.
--
-- -   Must contain at least one lowercase letter.
--
-- -   Must contain one number.
--
-- -   Can be any printable ASCII character (ASCII code 33-126) except @\'@
--     (single quote), @\"@ (double quote), @\\@, @\/@, or @\@@.
createCluster_masterUserPassword :: Lens.Lens' CreateCluster Prelude.Text
createCluster_masterUserPassword = Lens.lens (\CreateCluster' {masterUserPassword} -> masterUserPassword) (\s@CreateCluster' {} a -> s {masterUserPassword = a} :: CreateCluster)

instance Core.AWSRequest CreateCluster where
  type
    AWSResponse CreateCluster =
      CreateClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateClusterResult"
      ( \s h x ->
          CreateClusterResponse'
            Prelude.<$> (x Core..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCluster where
  hashWithSalt _salt CreateCluster' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` elasticIp
      `Prelude.hashWithSalt` manualSnapshotRetentionPeriod
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` allowVersionUpgrade
      `Prelude.hashWithSalt` clusterSubnetGroupName
      `Prelude.hashWithSalt` snapshotScheduleIdentifier
      `Prelude.hashWithSalt` aquaConfigurationStatus
      `Prelude.hashWithSalt` clusterVersion
      `Prelude.hashWithSalt` loadSampleData
      `Prelude.hashWithSalt` maintenanceTrackName
      `Prelude.hashWithSalt` iamRoles
      `Prelude.hashWithSalt` hsmClientCertificateIdentifier
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` clusterParameterGroupName
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` availabilityZoneRelocation
      `Prelude.hashWithSalt` defaultIamRoleArn
      `Prelude.hashWithSalt` enhancedVpcRouting
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` clusterType
      `Prelude.hashWithSalt` clusterSecurityGroups
      `Prelude.hashWithSalt` automatedSnapshotRetentionPeriod
      `Prelude.hashWithSalt` hsmConfigurationIdentifier
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` masterUserPassword

instance Prelude.NFData CreateCluster where
  rnf CreateCluster' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf elasticIp
      `Prelude.seq` Prelude.rnf manualSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf allowVersionUpgrade
      `Prelude.seq` Prelude.rnf clusterSubnetGroupName
      `Prelude.seq` Prelude.rnf snapshotScheduleIdentifier
      `Prelude.seq` Prelude.rnf aquaConfigurationStatus
      `Prelude.seq` Prelude.rnf clusterVersion
      `Prelude.seq` Prelude.rnf loadSampleData
      `Prelude.seq` Prelude.rnf maintenanceTrackName
      `Prelude.seq` Prelude.rnf iamRoles
      `Prelude.seq` Prelude.rnf
        hsmClientCertificateIdentifier
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf
        clusterParameterGroupName
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf numberOfNodes
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf
        availabilityZoneRelocation
      `Prelude.seq` Prelude.rnf
        defaultIamRoleArn
      `Prelude.seq` Prelude.rnf
        enhancedVpcRouting
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        clusterType
      `Prelude.seq` Prelude.rnf
        clusterSecurityGroups
      `Prelude.seq` Prelude.rnf
        automatedSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf
        hsmConfigurationIdentifier
      `Prelude.seq` Prelude.rnf
        dbName
      `Prelude.seq` Prelude.rnf
        clusterIdentifier
      `Prelude.seq` Prelude.rnf
        nodeType
      `Prelude.seq` Prelude.rnf
        masterUsername
      `Prelude.seq` Prelude.rnf
        masterUserPassword

instance Core.ToHeaders CreateCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCluster where
  toQuery CreateCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateCluster" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "Port" Core.=: port,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "ElasticIp" Core.=: elasticIp,
        "ManualSnapshotRetentionPeriod"
          Core.=: manualSnapshotRetentionPeriod,
        "AdditionalInfo" Core.=: additionalInfo,
        "AllowVersionUpgrade" Core.=: allowVersionUpgrade,
        "ClusterSubnetGroupName"
          Core.=: clusterSubnetGroupName,
        "SnapshotScheduleIdentifier"
          Core.=: snapshotScheduleIdentifier,
        "AquaConfigurationStatus"
          Core.=: aquaConfigurationStatus,
        "ClusterVersion" Core.=: clusterVersion,
        "LoadSampleData" Core.=: loadSampleData,
        "MaintenanceTrackName" Core.=: maintenanceTrackName,
        "IamRoles"
          Core.=: Core.toQuery
            (Core.toQueryList "IamRoleArn" Prelude.<$> iamRoles),
        "HsmClientCertificateIdentifier"
          Core.=: hsmClientCertificateIdentifier,
        "AvailabilityZone" Core.=: availabilityZone,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "ClusterParameterGroupName"
          Core.=: clusterParameterGroupName,
        "Encrypted" Core.=: encrypted,
        "NumberOfNodes" Core.=: numberOfNodes,
        "KmsKeyId" Core.=: kmsKeyId,
        "AvailabilityZoneRelocation"
          Core.=: availabilityZoneRelocation,
        "DefaultIamRoleArn" Core.=: defaultIamRoleArn,
        "EnhancedVpcRouting" Core.=: enhancedVpcRouting,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "ClusterType" Core.=: clusterType,
        "ClusterSecurityGroups"
          Core.=: Core.toQuery
            ( Core.toQueryList "ClusterSecurityGroupName"
                Prelude.<$> clusterSecurityGroups
            ),
        "AutomatedSnapshotRetentionPeriod"
          Core.=: automatedSnapshotRetentionPeriod,
        "HsmConfigurationIdentifier"
          Core.=: hsmConfigurationIdentifier,
        "DBName" Core.=: dbName,
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "NodeType" Core.=: nodeType,
        "MasterUsername" Core.=: masterUsername,
        "MasterUserPassword" Core.=: masterUserPassword
      ]

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'createClusterResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'createClusterResponse_httpStatus' - The response's http status code.
newCreateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClusterResponse
newCreateClusterResponse pHttpStatus_ =
  CreateClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createClusterResponse_cluster :: Lens.Lens' CreateClusterResponse (Prelude.Maybe Cluster)
createClusterResponse_cluster = Lens.lens (\CreateClusterResponse' {cluster} -> cluster) (\s@CreateClusterResponse' {} a -> s {cluster = a} :: CreateClusterResponse)

-- | The response's http status code.
createClusterResponse_httpStatus :: Lens.Lens' CreateClusterResponse Prelude.Int
createClusterResponse_httpStatus = Lens.lens (\CreateClusterResponse' {httpStatus} -> httpStatus) (\s@CreateClusterResponse' {} a -> s {httpStatus = a} :: CreateClusterResponse)

instance Prelude.NFData CreateClusterResponse where
  rnf CreateClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
