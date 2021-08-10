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
-- Module      : Network.AWS.Redshift.ModifyCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a cluster.
--
-- You can also change node type and the number of nodes to scale up or
-- down the cluster. When resizing a cluster, you must specify both the
-- number of nodes and the node type even if one of the parameters does not
-- change.
--
-- You can add another security or parameter group, or change the master
-- user password. Resetting a cluster password or modifying the security
-- groups associated with a cluster do not need a reboot. However,
-- modifying a parameter group requires a reboot for parameters to take
-- effect. For more information about managing clusters, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
module Network.AWS.Redshift.ModifyCluster
  ( -- * Creating a Request
    ModifyCluster (..),
    newModifyCluster,

    -- * Request Lenses
    modifyCluster_enhancedVpcRouting,
    modifyCluster_elasticIp,
    modifyCluster_hsmClientCertificateIdentifier,
    modifyCluster_encrypted,
    modifyCluster_allowVersionUpgrade,
    modifyCluster_automatedSnapshotRetentionPeriod,
    modifyCluster_clusterParameterGroupName,
    modifyCluster_newClusterIdentifier,
    modifyCluster_availabilityZoneRelocation,
    modifyCluster_masterUserPassword,
    modifyCluster_publiclyAccessible,
    modifyCluster_vpcSecurityGroupIds,
    modifyCluster_clusterType,
    modifyCluster_manualSnapshotRetentionPeriod,
    modifyCluster_kmsKeyId,
    modifyCluster_availabilityZone,
    modifyCluster_preferredMaintenanceWindow,
    modifyCluster_numberOfNodes,
    modifyCluster_port,
    modifyCluster_nodeType,
    modifyCluster_clusterVersion,
    modifyCluster_clusterSecurityGroups,
    modifyCluster_maintenanceTrackName,
    modifyCluster_hsmConfigurationIdentifier,
    modifyCluster_clusterIdentifier,

    -- * Destructuring the Response
    ModifyClusterResponse (..),
    newModifyClusterResponse,

    -- * Response Lenses
    modifyClusterResponse_cluster,
    modifyClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { -- | An option that specifies whether to create the cluster with enhanced VPC
    -- routing enabled. To create a cluster that uses enhanced VPC routing, the
    -- cluster must be in a VPC. For more information, see
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
    -- in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@, enhanced VPC routing is enabled.
    --
    -- Default: false
    enhancedVpcRouting :: Prelude.Maybe Prelude.Bool,
    -- | The Elastic IP (EIP) address for the cluster.
    --
    -- Constraints: The cluster must be provisioned in EC2-VPC and
    -- publicly-accessible through an Internet gateway. For more information
    -- about provisioning clusters in EC2-VPC, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster>
    -- in the Amazon Redshift Cluster Management Guide.
    elasticIp :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the HSM client certificate the Amazon Redshift
    -- cluster uses to retrieve the data encryption keys stored in an HSM.
    hsmClientCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the cluster is encrypted. If the value is encrypted
    -- (true) and you provide a value for the @KmsKeyId@ parameter, we encrypt
    -- the cluster with the provided @KmsKeyId@. If you don\'t provide a
    -- @KmsKeyId@, we encrypt with the default key.
    --
    -- If the value is not encrypted (false), then the cluster is decrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | If @true@, major version upgrades will be applied automatically to the
    -- cluster during the maintenance window.
    --
    -- Default: @false@
    allowVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The number of days that automated snapshots are retained. If the value
    -- is 0, automated snapshots are disabled. Even if automated snapshots are
    -- disabled, you can still create manual snapshots when you want with
    -- CreateClusterSnapshot.
    --
    -- If you decrease the automated snapshot retention period from its current
    -- value, existing automated snapshots that fall outside of the new
    -- retention period will be immediately deleted.
    --
    -- Default: Uses existing setting.
    --
    -- Constraints: Must be a value from 0 to 35.
    automatedSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The name of the cluster parameter group to apply to this cluster. This
    -- change is applied only after the cluster is rebooted. To reboot a
    -- cluster use RebootCluster.
    --
    -- Default: Uses existing setting.
    --
    -- Constraints: The cluster parameter group must be in the same parameter
    -- group family that matches the cluster version.
    clusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The new identifier for the cluster.
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
    -- -   Must be unique for all clusters within an AWS account.
    --
    -- Example: @examplecluster@
    newClusterIdentifier' :: Prelude.Maybe Prelude.Text,
    -- | The option to enable relocation for an Amazon Redshift cluster between
    -- Availability Zones after the cluster modification is complete.
    availabilityZoneRelocation :: Prelude.Maybe Prelude.Bool,
    -- | The new password for the cluster master user. This change is
    -- asynchronously applied as soon as possible. Between the time of the
    -- request and the completion of the request, the @MasterUserPassword@
    -- element exists in the @PendingModifiedValues@ element of the operation
    -- response.
    --
    -- Operations never return the password, so this operation provides a way
    -- to regain access to the master user account for a cluster if the
    -- password is lost.
    --
    -- Default: Uses existing setting.
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
    -- -   Can be any printable ASCII character (ASCII code 33 to 126) except
    --     \' (single quote), \" (double quote), \\, \/, \@, or space.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | If @true@, the cluster can be accessed from a public network. Only
    -- clusters in VPCs can be set to be publicly available.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | A list of virtual private cloud (VPC) security groups to be associated
    -- with the cluster. This change is asynchronously applied as soon as
    -- possible.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The new cluster type.
    --
    -- When you submit your cluster resize request, your existing cluster goes
    -- into a read-only mode. After Amazon Redshift provisions a new cluster
    -- based on your resize requirements, there will be outage for a period
    -- while the old cluster is deleted and your connection is switched to the
    -- new cluster. You can use DescribeResize to track the progress of the
    -- resize request.
    --
    -- Valid Values: @ multi-node | single-node @
    clusterType :: Prelude.Maybe Prelude.Text,
    -- | The default for number of days that a newly created manual snapshot is
    -- retained. If the value is -1, the manual snapshot is retained
    -- indefinitely. This value doesn\'t retroactively change the retention
    -- periods of existing manual snapshots.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    --
    -- The default value is -1.
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The AWS Key Management Service (KMS) key ID of the encryption key that
    -- you want to use to encrypt data in the cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The option to initiate relocation for an Amazon Redshift cluster to the
    -- target Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range (in UTC) during which system maintenance can
    -- occur, if necessary. If system maintenance is necessary during the
    -- window, it may result in an outage.
    --
    -- This maintenance window change is made immediately. If the new
    -- maintenance window indicates the current time, there must be at least
    -- 120 minutes between the current time and end of the window in order to
    -- ensure that pending changes are applied.
    --
    -- Default: Uses existing setting.
    --
    -- Format: ddd:hh24:mi-ddd:hh24:mi, for example @wed:07:30-wed:08:00@.
    --
    -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
    --
    -- Constraints: Must be at least 30 minutes.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The new number of nodes of the cluster. If you specify a new number of
    -- nodes, you must also specify the node type parameter.
    --
    -- For more information about resizing clusters, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift>
    -- in the /Amazon Redshift Cluster Management Guide/.
    --
    -- Valid Values: Integer greater than @0@.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The option to change the port of an Amazon Redshift cluster.
    port :: Prelude.Maybe Prelude.Int,
    -- | The new node type of the cluster. If you specify a new node type, you
    -- must also specify the number of nodes parameter.
    --
    -- For more information about resizing clusters, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift>
    -- in the /Amazon Redshift Cluster Management Guide/.
    --
    -- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@
    -- | @dc2.large@ | @dc2.8xlarge@ | @ra3.xlplus@ | @ra3.4xlarge@ |
    -- @ra3.16xlarge@
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The new version number of the Amazon Redshift engine to upgrade to.
    --
    -- For major version upgrades, if a non-default cluster parameter group is
    -- currently in use, a new cluster parameter group in the cluster parameter
    -- group family for the new version must be specified. The new cluster
    -- parameter group can be the default for that cluster parameter group
    -- family. For more information about parameters and parameter groups, go
    -- to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
    -- in the /Amazon Redshift Cluster Management Guide/.
    --
    -- Example: @1.0@
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of cluster security groups to be authorized on this cluster. This
    -- change is asynchronously applied as soon as possible.
    --
    -- Security groups currently associated with the cluster, and not in the
    -- list of groups to apply, will be revoked from the cluster.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 alphanumeric characters or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens
    clusterSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The name for the maintenance track that you want to assign for the
    -- cluster. This name change is asynchronous. The new track name stays in
    -- the @PendingModifiedValues@ for the cluster until the next maintenance
    -- window. When the maintenance track changes, the cluster is switched to
    -- the latest cluster release available for the maintenance track. At this
    -- point, the maintenance track name is applied.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the HSM configuration that contains the
    -- information the Amazon Redshift cluster can use to retrieve and store
    -- keys in an HSM.
    hsmConfigurationIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the cluster to be modified.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enhancedVpcRouting', 'modifyCluster_enhancedVpcRouting' - An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
--
-- 'elasticIp', 'modifyCluster_elasticIp' - The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and
-- publicly-accessible through an Internet gateway. For more information
-- about provisioning clusters in EC2-VPC, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster>
-- in the Amazon Redshift Cluster Management Guide.
--
-- 'hsmClientCertificateIdentifier', 'modifyCluster_hsmClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- 'encrypted', 'modifyCluster_encrypted' - Indicates whether the cluster is encrypted. If the value is encrypted
-- (true) and you provide a value for the @KmsKeyId@ parameter, we encrypt
-- the cluster with the provided @KmsKeyId@. If you don\'t provide a
-- @KmsKeyId@, we encrypt with the default key.
--
-- If the value is not encrypted (false), then the cluster is decrypted.
--
-- 'allowVersionUpgrade', 'modifyCluster_allowVersionUpgrade' - If @true@, major version upgrades will be applied automatically to the
-- cluster during the maintenance window.
--
-- Default: @false@
--
-- 'automatedSnapshotRetentionPeriod', 'modifyCluster_automatedSnapshotRetentionPeriod' - The number of days that automated snapshots are retained. If the value
-- is 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot.
--
-- If you decrease the automated snapshot retention period from its current
-- value, existing automated snapshots that fall outside of the new
-- retention period will be immediately deleted.
--
-- Default: Uses existing setting.
--
-- Constraints: Must be a value from 0 to 35.
--
-- 'clusterParameterGroupName', 'modifyCluster_clusterParameterGroupName' - The name of the cluster parameter group to apply to this cluster. This
-- change is applied only after the cluster is rebooted. To reboot a
-- cluster use RebootCluster.
--
-- Default: Uses existing setting.
--
-- Constraints: The cluster parameter group must be in the same parameter
-- group family that matches the cluster version.
--
-- 'newClusterIdentifier'', 'modifyCluster_newClusterIdentifier' - The new identifier for the cluster.
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
-- -   Must be unique for all clusters within an AWS account.
--
-- Example: @examplecluster@
--
-- 'availabilityZoneRelocation', 'modifyCluster_availabilityZoneRelocation' - The option to enable relocation for an Amazon Redshift cluster between
-- Availability Zones after the cluster modification is complete.
--
-- 'masterUserPassword', 'modifyCluster_masterUserPassword' - The new password for the cluster master user. This change is
-- asynchronously applied as soon as possible. Between the time of the
-- request and the completion of the request, the @MasterUserPassword@
-- element exists in the @PendingModifiedValues@ element of the operation
-- response.
--
-- Operations never return the password, so this operation provides a way
-- to regain access to the master user account for a cluster if the
-- password is lost.
--
-- Default: Uses existing setting.
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
-- -   Can be any printable ASCII character (ASCII code 33 to 126) except
--     \' (single quote), \" (double quote), \\, \/, \@, or space.
--
-- 'publiclyAccessible', 'modifyCluster_publiclyAccessible' - If @true@, the cluster can be accessed from a public network. Only
-- clusters in VPCs can be set to be publicly available.
--
-- 'vpcSecurityGroupIds', 'modifyCluster_vpcSecurityGroupIds' - A list of virtual private cloud (VPC) security groups to be associated
-- with the cluster. This change is asynchronously applied as soon as
-- possible.
--
-- 'clusterType', 'modifyCluster_clusterType' - The new cluster type.
--
-- When you submit your cluster resize request, your existing cluster goes
-- into a read-only mode. After Amazon Redshift provisions a new cluster
-- based on your resize requirements, there will be outage for a period
-- while the old cluster is deleted and your connection is switched to the
-- new cluster. You can use DescribeResize to track the progress of the
-- resize request.
--
-- Valid Values: @ multi-node | single-node @
--
-- 'manualSnapshotRetentionPeriod', 'modifyCluster_manualSnapshotRetentionPeriod' - The default for number of days that a newly created manual snapshot is
-- retained. If the value is -1, the manual snapshot is retained
-- indefinitely. This value doesn\'t retroactively change the retention
-- periods of existing manual snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- The default value is -1.
--
-- 'kmsKeyId', 'modifyCluster_kmsKeyId' - The AWS Key Management Service (KMS) key ID of the encryption key that
-- you want to use to encrypt data in the cluster.
--
-- 'availabilityZone', 'modifyCluster_availabilityZone' - The option to initiate relocation for an Amazon Redshift cluster to the
-- target Availability Zone.
--
-- 'preferredMaintenanceWindow', 'modifyCluster_preferredMaintenanceWindow' - The weekly time range (in UTC) during which system maintenance can
-- occur, if necessary. If system maintenance is necessary during the
-- window, it may result in an outage.
--
-- This maintenance window change is made immediately. If the new
-- maintenance window indicates the current time, there must be at least
-- 120 minutes between the current time and end of the window in order to
-- ensure that pending changes are applied.
--
-- Default: Uses existing setting.
--
-- Format: ddd:hh24:mi-ddd:hh24:mi, for example @wed:07:30-wed:08:00@.
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Must be at least 30 minutes.
--
-- 'numberOfNodes', 'modifyCluster_numberOfNodes' - The new number of nodes of the cluster. If you specify a new number of
-- nodes, you must also specify the node type parameter.
--
-- For more information about resizing clusters, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- Valid Values: Integer greater than @0@.
--
-- 'port', 'modifyCluster_port' - The option to change the port of an Amazon Redshift cluster.
--
-- 'nodeType', 'modifyCluster_nodeType' - The new node type of the cluster. If you specify a new node type, you
-- must also specify the number of nodes parameter.
--
-- For more information about resizing clusters, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@
-- | @dc2.large@ | @dc2.8xlarge@ | @ra3.xlplus@ | @ra3.4xlarge@ |
-- @ra3.16xlarge@
--
-- 'clusterVersion', 'modifyCluster_clusterVersion' - The new version number of the Amazon Redshift engine to upgrade to.
--
-- For major version upgrades, if a non-default cluster parameter group is
-- currently in use, a new cluster parameter group in the cluster parameter
-- group family for the new version must be specified. The new cluster
-- parameter group can be the default for that cluster parameter group
-- family. For more information about parameters and parameter groups, go
-- to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- Example: @1.0@
--
-- 'clusterSecurityGroups', 'modifyCluster_clusterSecurityGroups' - A list of cluster security groups to be authorized on this cluster. This
-- change is asynchronously applied as soon as possible.
--
-- Security groups currently associated with the cluster, and not in the
-- list of groups to apply, will be revoked from the cluster.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- 'maintenanceTrackName', 'modifyCluster_maintenanceTrackName' - The name for the maintenance track that you want to assign for the
-- cluster. This name change is asynchronous. The new track name stays in
-- the @PendingModifiedValues@ for the cluster until the next maintenance
-- window. When the maintenance track changes, the cluster is switched to
-- the latest cluster release available for the maintenance track. At this
-- point, the maintenance track name is applied.
--
-- 'hsmConfigurationIdentifier', 'modifyCluster_hsmConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
--
-- 'clusterIdentifier', 'modifyCluster_clusterIdentifier' - The unique identifier of the cluster to be modified.
--
-- Example: @examplecluster@
newModifyCluster ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  ModifyCluster
newModifyCluster pClusterIdentifier_ =
  ModifyCluster'
    { enhancedVpcRouting =
        Prelude.Nothing,
      elasticIp = Prelude.Nothing,
      hsmClientCertificateIdentifier = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      allowVersionUpgrade = Prelude.Nothing,
      automatedSnapshotRetentionPeriod = Prelude.Nothing,
      clusterParameterGroupName = Prelude.Nothing,
      newClusterIdentifier' = Prelude.Nothing,
      availabilityZoneRelocation = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      clusterType = Prelude.Nothing,
      manualSnapshotRetentionPeriod = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      port = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      clusterVersion = Prelude.Nothing,
      clusterSecurityGroups = Prelude.Nothing,
      maintenanceTrackName = Prelude.Nothing,
      hsmConfigurationIdentifier = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
modifyCluster_enhancedVpcRouting :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Bool)
modifyCluster_enhancedVpcRouting = Lens.lens (\ModifyCluster' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@ModifyCluster' {} a -> s {enhancedVpcRouting = a} :: ModifyCluster)

-- | The Elastic IP (EIP) address for the cluster.
--
-- Constraints: The cluster must be provisioned in EC2-VPC and
-- publicly-accessible through an Internet gateway. For more information
-- about provisioning clusters in EC2-VPC, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#cluster-platforms Supported Platforms to Launch Your Cluster>
-- in the Amazon Redshift Cluster Management Guide.
modifyCluster_elasticIp :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_elasticIp = Lens.lens (\ModifyCluster' {elasticIp} -> elasticIp) (\s@ModifyCluster' {} a -> s {elasticIp = a} :: ModifyCluster)

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
modifyCluster_hsmClientCertificateIdentifier :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_hsmClientCertificateIdentifier = Lens.lens (\ModifyCluster' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@ModifyCluster' {} a -> s {hsmClientCertificateIdentifier = a} :: ModifyCluster)

-- | Indicates whether the cluster is encrypted. If the value is encrypted
-- (true) and you provide a value for the @KmsKeyId@ parameter, we encrypt
-- the cluster with the provided @KmsKeyId@. If you don\'t provide a
-- @KmsKeyId@, we encrypt with the default key.
--
-- If the value is not encrypted (false), then the cluster is decrypted.
modifyCluster_encrypted :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Bool)
modifyCluster_encrypted = Lens.lens (\ModifyCluster' {encrypted} -> encrypted) (\s@ModifyCluster' {} a -> s {encrypted = a} :: ModifyCluster)

-- | If @true@, major version upgrades will be applied automatically to the
-- cluster during the maintenance window.
--
-- Default: @false@
modifyCluster_allowVersionUpgrade :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Bool)
modifyCluster_allowVersionUpgrade = Lens.lens (\ModifyCluster' {allowVersionUpgrade} -> allowVersionUpgrade) (\s@ModifyCluster' {} a -> s {allowVersionUpgrade = a} :: ModifyCluster)

-- | The number of days that automated snapshots are retained. If the value
-- is 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot.
--
-- If you decrease the automated snapshot retention period from its current
-- value, existing automated snapshots that fall outside of the new
-- retention period will be immediately deleted.
--
-- Default: Uses existing setting.
--
-- Constraints: Must be a value from 0 to 35.
modifyCluster_automatedSnapshotRetentionPeriod :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Int)
modifyCluster_automatedSnapshotRetentionPeriod = Lens.lens (\ModifyCluster' {automatedSnapshotRetentionPeriod} -> automatedSnapshotRetentionPeriod) (\s@ModifyCluster' {} a -> s {automatedSnapshotRetentionPeriod = a} :: ModifyCluster)

-- | The name of the cluster parameter group to apply to this cluster. This
-- change is applied only after the cluster is rebooted. To reboot a
-- cluster use RebootCluster.
--
-- Default: Uses existing setting.
--
-- Constraints: The cluster parameter group must be in the same parameter
-- group family that matches the cluster version.
modifyCluster_clusterParameterGroupName :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_clusterParameterGroupName = Lens.lens (\ModifyCluster' {clusterParameterGroupName} -> clusterParameterGroupName) (\s@ModifyCluster' {} a -> s {clusterParameterGroupName = a} :: ModifyCluster)

-- | The new identifier for the cluster.
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
-- -   Must be unique for all clusters within an AWS account.
--
-- Example: @examplecluster@
modifyCluster_newClusterIdentifier :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_newClusterIdentifier = Lens.lens (\ModifyCluster' {newClusterIdentifier'} -> newClusterIdentifier') (\s@ModifyCluster' {} a -> s {newClusterIdentifier' = a} :: ModifyCluster)

-- | The option to enable relocation for an Amazon Redshift cluster between
-- Availability Zones after the cluster modification is complete.
modifyCluster_availabilityZoneRelocation :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Bool)
modifyCluster_availabilityZoneRelocation = Lens.lens (\ModifyCluster' {availabilityZoneRelocation} -> availabilityZoneRelocation) (\s@ModifyCluster' {} a -> s {availabilityZoneRelocation = a} :: ModifyCluster)

-- | The new password for the cluster master user. This change is
-- asynchronously applied as soon as possible. Between the time of the
-- request and the completion of the request, the @MasterUserPassword@
-- element exists in the @PendingModifiedValues@ element of the operation
-- response.
--
-- Operations never return the password, so this operation provides a way
-- to regain access to the master user account for a cluster if the
-- password is lost.
--
-- Default: Uses existing setting.
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
-- -   Can be any printable ASCII character (ASCII code 33 to 126) except
--     \' (single quote), \" (double quote), \\, \/, \@, or space.
modifyCluster_masterUserPassword :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_masterUserPassword = Lens.lens (\ModifyCluster' {masterUserPassword} -> masterUserPassword) (\s@ModifyCluster' {} a -> s {masterUserPassword = a} :: ModifyCluster)

-- | If @true@, the cluster can be accessed from a public network. Only
-- clusters in VPCs can be set to be publicly available.
modifyCluster_publiclyAccessible :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Bool)
modifyCluster_publiclyAccessible = Lens.lens (\ModifyCluster' {publiclyAccessible} -> publiclyAccessible) (\s@ModifyCluster' {} a -> s {publiclyAccessible = a} :: ModifyCluster)

-- | A list of virtual private cloud (VPC) security groups to be associated
-- with the cluster. This change is asynchronously applied as soon as
-- possible.
modifyCluster_vpcSecurityGroupIds :: Lens.Lens' ModifyCluster (Prelude.Maybe [Prelude.Text])
modifyCluster_vpcSecurityGroupIds = Lens.lens (\ModifyCluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@ModifyCluster' {} a -> s {vpcSecurityGroupIds = a} :: ModifyCluster) Prelude.. Lens.mapping Lens._Coerce

-- | The new cluster type.
--
-- When you submit your cluster resize request, your existing cluster goes
-- into a read-only mode. After Amazon Redshift provisions a new cluster
-- based on your resize requirements, there will be outage for a period
-- while the old cluster is deleted and your connection is switched to the
-- new cluster. You can use DescribeResize to track the progress of the
-- resize request.
--
-- Valid Values: @ multi-node | single-node @
modifyCluster_clusterType :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_clusterType = Lens.lens (\ModifyCluster' {clusterType} -> clusterType) (\s@ModifyCluster' {} a -> s {clusterType = a} :: ModifyCluster)

-- | The default for number of days that a newly created manual snapshot is
-- retained. If the value is -1, the manual snapshot is retained
-- indefinitely. This value doesn\'t retroactively change the retention
-- periods of existing manual snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- The default value is -1.
modifyCluster_manualSnapshotRetentionPeriod :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Int)
modifyCluster_manualSnapshotRetentionPeriod = Lens.lens (\ModifyCluster' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@ModifyCluster' {} a -> s {manualSnapshotRetentionPeriod = a} :: ModifyCluster)

-- | The AWS Key Management Service (KMS) key ID of the encryption key that
-- you want to use to encrypt data in the cluster.
modifyCluster_kmsKeyId :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_kmsKeyId = Lens.lens (\ModifyCluster' {kmsKeyId} -> kmsKeyId) (\s@ModifyCluster' {} a -> s {kmsKeyId = a} :: ModifyCluster)

-- | The option to initiate relocation for an Amazon Redshift cluster to the
-- target Availability Zone.
modifyCluster_availabilityZone :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_availabilityZone = Lens.lens (\ModifyCluster' {availabilityZone} -> availabilityZone) (\s@ModifyCluster' {} a -> s {availabilityZone = a} :: ModifyCluster)

-- | The weekly time range (in UTC) during which system maintenance can
-- occur, if necessary. If system maintenance is necessary during the
-- window, it may result in an outage.
--
-- This maintenance window change is made immediately. If the new
-- maintenance window indicates the current time, there must be at least
-- 120 minutes between the current time and end of the window in order to
-- ensure that pending changes are applied.
--
-- Default: Uses existing setting.
--
-- Format: ddd:hh24:mi-ddd:hh24:mi, for example @wed:07:30-wed:08:00@.
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Must be at least 30 minutes.
modifyCluster_preferredMaintenanceWindow :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_preferredMaintenanceWindow = Lens.lens (\ModifyCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ModifyCluster' {} a -> s {preferredMaintenanceWindow = a} :: ModifyCluster)

-- | The new number of nodes of the cluster. If you specify a new number of
-- nodes, you must also specify the node type parameter.
--
-- For more information about resizing clusters, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- Valid Values: Integer greater than @0@.
modifyCluster_numberOfNodes :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Int)
modifyCluster_numberOfNodes = Lens.lens (\ModifyCluster' {numberOfNodes} -> numberOfNodes) (\s@ModifyCluster' {} a -> s {numberOfNodes = a} :: ModifyCluster)

-- | The option to change the port of an Amazon Redshift cluster.
modifyCluster_port :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Int)
modifyCluster_port = Lens.lens (\ModifyCluster' {port} -> port) (\s@ModifyCluster' {} a -> s {port = a} :: ModifyCluster)

-- | The new node type of the cluster. If you specify a new node type, you
-- must also specify the number of nodes parameter.
--
-- For more information about resizing clusters, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/rs-resize-tutorial.html Resizing Clusters in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- Valid Values: @ds2.xlarge@ | @ds2.8xlarge@ | @dc1.large@ | @dc1.8xlarge@
-- | @dc2.large@ | @dc2.8xlarge@ | @ra3.xlplus@ | @ra3.4xlarge@ |
-- @ra3.16xlarge@
modifyCluster_nodeType :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_nodeType = Lens.lens (\ModifyCluster' {nodeType} -> nodeType) (\s@ModifyCluster' {} a -> s {nodeType = a} :: ModifyCluster)

-- | The new version number of the Amazon Redshift engine to upgrade to.
--
-- For major version upgrades, if a non-default cluster parameter group is
-- currently in use, a new cluster parameter group in the cluster parameter
-- group family for the new version must be specified. The new cluster
-- parameter group can be the default for that cluster parameter group
-- family. For more information about parameters and parameter groups, go
-- to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- Example: @1.0@
modifyCluster_clusterVersion :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_clusterVersion = Lens.lens (\ModifyCluster' {clusterVersion} -> clusterVersion) (\s@ModifyCluster' {} a -> s {clusterVersion = a} :: ModifyCluster)

-- | A list of cluster security groups to be authorized on this cluster. This
-- change is asynchronously applied as soon as possible.
--
-- Security groups currently associated with the cluster, and not in the
-- list of groups to apply, will be revoked from the cluster.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
modifyCluster_clusterSecurityGroups :: Lens.Lens' ModifyCluster (Prelude.Maybe [Prelude.Text])
modifyCluster_clusterSecurityGroups = Lens.lens (\ModifyCluster' {clusterSecurityGroups} -> clusterSecurityGroups) (\s@ModifyCluster' {} a -> s {clusterSecurityGroups = a} :: ModifyCluster) Prelude.. Lens.mapping Lens._Coerce

-- | The name for the maintenance track that you want to assign for the
-- cluster. This name change is asynchronous. The new track name stays in
-- the @PendingModifiedValues@ for the cluster until the next maintenance
-- window. When the maintenance track changes, the cluster is switched to
-- the latest cluster release available for the maintenance track. At this
-- point, the maintenance track name is applied.
modifyCluster_maintenanceTrackName :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_maintenanceTrackName = Lens.lens (\ModifyCluster' {maintenanceTrackName} -> maintenanceTrackName) (\s@ModifyCluster' {} a -> s {maintenanceTrackName = a} :: ModifyCluster)

-- | Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
modifyCluster_hsmConfigurationIdentifier :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Text)
modifyCluster_hsmConfigurationIdentifier = Lens.lens (\ModifyCluster' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@ModifyCluster' {} a -> s {hsmConfigurationIdentifier = a} :: ModifyCluster)

-- | The unique identifier of the cluster to be modified.
--
-- Example: @examplecluster@
modifyCluster_clusterIdentifier :: Lens.Lens' ModifyCluster Prelude.Text
modifyCluster_clusterIdentifier = Lens.lens (\ModifyCluster' {clusterIdentifier} -> clusterIdentifier) (\s@ModifyCluster' {} a -> s {clusterIdentifier = a} :: ModifyCluster)

instance Core.AWSRequest ModifyCluster where
  type
    AWSResponse ModifyCluster =
      ModifyClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyClusterResult"
      ( \s h x ->
          ModifyClusterResponse'
            Prelude.<$> (x Core..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyCluster

instance Prelude.NFData ModifyCluster

instance Core.ToHeaders ModifyCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyCluster where
  toQuery ModifyCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyCluster" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "EnhancedVpcRouting" Core.=: enhancedVpcRouting,
        "ElasticIp" Core.=: elasticIp,
        "HsmClientCertificateIdentifier"
          Core.=: hsmClientCertificateIdentifier,
        "Encrypted" Core.=: encrypted,
        "AllowVersionUpgrade" Core.=: allowVersionUpgrade,
        "AutomatedSnapshotRetentionPeriod"
          Core.=: automatedSnapshotRetentionPeriod,
        "ClusterParameterGroupName"
          Core.=: clusterParameterGroupName,
        "NewClusterIdentifier" Core.=: newClusterIdentifier',
        "AvailabilityZoneRelocation"
          Core.=: availabilityZoneRelocation,
        "MasterUserPassword" Core.=: masterUserPassword,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "ClusterType" Core.=: clusterType,
        "ManualSnapshotRetentionPeriod"
          Core.=: manualSnapshotRetentionPeriod,
        "KmsKeyId" Core.=: kmsKeyId,
        "AvailabilityZone" Core.=: availabilityZone,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "NumberOfNodes" Core.=: numberOfNodes,
        "Port" Core.=: port,
        "NodeType" Core.=: nodeType,
        "ClusterVersion" Core.=: clusterVersion,
        "ClusterSecurityGroups"
          Core.=: Core.toQuery
            ( Core.toQueryList "ClusterSecurityGroupName"
                Prelude.<$> clusterSecurityGroups
            ),
        "MaintenanceTrackName" Core.=: maintenanceTrackName,
        "HsmConfigurationIdentifier"
          Core.=: hsmConfigurationIdentifier,
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]

-- | /See:/ 'newModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'modifyClusterResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'modifyClusterResponse_httpStatus' - The response's http status code.
newModifyClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyClusterResponse
newModifyClusterResponse pHttpStatus_ =
  ModifyClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterResponse_cluster :: Lens.Lens' ModifyClusterResponse (Prelude.Maybe Cluster)
modifyClusterResponse_cluster = Lens.lens (\ModifyClusterResponse' {cluster} -> cluster) (\s@ModifyClusterResponse' {} a -> s {cluster = a} :: ModifyClusterResponse)

-- | The response's http status code.
modifyClusterResponse_httpStatus :: Lens.Lens' ModifyClusterResponse Prelude.Int
modifyClusterResponse_httpStatus = Lens.lens (\ModifyClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterResponse' {} a -> s {httpStatus = a} :: ModifyClusterResponse)

instance Prelude.NFData ModifyClusterResponse
