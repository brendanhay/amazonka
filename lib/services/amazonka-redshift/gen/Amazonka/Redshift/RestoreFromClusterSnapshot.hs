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
-- Module      : Amazonka.Redshift.RestoreFromClusterSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cluster from a snapshot. By default, Amazon Redshift
-- creates the resulting cluster with the same configuration as the
-- original cluster from which the snapshot was created, except that the
-- new cluster is created with the default cluster security and parameter
-- groups. After Amazon Redshift creates the cluster, you can use the
-- ModifyCluster API to associate a different security group and different
-- parameter group with the restored cluster. If you are using a DS node
-- type, you can also choose to change to another DS node type of the same
-- size during restore.
--
-- If you restore a cluster into a VPC, you must provide a cluster subnet
-- group where you want the cluster restored.
--
-- For more information about working with snapshots, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
module Amazonka.Redshift.RestoreFromClusterSnapshot
  ( -- * Creating a Request
    RestoreFromClusterSnapshot (..),
    newRestoreFromClusterSnapshot,

    -- * Request Lenses
    restoreFromClusterSnapshot_additionalInfo,
    restoreFromClusterSnapshot_allowVersionUpgrade,
    restoreFromClusterSnapshot_aquaConfigurationStatus,
    restoreFromClusterSnapshot_automatedSnapshotRetentionPeriod,
    restoreFromClusterSnapshot_availabilityZone,
    restoreFromClusterSnapshot_availabilityZoneRelocation,
    restoreFromClusterSnapshot_clusterParameterGroupName,
    restoreFromClusterSnapshot_clusterSecurityGroups,
    restoreFromClusterSnapshot_clusterSubnetGroupName,
    restoreFromClusterSnapshot_defaultIamRoleArn,
    restoreFromClusterSnapshot_elasticIp,
    restoreFromClusterSnapshot_encrypted,
    restoreFromClusterSnapshot_enhancedVpcRouting,
    restoreFromClusterSnapshot_hsmClientCertificateIdentifier,
    restoreFromClusterSnapshot_hsmConfigurationIdentifier,
    restoreFromClusterSnapshot_iamRoles,
    restoreFromClusterSnapshot_kmsKeyId,
    restoreFromClusterSnapshot_maintenanceTrackName,
    restoreFromClusterSnapshot_manualSnapshotRetentionPeriod,
    restoreFromClusterSnapshot_nodeType,
    restoreFromClusterSnapshot_numberOfNodes,
    restoreFromClusterSnapshot_ownerAccount,
    restoreFromClusterSnapshot_port,
    restoreFromClusterSnapshot_preferredMaintenanceWindow,
    restoreFromClusterSnapshot_publiclyAccessible,
    restoreFromClusterSnapshot_reservedNodeId,
    restoreFromClusterSnapshot_snapshotArn,
    restoreFromClusterSnapshot_snapshotClusterIdentifier,
    restoreFromClusterSnapshot_snapshotIdentifier,
    restoreFromClusterSnapshot_snapshotScheduleIdentifier,
    restoreFromClusterSnapshot_targetReservedNodeOfferingId,
    restoreFromClusterSnapshot_vpcSecurityGroupIds,
    restoreFromClusterSnapshot_clusterIdentifier,

    -- * Destructuring the Response
    RestoreFromClusterSnapshotResponse (..),
    newRestoreFromClusterSnapshotResponse,

    -- * Response Lenses
    restoreFromClusterSnapshotResponse_cluster,
    restoreFromClusterSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newRestoreFromClusterSnapshot' smart constructor.
data RestoreFromClusterSnapshot = RestoreFromClusterSnapshot'
  { -- | Reserved.
    additionalInfo :: Prelude.Maybe Prelude.Text,
    -- | If @true@, major version upgrades can be applied during the maintenance
    -- window to the Amazon Redshift engine that is running on the cluster.
    --
    -- Default: @true@
    allowVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | This parameter is retired. It does not set the AQUA configuration
    -- status. Amazon Redshift automatically determines whether to use AQUA
    -- (Advanced Query Accelerator).
    aquaConfigurationStatus :: Prelude.Maybe AquaConfigurationStatus,
    -- | The number of days that automated snapshots are retained. If the value
    -- is 0, automated snapshots are disabled. Even if automated snapshots are
    -- disabled, you can still create manual snapshots when you want with
    -- CreateClusterSnapshot.
    --
    -- You can\'t disable automated snapshots for RA3 node types. Set the
    -- automated retention period from 1-35 days.
    --
    -- Default: The value selected for the cluster from which the snapshot was
    -- taken.
    --
    -- Constraints: Must be a value from 0 to 35.
    automatedSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The Amazon EC2 Availability Zone in which to restore the cluster.
    --
    -- Default: A random, system-chosen Availability Zone.
    --
    -- Example: @us-east-2a@
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The option to enable relocation for an Amazon Redshift cluster between
    -- Availability Zones after the cluster is restored.
    availabilityZoneRelocation :: Prelude.Maybe Prelude.Bool,
    -- | The name of the parameter group to be associated with this cluster.
    --
    -- Default: The default Amazon Redshift cluster parameter group. For
    -- information about the default parameter group, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 alphanumeric characters or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    clusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A list of security groups to be associated with this cluster.
    --
    -- Default: The default cluster security group for Amazon Redshift.
    --
    -- Cluster security groups only apply to clusters outside of VPCs.
    clusterSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The name of the subnet group where you want to cluster restored.
    --
    -- A snapshot of cluster in VPC can be restored only in VPC. Therefore, you
    -- must provide subnet group name where you want the cluster restored.
    clusterSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the IAM role that was set as default
    -- for the cluster when the cluster was last modified while it was restored
    -- from a snapshot.
    defaultIamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Elastic IP (EIP) address for the cluster. Don\'t specify the Elastic
    -- IP address for a publicly accessible cluster with availability zone
    -- relocation turned on.
    elasticIp :: Prelude.Maybe Prelude.Text,
    -- | Enables support for restoring an unencrypted snapshot to a cluster
    -- encrypted with Key Management Service (KMS) and a customer managed key.
    encrypted :: Prelude.Maybe Prelude.Bool,
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
    -- | Specifies the name of the HSM client certificate the Amazon Redshift
    -- cluster uses to retrieve the data encryption keys stored in an HSM.
    hsmClientCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the HSM configuration that contains the
    -- information the Amazon Redshift cluster can use to retrieve and store
    -- keys in an HSM.
    hsmConfigurationIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A list of Identity and Access Management (IAM) roles that can be used by
    -- the cluster to access other Amazon Web Services services. You must
    -- supply the IAM roles in their Amazon Resource Name (ARN) format.
    --
    -- The maximum number of IAM roles that you can associate is subject to a
    -- quota. For more information, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Quotas and limits>
    -- in the /Amazon Redshift Cluster Management Guide/.
    iamRoles :: Prelude.Maybe [Prelude.Text],
    -- | The Key Management Service (KMS) key ID of the encryption key that
    -- encrypts data in the cluster restored from a shared snapshot. You can
    -- also provide the key ID when you restore from an unencrypted snapshot to
    -- an encrypted cluster in the same account. Additionally, you can specify
    -- a new KMS key ID when you restore from an encrypted snapshot in the same
    -- account in order to change it. In that case, the restored cluster is
    -- encrypted with the new KMS key ID.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the maintenance track for the restored cluster. When you
    -- take a snapshot, the snapshot inherits the @MaintenanceTrack@ value from
    -- the cluster. The snapshot might be on a different track than the cluster
    -- that was the source for the snapshot. For example, suppose that you take
    -- a snapshot of a cluster that is on the current track and then change the
    -- cluster to be on the trailing track. In this case, the snapshot and the
    -- source cluster are on different tracks.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | The default number of days to retain a manual snapshot. If the value is
    -- -1, the snapshot is retained indefinitely. This setting doesn\'t change
    -- the retention period of existing snapshots.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The node type that the restored cluster will be provisioned with.
    --
    -- Default: The node type of the cluster from which the snapshot was taken.
    -- You can modify this if you are using any DS node type. In that case, you
    -- can choose to restore into another DS node type of the same size. For
    -- example, you can restore ds1.8xlarge into ds2.8xlarge, or ds1.xlarge
    -- into ds2.xlarge. If you have a DC instance type, you must restore into
    -- that same instance type and size. In other words, you can only restore a
    -- dc1.large instance type into another dc1.large instance type or
    -- dc2.large instance type. You can\'t restore dc1.8xlarge to dc2.8xlarge.
    -- First restore to a dc1.8xlarge cluster, then resize to a dc2.8large
    -- cluster. For more information about node types, see
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-about-clusters-and-nodes About Clusters and Nodes>
    -- in the /Amazon Redshift Cluster Management Guide/.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The number of nodes specified when provisioning the restored cluster.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services account used to create or copy the snapshot.
    -- Required if you are restoring a snapshot you do not own, optional if you
    -- own the snapshot.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the cluster accepts connections.
    --
    -- Default: The same port as the original cluster.
    --
    -- Constraints: Must be between @1115@ and @65535@.
    port :: Prelude.Maybe Prelude.Int,
    -- | The weekly time range (in UTC) during which automated cluster
    -- maintenance can occur.
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- Default: The value selected for the cluster from which the snapshot was
    -- taken. For more information about the time blocks for each region, see
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows>
    -- in Amazon Redshift Cluster Management Guide.
    --
    -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | If @true@, the cluster can be accessed from a public network.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the target reserved node offering.
    reservedNodeId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the snapshot associated with the
    -- message to restore from a cluster. You can specify this parameter or
    -- @snapshotIdentifier@, but not both.
    snapshotArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster the source snapshot was created from. This
    -- parameter is required if your IAM user has a policy containing a
    -- snapshot resource element that specifies anything other than * for the
    -- cluster name.
    snapshotClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the snapshot from which to create the new cluster. This
    -- parameter isn\'t case sensitive. You can specify this parameter or
    -- @snapshotArn@, but not both.
    --
    -- Example: @my-snapshot-id@
    snapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the snapshot schedule.
    snapshotScheduleIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the target reserved node offering.
    targetReservedNodeOfferingId :: Prelude.Maybe Prelude.Text,
    -- | A list of Virtual Private Cloud (VPC) security groups to be associated
    -- with the cluster.
    --
    -- Default: The default VPC security group is associated with the cluster.
    --
    -- VPC security groups only apply to clusters in VPCs.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the cluster that will be created from restoring the
    -- snapshot.
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
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreFromClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'restoreFromClusterSnapshot_additionalInfo' - Reserved.
--
-- 'allowVersionUpgrade', 'restoreFromClusterSnapshot_allowVersionUpgrade' - If @true@, major version upgrades can be applied during the maintenance
-- window to the Amazon Redshift engine that is running on the cluster.
--
-- Default: @true@
--
-- 'aquaConfigurationStatus', 'restoreFromClusterSnapshot_aquaConfigurationStatus' - This parameter is retired. It does not set the AQUA configuration
-- status. Amazon Redshift automatically determines whether to use AQUA
-- (Advanced Query Accelerator).
--
-- 'automatedSnapshotRetentionPeriod', 'restoreFromClusterSnapshot_automatedSnapshotRetentionPeriod' - The number of days that automated snapshots are retained. If the value
-- is 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot.
--
-- You can\'t disable automated snapshots for RA3 node types. Set the
-- automated retention period from 1-35 days.
--
-- Default: The value selected for the cluster from which the snapshot was
-- taken.
--
-- Constraints: Must be a value from 0 to 35.
--
-- 'availabilityZone', 'restoreFromClusterSnapshot_availabilityZone' - The Amazon EC2 Availability Zone in which to restore the cluster.
--
-- Default: A random, system-chosen Availability Zone.
--
-- Example: @us-east-2a@
--
-- 'availabilityZoneRelocation', 'restoreFromClusterSnapshot_availabilityZoneRelocation' - The option to enable relocation for an Amazon Redshift cluster between
-- Availability Zones after the cluster is restored.
--
-- 'clusterParameterGroupName', 'restoreFromClusterSnapshot_clusterParameterGroupName' - The name of the parameter group to be associated with this cluster.
--
-- Default: The default Amazon Redshift cluster parameter group. For
-- information about the default parameter group, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- 'clusterSecurityGroups', 'restoreFromClusterSnapshot_clusterSecurityGroups' - A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
--
-- Cluster security groups only apply to clusters outside of VPCs.
--
-- 'clusterSubnetGroupName', 'restoreFromClusterSnapshot_clusterSubnetGroupName' - The name of the subnet group where you want to cluster restored.
--
-- A snapshot of cluster in VPC can be restored only in VPC. Therefore, you
-- must provide subnet group name where you want the cluster restored.
--
-- 'defaultIamRoleArn', 'restoreFromClusterSnapshot_defaultIamRoleArn' - The Amazon Resource Name (ARN) for the IAM role that was set as default
-- for the cluster when the cluster was last modified while it was restored
-- from a snapshot.
--
-- 'elasticIp', 'restoreFromClusterSnapshot_elasticIp' - The Elastic IP (EIP) address for the cluster. Don\'t specify the Elastic
-- IP address for a publicly accessible cluster with availability zone
-- relocation turned on.
--
-- 'encrypted', 'restoreFromClusterSnapshot_encrypted' - Enables support for restoring an unencrypted snapshot to a cluster
-- encrypted with Key Management Service (KMS) and a customer managed key.
--
-- 'enhancedVpcRouting', 'restoreFromClusterSnapshot_enhancedVpcRouting' - An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
--
-- 'hsmClientCertificateIdentifier', 'restoreFromClusterSnapshot_hsmClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- 'hsmConfigurationIdentifier', 'restoreFromClusterSnapshot_hsmConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
--
-- 'iamRoles', 'restoreFromClusterSnapshot_iamRoles' - A list of Identity and Access Management (IAM) roles that can be used by
-- the cluster to access other Amazon Web Services services. You must
-- supply the IAM roles in their Amazon Resource Name (ARN) format.
--
-- The maximum number of IAM roles that you can associate is subject to a
-- quota. For more information, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Quotas and limits>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- 'kmsKeyId', 'restoreFromClusterSnapshot_kmsKeyId' - The Key Management Service (KMS) key ID of the encryption key that
-- encrypts data in the cluster restored from a shared snapshot. You can
-- also provide the key ID when you restore from an unencrypted snapshot to
-- an encrypted cluster in the same account. Additionally, you can specify
-- a new KMS key ID when you restore from an encrypted snapshot in the same
-- account in order to change it. In that case, the restored cluster is
-- encrypted with the new KMS key ID.
--
-- 'maintenanceTrackName', 'restoreFromClusterSnapshot_maintenanceTrackName' - The name of the maintenance track for the restored cluster. When you
-- take a snapshot, the snapshot inherits the @MaintenanceTrack@ value from
-- the cluster. The snapshot might be on a different track than the cluster
-- that was the source for the snapshot. For example, suppose that you take
-- a snapshot of a cluster that is on the current track and then change the
-- cluster to be on the trailing track. In this case, the snapshot and the
-- source cluster are on different tracks.
--
-- 'manualSnapshotRetentionPeriod', 'restoreFromClusterSnapshot_manualSnapshotRetentionPeriod' - The default number of days to retain a manual snapshot. If the value is
-- -1, the snapshot is retained indefinitely. This setting doesn\'t change
-- the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- 'nodeType', 'restoreFromClusterSnapshot_nodeType' - The node type that the restored cluster will be provisioned with.
--
-- Default: The node type of the cluster from which the snapshot was taken.
-- You can modify this if you are using any DS node type. In that case, you
-- can choose to restore into another DS node type of the same size. For
-- example, you can restore ds1.8xlarge into ds2.8xlarge, or ds1.xlarge
-- into ds2.xlarge. If you have a DC instance type, you must restore into
-- that same instance type and size. In other words, you can only restore a
-- dc1.large instance type into another dc1.large instance type or
-- dc2.large instance type. You can\'t restore dc1.8xlarge to dc2.8xlarge.
-- First restore to a dc1.8xlarge cluster, then resize to a dc2.8large
-- cluster. For more information about node types, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-about-clusters-and-nodes About Clusters and Nodes>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- 'numberOfNodes', 'restoreFromClusterSnapshot_numberOfNodes' - The number of nodes specified when provisioning the restored cluster.
--
-- 'ownerAccount', 'restoreFromClusterSnapshot_ownerAccount' - The Amazon Web Services account used to create or copy the snapshot.
-- Required if you are restoring a snapshot you do not own, optional if you
-- own the snapshot.
--
-- 'port', 'restoreFromClusterSnapshot_port' - The port number on which the cluster accepts connections.
--
-- Default: The same port as the original cluster.
--
-- Constraints: Must be between @1115@ and @65535@.
--
-- 'preferredMaintenanceWindow', 'restoreFromClusterSnapshot_preferredMaintenanceWindow' - The weekly time range (in UTC) during which automated cluster
-- maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: The value selected for the cluster from which the snapshot was
-- taken. For more information about the time blocks for each region, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows>
-- in Amazon Redshift Cluster Management Guide.
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Minimum 30-minute window.
--
-- 'publiclyAccessible', 'restoreFromClusterSnapshot_publiclyAccessible' - If @true@, the cluster can be accessed from a public network.
--
-- 'reservedNodeId', 'restoreFromClusterSnapshot_reservedNodeId' - The identifier of the target reserved node offering.
--
-- 'snapshotArn', 'restoreFromClusterSnapshot_snapshotArn' - The Amazon Resource Name (ARN) of the snapshot associated with the
-- message to restore from a cluster. You can specify this parameter or
-- @snapshotIdentifier@, but not both.
--
-- 'snapshotClusterIdentifier', 'restoreFromClusterSnapshot_snapshotClusterIdentifier' - The name of the cluster the source snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- 'snapshotIdentifier', 'restoreFromClusterSnapshot_snapshotIdentifier' - The name of the snapshot from which to create the new cluster. This
-- parameter isn\'t case sensitive. You can specify this parameter or
-- @snapshotArn@, but not both.
--
-- Example: @my-snapshot-id@
--
-- 'snapshotScheduleIdentifier', 'restoreFromClusterSnapshot_snapshotScheduleIdentifier' - A unique identifier for the snapshot schedule.
--
-- 'targetReservedNodeOfferingId', 'restoreFromClusterSnapshot_targetReservedNodeOfferingId' - The identifier of the target reserved node offering.
--
-- 'vpcSecurityGroupIds', 'restoreFromClusterSnapshot_vpcSecurityGroupIds' - A list of Virtual Private Cloud (VPC) security groups to be associated
-- with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
--
-- VPC security groups only apply to clusters in VPCs.
--
-- 'clusterIdentifier', 'restoreFromClusterSnapshot_clusterIdentifier' - The identifier of the cluster that will be created from restoring the
-- snapshot.
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
newRestoreFromClusterSnapshot ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  RestoreFromClusterSnapshot
newRestoreFromClusterSnapshot pClusterIdentifier_ =
  RestoreFromClusterSnapshot'
    { additionalInfo =
        Prelude.Nothing,
      allowVersionUpgrade = Prelude.Nothing,
      aquaConfigurationStatus = Prelude.Nothing,
      automatedSnapshotRetentionPeriod =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      availabilityZoneRelocation = Prelude.Nothing,
      clusterParameterGroupName = Prelude.Nothing,
      clusterSecurityGroups = Prelude.Nothing,
      clusterSubnetGroupName = Prelude.Nothing,
      defaultIamRoleArn = Prelude.Nothing,
      elasticIp = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      enhancedVpcRouting = Prelude.Nothing,
      hsmClientCertificateIdentifier =
        Prelude.Nothing,
      hsmConfigurationIdentifier = Prelude.Nothing,
      iamRoles = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      maintenanceTrackName = Prelude.Nothing,
      manualSnapshotRetentionPeriod = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      port = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      reservedNodeId = Prelude.Nothing,
      snapshotArn = Prelude.Nothing,
      snapshotClusterIdentifier = Prelude.Nothing,
      snapshotIdentifier = Prelude.Nothing,
      snapshotScheduleIdentifier = Prelude.Nothing,
      targetReservedNodeOfferingId = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | Reserved.
restoreFromClusterSnapshot_additionalInfo :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_additionalInfo = Lens.lens (\RestoreFromClusterSnapshot' {additionalInfo} -> additionalInfo) (\s@RestoreFromClusterSnapshot' {} a -> s {additionalInfo = a} :: RestoreFromClusterSnapshot)

-- | If @true@, major version upgrades can be applied during the maintenance
-- window to the Amazon Redshift engine that is running on the cluster.
--
-- Default: @true@
restoreFromClusterSnapshot_allowVersionUpgrade :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Bool)
restoreFromClusterSnapshot_allowVersionUpgrade = Lens.lens (\RestoreFromClusterSnapshot' {allowVersionUpgrade} -> allowVersionUpgrade) (\s@RestoreFromClusterSnapshot' {} a -> s {allowVersionUpgrade = a} :: RestoreFromClusterSnapshot)

-- | This parameter is retired. It does not set the AQUA configuration
-- status. Amazon Redshift automatically determines whether to use AQUA
-- (Advanced Query Accelerator).
restoreFromClusterSnapshot_aquaConfigurationStatus :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe AquaConfigurationStatus)
restoreFromClusterSnapshot_aquaConfigurationStatus = Lens.lens (\RestoreFromClusterSnapshot' {aquaConfigurationStatus} -> aquaConfigurationStatus) (\s@RestoreFromClusterSnapshot' {} a -> s {aquaConfigurationStatus = a} :: RestoreFromClusterSnapshot)

-- | The number of days that automated snapshots are retained. If the value
-- is 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot.
--
-- You can\'t disable automated snapshots for RA3 node types. Set the
-- automated retention period from 1-35 days.
--
-- Default: The value selected for the cluster from which the snapshot was
-- taken.
--
-- Constraints: Must be a value from 0 to 35.
restoreFromClusterSnapshot_automatedSnapshotRetentionPeriod :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Int)
restoreFromClusterSnapshot_automatedSnapshotRetentionPeriod = Lens.lens (\RestoreFromClusterSnapshot' {automatedSnapshotRetentionPeriod} -> automatedSnapshotRetentionPeriod) (\s@RestoreFromClusterSnapshot' {} a -> s {automatedSnapshotRetentionPeriod = a} :: RestoreFromClusterSnapshot)

-- | The Amazon EC2 Availability Zone in which to restore the cluster.
--
-- Default: A random, system-chosen Availability Zone.
--
-- Example: @us-east-2a@
restoreFromClusterSnapshot_availabilityZone :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_availabilityZone = Lens.lens (\RestoreFromClusterSnapshot' {availabilityZone} -> availabilityZone) (\s@RestoreFromClusterSnapshot' {} a -> s {availabilityZone = a} :: RestoreFromClusterSnapshot)

-- | The option to enable relocation for an Amazon Redshift cluster between
-- Availability Zones after the cluster is restored.
restoreFromClusterSnapshot_availabilityZoneRelocation :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Bool)
restoreFromClusterSnapshot_availabilityZoneRelocation = Lens.lens (\RestoreFromClusterSnapshot' {availabilityZoneRelocation} -> availabilityZoneRelocation) (\s@RestoreFromClusterSnapshot' {} a -> s {availabilityZoneRelocation = a} :: RestoreFromClusterSnapshot)

-- | The name of the parameter group to be associated with this cluster.
--
-- Default: The default Amazon Redshift cluster parameter group. For
-- information about the default parameter group, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Working with Amazon Redshift Parameter Groups>.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
restoreFromClusterSnapshot_clusterParameterGroupName :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_clusterParameterGroupName = Lens.lens (\RestoreFromClusterSnapshot' {clusterParameterGroupName} -> clusterParameterGroupName) (\s@RestoreFromClusterSnapshot' {} a -> s {clusterParameterGroupName = a} :: RestoreFromClusterSnapshot)

-- | A list of security groups to be associated with this cluster.
--
-- Default: The default cluster security group for Amazon Redshift.
--
-- Cluster security groups only apply to clusters outside of VPCs.
restoreFromClusterSnapshot_clusterSecurityGroups :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe [Prelude.Text])
restoreFromClusterSnapshot_clusterSecurityGroups = Lens.lens (\RestoreFromClusterSnapshot' {clusterSecurityGroups} -> clusterSecurityGroups) (\s@RestoreFromClusterSnapshot' {} a -> s {clusterSecurityGroups = a} :: RestoreFromClusterSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The name of the subnet group where you want to cluster restored.
--
-- A snapshot of cluster in VPC can be restored only in VPC. Therefore, you
-- must provide subnet group name where you want the cluster restored.
restoreFromClusterSnapshot_clusterSubnetGroupName :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_clusterSubnetGroupName = Lens.lens (\RestoreFromClusterSnapshot' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@RestoreFromClusterSnapshot' {} a -> s {clusterSubnetGroupName = a} :: RestoreFromClusterSnapshot)

-- | The Amazon Resource Name (ARN) for the IAM role that was set as default
-- for the cluster when the cluster was last modified while it was restored
-- from a snapshot.
restoreFromClusterSnapshot_defaultIamRoleArn :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_defaultIamRoleArn = Lens.lens (\RestoreFromClusterSnapshot' {defaultIamRoleArn} -> defaultIamRoleArn) (\s@RestoreFromClusterSnapshot' {} a -> s {defaultIamRoleArn = a} :: RestoreFromClusterSnapshot)

-- | The Elastic IP (EIP) address for the cluster. Don\'t specify the Elastic
-- IP address for a publicly accessible cluster with availability zone
-- relocation turned on.
restoreFromClusterSnapshot_elasticIp :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_elasticIp = Lens.lens (\RestoreFromClusterSnapshot' {elasticIp} -> elasticIp) (\s@RestoreFromClusterSnapshot' {} a -> s {elasticIp = a} :: RestoreFromClusterSnapshot)

-- | Enables support for restoring an unencrypted snapshot to a cluster
-- encrypted with Key Management Service (KMS) and a customer managed key.
restoreFromClusterSnapshot_encrypted :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Bool)
restoreFromClusterSnapshot_encrypted = Lens.lens (\RestoreFromClusterSnapshot' {encrypted} -> encrypted) (\s@RestoreFromClusterSnapshot' {} a -> s {encrypted = a} :: RestoreFromClusterSnapshot)

-- | An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
restoreFromClusterSnapshot_enhancedVpcRouting :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Bool)
restoreFromClusterSnapshot_enhancedVpcRouting = Lens.lens (\RestoreFromClusterSnapshot' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@RestoreFromClusterSnapshot' {} a -> s {enhancedVpcRouting = a} :: RestoreFromClusterSnapshot)

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
restoreFromClusterSnapshot_hsmClientCertificateIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_hsmClientCertificateIdentifier = Lens.lens (\RestoreFromClusterSnapshot' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@RestoreFromClusterSnapshot' {} a -> s {hsmClientCertificateIdentifier = a} :: RestoreFromClusterSnapshot)

-- | Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
restoreFromClusterSnapshot_hsmConfigurationIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_hsmConfigurationIdentifier = Lens.lens (\RestoreFromClusterSnapshot' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@RestoreFromClusterSnapshot' {} a -> s {hsmConfigurationIdentifier = a} :: RestoreFromClusterSnapshot)

-- | A list of Identity and Access Management (IAM) roles that can be used by
-- the cluster to access other Amazon Web Services services. You must
-- supply the IAM roles in their Amazon Resource Name (ARN) format.
--
-- The maximum number of IAM roles that you can associate is subject to a
-- quota. For more information, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Quotas and limits>
-- in the /Amazon Redshift Cluster Management Guide/.
restoreFromClusterSnapshot_iamRoles :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe [Prelude.Text])
restoreFromClusterSnapshot_iamRoles = Lens.lens (\RestoreFromClusterSnapshot' {iamRoles} -> iamRoles) (\s@RestoreFromClusterSnapshot' {} a -> s {iamRoles = a} :: RestoreFromClusterSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The Key Management Service (KMS) key ID of the encryption key that
-- encrypts data in the cluster restored from a shared snapshot. You can
-- also provide the key ID when you restore from an unencrypted snapshot to
-- an encrypted cluster in the same account. Additionally, you can specify
-- a new KMS key ID when you restore from an encrypted snapshot in the same
-- account in order to change it. In that case, the restored cluster is
-- encrypted with the new KMS key ID.
restoreFromClusterSnapshot_kmsKeyId :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_kmsKeyId = Lens.lens (\RestoreFromClusterSnapshot' {kmsKeyId} -> kmsKeyId) (\s@RestoreFromClusterSnapshot' {} a -> s {kmsKeyId = a} :: RestoreFromClusterSnapshot)

-- | The name of the maintenance track for the restored cluster. When you
-- take a snapshot, the snapshot inherits the @MaintenanceTrack@ value from
-- the cluster. The snapshot might be on a different track than the cluster
-- that was the source for the snapshot. For example, suppose that you take
-- a snapshot of a cluster that is on the current track and then change the
-- cluster to be on the trailing track. In this case, the snapshot and the
-- source cluster are on different tracks.
restoreFromClusterSnapshot_maintenanceTrackName :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_maintenanceTrackName = Lens.lens (\RestoreFromClusterSnapshot' {maintenanceTrackName} -> maintenanceTrackName) (\s@RestoreFromClusterSnapshot' {} a -> s {maintenanceTrackName = a} :: RestoreFromClusterSnapshot)

-- | The default number of days to retain a manual snapshot. If the value is
-- -1, the snapshot is retained indefinitely. This setting doesn\'t change
-- the retention period of existing snapshots.
--
-- The value must be either -1 or an integer between 1 and 3,653.
restoreFromClusterSnapshot_manualSnapshotRetentionPeriod :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Int)
restoreFromClusterSnapshot_manualSnapshotRetentionPeriod = Lens.lens (\RestoreFromClusterSnapshot' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@RestoreFromClusterSnapshot' {} a -> s {manualSnapshotRetentionPeriod = a} :: RestoreFromClusterSnapshot)

-- | The node type that the restored cluster will be provisioned with.
--
-- Default: The node type of the cluster from which the snapshot was taken.
-- You can modify this if you are using any DS node type. In that case, you
-- can choose to restore into another DS node type of the same size. For
-- example, you can restore ds1.8xlarge into ds2.8xlarge, or ds1.xlarge
-- into ds2.xlarge. If you have a DC instance type, you must restore into
-- that same instance type and size. In other words, you can only restore a
-- dc1.large instance type into another dc1.large instance type or
-- dc2.large instance type. You can\'t restore dc1.8xlarge to dc2.8xlarge.
-- First restore to a dc1.8xlarge cluster, then resize to a dc2.8large
-- cluster. For more information about node types, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-about-clusters-and-nodes About Clusters and Nodes>
-- in the /Amazon Redshift Cluster Management Guide/.
restoreFromClusterSnapshot_nodeType :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_nodeType = Lens.lens (\RestoreFromClusterSnapshot' {nodeType} -> nodeType) (\s@RestoreFromClusterSnapshot' {} a -> s {nodeType = a} :: RestoreFromClusterSnapshot)

-- | The number of nodes specified when provisioning the restored cluster.
restoreFromClusterSnapshot_numberOfNodes :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Int)
restoreFromClusterSnapshot_numberOfNodes = Lens.lens (\RestoreFromClusterSnapshot' {numberOfNodes} -> numberOfNodes) (\s@RestoreFromClusterSnapshot' {} a -> s {numberOfNodes = a} :: RestoreFromClusterSnapshot)

-- | The Amazon Web Services account used to create or copy the snapshot.
-- Required if you are restoring a snapshot you do not own, optional if you
-- own the snapshot.
restoreFromClusterSnapshot_ownerAccount :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_ownerAccount = Lens.lens (\RestoreFromClusterSnapshot' {ownerAccount} -> ownerAccount) (\s@RestoreFromClusterSnapshot' {} a -> s {ownerAccount = a} :: RestoreFromClusterSnapshot)

-- | The port number on which the cluster accepts connections.
--
-- Default: The same port as the original cluster.
--
-- Constraints: Must be between @1115@ and @65535@.
restoreFromClusterSnapshot_port :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Int)
restoreFromClusterSnapshot_port = Lens.lens (\RestoreFromClusterSnapshot' {port} -> port) (\s@RestoreFromClusterSnapshot' {} a -> s {port = a} :: RestoreFromClusterSnapshot)

-- | The weekly time range (in UTC) during which automated cluster
-- maintenance can occur.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: The value selected for the cluster from which the snapshot was
-- taken. For more information about the time blocks for each region, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html#rs-maintenance-windows Maintenance Windows>
-- in Amazon Redshift Cluster Management Guide.
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Minimum 30-minute window.
restoreFromClusterSnapshot_preferredMaintenanceWindow :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_preferredMaintenanceWindow = Lens.lens (\RestoreFromClusterSnapshot' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@RestoreFromClusterSnapshot' {} a -> s {preferredMaintenanceWindow = a} :: RestoreFromClusterSnapshot)

-- | If @true@, the cluster can be accessed from a public network.
restoreFromClusterSnapshot_publiclyAccessible :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Bool)
restoreFromClusterSnapshot_publiclyAccessible = Lens.lens (\RestoreFromClusterSnapshot' {publiclyAccessible} -> publiclyAccessible) (\s@RestoreFromClusterSnapshot' {} a -> s {publiclyAccessible = a} :: RestoreFromClusterSnapshot)

-- | The identifier of the target reserved node offering.
restoreFromClusterSnapshot_reservedNodeId :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_reservedNodeId = Lens.lens (\RestoreFromClusterSnapshot' {reservedNodeId} -> reservedNodeId) (\s@RestoreFromClusterSnapshot' {} a -> s {reservedNodeId = a} :: RestoreFromClusterSnapshot)

-- | The Amazon Resource Name (ARN) of the snapshot associated with the
-- message to restore from a cluster. You can specify this parameter or
-- @snapshotIdentifier@, but not both.
restoreFromClusterSnapshot_snapshotArn :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_snapshotArn = Lens.lens (\RestoreFromClusterSnapshot' {snapshotArn} -> snapshotArn) (\s@RestoreFromClusterSnapshot' {} a -> s {snapshotArn = a} :: RestoreFromClusterSnapshot)

-- | The name of the cluster the source snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
restoreFromClusterSnapshot_snapshotClusterIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_snapshotClusterIdentifier = Lens.lens (\RestoreFromClusterSnapshot' {snapshotClusterIdentifier} -> snapshotClusterIdentifier) (\s@RestoreFromClusterSnapshot' {} a -> s {snapshotClusterIdentifier = a} :: RestoreFromClusterSnapshot)

-- | The name of the snapshot from which to create the new cluster. This
-- parameter isn\'t case sensitive. You can specify this parameter or
-- @snapshotArn@, but not both.
--
-- Example: @my-snapshot-id@
restoreFromClusterSnapshot_snapshotIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_snapshotIdentifier = Lens.lens (\RestoreFromClusterSnapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@RestoreFromClusterSnapshot' {} a -> s {snapshotIdentifier = a} :: RestoreFromClusterSnapshot)

-- | A unique identifier for the snapshot schedule.
restoreFromClusterSnapshot_snapshotScheduleIdentifier :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_snapshotScheduleIdentifier = Lens.lens (\RestoreFromClusterSnapshot' {snapshotScheduleIdentifier} -> snapshotScheduleIdentifier) (\s@RestoreFromClusterSnapshot' {} a -> s {snapshotScheduleIdentifier = a} :: RestoreFromClusterSnapshot)

-- | The identifier of the target reserved node offering.
restoreFromClusterSnapshot_targetReservedNodeOfferingId :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreFromClusterSnapshot_targetReservedNodeOfferingId = Lens.lens (\RestoreFromClusterSnapshot' {targetReservedNodeOfferingId} -> targetReservedNodeOfferingId) (\s@RestoreFromClusterSnapshot' {} a -> s {targetReservedNodeOfferingId = a} :: RestoreFromClusterSnapshot)

-- | A list of Virtual Private Cloud (VPC) security groups to be associated
-- with the cluster.
--
-- Default: The default VPC security group is associated with the cluster.
--
-- VPC security groups only apply to clusters in VPCs.
restoreFromClusterSnapshot_vpcSecurityGroupIds :: Lens.Lens' RestoreFromClusterSnapshot (Prelude.Maybe [Prelude.Text])
restoreFromClusterSnapshot_vpcSecurityGroupIds = Lens.lens (\RestoreFromClusterSnapshot' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreFromClusterSnapshot' {} a -> s {vpcSecurityGroupIds = a} :: RestoreFromClusterSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the cluster that will be created from restoring the
-- snapshot.
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
restoreFromClusterSnapshot_clusterIdentifier :: Lens.Lens' RestoreFromClusterSnapshot Prelude.Text
restoreFromClusterSnapshot_clusterIdentifier = Lens.lens (\RestoreFromClusterSnapshot' {clusterIdentifier} -> clusterIdentifier) (\s@RestoreFromClusterSnapshot' {} a -> s {clusterIdentifier = a} :: RestoreFromClusterSnapshot)

instance Core.AWSRequest RestoreFromClusterSnapshot where
  type
    AWSResponse RestoreFromClusterSnapshot =
      RestoreFromClusterSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RestoreFromClusterSnapshotResult"
      ( \s h x ->
          RestoreFromClusterSnapshotResponse'
            Prelude.<$> (x Data..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreFromClusterSnapshot where
  hashWithSalt _salt RestoreFromClusterSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` allowVersionUpgrade
      `Prelude.hashWithSalt` aquaConfigurationStatus
      `Prelude.hashWithSalt` automatedSnapshotRetentionPeriod
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` availabilityZoneRelocation
      `Prelude.hashWithSalt` clusterParameterGroupName
      `Prelude.hashWithSalt` clusterSecurityGroups
      `Prelude.hashWithSalt` clusterSubnetGroupName
      `Prelude.hashWithSalt` defaultIamRoleArn
      `Prelude.hashWithSalt` elasticIp
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` enhancedVpcRouting
      `Prelude.hashWithSalt` hsmClientCertificateIdentifier
      `Prelude.hashWithSalt` hsmConfigurationIdentifier
      `Prelude.hashWithSalt` iamRoles
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` maintenanceTrackName
      `Prelude.hashWithSalt` manualSnapshotRetentionPeriod
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` reservedNodeId
      `Prelude.hashWithSalt` snapshotArn
      `Prelude.hashWithSalt` snapshotClusterIdentifier
      `Prelude.hashWithSalt` snapshotIdentifier
      `Prelude.hashWithSalt` snapshotScheduleIdentifier
      `Prelude.hashWithSalt` targetReservedNodeOfferingId
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData RestoreFromClusterSnapshot where
  rnf RestoreFromClusterSnapshot' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf allowVersionUpgrade
      `Prelude.seq` Prelude.rnf aquaConfigurationStatus
      `Prelude.seq` Prelude.rnf automatedSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf availabilityZoneRelocation
      `Prelude.seq` Prelude.rnf clusterParameterGroupName
      `Prelude.seq` Prelude.rnf clusterSecurityGroups
      `Prelude.seq` Prelude.rnf clusterSubnetGroupName
      `Prelude.seq` Prelude.rnf defaultIamRoleArn
      `Prelude.seq` Prelude.rnf elasticIp
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf enhancedVpcRouting
      `Prelude.seq` Prelude.rnf hsmClientCertificateIdentifier
      `Prelude.seq` Prelude.rnf hsmConfigurationIdentifier
      `Prelude.seq` Prelude.rnf iamRoles
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf maintenanceTrackName
      `Prelude.seq` Prelude.rnf
        manualSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf numberOfNodes
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        reservedNodeId
      `Prelude.seq` Prelude.rnf
        snapshotArn
      `Prelude.seq` Prelude.rnf
        snapshotClusterIdentifier
      `Prelude.seq` Prelude.rnf
        snapshotIdentifier
      `Prelude.seq` Prelude.rnf
        snapshotScheduleIdentifier
      `Prelude.seq` Prelude.rnf
        targetReservedNodeOfferingId
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf
        clusterIdentifier

instance Data.ToHeaders RestoreFromClusterSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RestoreFromClusterSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreFromClusterSnapshot where
  toQuery RestoreFromClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RestoreFromClusterSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "AdditionalInfo" Data.=: additionalInfo,
        "AllowVersionUpgrade" Data.=: allowVersionUpgrade,
        "AquaConfigurationStatus"
          Data.=: aquaConfigurationStatus,
        "AutomatedSnapshotRetentionPeriod"
          Data.=: automatedSnapshotRetentionPeriod,
        "AvailabilityZone" Data.=: availabilityZone,
        "AvailabilityZoneRelocation"
          Data.=: availabilityZoneRelocation,
        "ClusterParameterGroupName"
          Data.=: clusterParameterGroupName,
        "ClusterSecurityGroups"
          Data.=: Data.toQuery
            ( Data.toQueryList "ClusterSecurityGroupName"
                Prelude.<$> clusterSecurityGroups
            ),
        "ClusterSubnetGroupName"
          Data.=: clusterSubnetGroupName,
        "DefaultIamRoleArn" Data.=: defaultIamRoleArn,
        "ElasticIp" Data.=: elasticIp,
        "Encrypted" Data.=: encrypted,
        "EnhancedVpcRouting" Data.=: enhancedVpcRouting,
        "HsmClientCertificateIdentifier"
          Data.=: hsmClientCertificateIdentifier,
        "HsmConfigurationIdentifier"
          Data.=: hsmConfigurationIdentifier,
        "IamRoles"
          Data.=: Data.toQuery
            (Data.toQueryList "IamRoleArn" Prelude.<$> iamRoles),
        "KmsKeyId" Data.=: kmsKeyId,
        "MaintenanceTrackName" Data.=: maintenanceTrackName,
        "ManualSnapshotRetentionPeriod"
          Data.=: manualSnapshotRetentionPeriod,
        "NodeType" Data.=: nodeType,
        "NumberOfNodes" Data.=: numberOfNodes,
        "OwnerAccount" Data.=: ownerAccount,
        "Port" Data.=: port,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "PubliclyAccessible" Data.=: publiclyAccessible,
        "ReservedNodeId" Data.=: reservedNodeId,
        "SnapshotArn" Data.=: snapshotArn,
        "SnapshotClusterIdentifier"
          Data.=: snapshotClusterIdentifier,
        "SnapshotIdentifier" Data.=: snapshotIdentifier,
        "SnapshotScheduleIdentifier"
          Data.=: snapshotScheduleIdentifier,
        "TargetReservedNodeOfferingId"
          Data.=: targetReservedNodeOfferingId,
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]

-- | /See:/ 'newRestoreFromClusterSnapshotResponse' smart constructor.
data RestoreFromClusterSnapshotResponse = RestoreFromClusterSnapshotResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreFromClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'restoreFromClusterSnapshotResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'restoreFromClusterSnapshotResponse_httpStatus' - The response's http status code.
newRestoreFromClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreFromClusterSnapshotResponse
newRestoreFromClusterSnapshotResponse pHttpStatus_ =
  RestoreFromClusterSnapshotResponse'
    { cluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreFromClusterSnapshotResponse_cluster :: Lens.Lens' RestoreFromClusterSnapshotResponse (Prelude.Maybe Cluster)
restoreFromClusterSnapshotResponse_cluster = Lens.lens (\RestoreFromClusterSnapshotResponse' {cluster} -> cluster) (\s@RestoreFromClusterSnapshotResponse' {} a -> s {cluster = a} :: RestoreFromClusterSnapshotResponse)

-- | The response's http status code.
restoreFromClusterSnapshotResponse_httpStatus :: Lens.Lens' RestoreFromClusterSnapshotResponse Prelude.Int
restoreFromClusterSnapshotResponse_httpStatus = Lens.lens (\RestoreFromClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@RestoreFromClusterSnapshotResponse' {} a -> s {httpStatus = a} :: RestoreFromClusterSnapshotResponse)

instance
  Prelude.NFData
    RestoreFromClusterSnapshotResponse
  where
  rnf RestoreFromClusterSnapshotResponse' {..} =
    Prelude.rnf cluster `Prelude.seq`
      Prelude.rnf httpStatus
