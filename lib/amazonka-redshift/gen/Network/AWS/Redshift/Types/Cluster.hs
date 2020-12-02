{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Cluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Cluster where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ClusterIAMRole
import Network.AWS.Redshift.Types.ClusterNode
import Network.AWS.Redshift.Types.ClusterParameterGroupStatus
import Network.AWS.Redshift.Types.ClusterSecurityGroupMembership
import Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus
import Network.AWS.Redshift.Types.DataTransferProgress
import Network.AWS.Redshift.Types.DeferredMaintenanceWindow
import Network.AWS.Redshift.Types.ElasticIPStatus
import Network.AWS.Redshift.Types.Endpoint
import Network.AWS.Redshift.Types.HSMStatus
import Network.AWS.Redshift.Types.PendingModifiedValues
import Network.AWS.Redshift.Types.ResizeInfo
import Network.AWS.Redshift.Types.RestoreStatus
import Network.AWS.Redshift.Types.ScheduleState
import Network.AWS.Redshift.Types.Tag
import Network.AWS.Redshift.Types.VPCSecurityGroupMembership

-- | Describes a cluster.
--
--
--
-- /See:/ 'cluster' smart constructor.
data Cluster = Cluster'
  { _cResizeInfo :: !(Maybe ResizeInfo),
    _cRestoreStatus :: !(Maybe RestoreStatus),
    _cManualSnapshotRetentionPeriod :: !(Maybe Int),
    _cEnhancedVPCRouting :: !(Maybe Bool),
    _cClusterSnapshotCopyStatus :: !(Maybe ClusterSnapshotCopyStatus),
    _cClusterAvailabilityStatus :: !(Maybe Text),
    _cClusterRevisionNumber :: !(Maybe Text),
    _cSnapshotScheduleIdentifier :: !(Maybe Text),
    _cPubliclyAccessible :: !(Maybe Bool),
    _cMasterUsername :: !(Maybe Text),
    _cMaintenanceTrackName :: !(Maybe Text),
    _cExpectedNextSnapshotScheduleTime :: !(Maybe ISO8601),
    _cElasticResizeNumberOfNodeOptions :: !(Maybe Text),
    _cVPCId :: !(Maybe Text),
    _cClusterSecurityGroups ::
      !(Maybe [ClusterSecurityGroupMembership]),
    _cAutomatedSnapshotRetentionPeriod :: !(Maybe Int),
    _cSnapshotScheduleState :: !(Maybe ScheduleState),
    _cDataTransferProgress :: !(Maybe DataTransferProgress),
    _cEncrypted :: !(Maybe Bool),
    _cClusterSubnetGroupName :: !(Maybe Text),
    _cExpectedNextSnapshotScheduleTimeStatus :: !(Maybe Text),
    _cClusterIdentifier :: !(Maybe Text),
    _cDeferredMaintenanceWindows ::
      !(Maybe [DeferredMaintenanceWindow]),
    _cNumberOfNodes :: !(Maybe Int),
    _cClusterPublicKey :: !(Maybe Text),
    _cPreferredMaintenanceWindow :: !(Maybe Text),
    _cModifyStatus :: !(Maybe Text),
    _cClusterNamespaceARN :: !(Maybe Text),
    _cKMSKeyId :: !(Maybe Text),
    _cClusterParameterGroups :: !(Maybe [ClusterParameterGroupStatus]),
    _cAvailabilityZone :: !(Maybe Text),
    _cVPCSecurityGroups :: !(Maybe [VPCSecurityGroupMembership]),
    _cHSMStatus :: !(Maybe HSMStatus),
    _cIAMRoles :: !(Maybe [ClusterIAMRole]),
    _cPendingActions :: !(Maybe [Text]),
    _cElasticIPStatus :: !(Maybe ElasticIPStatus),
    _cClusterVersion :: !(Maybe Text),
    _cNodeType :: !(Maybe Text),
    _cNextMaintenanceWindowStartTime :: !(Maybe ISO8601),
    _cClusterCreateTime :: !(Maybe ISO8601),
    _cEndpoint :: !(Maybe Endpoint),
    _cAllowVersionUpgrade :: !(Maybe Bool),
    _cClusterStatus :: !(Maybe Text),
    _cPendingModifiedValues :: !(Maybe PendingModifiedValues),
    _cTags :: !(Maybe [Tag]),
    _cClusterNodes :: !(Maybe [ClusterNode]),
    _cDBName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cResizeInfo' - Returns the following:     * AllowCancelResize: a boolean value indicating if the resize operation can be cancelled.     * ResizeType: Returns ClassicResize
--
-- * 'cRestoreStatus' - A value that describes the status of a cluster restore action. This parameter returns null if the cluster was not created by restoring a snapshot.
--
-- * 'cManualSnapshotRetentionPeriod' - The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots. The value must be either -1 or an integer between 1 and 3,653.
--
-- * 'cEnhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
--
-- * 'cClusterSnapshotCopyStatus' - A value that returns the destination region and retention period that are configured for cross-region snapshot copy.
--
-- * 'cClusterAvailabilityStatus' - The availability status of the cluster for queries. Possible values are the following:     * Available - The cluster is available for queries.      * Unavailable - The cluster is not available for queries.     * Maintenance - The cluster is intermittently available for queries due to maintenance activities.     * Modifying - The cluster is intermittently available for queries due to changes that modify the cluster.     * Failed - The cluster failed and is not available for queries.
--
-- * 'cClusterRevisionNumber' - The specific revision number of the database in the cluster.
--
-- * 'cSnapshotScheduleIdentifier' - A unique identifier for the cluster snapshot schedule.
--
-- * 'cPubliclyAccessible' - A boolean value that, if @true@ , indicates that the cluster can be accessed from a public network.
--
-- * 'cMasterUsername' - The master user name for the cluster. This name is used to connect to the database that is specified in the __DBName__ parameter.
--
-- * 'cMaintenanceTrackName' - The name of the maintenance track for the cluster.
--
-- * 'cExpectedNextSnapshotScheduleTime' - The date and time when the next snapshot is expected to be taken for clusters with a valid snapshot schedule and backups enabled.
--
-- * 'cElasticResizeNumberOfNodeOptions' - The number of nodes that you can resize the cluster to with the elastic resize method.
--
-- * 'cVPCId' - The identifier of the VPC the cluster is in, if the cluster is in a VPC.
--
-- * 'cClusterSecurityGroups' - A list of cluster security group that are associated with the cluster. Each security group is represented by an element that contains @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@ subelements.  Cluster security groups are used when the cluster is not created in an Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC use VPC security groups, which are listed by the __VpcSecurityGroups__ parameter.
--
-- * 'cAutomatedSnapshotRetentionPeriod' - The number of days that automatic cluster snapshots are retained.
--
-- * 'cSnapshotScheduleState' - The current state of the cluster snapshot schedule.
--
-- * 'cDataTransferProgress' -
--
-- * 'cEncrypted' - A boolean value that, if @true@ , indicates that data in the cluster is encrypted at rest.
--
-- * 'cClusterSubnetGroupName' - The name of the subnet group that is associated with the cluster. This parameter is valid only when the cluster is in a VPC.
--
-- * 'cExpectedNextSnapshotScheduleTimeStatus' - The status of next expected snapshot for clusters having a valid snapshot schedule and backups enabled. Possible values are the following:     * OnTrack - The next snapshot is expected to be taken on time.      * Pending - The next snapshot is pending to be taken.
--
-- * 'cClusterIdentifier' - The unique identifier of the cluster.
--
-- * 'cDeferredMaintenanceWindows' - Describes a group of @DeferredMaintenanceWindow@ objects.
--
-- * 'cNumberOfNodes' - The number of compute nodes in the cluster.
--
-- * 'cClusterPublicKey' - The public key for the cluster.
--
-- * 'cPreferredMaintenanceWindow' - The weekly time range, in Universal Coordinated Time (UTC), during which system maintenance can occur.
--
-- * 'cModifyStatus' - The status of a modify operation, if any, initiated for the cluster.
--
-- * 'cClusterNamespaceARN' - The namespace Amazon Resource Name (ARN) of the cluster.
--
-- * 'cKMSKeyId' - The AWS Key Management Service (AWS KMS) key ID of the encryption key used to encrypt data in the cluster.
--
-- * 'cClusterParameterGroups' - The list of cluster parameter groups that are associated with this cluster. Each parameter group in the list is returned with its status.
--
-- * 'cAvailabilityZone' - The name of the Availability Zone in which the cluster is located.
--
-- * 'cVPCSecurityGroups' - A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that are associated with the cluster. This parameter is returned only if the cluster is in a VPC.
--
-- * 'cHSMStatus' - A value that reports whether the Amazon Redshift cluster has finished applying any hardware security module (HSM) settings changes specified in a modify cluster command. Values: active, applying
--
-- * 'cIAMRoles' - A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
--
-- * 'cPendingActions' - Cluster operations that are waiting to be started.
--
-- * 'cElasticIPStatus' - The status of the elastic IP (EIP) address.
--
-- * 'cClusterVersion' - The version ID of the Amazon Redshift engine that is running on the cluster.
--
-- * 'cNodeType' - The node type for the nodes in the cluster.
--
-- * 'cNextMaintenanceWindowStartTime' - The date and time in UTC when system maintenance can begin.
--
-- * 'cClusterCreateTime' - The date and time that the cluster was created.
--
-- * 'cEndpoint' - The connection endpoint.
--
-- * 'cAllowVersionUpgrade' - A boolean value that, if @true@ , indicates that major version upgrades will be applied automatically to the cluster during the maintenance window.
--
-- * 'cClusterStatus' - The current state of the cluster. Possible values are the following:     * @available@      * @available, prep-for-resize@      * @available, resize-cleanup@      * @cancelling-resize@      * @creating@      * @deleting@      * @final-snapshot@      * @hardware-failure@      * @incompatible-hsm@      * @incompatible-network@      * @incompatible-parameters@      * @incompatible-restore@      * @modifying@      * @paused@      * @rebooting@      * @renaming@      * @resizing@      * @rotating-keys@      * @storage-full@      * @updating-hsm@
--
-- * 'cPendingModifiedValues' - A value that, if present, indicates that changes to the cluster are pending. Specific pending changes are identified by subelements.
--
-- * 'cTags' - The list of tags for the cluster.
--
-- * 'cClusterNodes' - The nodes in the cluster.
--
-- * 'cDBName' - The name of the initial database that was created when the cluster was created. This same name is returned for the life of the cluster. If an initial database was not specified, a database named @dev@ dev was created by default.
cluster ::
  Cluster
cluster =
  Cluster'
    { _cResizeInfo = Nothing,
      _cRestoreStatus = Nothing,
      _cManualSnapshotRetentionPeriod = Nothing,
      _cEnhancedVPCRouting = Nothing,
      _cClusterSnapshotCopyStatus = Nothing,
      _cClusterAvailabilityStatus = Nothing,
      _cClusterRevisionNumber = Nothing,
      _cSnapshotScheduleIdentifier = Nothing,
      _cPubliclyAccessible = Nothing,
      _cMasterUsername = Nothing,
      _cMaintenanceTrackName = Nothing,
      _cExpectedNextSnapshotScheduleTime = Nothing,
      _cElasticResizeNumberOfNodeOptions = Nothing,
      _cVPCId = Nothing,
      _cClusterSecurityGroups = Nothing,
      _cAutomatedSnapshotRetentionPeriod = Nothing,
      _cSnapshotScheduleState = Nothing,
      _cDataTransferProgress = Nothing,
      _cEncrypted = Nothing,
      _cClusterSubnetGroupName = Nothing,
      _cExpectedNextSnapshotScheduleTimeStatus = Nothing,
      _cClusterIdentifier = Nothing,
      _cDeferredMaintenanceWindows = Nothing,
      _cNumberOfNodes = Nothing,
      _cClusterPublicKey = Nothing,
      _cPreferredMaintenanceWindow = Nothing,
      _cModifyStatus = Nothing,
      _cClusterNamespaceARN = Nothing,
      _cKMSKeyId = Nothing,
      _cClusterParameterGroups = Nothing,
      _cAvailabilityZone = Nothing,
      _cVPCSecurityGroups = Nothing,
      _cHSMStatus = Nothing,
      _cIAMRoles = Nothing,
      _cPendingActions = Nothing,
      _cElasticIPStatus = Nothing,
      _cClusterVersion = Nothing,
      _cNodeType = Nothing,
      _cNextMaintenanceWindowStartTime = Nothing,
      _cClusterCreateTime = Nothing,
      _cEndpoint = Nothing,
      _cAllowVersionUpgrade = Nothing,
      _cClusterStatus = Nothing,
      _cPendingModifiedValues = Nothing,
      _cTags = Nothing,
      _cClusterNodes = Nothing,
      _cDBName = Nothing
    }

-- | Returns the following:     * AllowCancelResize: a boolean value indicating if the resize operation can be cancelled.     * ResizeType: Returns ClassicResize
cResizeInfo :: Lens' Cluster (Maybe ResizeInfo)
cResizeInfo = lens _cResizeInfo (\s a -> s {_cResizeInfo = a})

-- | A value that describes the status of a cluster restore action. This parameter returns null if the cluster was not created by restoring a snapshot.
cRestoreStatus :: Lens' Cluster (Maybe RestoreStatus)
cRestoreStatus = lens _cRestoreStatus (\s a -> s {_cRestoreStatus = a})

-- | The default number of days to retain a manual snapshot. If the value is -1, the snapshot is retained indefinitely. This setting doesn't change the retention period of existing snapshots. The value must be either -1 or an integer between 1 and 3,653.
cManualSnapshotRetentionPeriod :: Lens' Cluster (Maybe Int)
cManualSnapshotRetentionPeriod = lens _cManualSnapshotRetentionPeriod (\s a -> s {_cManualSnapshotRetentionPeriod = a})

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
cEnhancedVPCRouting :: Lens' Cluster (Maybe Bool)
cEnhancedVPCRouting = lens _cEnhancedVPCRouting (\s a -> s {_cEnhancedVPCRouting = a})

-- | A value that returns the destination region and retention period that are configured for cross-region snapshot copy.
cClusterSnapshotCopyStatus :: Lens' Cluster (Maybe ClusterSnapshotCopyStatus)
cClusterSnapshotCopyStatus = lens _cClusterSnapshotCopyStatus (\s a -> s {_cClusterSnapshotCopyStatus = a})

-- | The availability status of the cluster for queries. Possible values are the following:     * Available - The cluster is available for queries.      * Unavailable - The cluster is not available for queries.     * Maintenance - The cluster is intermittently available for queries due to maintenance activities.     * Modifying - The cluster is intermittently available for queries due to changes that modify the cluster.     * Failed - The cluster failed and is not available for queries.
cClusterAvailabilityStatus :: Lens' Cluster (Maybe Text)
cClusterAvailabilityStatus = lens _cClusterAvailabilityStatus (\s a -> s {_cClusterAvailabilityStatus = a})

-- | The specific revision number of the database in the cluster.
cClusterRevisionNumber :: Lens' Cluster (Maybe Text)
cClusterRevisionNumber = lens _cClusterRevisionNumber (\s a -> s {_cClusterRevisionNumber = a})

-- | A unique identifier for the cluster snapshot schedule.
cSnapshotScheduleIdentifier :: Lens' Cluster (Maybe Text)
cSnapshotScheduleIdentifier = lens _cSnapshotScheduleIdentifier (\s a -> s {_cSnapshotScheduleIdentifier = a})

-- | A boolean value that, if @true@ , indicates that the cluster can be accessed from a public network.
cPubliclyAccessible :: Lens' Cluster (Maybe Bool)
cPubliclyAccessible = lens _cPubliclyAccessible (\s a -> s {_cPubliclyAccessible = a})

-- | The master user name for the cluster. This name is used to connect to the database that is specified in the __DBName__ parameter.
cMasterUsername :: Lens' Cluster (Maybe Text)
cMasterUsername = lens _cMasterUsername (\s a -> s {_cMasterUsername = a})

-- | The name of the maintenance track for the cluster.
cMaintenanceTrackName :: Lens' Cluster (Maybe Text)
cMaintenanceTrackName = lens _cMaintenanceTrackName (\s a -> s {_cMaintenanceTrackName = a})

-- | The date and time when the next snapshot is expected to be taken for clusters with a valid snapshot schedule and backups enabled.
cExpectedNextSnapshotScheduleTime :: Lens' Cluster (Maybe UTCTime)
cExpectedNextSnapshotScheduleTime = lens _cExpectedNextSnapshotScheduleTime (\s a -> s {_cExpectedNextSnapshotScheduleTime = a}) . mapping _Time

-- | The number of nodes that you can resize the cluster to with the elastic resize method.
cElasticResizeNumberOfNodeOptions :: Lens' Cluster (Maybe Text)
cElasticResizeNumberOfNodeOptions = lens _cElasticResizeNumberOfNodeOptions (\s a -> s {_cElasticResizeNumberOfNodeOptions = a})

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
cVPCId :: Lens' Cluster (Maybe Text)
cVPCId = lens _cVPCId (\s a -> s {_cVPCId = a})

-- | A list of cluster security group that are associated with the cluster. Each security group is represented by an element that contains @ClusterSecurityGroup.Name@ and @ClusterSecurityGroup.Status@ subelements.  Cluster security groups are used when the cluster is not created in an Amazon Virtual Private Cloud (VPC). Clusters that are created in a VPC use VPC security groups, which are listed by the __VpcSecurityGroups__ parameter.
cClusterSecurityGroups :: Lens' Cluster [ClusterSecurityGroupMembership]
cClusterSecurityGroups = lens _cClusterSecurityGroups (\s a -> s {_cClusterSecurityGroups = a}) . _Default . _Coerce

-- | The number of days that automatic cluster snapshots are retained.
cAutomatedSnapshotRetentionPeriod :: Lens' Cluster (Maybe Int)
cAutomatedSnapshotRetentionPeriod = lens _cAutomatedSnapshotRetentionPeriod (\s a -> s {_cAutomatedSnapshotRetentionPeriod = a})

-- | The current state of the cluster snapshot schedule.
cSnapshotScheduleState :: Lens' Cluster (Maybe ScheduleState)
cSnapshotScheduleState = lens _cSnapshotScheduleState (\s a -> s {_cSnapshotScheduleState = a})

-- |
cDataTransferProgress :: Lens' Cluster (Maybe DataTransferProgress)
cDataTransferProgress = lens _cDataTransferProgress (\s a -> s {_cDataTransferProgress = a})

-- | A boolean value that, if @true@ , indicates that data in the cluster is encrypted at rest.
cEncrypted :: Lens' Cluster (Maybe Bool)
cEncrypted = lens _cEncrypted (\s a -> s {_cEncrypted = a})

-- | The name of the subnet group that is associated with the cluster. This parameter is valid only when the cluster is in a VPC.
cClusterSubnetGroupName :: Lens' Cluster (Maybe Text)
cClusterSubnetGroupName = lens _cClusterSubnetGroupName (\s a -> s {_cClusterSubnetGroupName = a})

-- | The status of next expected snapshot for clusters having a valid snapshot schedule and backups enabled. Possible values are the following:     * OnTrack - The next snapshot is expected to be taken on time.      * Pending - The next snapshot is pending to be taken.
cExpectedNextSnapshotScheduleTimeStatus :: Lens' Cluster (Maybe Text)
cExpectedNextSnapshotScheduleTimeStatus = lens _cExpectedNextSnapshotScheduleTimeStatus (\s a -> s {_cExpectedNextSnapshotScheduleTimeStatus = a})

-- | The unique identifier of the cluster.
cClusterIdentifier :: Lens' Cluster (Maybe Text)
cClusterIdentifier = lens _cClusterIdentifier (\s a -> s {_cClusterIdentifier = a})

-- | Describes a group of @DeferredMaintenanceWindow@ objects.
cDeferredMaintenanceWindows :: Lens' Cluster [DeferredMaintenanceWindow]
cDeferredMaintenanceWindows = lens _cDeferredMaintenanceWindows (\s a -> s {_cDeferredMaintenanceWindows = a}) . _Default . _Coerce

-- | The number of compute nodes in the cluster.
cNumberOfNodes :: Lens' Cluster (Maybe Int)
cNumberOfNodes = lens _cNumberOfNodes (\s a -> s {_cNumberOfNodes = a})

-- | The public key for the cluster.
cClusterPublicKey :: Lens' Cluster (Maybe Text)
cClusterPublicKey = lens _cClusterPublicKey (\s a -> s {_cClusterPublicKey = a})

-- | The weekly time range, in Universal Coordinated Time (UTC), during which system maintenance can occur.
cPreferredMaintenanceWindow :: Lens' Cluster (Maybe Text)
cPreferredMaintenanceWindow = lens _cPreferredMaintenanceWindow (\s a -> s {_cPreferredMaintenanceWindow = a})

-- | The status of a modify operation, if any, initiated for the cluster.
cModifyStatus :: Lens' Cluster (Maybe Text)
cModifyStatus = lens _cModifyStatus (\s a -> s {_cModifyStatus = a})

-- | The namespace Amazon Resource Name (ARN) of the cluster.
cClusterNamespaceARN :: Lens' Cluster (Maybe Text)
cClusterNamespaceARN = lens _cClusterNamespaceARN (\s a -> s {_cClusterNamespaceARN = a})

-- | The AWS Key Management Service (AWS KMS) key ID of the encryption key used to encrypt data in the cluster.
cKMSKeyId :: Lens' Cluster (Maybe Text)
cKMSKeyId = lens _cKMSKeyId (\s a -> s {_cKMSKeyId = a})

-- | The list of cluster parameter groups that are associated with this cluster. Each parameter group in the list is returned with its status.
cClusterParameterGroups :: Lens' Cluster [ClusterParameterGroupStatus]
cClusterParameterGroups = lens _cClusterParameterGroups (\s a -> s {_cClusterParameterGroups = a}) . _Default . _Coerce

-- | The name of the Availability Zone in which the cluster is located.
cAvailabilityZone :: Lens' Cluster (Maybe Text)
cAvailabilityZone = lens _cAvailabilityZone (\s a -> s {_cAvailabilityZone = a})

-- | A list of Amazon Virtual Private Cloud (Amazon VPC) security groups that are associated with the cluster. This parameter is returned only if the cluster is in a VPC.
cVPCSecurityGroups :: Lens' Cluster [VPCSecurityGroupMembership]
cVPCSecurityGroups = lens _cVPCSecurityGroups (\s a -> s {_cVPCSecurityGroups = a}) . _Default . _Coerce

-- | A value that reports whether the Amazon Redshift cluster has finished applying any hardware security module (HSM) settings changes specified in a modify cluster command. Values: active, applying
cHSMStatus :: Lens' Cluster (Maybe HSMStatus)
cHSMStatus = lens _cHSMStatus (\s a -> s {_cHSMStatus = a})

-- | A list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
cIAMRoles :: Lens' Cluster [ClusterIAMRole]
cIAMRoles = lens _cIAMRoles (\s a -> s {_cIAMRoles = a}) . _Default . _Coerce

-- | Cluster operations that are waiting to be started.
cPendingActions :: Lens' Cluster [Text]
cPendingActions = lens _cPendingActions (\s a -> s {_cPendingActions = a}) . _Default . _Coerce

-- | The status of the elastic IP (EIP) address.
cElasticIPStatus :: Lens' Cluster (Maybe ElasticIPStatus)
cElasticIPStatus = lens _cElasticIPStatus (\s a -> s {_cElasticIPStatus = a})

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
cClusterVersion :: Lens' Cluster (Maybe Text)
cClusterVersion = lens _cClusterVersion (\s a -> s {_cClusterVersion = a})

-- | The node type for the nodes in the cluster.
cNodeType :: Lens' Cluster (Maybe Text)
cNodeType = lens _cNodeType (\s a -> s {_cNodeType = a})

-- | The date and time in UTC when system maintenance can begin.
cNextMaintenanceWindowStartTime :: Lens' Cluster (Maybe UTCTime)
cNextMaintenanceWindowStartTime = lens _cNextMaintenanceWindowStartTime (\s a -> s {_cNextMaintenanceWindowStartTime = a}) . mapping _Time

-- | The date and time that the cluster was created.
cClusterCreateTime :: Lens' Cluster (Maybe UTCTime)
cClusterCreateTime = lens _cClusterCreateTime (\s a -> s {_cClusterCreateTime = a}) . mapping _Time

-- | The connection endpoint.
cEndpoint :: Lens' Cluster (Maybe Endpoint)
cEndpoint = lens _cEndpoint (\s a -> s {_cEndpoint = a})

-- | A boolean value that, if @true@ , indicates that major version upgrades will be applied automatically to the cluster during the maintenance window.
cAllowVersionUpgrade :: Lens' Cluster (Maybe Bool)
cAllowVersionUpgrade = lens _cAllowVersionUpgrade (\s a -> s {_cAllowVersionUpgrade = a})

-- | The current state of the cluster. Possible values are the following:     * @available@      * @available, prep-for-resize@      * @available, resize-cleanup@      * @cancelling-resize@      * @creating@      * @deleting@      * @final-snapshot@      * @hardware-failure@      * @incompatible-hsm@      * @incompatible-network@      * @incompatible-parameters@      * @incompatible-restore@      * @modifying@      * @paused@      * @rebooting@      * @renaming@      * @resizing@      * @rotating-keys@      * @storage-full@      * @updating-hsm@
cClusterStatus :: Lens' Cluster (Maybe Text)
cClusterStatus = lens _cClusterStatus (\s a -> s {_cClusterStatus = a})

-- | A value that, if present, indicates that changes to the cluster are pending. Specific pending changes are identified by subelements.
cPendingModifiedValues :: Lens' Cluster (Maybe PendingModifiedValues)
cPendingModifiedValues = lens _cPendingModifiedValues (\s a -> s {_cPendingModifiedValues = a})

-- | The list of tags for the cluster.
cTags :: Lens' Cluster [Tag]
cTags = lens _cTags (\s a -> s {_cTags = a}) . _Default . _Coerce

-- | The nodes in the cluster.
cClusterNodes :: Lens' Cluster [ClusterNode]
cClusterNodes = lens _cClusterNodes (\s a -> s {_cClusterNodes = a}) . _Default . _Coerce

-- | The name of the initial database that was created when the cluster was created. This same name is returned for the life of the cluster. If an initial database was not specified, a database named @dev@ dev was created by default.
cDBName :: Lens' Cluster (Maybe Text)
cDBName = lens _cDBName (\s a -> s {_cDBName = a})

instance FromXML Cluster where
  parseXML x =
    Cluster'
      <$> (x .@? "ResizeInfo")
      <*> (x .@? "RestoreStatus")
      <*> (x .@? "ManualSnapshotRetentionPeriod")
      <*> (x .@? "EnhancedVpcRouting")
      <*> (x .@? "ClusterSnapshotCopyStatus")
      <*> (x .@? "ClusterAvailabilityStatus")
      <*> (x .@? "ClusterRevisionNumber")
      <*> (x .@? "SnapshotScheduleIdentifier")
      <*> (x .@? "PubliclyAccessible")
      <*> (x .@? "MasterUsername")
      <*> (x .@? "MaintenanceTrackName")
      <*> (x .@? "ExpectedNextSnapshotScheduleTime")
      <*> (x .@? "ElasticResizeNumberOfNodeOptions")
      <*> (x .@? "VpcId")
      <*> ( x .@? "ClusterSecurityGroups" .!@ mempty
              >>= may (parseXMLList "ClusterSecurityGroup")
          )
      <*> (x .@? "AutomatedSnapshotRetentionPeriod")
      <*> (x .@? "SnapshotScheduleState")
      <*> (x .@? "DataTransferProgress")
      <*> (x .@? "Encrypted")
      <*> (x .@? "ClusterSubnetGroupName")
      <*> (x .@? "ExpectedNextSnapshotScheduleTimeStatus")
      <*> (x .@? "ClusterIdentifier")
      <*> ( x .@? "DeferredMaintenanceWindows" .!@ mempty
              >>= may (parseXMLList "DeferredMaintenanceWindow")
          )
      <*> (x .@? "NumberOfNodes")
      <*> (x .@? "ClusterPublicKey")
      <*> (x .@? "PreferredMaintenanceWindow")
      <*> (x .@? "ModifyStatus")
      <*> (x .@? "ClusterNamespaceArn")
      <*> (x .@? "KmsKeyId")
      <*> ( x .@? "ClusterParameterGroups" .!@ mempty
              >>= may (parseXMLList "ClusterParameterGroup")
          )
      <*> (x .@? "AvailabilityZone")
      <*> ( x .@? "VpcSecurityGroups" .!@ mempty
              >>= may (parseXMLList "VpcSecurityGroup")
          )
      <*> (x .@? "HsmStatus")
      <*> ( x .@? "IamRoles" .!@ mempty
              >>= may (parseXMLList "ClusterIamRole")
          )
      <*> (x .@? "PendingActions" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "ElasticIpStatus")
      <*> (x .@? "ClusterVersion")
      <*> (x .@? "NodeType")
      <*> (x .@? "NextMaintenanceWindowStartTime")
      <*> (x .@? "ClusterCreateTime")
      <*> (x .@? "Endpoint")
      <*> (x .@? "AllowVersionUpgrade")
      <*> (x .@? "ClusterStatus")
      <*> (x .@? "PendingModifiedValues")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "Tag"))
      <*> (x .@? "ClusterNodes" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "DBName")

instance Hashable Cluster

instance NFData Cluster
