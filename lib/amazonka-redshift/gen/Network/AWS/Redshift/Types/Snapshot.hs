{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Snapshot where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AccountWithRestoreAccess
import Network.AWS.Redshift.Types.Tag

-- | Describes a snapshot.
--
--
--
-- /See:/ 'snapshot' smart constructor.
data Snapshot = Snapshot'
  { _sStatus :: !(Maybe Text),
    _sRestorableNodeTypes :: !(Maybe [Text]),
    _sAccountsWithRestoreAccess :: !(Maybe [AccountWithRestoreAccess]),
    _sManualSnapshotRetentionPeriod :: !(Maybe Int),
    _sEnhancedVPCRouting :: !(Maybe Bool),
    _sSnapshotIdentifier :: !(Maybe Text),
    _sEncryptedWithHSM :: !(Maybe Bool),
    _sMasterUsername :: !(Maybe Text),
    _sSourceRegion :: !(Maybe Text),
    _sMaintenanceTrackName :: !(Maybe Text),
    _sSnapshotRetentionStartTime :: !(Maybe ISO8601),
    _sManualSnapshotRemainingDays :: !(Maybe Int),
    _sVPCId :: !(Maybe Text),
    _sBackupProgressInMegaBytes :: !(Maybe Double),
    _sEncrypted :: !(Maybe Bool),
    _sClusterIdentifier :: !(Maybe Text),
    _sNumberOfNodes :: !(Maybe Int),
    _sSnapshotType :: !(Maybe Text),
    _sKMSKeyId :: !(Maybe Text),
    _sAvailabilityZone :: !(Maybe Text),
    _sCurrentBackupRateInMegaBytesPerSecond :: !(Maybe Double),
    _sSnapshotCreateTime :: !(Maybe ISO8601),
    _sClusterVersion :: !(Maybe Text),
    _sOwnerAccount :: !(Maybe Text),
    _sNodeType :: !(Maybe Text),
    _sElapsedTimeInSeconds :: !(Maybe Integer),
    _sClusterCreateTime :: !(Maybe ISO8601),
    _sEstimatedSecondsToCompletion :: !(Maybe Integer),
    _sActualIncrementalBackupSizeInMegaBytes :: !(Maybe Double),
    _sTags :: !(Maybe [Tag]),
    _sPort :: !(Maybe Int),
    _sTotalBackupSizeInMegaBytes :: !(Maybe Double),
    _sDBName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus' - The snapshot status. The value of the status depends on the API operation used:      * 'CreateClusterSnapshot' and 'CopyClusterSnapshot' returns status as "creating".      * 'DescribeClusterSnapshots' returns status as "creating", "available", "final snapshot", or "failed".     * 'DeleteClusterSnapshot' returns status as "deleted".
--
-- * 'sRestorableNodeTypes' - The list of node types that this cluster snapshot is able to restore into.
--
-- * 'sAccountsWithRestoreAccess' - A list of the AWS customer accounts authorized to restore the snapshot. Returns @null@ if no accounts are authorized. Visible only to the snapshot owner.
--
-- * 'sManualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.  The value must be either -1 or an integer between 1 and 3,653.
--
-- * 'sEnhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
--
-- * 'sSnapshotIdentifier' - The snapshot identifier that is provided in the request.
--
-- * 'sEncryptedWithHSM' - A boolean that indicates whether the snapshot data is encrypted using the HSM keys of the source cluster. @true@ indicates that the data is encrypted using HSM keys.
--
-- * 'sMasterUsername' - The master user name for the cluster.
--
-- * 'sSourceRegion' - The source region from which the snapshot was copied.
--
-- * 'sMaintenanceTrackName' - The name of the maintenance track for the snapshot.
--
-- * 'sSnapshotRetentionStartTime' - A timestamp representing the start of the retention period for the snapshot.
--
-- * 'sManualSnapshotRemainingDays' - The number of days until a manual snapshot will pass its retention period.
--
-- * 'sVPCId' - The VPC identifier of the cluster if the snapshot is from a cluster in a VPC. Otherwise, this field is not in the output.
--
-- * 'sBackupProgressInMegaBytes' - The number of megabytes that have been transferred to the snapshot backup.
--
-- * 'sEncrypted' - If @true@ , the data in the snapshot is encrypted at rest.
--
-- * 'sClusterIdentifier' - The identifier of the cluster for which the snapshot was taken.
--
-- * 'sNumberOfNodes' - The number of nodes in the cluster.
--
-- * 'sSnapshotType' - The snapshot type. Snapshots created using 'CreateClusterSnapshot' and 'CopyClusterSnapshot' are of type "manual".
--
-- * 'sKMSKeyId' - The AWS Key Management Service (KMS) key ID of the encryption key that was used to encrypt data in the cluster from which the snapshot was taken.
--
-- * 'sAvailabilityZone' - The Availability Zone in which the cluster was created.
--
-- * 'sCurrentBackupRateInMegaBytesPerSecond' - The number of megabytes per second being transferred to the snapshot backup. Returns @0@ for a completed backup.
--
-- * 'sSnapshotCreateTime' - The time (in UTC format) when Amazon Redshift began the snapshot. A snapshot contains a copy of the cluster data as of this exact time.
--
-- * 'sClusterVersion' - The version ID of the Amazon Redshift engine that is running on the cluster.
--
-- * 'sOwnerAccount' - For manual snapshots, the AWS customer account used to create or copy the snapshot. For automatic snapshots, the owner of the cluster. The owner can perform all snapshot actions, such as sharing a manual snapshot.
--
-- * 'sNodeType' - The node type of the nodes in the cluster.
--
-- * 'sElapsedTimeInSeconds' - The amount of time an in-progress snapshot backup has been running, or the amount of time it took a completed backup to finish.
--
-- * 'sClusterCreateTime' - The time (UTC) when the cluster was originally created.
--
-- * 'sEstimatedSecondsToCompletion' - The estimate of the time remaining before the snapshot backup will complete. Returns @0@ for a completed backup.
--
-- * 'sActualIncrementalBackupSizeInMegaBytes' - The size of the incremental backup.
--
-- * 'sTags' - The list of tags for the cluster snapshot.
--
-- * 'sPort' - The port that the cluster is listening on.
--
-- * 'sTotalBackupSizeInMegaBytes' - The size of the complete set of backup data that would be used to restore the cluster.
--
-- * 'sDBName' - The name of the database that was created when the cluster was created.
snapshot ::
  Snapshot
snapshot =
  Snapshot'
    { _sStatus = Nothing,
      _sRestorableNodeTypes = Nothing,
      _sAccountsWithRestoreAccess = Nothing,
      _sManualSnapshotRetentionPeriod = Nothing,
      _sEnhancedVPCRouting = Nothing,
      _sSnapshotIdentifier = Nothing,
      _sEncryptedWithHSM = Nothing,
      _sMasterUsername = Nothing,
      _sSourceRegion = Nothing,
      _sMaintenanceTrackName = Nothing,
      _sSnapshotRetentionStartTime = Nothing,
      _sManualSnapshotRemainingDays = Nothing,
      _sVPCId = Nothing,
      _sBackupProgressInMegaBytes = Nothing,
      _sEncrypted = Nothing,
      _sClusterIdentifier = Nothing,
      _sNumberOfNodes = Nothing,
      _sSnapshotType = Nothing,
      _sKMSKeyId = Nothing,
      _sAvailabilityZone = Nothing,
      _sCurrentBackupRateInMegaBytesPerSecond = Nothing,
      _sSnapshotCreateTime = Nothing,
      _sClusterVersion = Nothing,
      _sOwnerAccount = Nothing,
      _sNodeType = Nothing,
      _sElapsedTimeInSeconds = Nothing,
      _sClusterCreateTime = Nothing,
      _sEstimatedSecondsToCompletion = Nothing,
      _sActualIncrementalBackupSizeInMegaBytes = Nothing,
      _sTags = Nothing,
      _sPort = Nothing,
      _sTotalBackupSizeInMegaBytes = Nothing,
      _sDBName = Nothing
    }

-- | The snapshot status. The value of the status depends on the API operation used:      * 'CreateClusterSnapshot' and 'CopyClusterSnapshot' returns status as "creating".      * 'DescribeClusterSnapshots' returns status as "creating", "available", "final snapshot", or "failed".     * 'DeleteClusterSnapshot' returns status as "deleted".
sStatus :: Lens' Snapshot (Maybe Text)
sStatus = lens _sStatus (\s a -> s {_sStatus = a})

-- | The list of node types that this cluster snapshot is able to restore into.
sRestorableNodeTypes :: Lens' Snapshot [Text]
sRestorableNodeTypes = lens _sRestorableNodeTypes (\s a -> s {_sRestorableNodeTypes = a}) . _Default . _Coerce

-- | A list of the AWS customer accounts authorized to restore the snapshot. Returns @null@ if no accounts are authorized. Visible only to the snapshot owner.
sAccountsWithRestoreAccess :: Lens' Snapshot [AccountWithRestoreAccess]
sAccountsWithRestoreAccess = lens _sAccountsWithRestoreAccess (\s a -> s {_sAccountsWithRestoreAccess = a}) . _Default . _Coerce

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.  The value must be either -1 or an integer between 1 and 3,653.
sManualSnapshotRetentionPeriod :: Lens' Snapshot (Maybe Int)
sManualSnapshotRetentionPeriod = lens _sManualSnapshotRetentionPeriod (\s a -> s {_sManualSnapshotRetentionPeriod = a})

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide. If this option is @true@ , enhanced VPC routing is enabled.  Default: false
sEnhancedVPCRouting :: Lens' Snapshot (Maybe Bool)
sEnhancedVPCRouting = lens _sEnhancedVPCRouting (\s a -> s {_sEnhancedVPCRouting = a})

-- | The snapshot identifier that is provided in the request.
sSnapshotIdentifier :: Lens' Snapshot (Maybe Text)
sSnapshotIdentifier = lens _sSnapshotIdentifier (\s a -> s {_sSnapshotIdentifier = a})

-- | A boolean that indicates whether the snapshot data is encrypted using the HSM keys of the source cluster. @true@ indicates that the data is encrypted using HSM keys.
sEncryptedWithHSM :: Lens' Snapshot (Maybe Bool)
sEncryptedWithHSM = lens _sEncryptedWithHSM (\s a -> s {_sEncryptedWithHSM = a})

-- | The master user name for the cluster.
sMasterUsername :: Lens' Snapshot (Maybe Text)
sMasterUsername = lens _sMasterUsername (\s a -> s {_sMasterUsername = a})

-- | The source region from which the snapshot was copied.
sSourceRegion :: Lens' Snapshot (Maybe Text)
sSourceRegion = lens _sSourceRegion (\s a -> s {_sSourceRegion = a})

-- | The name of the maintenance track for the snapshot.
sMaintenanceTrackName :: Lens' Snapshot (Maybe Text)
sMaintenanceTrackName = lens _sMaintenanceTrackName (\s a -> s {_sMaintenanceTrackName = a})

-- | A timestamp representing the start of the retention period for the snapshot.
sSnapshotRetentionStartTime :: Lens' Snapshot (Maybe UTCTime)
sSnapshotRetentionStartTime = lens _sSnapshotRetentionStartTime (\s a -> s {_sSnapshotRetentionStartTime = a}) . mapping _Time

-- | The number of days until a manual snapshot will pass its retention period.
sManualSnapshotRemainingDays :: Lens' Snapshot (Maybe Int)
sManualSnapshotRemainingDays = lens _sManualSnapshotRemainingDays (\s a -> s {_sManualSnapshotRemainingDays = a})

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a VPC. Otherwise, this field is not in the output.
sVPCId :: Lens' Snapshot (Maybe Text)
sVPCId = lens _sVPCId (\s a -> s {_sVPCId = a})

-- | The number of megabytes that have been transferred to the snapshot backup.
sBackupProgressInMegaBytes :: Lens' Snapshot (Maybe Double)
sBackupProgressInMegaBytes = lens _sBackupProgressInMegaBytes (\s a -> s {_sBackupProgressInMegaBytes = a})

-- | If @true@ , the data in the snapshot is encrypted at rest.
sEncrypted :: Lens' Snapshot (Maybe Bool)
sEncrypted = lens _sEncrypted (\s a -> s {_sEncrypted = a})

-- | The identifier of the cluster for which the snapshot was taken.
sClusterIdentifier :: Lens' Snapshot (Maybe Text)
sClusterIdentifier = lens _sClusterIdentifier (\s a -> s {_sClusterIdentifier = a})

-- | The number of nodes in the cluster.
sNumberOfNodes :: Lens' Snapshot (Maybe Int)
sNumberOfNodes = lens _sNumberOfNodes (\s a -> s {_sNumberOfNodes = a})

-- | The snapshot type. Snapshots created using 'CreateClusterSnapshot' and 'CopyClusterSnapshot' are of type "manual".
sSnapshotType :: Lens' Snapshot (Maybe Text)
sSnapshotType = lens _sSnapshotType (\s a -> s {_sSnapshotType = a})

-- | The AWS Key Management Service (KMS) key ID of the encryption key that was used to encrypt data in the cluster from which the snapshot was taken.
sKMSKeyId :: Lens' Snapshot (Maybe Text)
sKMSKeyId = lens _sKMSKeyId (\s a -> s {_sKMSKeyId = a})

-- | The Availability Zone in which the cluster was created.
sAvailabilityZone :: Lens' Snapshot (Maybe Text)
sAvailabilityZone = lens _sAvailabilityZone (\s a -> s {_sAvailabilityZone = a})

-- | The number of megabytes per second being transferred to the snapshot backup. Returns @0@ for a completed backup.
sCurrentBackupRateInMegaBytesPerSecond :: Lens' Snapshot (Maybe Double)
sCurrentBackupRateInMegaBytesPerSecond = lens _sCurrentBackupRateInMegaBytesPerSecond (\s a -> s {_sCurrentBackupRateInMegaBytesPerSecond = a})

-- | The time (in UTC format) when Amazon Redshift began the snapshot. A snapshot contains a copy of the cluster data as of this exact time.
sSnapshotCreateTime :: Lens' Snapshot (Maybe UTCTime)
sSnapshotCreateTime = lens _sSnapshotCreateTime (\s a -> s {_sSnapshotCreateTime = a}) . mapping _Time

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
sClusterVersion :: Lens' Snapshot (Maybe Text)
sClusterVersion = lens _sClusterVersion (\s a -> s {_sClusterVersion = a})

-- | For manual snapshots, the AWS customer account used to create or copy the snapshot. For automatic snapshots, the owner of the cluster. The owner can perform all snapshot actions, such as sharing a manual snapshot.
sOwnerAccount :: Lens' Snapshot (Maybe Text)
sOwnerAccount = lens _sOwnerAccount (\s a -> s {_sOwnerAccount = a})

-- | The node type of the nodes in the cluster.
sNodeType :: Lens' Snapshot (Maybe Text)
sNodeType = lens _sNodeType (\s a -> s {_sNodeType = a})

-- | The amount of time an in-progress snapshot backup has been running, or the amount of time it took a completed backup to finish.
sElapsedTimeInSeconds :: Lens' Snapshot (Maybe Integer)
sElapsedTimeInSeconds = lens _sElapsedTimeInSeconds (\s a -> s {_sElapsedTimeInSeconds = a})

-- | The time (UTC) when the cluster was originally created.
sClusterCreateTime :: Lens' Snapshot (Maybe UTCTime)
sClusterCreateTime = lens _sClusterCreateTime (\s a -> s {_sClusterCreateTime = a}) . mapping _Time

-- | The estimate of the time remaining before the snapshot backup will complete. Returns @0@ for a completed backup.
sEstimatedSecondsToCompletion :: Lens' Snapshot (Maybe Integer)
sEstimatedSecondsToCompletion = lens _sEstimatedSecondsToCompletion (\s a -> s {_sEstimatedSecondsToCompletion = a})

-- | The size of the incremental backup.
sActualIncrementalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
sActualIncrementalBackupSizeInMegaBytes = lens _sActualIncrementalBackupSizeInMegaBytes (\s a -> s {_sActualIncrementalBackupSizeInMegaBytes = a})

-- | The list of tags for the cluster snapshot.
sTags :: Lens' Snapshot [Tag]
sTags = lens _sTags (\s a -> s {_sTags = a}) . _Default . _Coerce

-- | The port that the cluster is listening on.
sPort :: Lens' Snapshot (Maybe Int)
sPort = lens _sPort (\s a -> s {_sPort = a})

-- | The size of the complete set of backup data that would be used to restore the cluster.
sTotalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
sTotalBackupSizeInMegaBytes = lens _sTotalBackupSizeInMegaBytes (\s a -> s {_sTotalBackupSizeInMegaBytes = a})

-- | The name of the database that was created when the cluster was created.
sDBName :: Lens' Snapshot (Maybe Text)
sDBName = lens _sDBName (\s a -> s {_sDBName = a})

instance FromXML Snapshot where
  parseXML x =
    Snapshot'
      <$> (x .@? "Status")
      <*> ( x .@? "RestorableNodeTypes" .!@ mempty
              >>= may (parseXMLList "NodeType")
          )
      <*> ( x .@? "AccountsWithRestoreAccess" .!@ mempty
              >>= may (parseXMLList "AccountWithRestoreAccess")
          )
      <*> (x .@? "ManualSnapshotRetentionPeriod")
      <*> (x .@? "EnhancedVpcRouting")
      <*> (x .@? "SnapshotIdentifier")
      <*> (x .@? "EncryptedWithHSM")
      <*> (x .@? "MasterUsername")
      <*> (x .@? "SourceRegion")
      <*> (x .@? "MaintenanceTrackName")
      <*> (x .@? "SnapshotRetentionStartTime")
      <*> (x .@? "ManualSnapshotRemainingDays")
      <*> (x .@? "VpcId")
      <*> (x .@? "BackupProgressInMegaBytes")
      <*> (x .@? "Encrypted")
      <*> (x .@? "ClusterIdentifier")
      <*> (x .@? "NumberOfNodes")
      <*> (x .@? "SnapshotType")
      <*> (x .@? "KmsKeyId")
      <*> (x .@? "AvailabilityZone")
      <*> (x .@? "CurrentBackupRateInMegaBytesPerSecond")
      <*> (x .@? "SnapshotCreateTime")
      <*> (x .@? "ClusterVersion")
      <*> (x .@? "OwnerAccount")
      <*> (x .@? "NodeType")
      <*> (x .@? "ElapsedTimeInSeconds")
      <*> (x .@? "ClusterCreateTime")
      <*> (x .@? "EstimatedSecondsToCompletion")
      <*> (x .@? "ActualIncrementalBackupSizeInMegaBytes")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "Tag"))
      <*> (x .@? "Port")
      <*> (x .@? "TotalBackupSizeInMegaBytes")
      <*> (x .@? "DBName")

instance Hashable Snapshot

instance NFData Snapshot
