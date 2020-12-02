{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterSnapshot where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.Tag

-- | Contains the details for an Amazon RDS DB cluster snapshot
--
--
-- This data type is used as a response element in the @DescribeDBClusterSnapshots@ action.
--
--
-- /See:/ 'dbClusterSnapshot' smart constructor.
data DBClusterSnapshot = DBClusterSnapshot'
  { _dcsEngineVersion ::
      !(Maybe Text),
    _dcsStatus :: !(Maybe Text),
    _dcsStorageEncrypted :: !(Maybe Bool),
    _dcsDBClusterIdentifier :: !(Maybe Text),
    _dcsMasterUsername :: !(Maybe Text),
    _dcsIAMDatabaseAuthenticationEnabled :: !(Maybe Bool),
    _dcsDBClusterSnapshotARN :: !(Maybe Text),
    _dcsVPCId :: !(Maybe Text),
    _dcsTagList :: !(Maybe [Tag]),
    _dcsDBClusterSnapshotIdentifier :: !(Maybe Text),
    _dcsEngine :: !(Maybe Text),
    _dcsLicenseModel :: !(Maybe Text),
    _dcsAvailabilityZones :: !(Maybe [Text]),
    _dcsSnapshotType :: !(Maybe Text),
    _dcsKMSKeyId :: !(Maybe Text),
    _dcsSnapshotCreateTime :: !(Maybe ISO8601),
    _dcsAllocatedStorage :: !(Maybe Int),
    _dcsSourceDBClusterSnapshotARN :: !(Maybe Text),
    _dcsClusterCreateTime :: !(Maybe ISO8601),
    _dcsPercentProgress :: !(Maybe Int),
    _dcsPort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBClusterSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsEngineVersion' - Provides the version of the database engine for this DB cluster snapshot.
--
-- * 'dcsStatus' - Specifies the status of this DB cluster snapshot.
--
-- * 'dcsStorageEncrypted' - Specifies whether the DB cluster snapshot is encrypted.
--
-- * 'dcsDBClusterIdentifier' - Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
--
-- * 'dcsMasterUsername' - Provides the master username for the DB cluster snapshot.
--
-- * 'dcsIAMDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- * 'dcsDBClusterSnapshotARN' - The Amazon Resource Name (ARN) for the DB cluster snapshot.
--
-- * 'dcsVPCId' - Provides the VPC ID associated with the DB cluster snapshot.
--
-- * 'dcsTagList' - Undocumented member.
--
-- * 'dcsDBClusterSnapshotIdentifier' - Specifies the identifier for the DB cluster snapshot.
--
-- * 'dcsEngine' - Specifies the name of the database engine.
--
-- * 'dcsLicenseModel' - Provides the license model information for this DB cluster snapshot.
--
-- * 'dcsAvailabilityZones' - Provides the list of Availability Zones (AZs) where instances in the DB cluster snapshot can be restored.
--
-- * 'dcsSnapshotType' - Provides the type of the DB cluster snapshot.
--
-- * 'dcsKMSKeyId' - If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster snapshot.
--
-- * 'dcsSnapshotCreateTime' - Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
--
-- * 'dcsAllocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- * 'dcsSourceDBClusterSnapshotARN' - If the DB cluster snapshot was copied from a source DB cluster snapshot, the Amazon Resource Name (ARN) for the source DB cluster snapshot, otherwise, a null value.
--
-- * 'dcsClusterCreateTime' - Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
--
-- * 'dcsPercentProgress' - Specifies the percentage of the estimated data that has been transferred.
--
-- * 'dcsPort' - Specifies the port that the DB cluster was listening on at the time of the snapshot.
dbClusterSnapshot ::
  DBClusterSnapshot
dbClusterSnapshot =
  DBClusterSnapshot'
    { _dcsEngineVersion = Nothing,
      _dcsStatus = Nothing,
      _dcsStorageEncrypted = Nothing,
      _dcsDBClusterIdentifier = Nothing,
      _dcsMasterUsername = Nothing,
      _dcsIAMDatabaseAuthenticationEnabled = Nothing,
      _dcsDBClusterSnapshotARN = Nothing,
      _dcsVPCId = Nothing,
      _dcsTagList = Nothing,
      _dcsDBClusterSnapshotIdentifier = Nothing,
      _dcsEngine = Nothing,
      _dcsLicenseModel = Nothing,
      _dcsAvailabilityZones = Nothing,
      _dcsSnapshotType = Nothing,
      _dcsKMSKeyId = Nothing,
      _dcsSnapshotCreateTime = Nothing,
      _dcsAllocatedStorage = Nothing,
      _dcsSourceDBClusterSnapshotARN = Nothing,
      _dcsClusterCreateTime = Nothing,
      _dcsPercentProgress = Nothing,
      _dcsPort = Nothing
    }

-- | Provides the version of the database engine for this DB cluster snapshot.
dcsEngineVersion :: Lens' DBClusterSnapshot (Maybe Text)
dcsEngineVersion = lens _dcsEngineVersion (\s a -> s {_dcsEngineVersion = a})

-- | Specifies the status of this DB cluster snapshot.
dcsStatus :: Lens' DBClusterSnapshot (Maybe Text)
dcsStatus = lens _dcsStatus (\s a -> s {_dcsStatus = a})

-- | Specifies whether the DB cluster snapshot is encrypted.
dcsStorageEncrypted :: Lens' DBClusterSnapshot (Maybe Bool)
dcsStorageEncrypted = lens _dcsStorageEncrypted (\s a -> s {_dcsStorageEncrypted = a})

-- | Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
dcsDBClusterIdentifier :: Lens' DBClusterSnapshot (Maybe Text)
dcsDBClusterIdentifier = lens _dcsDBClusterIdentifier (\s a -> s {_dcsDBClusterIdentifier = a})

-- | Provides the master username for the DB cluster snapshot.
dcsMasterUsername :: Lens' DBClusterSnapshot (Maybe Text)
dcsMasterUsername = lens _dcsMasterUsername (\s a -> s {_dcsMasterUsername = a})

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
dcsIAMDatabaseAuthenticationEnabled :: Lens' DBClusterSnapshot (Maybe Bool)
dcsIAMDatabaseAuthenticationEnabled = lens _dcsIAMDatabaseAuthenticationEnabled (\s a -> s {_dcsIAMDatabaseAuthenticationEnabled = a})

-- | The Amazon Resource Name (ARN) for the DB cluster snapshot.
dcsDBClusterSnapshotARN :: Lens' DBClusterSnapshot (Maybe Text)
dcsDBClusterSnapshotARN = lens _dcsDBClusterSnapshotARN (\s a -> s {_dcsDBClusterSnapshotARN = a})

-- | Provides the VPC ID associated with the DB cluster snapshot.
dcsVPCId :: Lens' DBClusterSnapshot (Maybe Text)
dcsVPCId = lens _dcsVPCId (\s a -> s {_dcsVPCId = a})

-- | Undocumented member.
dcsTagList :: Lens' DBClusterSnapshot [Tag]
dcsTagList = lens _dcsTagList (\s a -> s {_dcsTagList = a}) . _Default . _Coerce

-- | Specifies the identifier for the DB cluster snapshot.
dcsDBClusterSnapshotIdentifier :: Lens' DBClusterSnapshot (Maybe Text)
dcsDBClusterSnapshotIdentifier = lens _dcsDBClusterSnapshotIdentifier (\s a -> s {_dcsDBClusterSnapshotIdentifier = a})

-- | Specifies the name of the database engine.
dcsEngine :: Lens' DBClusterSnapshot (Maybe Text)
dcsEngine = lens _dcsEngine (\s a -> s {_dcsEngine = a})

-- | Provides the license model information for this DB cluster snapshot.
dcsLicenseModel :: Lens' DBClusterSnapshot (Maybe Text)
dcsLicenseModel = lens _dcsLicenseModel (\s a -> s {_dcsLicenseModel = a})

-- | Provides the list of Availability Zones (AZs) where instances in the DB cluster snapshot can be restored.
dcsAvailabilityZones :: Lens' DBClusterSnapshot [Text]
dcsAvailabilityZones = lens _dcsAvailabilityZones (\s a -> s {_dcsAvailabilityZones = a}) . _Default . _Coerce

-- | Provides the type of the DB cluster snapshot.
dcsSnapshotType :: Lens' DBClusterSnapshot (Maybe Text)
dcsSnapshotType = lens _dcsSnapshotType (\s a -> s {_dcsSnapshotType = a})

-- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster snapshot.
dcsKMSKeyId :: Lens' DBClusterSnapshot (Maybe Text)
dcsKMSKeyId = lens _dcsKMSKeyId (\s a -> s {_dcsKMSKeyId = a})

-- | Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
dcsSnapshotCreateTime :: Lens' DBClusterSnapshot (Maybe UTCTime)
dcsSnapshotCreateTime = lens _dcsSnapshotCreateTime (\s a -> s {_dcsSnapshotCreateTime = a}) . mapping _Time

-- | Specifies the allocated storage size in gibibytes (GiB).
dcsAllocatedStorage :: Lens' DBClusterSnapshot (Maybe Int)
dcsAllocatedStorage = lens _dcsAllocatedStorage (\s a -> s {_dcsAllocatedStorage = a})

-- | If the DB cluster snapshot was copied from a source DB cluster snapshot, the Amazon Resource Name (ARN) for the source DB cluster snapshot, otherwise, a null value.
dcsSourceDBClusterSnapshotARN :: Lens' DBClusterSnapshot (Maybe Text)
dcsSourceDBClusterSnapshotARN = lens _dcsSourceDBClusterSnapshotARN (\s a -> s {_dcsSourceDBClusterSnapshotARN = a})

-- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
dcsClusterCreateTime :: Lens' DBClusterSnapshot (Maybe UTCTime)
dcsClusterCreateTime = lens _dcsClusterCreateTime (\s a -> s {_dcsClusterCreateTime = a}) . mapping _Time

-- | Specifies the percentage of the estimated data that has been transferred.
dcsPercentProgress :: Lens' DBClusterSnapshot (Maybe Int)
dcsPercentProgress = lens _dcsPercentProgress (\s a -> s {_dcsPercentProgress = a})

-- | Specifies the port that the DB cluster was listening on at the time of the snapshot.
dcsPort :: Lens' DBClusterSnapshot (Maybe Int)
dcsPort = lens _dcsPort (\s a -> s {_dcsPort = a})

instance FromXML DBClusterSnapshot where
  parseXML x =
    DBClusterSnapshot'
      <$> (x .@? "EngineVersion")
      <*> (x .@? "Status")
      <*> (x .@? "StorageEncrypted")
      <*> (x .@? "DBClusterIdentifier")
      <*> (x .@? "MasterUsername")
      <*> (x .@? "IAMDatabaseAuthenticationEnabled")
      <*> (x .@? "DBClusterSnapshotArn")
      <*> (x .@? "VpcId")
      <*> (x .@? "TagList" .!@ mempty >>= may (parseXMLList "Tag"))
      <*> (x .@? "DBClusterSnapshotIdentifier")
      <*> (x .@? "Engine")
      <*> (x .@? "LicenseModel")
      <*> ( x .@? "AvailabilityZones" .!@ mempty
              >>= may (parseXMLList "AvailabilityZone")
          )
      <*> (x .@? "SnapshotType")
      <*> (x .@? "KmsKeyId")
      <*> (x .@? "SnapshotCreateTime")
      <*> (x .@? "AllocatedStorage")
      <*> (x .@? "SourceDBClusterSnapshotArn")
      <*> (x .@? "ClusterCreateTime")
      <*> (x .@? "PercentProgress")
      <*> (x .@? "Port")

instance Hashable DBClusterSnapshot

instance NFData DBClusterSnapshot
