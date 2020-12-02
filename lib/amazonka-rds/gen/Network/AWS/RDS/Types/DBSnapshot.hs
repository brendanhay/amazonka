{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSnapshot where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.ProcessorFeature
import Network.AWS.RDS.Types.Tag

-- | Contains the details of an Amazon RDS DB snapshot.
--
--
-- This data type is used as a response element in the @DescribeDBSnapshots@ action.
--
--
-- /See:/ 'dbSnapshot' smart constructor.
data DBSnapshot = DBSnapshot'
  { _dsEngineVersion :: !(Maybe Text),
    _dsStatus :: !(Maybe Text),
    _dsDBSnapshotARN :: !(Maybe Text),
    _dsMasterUsername :: !(Maybe Text),
    _dsSourceRegion :: !(Maybe Text),
    _dsIAMDatabaseAuthenticationEnabled :: !(Maybe Bool),
    _dsIOPS :: !(Maybe Int),
    _dsVPCId :: !(Maybe Text),
    _dsInstanceCreateTime :: !(Maybe ISO8601),
    _dsTagList :: !(Maybe [Tag]),
    _dsEngine :: !(Maybe Text),
    _dsEncrypted :: !(Maybe Bool),
    _dsDBSnapshotIdentifier :: !(Maybe Text),
    _dsProcessorFeatures :: !(Maybe [ProcessorFeature]),
    _dsLicenseModel :: !(Maybe Text),
    _dsSourceDBSnapshotIdentifier :: !(Maybe Text),
    _dsSnapshotType :: !(Maybe Text),
    _dsDBInstanceIdentifier :: !(Maybe Text),
    _dsKMSKeyId :: !(Maybe Text),
    _dsAvailabilityZone :: !(Maybe Text),
    _dsSnapshotCreateTime :: !(Maybe ISO8601),
    _dsAllocatedStorage :: !(Maybe Int),
    _dsDBiResourceId :: !(Maybe Text),
    _dsOptionGroupName :: !(Maybe Text),
    _dsTimezone :: !(Maybe Text),
    _dsTDECredentialARN :: !(Maybe Text),
    _dsPercentProgress :: !(Maybe Int),
    _dsPort :: !(Maybe Int),
    _dsStorageType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsEngineVersion' - Specifies the version of the database engine.
--
-- * 'dsStatus' - Specifies the status of this DB snapshot.
--
-- * 'dsDBSnapshotARN' - The Amazon Resource Name (ARN) for the DB snapshot.
--
-- * 'dsMasterUsername' - Provides the master username for the DB snapshot.
--
-- * 'dsSourceRegion' - The AWS Region that the DB snapshot was created in or copied from.
--
-- * 'dsIAMDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- * 'dsIOPS' - Specifies the Provisioned IOPS (I/O operations per second) value of the DB instance at the time of the snapshot.
--
-- * 'dsVPCId' - Provides the VPC ID associated with the DB snapshot.
--
-- * 'dsInstanceCreateTime' - Specifies the time in Coordinated Universal Time (UTC) when the DB instance, from which the snapshot was taken, was created.
--
-- * 'dsTagList' - Undocumented member.
--
-- * 'dsEngine' - Specifies the name of the database engine.
--
-- * 'dsEncrypted' - Specifies whether the DB snapshot is encrypted.
--
-- * 'dsDBSnapshotIdentifier' - Specifies the identifier for the DB snapshot.
--
-- * 'dsProcessorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance when the DB snapshot was created.
--
-- * 'dsLicenseModel' - License model information for the restored DB instance.
--
-- * 'dsSourceDBSnapshotIdentifier' - The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was copied from. It only has value in case of cross-customer or cross-region copy.
--
-- * 'dsSnapshotType' - Provides the type of the DB snapshot.
--
-- * 'dsDBInstanceIdentifier' - Specifies the DB instance identifier of the DB instance this DB snapshot was created from.
--
-- * 'dsKMSKeyId' - If @Encrypted@ is true, the AWS KMS key identifier for the encrypted DB snapshot.
--
-- * 'dsAvailabilityZone' - Specifies the name of the Availability Zone the DB instance was located in at the time of the DB snapshot.
--
-- * 'dsSnapshotCreateTime' - Specifies when the snapshot was taken in Coordinated Universal Time (UTC).
--
-- * 'dsAllocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- * 'dsDBiResourceId' - The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
--
-- * 'dsOptionGroupName' - Provides the option group name for the DB snapshot.
--
-- * 'dsTimezone' - The time zone of the DB snapshot. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for snapshots taken from Microsoft SQL Server DB instances that were created with a time zone specified.
--
-- * 'dsTDECredentialARN' - The ARN from the key store with which to associate the instance for TDE encryption.
--
-- * 'dsPercentProgress' - The percentage of the estimated data that has been transferred.
--
-- * 'dsPort' - Specifies the port that the database engine was listening on at the time of the snapshot.
--
-- * 'dsStorageType' - Specifies the storage type associated with DB snapshot.
dbSnapshot ::
  DBSnapshot
dbSnapshot =
  DBSnapshot'
    { _dsEngineVersion = Nothing,
      _dsStatus = Nothing,
      _dsDBSnapshotARN = Nothing,
      _dsMasterUsername = Nothing,
      _dsSourceRegion = Nothing,
      _dsIAMDatabaseAuthenticationEnabled = Nothing,
      _dsIOPS = Nothing,
      _dsVPCId = Nothing,
      _dsInstanceCreateTime = Nothing,
      _dsTagList = Nothing,
      _dsEngine = Nothing,
      _dsEncrypted = Nothing,
      _dsDBSnapshotIdentifier = Nothing,
      _dsProcessorFeatures = Nothing,
      _dsLicenseModel = Nothing,
      _dsSourceDBSnapshotIdentifier = Nothing,
      _dsSnapshotType = Nothing,
      _dsDBInstanceIdentifier = Nothing,
      _dsKMSKeyId = Nothing,
      _dsAvailabilityZone = Nothing,
      _dsSnapshotCreateTime = Nothing,
      _dsAllocatedStorage = Nothing,
      _dsDBiResourceId = Nothing,
      _dsOptionGroupName = Nothing,
      _dsTimezone = Nothing,
      _dsTDECredentialARN = Nothing,
      _dsPercentProgress = Nothing,
      _dsPort = Nothing,
      _dsStorageType = Nothing
    }

-- | Specifies the version of the database engine.
dsEngineVersion :: Lens' DBSnapshot (Maybe Text)
dsEngineVersion = lens _dsEngineVersion (\s a -> s {_dsEngineVersion = a})

-- | Specifies the status of this DB snapshot.
dsStatus :: Lens' DBSnapshot (Maybe Text)
dsStatus = lens _dsStatus (\s a -> s {_dsStatus = a})

-- | The Amazon Resource Name (ARN) for the DB snapshot.
dsDBSnapshotARN :: Lens' DBSnapshot (Maybe Text)
dsDBSnapshotARN = lens _dsDBSnapshotARN (\s a -> s {_dsDBSnapshotARN = a})

-- | Provides the master username for the DB snapshot.
dsMasterUsername :: Lens' DBSnapshot (Maybe Text)
dsMasterUsername = lens _dsMasterUsername (\s a -> s {_dsMasterUsername = a})

-- | The AWS Region that the DB snapshot was created in or copied from.
dsSourceRegion :: Lens' DBSnapshot (Maybe Text)
dsSourceRegion = lens _dsSourceRegion (\s a -> s {_dsSourceRegion = a})

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
dsIAMDatabaseAuthenticationEnabled :: Lens' DBSnapshot (Maybe Bool)
dsIAMDatabaseAuthenticationEnabled = lens _dsIAMDatabaseAuthenticationEnabled (\s a -> s {_dsIAMDatabaseAuthenticationEnabled = a})

-- | Specifies the Provisioned IOPS (I/O operations per second) value of the DB instance at the time of the snapshot.
dsIOPS :: Lens' DBSnapshot (Maybe Int)
dsIOPS = lens _dsIOPS (\s a -> s {_dsIOPS = a})

-- | Provides the VPC ID associated with the DB snapshot.
dsVPCId :: Lens' DBSnapshot (Maybe Text)
dsVPCId = lens _dsVPCId (\s a -> s {_dsVPCId = a})

-- | Specifies the time in Coordinated Universal Time (UTC) when the DB instance, from which the snapshot was taken, was created.
dsInstanceCreateTime :: Lens' DBSnapshot (Maybe UTCTime)
dsInstanceCreateTime = lens _dsInstanceCreateTime (\s a -> s {_dsInstanceCreateTime = a}) . mapping _Time

-- | Undocumented member.
dsTagList :: Lens' DBSnapshot [Tag]
dsTagList = lens _dsTagList (\s a -> s {_dsTagList = a}) . _Default . _Coerce

-- | Specifies the name of the database engine.
dsEngine :: Lens' DBSnapshot (Maybe Text)
dsEngine = lens _dsEngine (\s a -> s {_dsEngine = a})

-- | Specifies whether the DB snapshot is encrypted.
dsEncrypted :: Lens' DBSnapshot (Maybe Bool)
dsEncrypted = lens _dsEncrypted (\s a -> s {_dsEncrypted = a})

-- | Specifies the identifier for the DB snapshot.
dsDBSnapshotIdentifier :: Lens' DBSnapshot (Maybe Text)
dsDBSnapshotIdentifier = lens _dsDBSnapshotIdentifier (\s a -> s {_dsDBSnapshotIdentifier = a})

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance when the DB snapshot was created.
dsProcessorFeatures :: Lens' DBSnapshot [ProcessorFeature]
dsProcessorFeatures = lens _dsProcessorFeatures (\s a -> s {_dsProcessorFeatures = a}) . _Default . _Coerce

-- | License model information for the restored DB instance.
dsLicenseModel :: Lens' DBSnapshot (Maybe Text)
dsLicenseModel = lens _dsLicenseModel (\s a -> s {_dsLicenseModel = a})

-- | The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was copied from. It only has value in case of cross-customer or cross-region copy.
dsSourceDBSnapshotIdentifier :: Lens' DBSnapshot (Maybe Text)
dsSourceDBSnapshotIdentifier = lens _dsSourceDBSnapshotIdentifier (\s a -> s {_dsSourceDBSnapshotIdentifier = a})

-- | Provides the type of the DB snapshot.
dsSnapshotType :: Lens' DBSnapshot (Maybe Text)
dsSnapshotType = lens _dsSnapshotType (\s a -> s {_dsSnapshotType = a})

-- | Specifies the DB instance identifier of the DB instance this DB snapshot was created from.
dsDBInstanceIdentifier :: Lens' DBSnapshot (Maybe Text)
dsDBInstanceIdentifier = lens _dsDBInstanceIdentifier (\s a -> s {_dsDBInstanceIdentifier = a})

-- | If @Encrypted@ is true, the AWS KMS key identifier for the encrypted DB snapshot.
dsKMSKeyId :: Lens' DBSnapshot (Maybe Text)
dsKMSKeyId = lens _dsKMSKeyId (\s a -> s {_dsKMSKeyId = a})

-- | Specifies the name of the Availability Zone the DB instance was located in at the time of the DB snapshot.
dsAvailabilityZone :: Lens' DBSnapshot (Maybe Text)
dsAvailabilityZone = lens _dsAvailabilityZone (\s a -> s {_dsAvailabilityZone = a})

-- | Specifies when the snapshot was taken in Coordinated Universal Time (UTC).
dsSnapshotCreateTime :: Lens' DBSnapshot (Maybe UTCTime)
dsSnapshotCreateTime = lens _dsSnapshotCreateTime (\s a -> s {_dsSnapshotCreateTime = a}) . mapping _Time

-- | Specifies the allocated storage size in gibibytes (GiB).
dsAllocatedStorage :: Lens' DBSnapshot (Maybe Int)
dsAllocatedStorage = lens _dsAllocatedStorage (\s a -> s {_dsAllocatedStorage = a})

-- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
dsDBiResourceId :: Lens' DBSnapshot (Maybe Text)
dsDBiResourceId = lens _dsDBiResourceId (\s a -> s {_dsDBiResourceId = a})

-- | Provides the option group name for the DB snapshot.
dsOptionGroupName :: Lens' DBSnapshot (Maybe Text)
dsOptionGroupName = lens _dsOptionGroupName (\s a -> s {_dsOptionGroupName = a})

-- | The time zone of the DB snapshot. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for snapshots taken from Microsoft SQL Server DB instances that were created with a time zone specified.
dsTimezone :: Lens' DBSnapshot (Maybe Text)
dsTimezone = lens _dsTimezone (\s a -> s {_dsTimezone = a})

-- | The ARN from the key store with which to associate the instance for TDE encryption.
dsTDECredentialARN :: Lens' DBSnapshot (Maybe Text)
dsTDECredentialARN = lens _dsTDECredentialARN (\s a -> s {_dsTDECredentialARN = a})

-- | The percentage of the estimated data that has been transferred.
dsPercentProgress :: Lens' DBSnapshot (Maybe Int)
dsPercentProgress = lens _dsPercentProgress (\s a -> s {_dsPercentProgress = a})

-- | Specifies the port that the database engine was listening on at the time of the snapshot.
dsPort :: Lens' DBSnapshot (Maybe Int)
dsPort = lens _dsPort (\s a -> s {_dsPort = a})

-- | Specifies the storage type associated with DB snapshot.
dsStorageType :: Lens' DBSnapshot (Maybe Text)
dsStorageType = lens _dsStorageType (\s a -> s {_dsStorageType = a})

instance FromXML DBSnapshot where
  parseXML x =
    DBSnapshot'
      <$> (x .@? "EngineVersion")
      <*> (x .@? "Status")
      <*> (x .@? "DBSnapshotArn")
      <*> (x .@? "MasterUsername")
      <*> (x .@? "SourceRegion")
      <*> (x .@? "IAMDatabaseAuthenticationEnabled")
      <*> (x .@? "Iops")
      <*> (x .@? "VpcId")
      <*> (x .@? "InstanceCreateTime")
      <*> (x .@? "TagList" .!@ mempty >>= may (parseXMLList "Tag"))
      <*> (x .@? "Engine")
      <*> (x .@? "Encrypted")
      <*> (x .@? "DBSnapshotIdentifier")
      <*> ( x .@? "ProcessorFeatures" .!@ mempty
              >>= may (parseXMLList "ProcessorFeature")
          )
      <*> (x .@? "LicenseModel")
      <*> (x .@? "SourceDBSnapshotIdentifier")
      <*> (x .@? "SnapshotType")
      <*> (x .@? "DBInstanceIdentifier")
      <*> (x .@? "KmsKeyId")
      <*> (x .@? "AvailabilityZone")
      <*> (x .@? "SnapshotCreateTime")
      <*> (x .@? "AllocatedStorage")
      <*> (x .@? "DbiResourceId")
      <*> (x .@? "OptionGroupName")
      <*> (x .@? "Timezone")
      <*> (x .@? "TdeCredentialArn")
      <*> (x .@? "PercentProgress")
      <*> (x .@? "Port")
      <*> (x .@? "StorageType")

instance Hashable DBSnapshot

instance NFData DBSnapshot
