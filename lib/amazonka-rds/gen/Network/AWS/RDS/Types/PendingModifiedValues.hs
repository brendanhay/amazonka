{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.PendingModifiedValues where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.PendingCloudwatchLogsExports
import Network.AWS.RDS.Types.ProcessorFeature

-- | This data type is used as a response element in the @ModifyDBInstance@ action.
--
--
--
-- /See:/ 'pendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { _pmvEngineVersion ::
      !(Maybe Text),
    _pmvMasterUserPassword :: !(Maybe Text),
    _pmvDBSubnetGroupName :: !(Maybe Text),
    _pmvIOPS :: !(Maybe Int),
    _pmvProcessorFeatures ::
      !(Maybe [ProcessorFeature]),
    _pmvDBInstanceClass :: !(Maybe Text),
    _pmvLicenseModel :: !(Maybe Text),
    _pmvCACertificateIdentifier :: !(Maybe Text),
    _pmvDBInstanceIdentifier :: !(Maybe Text),
    _pmvPendingCloudwatchLogsExports ::
      !(Maybe PendingCloudwatchLogsExports),
    _pmvBackupRetentionPeriod :: !(Maybe Int),
    _pmvMultiAZ :: !(Maybe Bool),
    _pmvAllocatedStorage :: !(Maybe Int),
    _pmvPort :: !(Maybe Int),
    _pmvStorageType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmvEngineVersion' - Indicates the database engine version.
--
-- * 'pmvMasterUserPassword' - Contains the pending or currently-in-progress change of the master credentials for the DB instance.
--
-- * 'pmvDBSubnetGroupName' - The new DB subnet group for the DB instance.
--
-- * 'pmvIOPS' - Specifies the new Provisioned IOPS value for the DB instance that will be applied or is currently being applied.
--
-- * 'pmvProcessorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- * 'pmvDBInstanceClass' - Contains the new @DBInstanceClass@ for the DB instance that will be applied or is currently being applied.
--
-- * 'pmvLicenseModel' - The license model for the DB instance. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- * 'pmvCACertificateIdentifier' - Specifies the identifier of the CA certificate for the DB instance.
--
-- * 'pmvDBInstanceIdentifier' - Contains the new @DBInstanceIdentifier@ for the DB instance that will be applied or is currently being applied.
--
-- * 'pmvPendingCloudwatchLogsExports' - Undocumented member.
--
-- * 'pmvBackupRetentionPeriod' - Specifies the pending number of days for which automated backups are retained.
--
-- * 'pmvMultiAZ' - Indicates that the Single-AZ DB instance is to change to a Multi-AZ deployment.
--
-- * 'pmvAllocatedStorage' - Contains the new @AllocatedStorage@ size for the DB instance that will be applied or is currently being applied.
--
-- * 'pmvPort' - Specifies the pending port for the DB instance.
--
-- * 'pmvStorageType' - Specifies the storage type to be associated with the DB instance.
pendingModifiedValues ::
  PendingModifiedValues
pendingModifiedValues =
  PendingModifiedValues'
    { _pmvEngineVersion = Nothing,
      _pmvMasterUserPassword = Nothing,
      _pmvDBSubnetGroupName = Nothing,
      _pmvIOPS = Nothing,
      _pmvProcessorFeatures = Nothing,
      _pmvDBInstanceClass = Nothing,
      _pmvLicenseModel = Nothing,
      _pmvCACertificateIdentifier = Nothing,
      _pmvDBInstanceIdentifier = Nothing,
      _pmvPendingCloudwatchLogsExports = Nothing,
      _pmvBackupRetentionPeriod = Nothing,
      _pmvMultiAZ = Nothing,
      _pmvAllocatedStorage = Nothing,
      _pmvPort = Nothing,
      _pmvStorageType = Nothing
    }

-- | Indicates the database engine version.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion = lens _pmvEngineVersion (\s a -> s {_pmvEngineVersion = a})

-- | Contains the pending or currently-in-progress change of the master credentials for the DB instance.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword = lens _pmvMasterUserPassword (\s a -> s {_pmvMasterUserPassword = a})

-- | The new DB subnet group for the DB instance.
pmvDBSubnetGroupName :: Lens' PendingModifiedValues (Maybe Text)
pmvDBSubnetGroupName = lens _pmvDBSubnetGroupName (\s a -> s {_pmvDBSubnetGroupName = a})

-- | Specifies the new Provisioned IOPS value for the DB instance that will be applied or is currently being applied.
pmvIOPS :: Lens' PendingModifiedValues (Maybe Int)
pmvIOPS = lens _pmvIOPS (\s a -> s {_pmvIOPS = a})

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
pmvProcessorFeatures :: Lens' PendingModifiedValues [ProcessorFeature]
pmvProcessorFeatures = lens _pmvProcessorFeatures (\s a -> s {_pmvProcessorFeatures = a}) . _Default . _Coerce

-- | Contains the new @DBInstanceClass@ for the DB instance that will be applied or is currently being applied.
pmvDBInstanceClass :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceClass = lens _pmvDBInstanceClass (\s a -> s {_pmvDBInstanceClass = a})

-- | The license model for the DB instance. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
pmvLicenseModel :: Lens' PendingModifiedValues (Maybe Text)
pmvLicenseModel = lens _pmvLicenseModel (\s a -> s {_pmvLicenseModel = a})

-- | Specifies the identifier of the CA certificate for the DB instance.
pmvCACertificateIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvCACertificateIdentifier = lens _pmvCACertificateIdentifier (\s a -> s {_pmvCACertificateIdentifier = a})

-- | Contains the new @DBInstanceIdentifier@ for the DB instance that will be applied or is currently being applied.
pmvDBInstanceIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceIdentifier = lens _pmvDBInstanceIdentifier (\s a -> s {_pmvDBInstanceIdentifier = a})

-- | Undocumented member.
pmvPendingCloudwatchLogsExports :: Lens' PendingModifiedValues (Maybe PendingCloudwatchLogsExports)
pmvPendingCloudwatchLogsExports = lens _pmvPendingCloudwatchLogsExports (\s a -> s {_pmvPendingCloudwatchLogsExports = a})

-- | Specifies the pending number of days for which automated backups are retained.
pmvBackupRetentionPeriod :: Lens' PendingModifiedValues (Maybe Int)
pmvBackupRetentionPeriod = lens _pmvBackupRetentionPeriod (\s a -> s {_pmvBackupRetentionPeriod = a})

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ deployment.
pmvMultiAZ :: Lens' PendingModifiedValues (Maybe Bool)
pmvMultiAZ = lens _pmvMultiAZ (\s a -> s {_pmvMultiAZ = a})

-- | Contains the new @AllocatedStorage@ size for the DB instance that will be applied or is currently being applied.
pmvAllocatedStorage :: Lens' PendingModifiedValues (Maybe Int)
pmvAllocatedStorage = lens _pmvAllocatedStorage (\s a -> s {_pmvAllocatedStorage = a})

-- | Specifies the pending port for the DB instance.
pmvPort :: Lens' PendingModifiedValues (Maybe Int)
pmvPort = lens _pmvPort (\s a -> s {_pmvPort = a})

-- | Specifies the storage type to be associated with the DB instance.
pmvStorageType :: Lens' PendingModifiedValues (Maybe Text)
pmvStorageType = lens _pmvStorageType (\s a -> s {_pmvStorageType = a})

instance FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      <$> (x .@? "EngineVersion")
      <*> (x .@? "MasterUserPassword")
      <*> (x .@? "DBSubnetGroupName")
      <*> (x .@? "Iops")
      <*> ( x .@? "ProcessorFeatures" .!@ mempty
              >>= may (parseXMLList "ProcessorFeature")
          )
      <*> (x .@? "DBInstanceClass")
      <*> (x .@? "LicenseModel")
      <*> (x .@? "CACertificateIdentifier")
      <*> (x .@? "DBInstanceIdentifier")
      <*> (x .@? "PendingCloudwatchLogsExports")
      <*> (x .@? "BackupRetentionPeriod")
      <*> (x .@? "MultiAZ")
      <*> (x .@? "AllocatedStorage")
      <*> (x .@? "Port")
      <*> (x .@? "StorageType")

instance Hashable PendingModifiedValues

instance NFData PendingModifiedValues
