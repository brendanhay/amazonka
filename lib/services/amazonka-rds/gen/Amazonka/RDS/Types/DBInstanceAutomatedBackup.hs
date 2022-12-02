{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.Types.DBInstanceAutomatedBackup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBInstanceAutomatedBackup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.DBInstanceAutomatedBackupsReplication
import Amazonka.RDS.Types.RestoreWindow

-- | An automated backup of a DB instance. It consists of system backups,
-- transaction logs, and the database instance properties that existed at
-- the time you deleted the source instance.
--
-- /See:/ 'newDBInstanceAutomatedBackup' smart constructor.
data DBInstanceAutomatedBackup = DBInstanceAutomatedBackup'
  { -- | The port number that the automated backup used for connections.
    --
    -- Default: Inherits from the source DB instance
    --
    -- Valid Values: @1150-65535@
    port :: Prelude.Maybe Prelude.Int,
    -- | The list of replications to different Amazon Web Services Regions
    -- associated with the automated backup.
    dbInstanceAutomatedBackupsReplications :: Prelude.Maybe [DBInstanceAutomatedBackupsReplication],
    -- | Specifies where automated backups are stored: Amazon Web Services
    -- Outposts or the Amazon Web Services Region.
    backupTarget :: Prelude.Maybe Prelude.Text,
    -- | Specifies the storage throughput for the automated backup.
    storageThroughput :: Prelude.Maybe Prelude.Int,
    -- | The retention period for the automated backups.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The license model of an automated backup.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The customer id of the instance that is\/was associated with the
    -- automated backup.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The option group the automated backup is associated with. If omitted,
    -- the default option group for the engine specified is used.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the automated backup. In most cases, the @Timezone@
    -- element is empty. @Timezone@ content appears only for Microsoft SQL
    -- Server DB instances that were created with a time zone specified.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the replicated automated backups.
    dbInstanceAutomatedBackupsArn :: Prelude.Maybe Prelude.Text,
    -- | Provides the date and time that the DB instance was created.
    instanceCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | Provides a list of status information for an automated backup:
    --
    -- -   @active@ - automated backups for current instances
    --
    -- -   @retained@ - automated backups for deleted instances
    --
    -- -   @creating@ - automated backups that are waiting for the first
    --     automated snapshot to be available.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone that the automated backup was created in. For
    -- information on Amazon Web Services Regions and Availability Zones, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Specifies the storage type associated with the automated backup.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region associated with the automated backup.
    region :: Prelude.Maybe Prelude.Text,
    -- | The ARN from the key store with which the automated backup is associated
    -- for TDE encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the automated backups.
    dbInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the automated backup is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services KMS key ID for an automated backup.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database engine for this automated backup.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | True if mapping of Amazon Web Services Identity and Access Management
    -- (IAM) accounts to database accounts is enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Provides the VPC ID associated with the DB instance.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the source DB instance, which can\'t be changed and
    -- which is unique to an Amazon Web Services Region.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | The IOPS (I\/O operations per second) value for the automated backup.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The version of the database engine for the automated backup.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Earliest and latest time an instance can be restored to.
    restoreWindow :: Prelude.Maybe RestoreWindow,
    -- | License model information for the automated backup.
    licenseModel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBInstanceAutomatedBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'dbInstanceAutomatedBackup_port' - The port number that the automated backup used for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
--
-- 'dbInstanceAutomatedBackupsReplications', 'dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications' - The list of replications to different Amazon Web Services Regions
-- associated with the automated backup.
--
-- 'backupTarget', 'dbInstanceAutomatedBackup_backupTarget' - Specifies where automated backups are stored: Amazon Web Services
-- Outposts or the Amazon Web Services Region.
--
-- 'storageThroughput', 'dbInstanceAutomatedBackup_storageThroughput' - Specifies the storage throughput for the automated backup.
--
-- 'backupRetentionPeriod', 'dbInstanceAutomatedBackup_backupRetentionPeriod' - The retention period for the automated backups.
--
-- 'masterUsername', 'dbInstanceAutomatedBackup_masterUsername' - The license model of an automated backup.
--
-- 'dbInstanceIdentifier', 'dbInstanceAutomatedBackup_dbInstanceIdentifier' - The customer id of the instance that is\/was associated with the
-- automated backup.
--
-- 'optionGroupName', 'dbInstanceAutomatedBackup_optionGroupName' - The option group the automated backup is associated with. If omitted,
-- the default option group for the engine specified is used.
--
-- 'timezone', 'dbInstanceAutomatedBackup_timezone' - The time zone of the automated backup. In most cases, the @Timezone@
-- element is empty. @Timezone@ content appears only for Microsoft SQL
-- Server DB instances that were created with a time zone specified.
--
-- 'dbInstanceAutomatedBackupsArn', 'dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn' - The Amazon Resource Name (ARN) for the replicated automated backups.
--
-- 'instanceCreateTime', 'dbInstanceAutomatedBackup_instanceCreateTime' - Provides the date and time that the DB instance was created.
--
-- 'status', 'dbInstanceAutomatedBackup_status' - Provides a list of status information for an automated backup:
--
-- -   @active@ - automated backups for current instances
--
-- -   @retained@ - automated backups for deleted instances
--
-- -   @creating@ - automated backups that are waiting for the first
--     automated snapshot to be available.
--
-- 'availabilityZone', 'dbInstanceAutomatedBackup_availabilityZone' - The Availability Zone that the automated backup was created in. For
-- information on Amazon Web Services Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
--
-- 'storageType', 'dbInstanceAutomatedBackup_storageType' - Specifies the storage type associated with the automated backup.
--
-- 'region', 'dbInstanceAutomatedBackup_region' - The Amazon Web Services Region associated with the automated backup.
--
-- 'tdeCredentialArn', 'dbInstanceAutomatedBackup_tdeCredentialArn' - The ARN from the key store with which the automated backup is associated
-- for TDE encryption.
--
-- 'dbInstanceArn', 'dbInstanceAutomatedBackup_dbInstanceArn' - The Amazon Resource Name (ARN) for the automated backups.
--
-- 'encrypted', 'dbInstanceAutomatedBackup_encrypted' - Specifies whether the automated backup is encrypted.
--
-- 'kmsKeyId', 'dbInstanceAutomatedBackup_kmsKeyId' - The Amazon Web Services KMS key ID for an automated backup.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- 'engine', 'dbInstanceAutomatedBackup_engine' - The name of the database engine for this automated backup.
--
-- 'allocatedStorage', 'dbInstanceAutomatedBackup_allocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled' - True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- 'vpcId', 'dbInstanceAutomatedBackup_vpcId' - Provides the VPC ID associated with the DB instance.
--
-- 'dbiResourceId', 'dbInstanceAutomatedBackup_dbiResourceId' - The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an Amazon Web Services Region.
--
-- 'iops', 'dbInstanceAutomatedBackup_iops' - The IOPS (I\/O operations per second) value for the automated backup.
--
-- 'engineVersion', 'dbInstanceAutomatedBackup_engineVersion' - The version of the database engine for the automated backup.
--
-- 'restoreWindow', 'dbInstanceAutomatedBackup_restoreWindow' - Earliest and latest time an instance can be restored to.
--
-- 'licenseModel', 'dbInstanceAutomatedBackup_licenseModel' - License model information for the automated backup.
newDBInstanceAutomatedBackup ::
  DBInstanceAutomatedBackup
newDBInstanceAutomatedBackup =
  DBInstanceAutomatedBackup'
    { port = Prelude.Nothing,
      dbInstanceAutomatedBackupsReplications =
        Prelude.Nothing,
      backupTarget = Prelude.Nothing,
      storageThroughput = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      timezone = Prelude.Nothing,
      dbInstanceAutomatedBackupsArn = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      status = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      storageType = Prelude.Nothing,
      region = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      dbInstanceArn = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      engine = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      iops = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      restoreWindow = Prelude.Nothing,
      licenseModel = Prelude.Nothing
    }

-- | The port number that the automated backup used for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
dbInstanceAutomatedBackup_port :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Int)
dbInstanceAutomatedBackup_port = Lens.lens (\DBInstanceAutomatedBackup' {port} -> port) (\s@DBInstanceAutomatedBackup' {} a -> s {port = a} :: DBInstanceAutomatedBackup)

-- | The list of replications to different Amazon Web Services Regions
-- associated with the automated backup.
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe [DBInstanceAutomatedBackupsReplication])
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceAutomatedBackupsReplications} -> dbInstanceAutomatedBackupsReplications) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceAutomatedBackupsReplications = a} :: DBInstanceAutomatedBackup) Prelude.. Lens.mapping Lens.coerced

-- | Specifies where automated backups are stored: Amazon Web Services
-- Outposts or the Amazon Web Services Region.
dbInstanceAutomatedBackup_backupTarget :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_backupTarget = Lens.lens (\DBInstanceAutomatedBackup' {backupTarget} -> backupTarget) (\s@DBInstanceAutomatedBackup' {} a -> s {backupTarget = a} :: DBInstanceAutomatedBackup)

-- | Specifies the storage throughput for the automated backup.
dbInstanceAutomatedBackup_storageThroughput :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Int)
dbInstanceAutomatedBackup_storageThroughput = Lens.lens (\DBInstanceAutomatedBackup' {storageThroughput} -> storageThroughput) (\s@DBInstanceAutomatedBackup' {} a -> s {storageThroughput = a} :: DBInstanceAutomatedBackup)

-- | The retention period for the automated backups.
dbInstanceAutomatedBackup_backupRetentionPeriod :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Int)
dbInstanceAutomatedBackup_backupRetentionPeriod = Lens.lens (\DBInstanceAutomatedBackup' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBInstanceAutomatedBackup' {} a -> s {backupRetentionPeriod = a} :: DBInstanceAutomatedBackup)

-- | The license model of an automated backup.
dbInstanceAutomatedBackup_masterUsername :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_masterUsername = Lens.lens (\DBInstanceAutomatedBackup' {masterUsername} -> masterUsername) (\s@DBInstanceAutomatedBackup' {} a -> s {masterUsername = a} :: DBInstanceAutomatedBackup)

-- | The customer id of the instance that is\/was associated with the
-- automated backup.
dbInstanceAutomatedBackup_dbInstanceIdentifier :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_dbInstanceIdentifier = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceIdentifier = a} :: DBInstanceAutomatedBackup)

-- | The option group the automated backup is associated with. If omitted,
-- the default option group for the engine specified is used.
dbInstanceAutomatedBackup_optionGroupName :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_optionGroupName = Lens.lens (\DBInstanceAutomatedBackup' {optionGroupName} -> optionGroupName) (\s@DBInstanceAutomatedBackup' {} a -> s {optionGroupName = a} :: DBInstanceAutomatedBackup)

-- | The time zone of the automated backup. In most cases, the @Timezone@
-- element is empty. @Timezone@ content appears only for Microsoft SQL
-- Server DB instances that were created with a time zone specified.
dbInstanceAutomatedBackup_timezone :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_timezone = Lens.lens (\DBInstanceAutomatedBackup' {timezone} -> timezone) (\s@DBInstanceAutomatedBackup' {} a -> s {timezone = a} :: DBInstanceAutomatedBackup)

-- | The Amazon Resource Name (ARN) for the replicated automated backups.
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceAutomatedBackupsArn} -> dbInstanceAutomatedBackupsArn) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceAutomatedBackupsArn = a} :: DBInstanceAutomatedBackup)

-- | Provides the date and time that the DB instance was created.
dbInstanceAutomatedBackup_instanceCreateTime :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.UTCTime)
dbInstanceAutomatedBackup_instanceCreateTime = Lens.lens (\DBInstanceAutomatedBackup' {instanceCreateTime} -> instanceCreateTime) (\s@DBInstanceAutomatedBackup' {} a -> s {instanceCreateTime = a} :: DBInstanceAutomatedBackup) Prelude.. Lens.mapping Data._Time

-- | Provides a list of status information for an automated backup:
--
-- -   @active@ - automated backups for current instances
--
-- -   @retained@ - automated backups for deleted instances
--
-- -   @creating@ - automated backups that are waiting for the first
--     automated snapshot to be available.
dbInstanceAutomatedBackup_status :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_status = Lens.lens (\DBInstanceAutomatedBackup' {status} -> status) (\s@DBInstanceAutomatedBackup' {} a -> s {status = a} :: DBInstanceAutomatedBackup)

-- | The Availability Zone that the automated backup was created in. For
-- information on Amazon Web Services Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
dbInstanceAutomatedBackup_availabilityZone :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_availabilityZone = Lens.lens (\DBInstanceAutomatedBackup' {availabilityZone} -> availabilityZone) (\s@DBInstanceAutomatedBackup' {} a -> s {availabilityZone = a} :: DBInstanceAutomatedBackup)

-- | Specifies the storage type associated with the automated backup.
dbInstanceAutomatedBackup_storageType :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_storageType = Lens.lens (\DBInstanceAutomatedBackup' {storageType} -> storageType) (\s@DBInstanceAutomatedBackup' {} a -> s {storageType = a} :: DBInstanceAutomatedBackup)

-- | The Amazon Web Services Region associated with the automated backup.
dbInstanceAutomatedBackup_region :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_region = Lens.lens (\DBInstanceAutomatedBackup' {region} -> region) (\s@DBInstanceAutomatedBackup' {} a -> s {region = a} :: DBInstanceAutomatedBackup)

-- | The ARN from the key store with which the automated backup is associated
-- for TDE encryption.
dbInstanceAutomatedBackup_tdeCredentialArn :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_tdeCredentialArn = Lens.lens (\DBInstanceAutomatedBackup' {tdeCredentialArn} -> tdeCredentialArn) (\s@DBInstanceAutomatedBackup' {} a -> s {tdeCredentialArn = a} :: DBInstanceAutomatedBackup)

-- | The Amazon Resource Name (ARN) for the automated backups.
dbInstanceAutomatedBackup_dbInstanceArn :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_dbInstanceArn = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceArn} -> dbInstanceArn) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceArn = a} :: DBInstanceAutomatedBackup)

-- | Specifies whether the automated backup is encrypted.
dbInstanceAutomatedBackup_encrypted :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Bool)
dbInstanceAutomatedBackup_encrypted = Lens.lens (\DBInstanceAutomatedBackup' {encrypted} -> encrypted) (\s@DBInstanceAutomatedBackup' {} a -> s {encrypted = a} :: DBInstanceAutomatedBackup)

-- | The Amazon Web Services KMS key ID for an automated backup.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
dbInstanceAutomatedBackup_kmsKeyId :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_kmsKeyId = Lens.lens (\DBInstanceAutomatedBackup' {kmsKeyId} -> kmsKeyId) (\s@DBInstanceAutomatedBackup' {} a -> s {kmsKeyId = a} :: DBInstanceAutomatedBackup)

-- | The name of the database engine for this automated backup.
dbInstanceAutomatedBackup_engine :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_engine = Lens.lens (\DBInstanceAutomatedBackup' {engine} -> engine) (\s@DBInstanceAutomatedBackup' {} a -> s {engine = a} :: DBInstanceAutomatedBackup)

-- | Specifies the allocated storage size in gibibytes (GiB).
dbInstanceAutomatedBackup_allocatedStorage :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Int)
dbInstanceAutomatedBackup_allocatedStorage = Lens.lens (\DBInstanceAutomatedBackup' {allocatedStorage} -> allocatedStorage) (\s@DBInstanceAutomatedBackup' {} a -> s {allocatedStorage = a} :: DBInstanceAutomatedBackup)

-- | True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Bool)
dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBInstanceAutomatedBackup' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBInstanceAutomatedBackup' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBInstanceAutomatedBackup)

-- | Provides the VPC ID associated with the DB instance.
dbInstanceAutomatedBackup_vpcId :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_vpcId = Lens.lens (\DBInstanceAutomatedBackup' {vpcId} -> vpcId) (\s@DBInstanceAutomatedBackup' {} a -> s {vpcId = a} :: DBInstanceAutomatedBackup)

-- | The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an Amazon Web Services Region.
dbInstanceAutomatedBackup_dbiResourceId :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_dbiResourceId = Lens.lens (\DBInstanceAutomatedBackup' {dbiResourceId} -> dbiResourceId) (\s@DBInstanceAutomatedBackup' {} a -> s {dbiResourceId = a} :: DBInstanceAutomatedBackup)

-- | The IOPS (I\/O operations per second) value for the automated backup.
dbInstanceAutomatedBackup_iops :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Int)
dbInstanceAutomatedBackup_iops = Lens.lens (\DBInstanceAutomatedBackup' {iops} -> iops) (\s@DBInstanceAutomatedBackup' {} a -> s {iops = a} :: DBInstanceAutomatedBackup)

-- | The version of the database engine for the automated backup.
dbInstanceAutomatedBackup_engineVersion :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_engineVersion = Lens.lens (\DBInstanceAutomatedBackup' {engineVersion} -> engineVersion) (\s@DBInstanceAutomatedBackup' {} a -> s {engineVersion = a} :: DBInstanceAutomatedBackup)

-- | Earliest and latest time an instance can be restored to.
dbInstanceAutomatedBackup_restoreWindow :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe RestoreWindow)
dbInstanceAutomatedBackup_restoreWindow = Lens.lens (\DBInstanceAutomatedBackup' {restoreWindow} -> restoreWindow) (\s@DBInstanceAutomatedBackup' {} a -> s {restoreWindow = a} :: DBInstanceAutomatedBackup)

-- | License model information for the automated backup.
dbInstanceAutomatedBackup_licenseModel :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_licenseModel = Lens.lens (\DBInstanceAutomatedBackup' {licenseModel} -> licenseModel) (\s@DBInstanceAutomatedBackup' {} a -> s {licenseModel = a} :: DBInstanceAutomatedBackup)

instance Data.FromXML DBInstanceAutomatedBackup where
  parseXML x =
    DBInstanceAutomatedBackup'
      Prelude.<$> (x Data..@? "Port")
      Prelude.<*> ( x Data..@? "DBInstanceAutomatedBackupsReplications"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Data.parseXMLList
                            "DBInstanceAutomatedBackupsReplication"
                        )
                  )
      Prelude.<*> (x Data..@? "BackupTarget")
      Prelude.<*> (x Data..@? "StorageThroughput")
      Prelude.<*> (x Data..@? "BackupRetentionPeriod")
      Prelude.<*> (x Data..@? "MasterUsername")
      Prelude.<*> (x Data..@? "DBInstanceIdentifier")
      Prelude.<*> (x Data..@? "OptionGroupName")
      Prelude.<*> (x Data..@? "Timezone")
      Prelude.<*> (x Data..@? "DBInstanceAutomatedBackupsArn")
      Prelude.<*> (x Data..@? "InstanceCreateTime")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "AvailabilityZone")
      Prelude.<*> (x Data..@? "StorageType")
      Prelude.<*> (x Data..@? "Region")
      Prelude.<*> (x Data..@? "TdeCredentialArn")
      Prelude.<*> (x Data..@? "DBInstanceArn")
      Prelude.<*> (x Data..@? "Encrypted")
      Prelude.<*> (x Data..@? "KmsKeyId")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "AllocatedStorage")
      Prelude.<*> (x Data..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Data..@? "VpcId")
      Prelude.<*> (x Data..@? "DbiResourceId")
      Prelude.<*> (x Data..@? "Iops")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "RestoreWindow")
      Prelude.<*> (x Data..@? "LicenseModel")

instance Prelude.Hashable DBInstanceAutomatedBackup where
  hashWithSalt _salt DBInstanceAutomatedBackup' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` dbInstanceAutomatedBackupsReplications
      `Prelude.hashWithSalt` backupTarget
      `Prelude.hashWithSalt` storageThroughput
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` dbInstanceAutomatedBackupsArn
      `Prelude.hashWithSalt` instanceCreateTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` tdeCredentialArn
      `Prelude.hashWithSalt` dbInstanceArn
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` iAMDatabaseAuthenticationEnabled
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` dbiResourceId
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` restoreWindow
      `Prelude.hashWithSalt` licenseModel

instance Prelude.NFData DBInstanceAutomatedBackup where
  rnf DBInstanceAutomatedBackup' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf dbInstanceAutomatedBackupsReplications
      `Prelude.seq` Prelude.rnf backupTarget
      `Prelude.seq` Prelude.rnf storageThroughput
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf dbInstanceAutomatedBackupsArn
      `Prelude.seq` Prelude.rnf instanceCreateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf tdeCredentialArn
      `Prelude.seq` Prelude.rnf dbInstanceArn
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf
        iAMDatabaseAuthenticationEnabled
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf
        dbiResourceId
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        restoreWindow
      `Prelude.seq` Prelude.rnf
        licenseModel
