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
-- Module      : Network.AWS.RDS.Types.DBInstanceAutomatedBackup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBInstanceAutomatedBackup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.DBInstanceAutomatedBackupsReplication
import Network.AWS.RDS.Types.RestoreWindow

-- | An automated backup of a DB instance. It consists of system backups,
-- transaction logs, and the database instance properties that existed at
-- the time you deleted the source instance.
--
-- /See:/ 'newDBInstanceAutomatedBackup' smart constructor.
data DBInstanceAutomatedBackup = DBInstanceAutomatedBackup'
  { -- | The retention period for the automated backups.
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | Provides a list of status information for an automated backup:
    --
    -- -   @active@ - automated backups for current instances
    --
    -- -   @retained@ - automated backups for deleted instances
    --
    -- -   @creating@ - automated backups that are waiting for the first
    --     automated snapshot to be available.
    status :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the replicated automated backups.
    dbInstanceAutomatedBackupsArn :: Core.Maybe Core.Text,
    -- | Specifies the storage type associated with the automated backup.
    storageType :: Core.Maybe Core.Text,
    -- | Specifies whether the automated backup is encrypted.
    encrypted :: Core.Maybe Core.Bool,
    -- | The option group the automated backup is associated with. If omitted,
    -- the default option group for the engine specified is used.
    optionGroupName :: Core.Maybe Core.Text,
    -- | Provides the date and time that the DB instance was created.
    instanceCreateTime :: Core.Maybe Core.ISO8601,
    -- | The identifier for the source DB instance, which can\'t be changed and
    -- which is unique to an AWS Region.
    dbiResourceId :: Core.Maybe Core.Text,
    -- | The license model of an automated backup.
    masterUsername :: Core.Maybe Core.Text,
    -- | The AWS KMS key ID for an automated backup.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The Availability Zone that the automated backup was created in. For
    -- information on AWS Regions and Availability Zones, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The version of the database engine for the automated backup.
    engineVersion :: Core.Maybe Core.Text,
    -- | Earliest and latest time an instance can be restored to.
    restoreWindow :: Core.Maybe RestoreWindow,
    -- | License model information for the automated backup.
    licenseModel :: Core.Maybe Core.Text,
    -- | The customer id of the instance that is\/was associated with the
    -- automated backup.
    dbInstanceIdentifier :: Core.Maybe Core.Text,
    -- | The port number that the automated backup used for connections.
    --
    -- Default: Inherits from the source DB instance
    --
    -- Valid Values: @1150-65535@
    port :: Core.Maybe Core.Int,
    -- | The name of the database engine for this automated backup.
    engine :: Core.Maybe Core.Text,
    -- | The ARN from the key store with which the automated backup is associated
    -- for TDE encryption.
    tdeCredentialArn :: Core.Maybe Core.Text,
    -- | The time zone of the automated backup. In most cases, the @Timezone@
    -- element is empty. @Timezone@ content appears only for Microsoft SQL
    -- Server DB instances that were created with a time zone specified.
    timezone :: Core.Maybe Core.Text,
    -- | The list of replications to different AWS Regions associated with the
    -- automated backup.
    dbInstanceAutomatedBackupsReplications :: Core.Maybe [DBInstanceAutomatedBackupsReplication],
    -- | The AWS Region associated with the automated backup.
    region :: Core.Maybe Core.Text,
    -- | Provides the VPC ID associated with the DB instance
    vpcId :: Core.Maybe Core.Text,
    -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Core.Maybe Core.Int,
    -- | True if mapping of AWS Identity and Access Management (IAM) accounts to
    -- database accounts is enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool,
    -- | The IOPS (I\/O operations per second) value for the automated backup.
    iops :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) for the automated backups.
    dbInstanceArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBInstanceAutomatedBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupRetentionPeriod', 'dbInstanceAutomatedBackup_backupRetentionPeriod' - The retention period for the automated backups.
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
-- 'dbInstanceAutomatedBackupsArn', 'dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn' - The Amazon Resource Name (ARN) for the replicated automated backups.
--
-- 'storageType', 'dbInstanceAutomatedBackup_storageType' - Specifies the storage type associated with the automated backup.
--
-- 'encrypted', 'dbInstanceAutomatedBackup_encrypted' - Specifies whether the automated backup is encrypted.
--
-- 'optionGroupName', 'dbInstanceAutomatedBackup_optionGroupName' - The option group the automated backup is associated with. If omitted,
-- the default option group for the engine specified is used.
--
-- 'instanceCreateTime', 'dbInstanceAutomatedBackup_instanceCreateTime' - Provides the date and time that the DB instance was created.
--
-- 'dbiResourceId', 'dbInstanceAutomatedBackup_dbiResourceId' - The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an AWS Region.
--
-- 'masterUsername', 'dbInstanceAutomatedBackup_masterUsername' - The license model of an automated backup.
--
-- 'kmsKeyId', 'dbInstanceAutomatedBackup_kmsKeyId' - The AWS KMS key ID for an automated backup.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- 'availabilityZone', 'dbInstanceAutomatedBackup_availabilityZone' - The Availability Zone that the automated backup was created in. For
-- information on AWS Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
--
-- 'engineVersion', 'dbInstanceAutomatedBackup_engineVersion' - The version of the database engine for the automated backup.
--
-- 'restoreWindow', 'dbInstanceAutomatedBackup_restoreWindow' - Earliest and latest time an instance can be restored to.
--
-- 'licenseModel', 'dbInstanceAutomatedBackup_licenseModel' - License model information for the automated backup.
--
-- 'dbInstanceIdentifier', 'dbInstanceAutomatedBackup_dbInstanceIdentifier' - The customer id of the instance that is\/was associated with the
-- automated backup.
--
-- 'port', 'dbInstanceAutomatedBackup_port' - The port number that the automated backup used for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
--
-- 'engine', 'dbInstanceAutomatedBackup_engine' - The name of the database engine for this automated backup.
--
-- 'tdeCredentialArn', 'dbInstanceAutomatedBackup_tdeCredentialArn' - The ARN from the key store with which the automated backup is associated
-- for TDE encryption.
--
-- 'timezone', 'dbInstanceAutomatedBackup_timezone' - The time zone of the automated backup. In most cases, the @Timezone@
-- element is empty. @Timezone@ content appears only for Microsoft SQL
-- Server DB instances that were created with a time zone specified.
--
-- 'dbInstanceAutomatedBackupsReplications', 'dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications' - The list of replications to different AWS Regions associated with the
-- automated backup.
--
-- 'region', 'dbInstanceAutomatedBackup_region' - The AWS Region associated with the automated backup.
--
-- 'vpcId', 'dbInstanceAutomatedBackup_vpcId' - Provides the VPC ID associated with the DB instance
--
-- 'allocatedStorage', 'dbInstanceAutomatedBackup_allocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to
-- database accounts is enabled, and otherwise false.
--
-- 'iops', 'dbInstanceAutomatedBackup_iops' - The IOPS (I\/O operations per second) value for the automated backup.
--
-- 'dbInstanceArn', 'dbInstanceAutomatedBackup_dbInstanceArn' - The Amazon Resource Name (ARN) for the automated backups.
newDBInstanceAutomatedBackup ::
  DBInstanceAutomatedBackup
newDBInstanceAutomatedBackup =
  DBInstanceAutomatedBackup'
    { backupRetentionPeriod =
        Core.Nothing,
      status = Core.Nothing,
      dbInstanceAutomatedBackupsArn = Core.Nothing,
      storageType = Core.Nothing,
      encrypted = Core.Nothing,
      optionGroupName = Core.Nothing,
      instanceCreateTime = Core.Nothing,
      dbiResourceId = Core.Nothing,
      masterUsername = Core.Nothing,
      kmsKeyId = Core.Nothing,
      availabilityZone = Core.Nothing,
      engineVersion = Core.Nothing,
      restoreWindow = Core.Nothing,
      licenseModel = Core.Nothing,
      dbInstanceIdentifier = Core.Nothing,
      port = Core.Nothing,
      engine = Core.Nothing,
      tdeCredentialArn = Core.Nothing,
      timezone = Core.Nothing,
      dbInstanceAutomatedBackupsReplications =
        Core.Nothing,
      region = Core.Nothing,
      vpcId = Core.Nothing,
      allocatedStorage = Core.Nothing,
      iAMDatabaseAuthenticationEnabled = Core.Nothing,
      iops = Core.Nothing,
      dbInstanceArn = Core.Nothing
    }

-- | The retention period for the automated backups.
dbInstanceAutomatedBackup_backupRetentionPeriod :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Int)
dbInstanceAutomatedBackup_backupRetentionPeriod = Lens.lens (\DBInstanceAutomatedBackup' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBInstanceAutomatedBackup' {} a -> s {backupRetentionPeriod = a} :: DBInstanceAutomatedBackup)

-- | Provides a list of status information for an automated backup:
--
-- -   @active@ - automated backups for current instances
--
-- -   @retained@ - automated backups for deleted instances
--
-- -   @creating@ - automated backups that are waiting for the first
--     automated snapshot to be available.
dbInstanceAutomatedBackup_status :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_status = Lens.lens (\DBInstanceAutomatedBackup' {status} -> status) (\s@DBInstanceAutomatedBackup' {} a -> s {status = a} :: DBInstanceAutomatedBackup)

-- | The Amazon Resource Name (ARN) for the replicated automated backups.
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceAutomatedBackupsArn} -> dbInstanceAutomatedBackupsArn) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceAutomatedBackupsArn = a} :: DBInstanceAutomatedBackup)

-- | Specifies the storage type associated with the automated backup.
dbInstanceAutomatedBackup_storageType :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_storageType = Lens.lens (\DBInstanceAutomatedBackup' {storageType} -> storageType) (\s@DBInstanceAutomatedBackup' {} a -> s {storageType = a} :: DBInstanceAutomatedBackup)

-- | Specifies whether the automated backup is encrypted.
dbInstanceAutomatedBackup_encrypted :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Bool)
dbInstanceAutomatedBackup_encrypted = Lens.lens (\DBInstanceAutomatedBackup' {encrypted} -> encrypted) (\s@DBInstanceAutomatedBackup' {} a -> s {encrypted = a} :: DBInstanceAutomatedBackup)

-- | The option group the automated backup is associated with. If omitted,
-- the default option group for the engine specified is used.
dbInstanceAutomatedBackup_optionGroupName :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_optionGroupName = Lens.lens (\DBInstanceAutomatedBackup' {optionGroupName} -> optionGroupName) (\s@DBInstanceAutomatedBackup' {} a -> s {optionGroupName = a} :: DBInstanceAutomatedBackup)

-- | Provides the date and time that the DB instance was created.
dbInstanceAutomatedBackup_instanceCreateTime :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.UTCTime)
dbInstanceAutomatedBackup_instanceCreateTime = Lens.lens (\DBInstanceAutomatedBackup' {instanceCreateTime} -> instanceCreateTime) (\s@DBInstanceAutomatedBackup' {} a -> s {instanceCreateTime = a} :: DBInstanceAutomatedBackup) Core.. Lens.mapping Core._Time

-- | The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an AWS Region.
dbInstanceAutomatedBackup_dbiResourceId :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_dbiResourceId = Lens.lens (\DBInstanceAutomatedBackup' {dbiResourceId} -> dbiResourceId) (\s@DBInstanceAutomatedBackup' {} a -> s {dbiResourceId = a} :: DBInstanceAutomatedBackup)

-- | The license model of an automated backup.
dbInstanceAutomatedBackup_masterUsername :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_masterUsername = Lens.lens (\DBInstanceAutomatedBackup' {masterUsername} -> masterUsername) (\s@DBInstanceAutomatedBackup' {} a -> s {masterUsername = a} :: DBInstanceAutomatedBackup)

-- | The AWS KMS key ID for an automated backup.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
dbInstanceAutomatedBackup_kmsKeyId :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_kmsKeyId = Lens.lens (\DBInstanceAutomatedBackup' {kmsKeyId} -> kmsKeyId) (\s@DBInstanceAutomatedBackup' {} a -> s {kmsKeyId = a} :: DBInstanceAutomatedBackup)

-- | The Availability Zone that the automated backup was created in. For
-- information on AWS Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
dbInstanceAutomatedBackup_availabilityZone :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_availabilityZone = Lens.lens (\DBInstanceAutomatedBackup' {availabilityZone} -> availabilityZone) (\s@DBInstanceAutomatedBackup' {} a -> s {availabilityZone = a} :: DBInstanceAutomatedBackup)

-- | The version of the database engine for the automated backup.
dbInstanceAutomatedBackup_engineVersion :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_engineVersion = Lens.lens (\DBInstanceAutomatedBackup' {engineVersion} -> engineVersion) (\s@DBInstanceAutomatedBackup' {} a -> s {engineVersion = a} :: DBInstanceAutomatedBackup)

-- | Earliest and latest time an instance can be restored to.
dbInstanceAutomatedBackup_restoreWindow :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe RestoreWindow)
dbInstanceAutomatedBackup_restoreWindow = Lens.lens (\DBInstanceAutomatedBackup' {restoreWindow} -> restoreWindow) (\s@DBInstanceAutomatedBackup' {} a -> s {restoreWindow = a} :: DBInstanceAutomatedBackup)

-- | License model information for the automated backup.
dbInstanceAutomatedBackup_licenseModel :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_licenseModel = Lens.lens (\DBInstanceAutomatedBackup' {licenseModel} -> licenseModel) (\s@DBInstanceAutomatedBackup' {} a -> s {licenseModel = a} :: DBInstanceAutomatedBackup)

-- | The customer id of the instance that is\/was associated with the
-- automated backup.
dbInstanceAutomatedBackup_dbInstanceIdentifier :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_dbInstanceIdentifier = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceIdentifier = a} :: DBInstanceAutomatedBackup)

-- | The port number that the automated backup used for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
dbInstanceAutomatedBackup_port :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Int)
dbInstanceAutomatedBackup_port = Lens.lens (\DBInstanceAutomatedBackup' {port} -> port) (\s@DBInstanceAutomatedBackup' {} a -> s {port = a} :: DBInstanceAutomatedBackup)

-- | The name of the database engine for this automated backup.
dbInstanceAutomatedBackup_engine :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_engine = Lens.lens (\DBInstanceAutomatedBackup' {engine} -> engine) (\s@DBInstanceAutomatedBackup' {} a -> s {engine = a} :: DBInstanceAutomatedBackup)

-- | The ARN from the key store with which the automated backup is associated
-- for TDE encryption.
dbInstanceAutomatedBackup_tdeCredentialArn :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_tdeCredentialArn = Lens.lens (\DBInstanceAutomatedBackup' {tdeCredentialArn} -> tdeCredentialArn) (\s@DBInstanceAutomatedBackup' {} a -> s {tdeCredentialArn = a} :: DBInstanceAutomatedBackup)

-- | The time zone of the automated backup. In most cases, the @Timezone@
-- element is empty. @Timezone@ content appears only for Microsoft SQL
-- Server DB instances that were created with a time zone specified.
dbInstanceAutomatedBackup_timezone :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_timezone = Lens.lens (\DBInstanceAutomatedBackup' {timezone} -> timezone) (\s@DBInstanceAutomatedBackup' {} a -> s {timezone = a} :: DBInstanceAutomatedBackup)

-- | The list of replications to different AWS Regions associated with the
-- automated backup.
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe [DBInstanceAutomatedBackupsReplication])
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceAutomatedBackupsReplications} -> dbInstanceAutomatedBackupsReplications) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceAutomatedBackupsReplications = a} :: DBInstanceAutomatedBackup) Core.. Lens.mapping Lens._Coerce

-- | The AWS Region associated with the automated backup.
dbInstanceAutomatedBackup_region :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_region = Lens.lens (\DBInstanceAutomatedBackup' {region} -> region) (\s@DBInstanceAutomatedBackup' {} a -> s {region = a} :: DBInstanceAutomatedBackup)

-- | Provides the VPC ID associated with the DB instance
dbInstanceAutomatedBackup_vpcId :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_vpcId = Lens.lens (\DBInstanceAutomatedBackup' {vpcId} -> vpcId) (\s@DBInstanceAutomatedBackup' {} a -> s {vpcId = a} :: DBInstanceAutomatedBackup)

-- | Specifies the allocated storage size in gibibytes (GiB).
dbInstanceAutomatedBackup_allocatedStorage :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Int)
dbInstanceAutomatedBackup_allocatedStorage = Lens.lens (\DBInstanceAutomatedBackup' {allocatedStorage} -> allocatedStorage) (\s@DBInstanceAutomatedBackup' {} a -> s {allocatedStorage = a} :: DBInstanceAutomatedBackup)

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to
-- database accounts is enabled, and otherwise false.
dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Bool)
dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBInstanceAutomatedBackup' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBInstanceAutomatedBackup' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBInstanceAutomatedBackup)

-- | The IOPS (I\/O operations per second) value for the automated backup.
dbInstanceAutomatedBackup_iops :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Int)
dbInstanceAutomatedBackup_iops = Lens.lens (\DBInstanceAutomatedBackup' {iops} -> iops) (\s@DBInstanceAutomatedBackup' {} a -> s {iops = a} :: DBInstanceAutomatedBackup)

-- | The Amazon Resource Name (ARN) for the automated backups.
dbInstanceAutomatedBackup_dbInstanceArn :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbInstanceAutomatedBackup_dbInstanceArn = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceArn} -> dbInstanceArn) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceArn = a} :: DBInstanceAutomatedBackup)

instance Core.FromXML DBInstanceAutomatedBackup where
  parseXML x =
    DBInstanceAutomatedBackup'
      Core.<$> (x Core..@? "BackupRetentionPeriod")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "DBInstanceAutomatedBackupsArn")
      Core.<*> (x Core..@? "StorageType")
      Core.<*> (x Core..@? "Encrypted")
      Core.<*> (x Core..@? "OptionGroupName")
      Core.<*> (x Core..@? "InstanceCreateTime")
      Core.<*> (x Core..@? "DbiResourceId")
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "AvailabilityZone")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "RestoreWindow")
      Core.<*> (x Core..@? "LicenseModel")
      Core.<*> (x Core..@? "DBInstanceIdentifier")
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "TdeCredentialArn")
      Core.<*> (x Core..@? "Timezone")
      Core.<*> ( x Core..@? "DBInstanceAutomatedBackupsReplications"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     ( Core.parseXMLList
                         "DBInstanceAutomatedBackupsReplication"
                     )
               )
      Core.<*> (x Core..@? "Region")
      Core.<*> (x Core..@? "VpcId")
      Core.<*> (x Core..@? "AllocatedStorage")
      Core.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Core.<*> (x Core..@? "Iops")
      Core.<*> (x Core..@? "DBInstanceArn")

instance Core.Hashable DBInstanceAutomatedBackup

instance Core.NFData DBInstanceAutomatedBackup
