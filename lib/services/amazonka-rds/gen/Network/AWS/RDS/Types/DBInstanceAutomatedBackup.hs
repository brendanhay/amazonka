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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.DBInstanceAutomatedBackupsReplication
import Network.AWS.RDS.Types.RestoreWindow

-- | An automated backup of a DB instance. It consists of system backups,
-- transaction logs, and the database instance properties that existed at
-- the time you deleted the source instance.
--
-- /See:/ 'newDBInstanceAutomatedBackup' smart constructor.
data DBInstanceAutomatedBackup = DBInstanceAutomatedBackup'
  { -- | Earliest and latest time an instance can be restored to.
    restoreWindow :: Prelude.Maybe RestoreWindow,
    -- | The version of the database engine for the automated backup.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of status information for an automated backup:
    --
    -- -   @active@ - automated backups for current instances
    --
    -- -   @retained@ - automated backups for deleted instances
    --
    -- -   @creating@ - automated backups that are waiting for the first
    --     automated snapshot to be available.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the automated backups.
    dbInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The license model of an automated backup.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | True if mapping of Amazon Web Services Identity and Access Management
    -- (IAM) accounts to database accounts is enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The IOPS (I\/O operations per second) value for the automated backup.
    iops :: Prelude.Maybe Prelude.Int,
    -- | Provides the VPC ID associated with the DB instance
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Provides the date and time that the DB instance was created.
    instanceCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | The name of the database engine for this automated backup.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the automated backup is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | License model information for the automated backup.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The customer id of the instance that is\/was associated with the
    -- automated backup.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key ID for an automated backup.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK).
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone that the automated backup was created in. For
    -- information on Amazon Web Services Regions and Availability Zones, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The retention period for the automated backups.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services Region associated with the automated backup.
    region :: Prelude.Maybe Prelude.Text,
    -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the source DB instance, which can\'t be changed and
    -- which is unique to an Amazon Web Services Region.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | The option group the automated backup is associated with. If omitted,
    -- the default option group for the engine specified is used.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the automated backup. In most cases, the @Timezone@
    -- element is empty. @Timezone@ content appears only for Microsoft SQL
    -- Server DB instances that were created with a time zone specified.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The ARN from the key store with which the automated backup is associated
    -- for TDE encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The list of replications to different Amazon Web Services Regions
    -- associated with the automated backup.
    dbInstanceAutomatedBackupsReplications :: Prelude.Maybe [DBInstanceAutomatedBackupsReplication],
    -- | The Amazon Resource Name (ARN) for the replicated automated backups.
    dbInstanceAutomatedBackupsArn :: Prelude.Maybe Prelude.Text,
    -- | The port number that the automated backup used for connections.
    --
    -- Default: Inherits from the source DB instance
    --
    -- Valid Values: @1150-65535@
    port :: Prelude.Maybe Prelude.Int,
    -- | Specifies the storage type associated with the automated backup.
    storageType :: Prelude.Maybe Prelude.Text
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
-- 'restoreWindow', 'dbInstanceAutomatedBackup_restoreWindow' - Earliest and latest time an instance can be restored to.
--
-- 'engineVersion', 'dbInstanceAutomatedBackup_engineVersion' - The version of the database engine for the automated backup.
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
-- 'dbInstanceArn', 'dbInstanceAutomatedBackup_dbInstanceArn' - The Amazon Resource Name (ARN) for the automated backups.
--
-- 'masterUsername', 'dbInstanceAutomatedBackup_masterUsername' - The license model of an automated backup.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled' - True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- 'iops', 'dbInstanceAutomatedBackup_iops' - The IOPS (I\/O operations per second) value for the automated backup.
--
-- 'vpcId', 'dbInstanceAutomatedBackup_vpcId' - Provides the VPC ID associated with the DB instance
--
-- 'instanceCreateTime', 'dbInstanceAutomatedBackup_instanceCreateTime' - Provides the date and time that the DB instance was created.
--
-- 'engine', 'dbInstanceAutomatedBackup_engine' - The name of the database engine for this automated backup.
--
-- 'encrypted', 'dbInstanceAutomatedBackup_encrypted' - Specifies whether the automated backup is encrypted.
--
-- 'licenseModel', 'dbInstanceAutomatedBackup_licenseModel' - License model information for the automated backup.
--
-- 'dbInstanceIdentifier', 'dbInstanceAutomatedBackup_dbInstanceIdentifier' - The customer id of the instance that is\/was associated with the
-- automated backup.
--
-- 'kmsKeyId', 'dbInstanceAutomatedBackup_kmsKeyId' - The Amazon Web Services KMS key ID for an automated backup.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
--
-- 'availabilityZone', 'dbInstanceAutomatedBackup_availabilityZone' - The Availability Zone that the automated backup was created in. For
-- information on Amazon Web Services Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
--
-- 'backupRetentionPeriod', 'dbInstanceAutomatedBackup_backupRetentionPeriod' - The retention period for the automated backups.
--
-- 'region', 'dbInstanceAutomatedBackup_region' - The Amazon Web Services Region associated with the automated backup.
--
-- 'allocatedStorage', 'dbInstanceAutomatedBackup_allocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- 'dbiResourceId', 'dbInstanceAutomatedBackup_dbiResourceId' - The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an Amazon Web Services Region.
--
-- 'optionGroupName', 'dbInstanceAutomatedBackup_optionGroupName' - The option group the automated backup is associated with. If omitted,
-- the default option group for the engine specified is used.
--
-- 'timezone', 'dbInstanceAutomatedBackup_timezone' - The time zone of the automated backup. In most cases, the @Timezone@
-- element is empty. @Timezone@ content appears only for Microsoft SQL
-- Server DB instances that were created with a time zone specified.
--
-- 'tdeCredentialArn', 'dbInstanceAutomatedBackup_tdeCredentialArn' - The ARN from the key store with which the automated backup is associated
-- for TDE encryption.
--
-- 'dbInstanceAutomatedBackupsReplications', 'dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications' - The list of replications to different Amazon Web Services Regions
-- associated with the automated backup.
--
-- 'dbInstanceAutomatedBackupsArn', 'dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn' - The Amazon Resource Name (ARN) for the replicated automated backups.
--
-- 'port', 'dbInstanceAutomatedBackup_port' - The port number that the automated backup used for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
--
-- 'storageType', 'dbInstanceAutomatedBackup_storageType' - Specifies the storage type associated with the automated backup.
newDBInstanceAutomatedBackup ::
  DBInstanceAutomatedBackup
newDBInstanceAutomatedBackup =
  DBInstanceAutomatedBackup'
    { restoreWindow =
        Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      status = Prelude.Nothing,
      dbInstanceArn = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled =
        Prelude.Nothing,
      iops = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      engine = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      region = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      timezone = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      dbInstanceAutomatedBackupsReplications =
        Prelude.Nothing,
      dbInstanceAutomatedBackupsArn = Prelude.Nothing,
      port = Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | Earliest and latest time an instance can be restored to.
dbInstanceAutomatedBackup_restoreWindow :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe RestoreWindow)
dbInstanceAutomatedBackup_restoreWindow = Lens.lens (\DBInstanceAutomatedBackup' {restoreWindow} -> restoreWindow) (\s@DBInstanceAutomatedBackup' {} a -> s {restoreWindow = a} :: DBInstanceAutomatedBackup)

-- | The version of the database engine for the automated backup.
dbInstanceAutomatedBackup_engineVersion :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_engineVersion = Lens.lens (\DBInstanceAutomatedBackup' {engineVersion} -> engineVersion) (\s@DBInstanceAutomatedBackup' {} a -> s {engineVersion = a} :: DBInstanceAutomatedBackup)

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

-- | The Amazon Resource Name (ARN) for the automated backups.
dbInstanceAutomatedBackup_dbInstanceArn :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_dbInstanceArn = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceArn} -> dbInstanceArn) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceArn = a} :: DBInstanceAutomatedBackup)

-- | The license model of an automated backup.
dbInstanceAutomatedBackup_masterUsername :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_masterUsername = Lens.lens (\DBInstanceAutomatedBackup' {masterUsername} -> masterUsername) (\s@DBInstanceAutomatedBackup' {} a -> s {masterUsername = a} :: DBInstanceAutomatedBackup)

-- | True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Bool)
dbInstanceAutomatedBackup_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBInstanceAutomatedBackup' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBInstanceAutomatedBackup' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBInstanceAutomatedBackup)

-- | The IOPS (I\/O operations per second) value for the automated backup.
dbInstanceAutomatedBackup_iops :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Int)
dbInstanceAutomatedBackup_iops = Lens.lens (\DBInstanceAutomatedBackup' {iops} -> iops) (\s@DBInstanceAutomatedBackup' {} a -> s {iops = a} :: DBInstanceAutomatedBackup)

-- | Provides the VPC ID associated with the DB instance
dbInstanceAutomatedBackup_vpcId :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_vpcId = Lens.lens (\DBInstanceAutomatedBackup' {vpcId} -> vpcId) (\s@DBInstanceAutomatedBackup' {} a -> s {vpcId = a} :: DBInstanceAutomatedBackup)

-- | Provides the date and time that the DB instance was created.
dbInstanceAutomatedBackup_instanceCreateTime :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.UTCTime)
dbInstanceAutomatedBackup_instanceCreateTime = Lens.lens (\DBInstanceAutomatedBackup' {instanceCreateTime} -> instanceCreateTime) (\s@DBInstanceAutomatedBackup' {} a -> s {instanceCreateTime = a} :: DBInstanceAutomatedBackup) Prelude.. Lens.mapping Core._Time

-- | The name of the database engine for this automated backup.
dbInstanceAutomatedBackup_engine :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_engine = Lens.lens (\DBInstanceAutomatedBackup' {engine} -> engine) (\s@DBInstanceAutomatedBackup' {} a -> s {engine = a} :: DBInstanceAutomatedBackup)

-- | Specifies whether the automated backup is encrypted.
dbInstanceAutomatedBackup_encrypted :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Bool)
dbInstanceAutomatedBackup_encrypted = Lens.lens (\DBInstanceAutomatedBackup' {encrypted} -> encrypted) (\s@DBInstanceAutomatedBackup' {} a -> s {encrypted = a} :: DBInstanceAutomatedBackup)

-- | License model information for the automated backup.
dbInstanceAutomatedBackup_licenseModel :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_licenseModel = Lens.lens (\DBInstanceAutomatedBackup' {licenseModel} -> licenseModel) (\s@DBInstanceAutomatedBackup' {} a -> s {licenseModel = a} :: DBInstanceAutomatedBackup)

-- | The customer id of the instance that is\/was associated with the
-- automated backup.
dbInstanceAutomatedBackup_dbInstanceIdentifier :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_dbInstanceIdentifier = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceIdentifier = a} :: DBInstanceAutomatedBackup)

-- | The Amazon Web Services KMS key ID for an automated backup.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
dbInstanceAutomatedBackup_kmsKeyId :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_kmsKeyId = Lens.lens (\DBInstanceAutomatedBackup' {kmsKeyId} -> kmsKeyId) (\s@DBInstanceAutomatedBackup' {} a -> s {kmsKeyId = a} :: DBInstanceAutomatedBackup)

-- | The Availability Zone that the automated backup was created in. For
-- information on Amazon Web Services Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
dbInstanceAutomatedBackup_availabilityZone :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_availabilityZone = Lens.lens (\DBInstanceAutomatedBackup' {availabilityZone} -> availabilityZone) (\s@DBInstanceAutomatedBackup' {} a -> s {availabilityZone = a} :: DBInstanceAutomatedBackup)

-- | The retention period for the automated backups.
dbInstanceAutomatedBackup_backupRetentionPeriod :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Int)
dbInstanceAutomatedBackup_backupRetentionPeriod = Lens.lens (\DBInstanceAutomatedBackup' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBInstanceAutomatedBackup' {} a -> s {backupRetentionPeriod = a} :: DBInstanceAutomatedBackup)

-- | The Amazon Web Services Region associated with the automated backup.
dbInstanceAutomatedBackup_region :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_region = Lens.lens (\DBInstanceAutomatedBackup' {region} -> region) (\s@DBInstanceAutomatedBackup' {} a -> s {region = a} :: DBInstanceAutomatedBackup)

-- | Specifies the allocated storage size in gibibytes (GiB).
dbInstanceAutomatedBackup_allocatedStorage :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Int)
dbInstanceAutomatedBackup_allocatedStorage = Lens.lens (\DBInstanceAutomatedBackup' {allocatedStorage} -> allocatedStorage) (\s@DBInstanceAutomatedBackup' {} a -> s {allocatedStorage = a} :: DBInstanceAutomatedBackup)

-- | The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an Amazon Web Services Region.
dbInstanceAutomatedBackup_dbiResourceId :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_dbiResourceId = Lens.lens (\DBInstanceAutomatedBackup' {dbiResourceId} -> dbiResourceId) (\s@DBInstanceAutomatedBackup' {} a -> s {dbiResourceId = a} :: DBInstanceAutomatedBackup)

-- | The option group the automated backup is associated with. If omitted,
-- the default option group for the engine specified is used.
dbInstanceAutomatedBackup_optionGroupName :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_optionGroupName = Lens.lens (\DBInstanceAutomatedBackup' {optionGroupName} -> optionGroupName) (\s@DBInstanceAutomatedBackup' {} a -> s {optionGroupName = a} :: DBInstanceAutomatedBackup)

-- | The time zone of the automated backup. In most cases, the @Timezone@
-- element is empty. @Timezone@ content appears only for Microsoft SQL
-- Server DB instances that were created with a time zone specified.
dbInstanceAutomatedBackup_timezone :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_timezone = Lens.lens (\DBInstanceAutomatedBackup' {timezone} -> timezone) (\s@DBInstanceAutomatedBackup' {} a -> s {timezone = a} :: DBInstanceAutomatedBackup)

-- | The ARN from the key store with which the automated backup is associated
-- for TDE encryption.
dbInstanceAutomatedBackup_tdeCredentialArn :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_tdeCredentialArn = Lens.lens (\DBInstanceAutomatedBackup' {tdeCredentialArn} -> tdeCredentialArn) (\s@DBInstanceAutomatedBackup' {} a -> s {tdeCredentialArn = a} :: DBInstanceAutomatedBackup)

-- | The list of replications to different Amazon Web Services Regions
-- associated with the automated backup.
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe [DBInstanceAutomatedBackupsReplication])
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsReplications = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceAutomatedBackupsReplications} -> dbInstanceAutomatedBackupsReplications) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceAutomatedBackupsReplications = a} :: DBInstanceAutomatedBackup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the replicated automated backups.
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn = Lens.lens (\DBInstanceAutomatedBackup' {dbInstanceAutomatedBackupsArn} -> dbInstanceAutomatedBackupsArn) (\s@DBInstanceAutomatedBackup' {} a -> s {dbInstanceAutomatedBackupsArn = a} :: DBInstanceAutomatedBackup)

-- | The port number that the automated backup used for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
dbInstanceAutomatedBackup_port :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Int)
dbInstanceAutomatedBackup_port = Lens.lens (\DBInstanceAutomatedBackup' {port} -> port) (\s@DBInstanceAutomatedBackup' {} a -> s {port = a} :: DBInstanceAutomatedBackup)

-- | Specifies the storage type associated with the automated backup.
dbInstanceAutomatedBackup_storageType :: Lens.Lens' DBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackup_storageType = Lens.lens (\DBInstanceAutomatedBackup' {storageType} -> storageType) (\s@DBInstanceAutomatedBackup' {} a -> s {storageType = a} :: DBInstanceAutomatedBackup)

instance Core.FromXML DBInstanceAutomatedBackup where
  parseXML x =
    DBInstanceAutomatedBackup'
      Prelude.<$> (x Core..@? "RestoreWindow")
      Prelude.<*> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "DBInstanceArn")
      Prelude.<*> (x Core..@? "MasterUsername")
      Prelude.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Core..@? "Iops")
      Prelude.<*> (x Core..@? "VpcId")
      Prelude.<*> (x Core..@? "InstanceCreateTime")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "Encrypted")
      Prelude.<*> (x Core..@? "LicenseModel")
      Prelude.<*> (x Core..@? "DBInstanceIdentifier")
      Prelude.<*> (x Core..@? "KmsKeyId")
      Prelude.<*> (x Core..@? "AvailabilityZone")
      Prelude.<*> (x Core..@? "BackupRetentionPeriod")
      Prelude.<*> (x Core..@? "Region")
      Prelude.<*> (x Core..@? "AllocatedStorage")
      Prelude.<*> (x Core..@? "DbiResourceId")
      Prelude.<*> (x Core..@? "OptionGroupName")
      Prelude.<*> (x Core..@? "Timezone")
      Prelude.<*> (x Core..@? "TdeCredentialArn")
      Prelude.<*> ( x Core..@? "DBInstanceAutomatedBackupsReplications"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Core.parseXMLList
                            "DBInstanceAutomatedBackupsReplication"
                        )
                  )
      Prelude.<*> (x Core..@? "DBInstanceAutomatedBackupsArn")
      Prelude.<*> (x Core..@? "Port")
      Prelude.<*> (x Core..@? "StorageType")

instance Prelude.Hashable DBInstanceAutomatedBackup

instance Prelude.NFData DBInstanceAutomatedBackup
