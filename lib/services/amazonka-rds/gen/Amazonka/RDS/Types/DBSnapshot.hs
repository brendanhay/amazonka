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
-- Module      : Amazonka.RDS.Types.DBSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBSnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.ProcessorFeature
import Amazonka.RDS.Types.Tag

-- | Contains the details of an Amazon RDS DB snapshot.
--
-- This data type is used as a response element in the
-- @DescribeDBSnapshots@ action.
--
-- /See:/ 'newDBSnapshot' smart constructor.
data DBSnapshot = DBSnapshot'
  { -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Specifies the name of the Availability Zone the DB instance was located
    -- in at the time of the DB snapshot.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Specifies the DB instance identifier of the DB instance this DB snapshot
    -- was created from.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the DB snapshot.
    dbSnapshotArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the identifier for the DB snapshot.
    dbSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the source DB instance, which can\'t be changed and
    -- which is unique to an Amazon Web Services Region.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the DB snapshot is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the database engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Specifies the version of the database engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | True if mapping of Amazon Web Services Identity and Access Management
    -- (IAM) accounts to database accounts is enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the time in Coordinated Universal Time (UTC) when the DB
    -- instance, from which the snapshot was taken, was created.
    instanceCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | Specifies the Provisioned IOPS (I\/O operations per second) value of the
    -- DB instance at the time of the snapshot.
    iops :: Prelude.Maybe Prelude.Int,
    -- | If @Encrypted@ is true, the Amazon Web Services KMS key identifier for
    -- the encrypted DB snapshot.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | License model information for the restored DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | Provides the master username for the DB snapshot.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | Provides the option group name for the DB snapshot.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time of the CreateDBSnapshot operation in Coordinated
    -- Universal Time (UTC). Doesn\'t change when the snapshot is copied.
    originalSnapshotCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | The percentage of the estimated data that has been transferred.
    percentProgress :: Prelude.Maybe Prelude.Int,
    -- | Specifies the port that the database engine was listening on at the time
    -- of the snapshot.
    port :: Prelude.Maybe Prelude.Int,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance when the DB snapshot was created.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | Specifies when the snapshot was taken in Coordinated Universal Time
    -- (UTC). Changes for the copy when the snapshot is copied.
    snapshotCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | The timestamp of the most recent transaction applied to the database
    -- that you\'re backing up. Thus, if you restore a snapshot,
    -- SnapshotDatabaseTime is the most recent transaction in the restored DB
    -- instance. In contrast, originalSnapshotCreateTime specifies the system
    -- time that the snapshot completed.
    --
    -- If you back up a read replica, you can determine the replica lag by
    -- comparing SnapshotDatabaseTime with originalSnapshotCreateTime. For
    -- example, if originalSnapshotCreateTime is two hours later than
    -- SnapshotDatabaseTime, then the replica lag is two hours.
    snapshotDatabaseTime :: Prelude.Maybe Data.ISO8601,
    -- | Specifies where manual snapshots are stored: Amazon Web Services
    -- Outposts or the Amazon Web Services Region.
    snapshotTarget :: Prelude.Maybe Prelude.Text,
    -- | Provides the type of the DB snapshot.
    snapshotType :: Prelude.Maybe Prelude.Text,
    -- | The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was
    -- copied from. It only has a value in the case of a cross-account or
    -- cross-Region copy.
    sourceDBSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region that the DB snapshot was created in or
    -- copied from.
    sourceRegion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the status of this DB snapshot.
    status :: Prelude.Maybe Prelude.Text,
    -- | Specifies the storage throughput for the DB snapshot.
    storageThroughput :: Prelude.Maybe Prelude.Int,
    -- | Specifies the storage type associated with DB snapshot.
    storageType :: Prelude.Maybe Prelude.Text,
    tagList :: Prelude.Maybe [Tag],
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the DB snapshot. In most cases, the @Timezone@ element
    -- is empty. @Timezone@ content appears only for snapshots taken from
    -- Microsoft SQL Server DB instances that were created with a time zone
    -- specified.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | Provides the VPC ID associated with the DB snapshot.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedStorage', 'dbSnapshot_allocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- 'availabilityZone', 'dbSnapshot_availabilityZone' - Specifies the name of the Availability Zone the DB instance was located
-- in at the time of the DB snapshot.
--
-- 'dbInstanceIdentifier', 'dbSnapshot_dbInstanceIdentifier' - Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
--
-- 'dbSnapshotArn', 'dbSnapshot_dbSnapshotArn' - The Amazon Resource Name (ARN) for the DB snapshot.
--
-- 'dbSnapshotIdentifier', 'dbSnapshot_dbSnapshotIdentifier' - Specifies the identifier for the DB snapshot.
--
-- 'dbiResourceId', 'dbSnapshot_dbiResourceId' - The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an Amazon Web Services Region.
--
-- 'encrypted', 'dbSnapshot_encrypted' - Specifies whether the DB snapshot is encrypted.
--
-- 'engine', 'dbSnapshot_engine' - Specifies the name of the database engine.
--
-- 'engineVersion', 'dbSnapshot_engineVersion' - Specifies the version of the database engine.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbSnapshot_iAMDatabaseAuthenticationEnabled' - True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- 'instanceCreateTime', 'dbSnapshot_instanceCreateTime' - Specifies the time in Coordinated Universal Time (UTC) when the DB
-- instance, from which the snapshot was taken, was created.
--
-- 'iops', 'dbSnapshot_iops' - Specifies the Provisioned IOPS (I\/O operations per second) value of the
-- DB instance at the time of the snapshot.
--
-- 'kmsKeyId', 'dbSnapshot_kmsKeyId' - If @Encrypted@ is true, the Amazon Web Services KMS key identifier for
-- the encrypted DB snapshot.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- 'licenseModel', 'dbSnapshot_licenseModel' - License model information for the restored DB instance.
--
-- 'masterUsername', 'dbSnapshot_masterUsername' - Provides the master username for the DB snapshot.
--
-- 'optionGroupName', 'dbSnapshot_optionGroupName' - Provides the option group name for the DB snapshot.
--
-- 'originalSnapshotCreateTime', 'dbSnapshot_originalSnapshotCreateTime' - Specifies the time of the CreateDBSnapshot operation in Coordinated
-- Universal Time (UTC). Doesn\'t change when the snapshot is copied.
--
-- 'percentProgress', 'dbSnapshot_percentProgress' - The percentage of the estimated data that has been transferred.
--
-- 'port', 'dbSnapshot_port' - Specifies the port that the database engine was listening on at the time
-- of the snapshot.
--
-- 'processorFeatures', 'dbSnapshot_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance when the DB snapshot was created.
--
-- 'snapshotCreateTime', 'dbSnapshot_snapshotCreateTime' - Specifies when the snapshot was taken in Coordinated Universal Time
-- (UTC). Changes for the copy when the snapshot is copied.
--
-- 'snapshotDatabaseTime', 'dbSnapshot_snapshotDatabaseTime' - The timestamp of the most recent transaction applied to the database
-- that you\'re backing up. Thus, if you restore a snapshot,
-- SnapshotDatabaseTime is the most recent transaction in the restored DB
-- instance. In contrast, originalSnapshotCreateTime specifies the system
-- time that the snapshot completed.
--
-- If you back up a read replica, you can determine the replica lag by
-- comparing SnapshotDatabaseTime with originalSnapshotCreateTime. For
-- example, if originalSnapshotCreateTime is two hours later than
-- SnapshotDatabaseTime, then the replica lag is two hours.
--
-- 'snapshotTarget', 'dbSnapshot_snapshotTarget' - Specifies where manual snapshots are stored: Amazon Web Services
-- Outposts or the Amazon Web Services Region.
--
-- 'snapshotType', 'dbSnapshot_snapshotType' - Provides the type of the DB snapshot.
--
-- 'sourceDBSnapshotIdentifier', 'dbSnapshot_sourceDBSnapshotIdentifier' - The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was
-- copied from. It only has a value in the case of a cross-account or
-- cross-Region copy.
--
-- 'sourceRegion', 'dbSnapshot_sourceRegion' - The Amazon Web Services Region that the DB snapshot was created in or
-- copied from.
--
-- 'status', 'dbSnapshot_status' - Specifies the status of this DB snapshot.
--
-- 'storageThroughput', 'dbSnapshot_storageThroughput' - Specifies the storage throughput for the DB snapshot.
--
-- 'storageType', 'dbSnapshot_storageType' - Specifies the storage type associated with DB snapshot.
--
-- 'tagList', 'dbSnapshot_tagList' - Undocumented member.
--
-- 'tdeCredentialArn', 'dbSnapshot_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- 'timezone', 'dbSnapshot_timezone' - The time zone of the DB snapshot. In most cases, the @Timezone@ element
-- is empty. @Timezone@ content appears only for snapshots taken from
-- Microsoft SQL Server DB instances that were created with a time zone
-- specified.
--
-- 'vpcId', 'dbSnapshot_vpcId' - Provides the VPC ID associated with the DB snapshot.
newDBSnapshot ::
  DBSnapshot
newDBSnapshot =
  DBSnapshot'
    { allocatedStorage = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      dbSnapshotArn = Prelude.Nothing,
      dbSnapshotIdentifier = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      iops = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      originalSnapshotCreateTime = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      port = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      snapshotDatabaseTime = Prelude.Nothing,
      snapshotTarget = Prelude.Nothing,
      snapshotType = Prelude.Nothing,
      sourceDBSnapshotIdentifier = Prelude.Nothing,
      sourceRegion = Prelude.Nothing,
      status = Prelude.Nothing,
      storageThroughput = Prelude.Nothing,
      storageType = Prelude.Nothing,
      tagList = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      timezone = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Specifies the allocated storage size in gibibytes (GiB).
dbSnapshot_allocatedStorage :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Int)
dbSnapshot_allocatedStorage = Lens.lens (\DBSnapshot' {allocatedStorage} -> allocatedStorage) (\s@DBSnapshot' {} a -> s {allocatedStorage = a} :: DBSnapshot)

-- | Specifies the name of the Availability Zone the DB instance was located
-- in at the time of the DB snapshot.
dbSnapshot_availabilityZone :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_availabilityZone = Lens.lens (\DBSnapshot' {availabilityZone} -> availabilityZone) (\s@DBSnapshot' {} a -> s {availabilityZone = a} :: DBSnapshot)

-- | Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
dbSnapshot_dbInstanceIdentifier :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_dbInstanceIdentifier = Lens.lens (\DBSnapshot' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBSnapshot' {} a -> s {dbInstanceIdentifier = a} :: DBSnapshot)

-- | The Amazon Resource Name (ARN) for the DB snapshot.
dbSnapshot_dbSnapshotArn :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_dbSnapshotArn = Lens.lens (\DBSnapshot' {dbSnapshotArn} -> dbSnapshotArn) (\s@DBSnapshot' {} a -> s {dbSnapshotArn = a} :: DBSnapshot)

-- | Specifies the identifier for the DB snapshot.
dbSnapshot_dbSnapshotIdentifier :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_dbSnapshotIdentifier = Lens.lens (\DBSnapshot' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@DBSnapshot' {} a -> s {dbSnapshotIdentifier = a} :: DBSnapshot)

-- | The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an Amazon Web Services Region.
dbSnapshot_dbiResourceId :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_dbiResourceId = Lens.lens (\DBSnapshot' {dbiResourceId} -> dbiResourceId) (\s@DBSnapshot' {} a -> s {dbiResourceId = a} :: DBSnapshot)

-- | Specifies whether the DB snapshot is encrypted.
dbSnapshot_encrypted :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Bool)
dbSnapshot_encrypted = Lens.lens (\DBSnapshot' {encrypted} -> encrypted) (\s@DBSnapshot' {} a -> s {encrypted = a} :: DBSnapshot)

-- | Specifies the name of the database engine.
dbSnapshot_engine :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_engine = Lens.lens (\DBSnapshot' {engine} -> engine) (\s@DBSnapshot' {} a -> s {engine = a} :: DBSnapshot)

-- | Specifies the version of the database engine.
dbSnapshot_engineVersion :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_engineVersion = Lens.lens (\DBSnapshot' {engineVersion} -> engineVersion) (\s@DBSnapshot' {} a -> s {engineVersion = a} :: DBSnapshot)

-- | True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
dbSnapshot_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Bool)
dbSnapshot_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBSnapshot' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBSnapshot' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBSnapshot)

-- | Specifies the time in Coordinated Universal Time (UTC) when the DB
-- instance, from which the snapshot was taken, was created.
dbSnapshot_instanceCreateTime :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.UTCTime)
dbSnapshot_instanceCreateTime = Lens.lens (\DBSnapshot' {instanceCreateTime} -> instanceCreateTime) (\s@DBSnapshot' {} a -> s {instanceCreateTime = a} :: DBSnapshot) Prelude.. Lens.mapping Data._Time

-- | Specifies the Provisioned IOPS (I\/O operations per second) value of the
-- DB instance at the time of the snapshot.
dbSnapshot_iops :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Int)
dbSnapshot_iops = Lens.lens (\DBSnapshot' {iops} -> iops) (\s@DBSnapshot' {} a -> s {iops = a} :: DBSnapshot)

-- | If @Encrypted@ is true, the Amazon Web Services KMS key identifier for
-- the encrypted DB snapshot.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
dbSnapshot_kmsKeyId :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_kmsKeyId = Lens.lens (\DBSnapshot' {kmsKeyId} -> kmsKeyId) (\s@DBSnapshot' {} a -> s {kmsKeyId = a} :: DBSnapshot)

-- | License model information for the restored DB instance.
dbSnapshot_licenseModel :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_licenseModel = Lens.lens (\DBSnapshot' {licenseModel} -> licenseModel) (\s@DBSnapshot' {} a -> s {licenseModel = a} :: DBSnapshot)

-- | Provides the master username for the DB snapshot.
dbSnapshot_masterUsername :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_masterUsername = Lens.lens (\DBSnapshot' {masterUsername} -> masterUsername) (\s@DBSnapshot' {} a -> s {masterUsername = a} :: DBSnapshot)

-- | Provides the option group name for the DB snapshot.
dbSnapshot_optionGroupName :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_optionGroupName = Lens.lens (\DBSnapshot' {optionGroupName} -> optionGroupName) (\s@DBSnapshot' {} a -> s {optionGroupName = a} :: DBSnapshot)

-- | Specifies the time of the CreateDBSnapshot operation in Coordinated
-- Universal Time (UTC). Doesn\'t change when the snapshot is copied.
dbSnapshot_originalSnapshotCreateTime :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.UTCTime)
dbSnapshot_originalSnapshotCreateTime = Lens.lens (\DBSnapshot' {originalSnapshotCreateTime} -> originalSnapshotCreateTime) (\s@DBSnapshot' {} a -> s {originalSnapshotCreateTime = a} :: DBSnapshot) Prelude.. Lens.mapping Data._Time

-- | The percentage of the estimated data that has been transferred.
dbSnapshot_percentProgress :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Int)
dbSnapshot_percentProgress = Lens.lens (\DBSnapshot' {percentProgress} -> percentProgress) (\s@DBSnapshot' {} a -> s {percentProgress = a} :: DBSnapshot)

-- | Specifies the port that the database engine was listening on at the time
-- of the snapshot.
dbSnapshot_port :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Int)
dbSnapshot_port = Lens.lens (\DBSnapshot' {port} -> port) (\s@DBSnapshot' {} a -> s {port = a} :: DBSnapshot)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance when the DB snapshot was created.
dbSnapshot_processorFeatures :: Lens.Lens' DBSnapshot (Prelude.Maybe [ProcessorFeature])
dbSnapshot_processorFeatures = Lens.lens (\DBSnapshot' {processorFeatures} -> processorFeatures) (\s@DBSnapshot' {} a -> s {processorFeatures = a} :: DBSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | Specifies when the snapshot was taken in Coordinated Universal Time
-- (UTC). Changes for the copy when the snapshot is copied.
dbSnapshot_snapshotCreateTime :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.UTCTime)
dbSnapshot_snapshotCreateTime = Lens.lens (\DBSnapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@DBSnapshot' {} a -> s {snapshotCreateTime = a} :: DBSnapshot) Prelude.. Lens.mapping Data._Time

-- | The timestamp of the most recent transaction applied to the database
-- that you\'re backing up. Thus, if you restore a snapshot,
-- SnapshotDatabaseTime is the most recent transaction in the restored DB
-- instance. In contrast, originalSnapshotCreateTime specifies the system
-- time that the snapshot completed.
--
-- If you back up a read replica, you can determine the replica lag by
-- comparing SnapshotDatabaseTime with originalSnapshotCreateTime. For
-- example, if originalSnapshotCreateTime is two hours later than
-- SnapshotDatabaseTime, then the replica lag is two hours.
dbSnapshot_snapshotDatabaseTime :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.UTCTime)
dbSnapshot_snapshotDatabaseTime = Lens.lens (\DBSnapshot' {snapshotDatabaseTime} -> snapshotDatabaseTime) (\s@DBSnapshot' {} a -> s {snapshotDatabaseTime = a} :: DBSnapshot) Prelude.. Lens.mapping Data._Time

-- | Specifies where manual snapshots are stored: Amazon Web Services
-- Outposts or the Amazon Web Services Region.
dbSnapshot_snapshotTarget :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_snapshotTarget = Lens.lens (\DBSnapshot' {snapshotTarget} -> snapshotTarget) (\s@DBSnapshot' {} a -> s {snapshotTarget = a} :: DBSnapshot)

-- | Provides the type of the DB snapshot.
dbSnapshot_snapshotType :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_snapshotType = Lens.lens (\DBSnapshot' {snapshotType} -> snapshotType) (\s@DBSnapshot' {} a -> s {snapshotType = a} :: DBSnapshot)

-- | The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was
-- copied from. It only has a value in the case of a cross-account or
-- cross-Region copy.
dbSnapshot_sourceDBSnapshotIdentifier :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_sourceDBSnapshotIdentifier = Lens.lens (\DBSnapshot' {sourceDBSnapshotIdentifier} -> sourceDBSnapshotIdentifier) (\s@DBSnapshot' {} a -> s {sourceDBSnapshotIdentifier = a} :: DBSnapshot)

-- | The Amazon Web Services Region that the DB snapshot was created in or
-- copied from.
dbSnapshot_sourceRegion :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_sourceRegion = Lens.lens (\DBSnapshot' {sourceRegion} -> sourceRegion) (\s@DBSnapshot' {} a -> s {sourceRegion = a} :: DBSnapshot)

-- | Specifies the status of this DB snapshot.
dbSnapshot_status :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_status = Lens.lens (\DBSnapshot' {status} -> status) (\s@DBSnapshot' {} a -> s {status = a} :: DBSnapshot)

-- | Specifies the storage throughput for the DB snapshot.
dbSnapshot_storageThroughput :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Int)
dbSnapshot_storageThroughput = Lens.lens (\DBSnapshot' {storageThroughput} -> storageThroughput) (\s@DBSnapshot' {} a -> s {storageThroughput = a} :: DBSnapshot)

-- | Specifies the storage type associated with DB snapshot.
dbSnapshot_storageType :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_storageType = Lens.lens (\DBSnapshot' {storageType} -> storageType) (\s@DBSnapshot' {} a -> s {storageType = a} :: DBSnapshot)

-- | Undocumented member.
dbSnapshot_tagList :: Lens.Lens' DBSnapshot (Prelude.Maybe [Tag])
dbSnapshot_tagList = Lens.lens (\DBSnapshot' {tagList} -> tagList) (\s@DBSnapshot' {} a -> s {tagList = a} :: DBSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
dbSnapshot_tdeCredentialArn :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_tdeCredentialArn = Lens.lens (\DBSnapshot' {tdeCredentialArn} -> tdeCredentialArn) (\s@DBSnapshot' {} a -> s {tdeCredentialArn = a} :: DBSnapshot)

-- | The time zone of the DB snapshot. In most cases, the @Timezone@ element
-- is empty. @Timezone@ content appears only for snapshots taken from
-- Microsoft SQL Server DB instances that were created with a time zone
-- specified.
dbSnapshot_timezone :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_timezone = Lens.lens (\DBSnapshot' {timezone} -> timezone) (\s@DBSnapshot' {} a -> s {timezone = a} :: DBSnapshot)

-- | Provides the VPC ID associated with the DB snapshot.
dbSnapshot_vpcId :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_vpcId = Lens.lens (\DBSnapshot' {vpcId} -> vpcId) (\s@DBSnapshot' {} a -> s {vpcId = a} :: DBSnapshot)

instance Data.FromXML DBSnapshot where
  parseXML x =
    DBSnapshot'
      Prelude.<$> (x Data..@? "AllocatedStorage")
      Prelude.<*> (x Data..@? "AvailabilityZone")
      Prelude.<*> (x Data..@? "DBInstanceIdentifier")
      Prelude.<*> (x Data..@? "DBSnapshotArn")
      Prelude.<*> (x Data..@? "DBSnapshotIdentifier")
      Prelude.<*> (x Data..@? "DbiResourceId")
      Prelude.<*> (x Data..@? "Encrypted")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Data..@? "InstanceCreateTime")
      Prelude.<*> (x Data..@? "Iops")
      Prelude.<*> (x Data..@? "KmsKeyId")
      Prelude.<*> (x Data..@? "LicenseModel")
      Prelude.<*> (x Data..@? "MasterUsername")
      Prelude.<*> (x Data..@? "OptionGroupName")
      Prelude.<*> (x Data..@? "OriginalSnapshotCreateTime")
      Prelude.<*> (x Data..@? "PercentProgress")
      Prelude.<*> (x Data..@? "Port")
      Prelude.<*> ( x Data..@? "ProcessorFeatures"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "ProcessorFeature")
                  )
      Prelude.<*> (x Data..@? "SnapshotCreateTime")
      Prelude.<*> (x Data..@? "SnapshotDatabaseTime")
      Prelude.<*> (x Data..@? "SnapshotTarget")
      Prelude.<*> (x Data..@? "SnapshotType")
      Prelude.<*> (x Data..@? "SourceDBSnapshotIdentifier")
      Prelude.<*> (x Data..@? "SourceRegion")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "StorageThroughput")
      Prelude.<*> (x Data..@? "StorageType")
      Prelude.<*> ( x Data..@? "TagList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )
      Prelude.<*> (x Data..@? "TdeCredentialArn")
      Prelude.<*> (x Data..@? "Timezone")
      Prelude.<*> (x Data..@? "VpcId")

instance Prelude.Hashable DBSnapshot where
  hashWithSalt _salt DBSnapshot' {..} =
    _salt `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbSnapshotArn
      `Prelude.hashWithSalt` dbSnapshotIdentifier
      `Prelude.hashWithSalt` dbiResourceId
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` iAMDatabaseAuthenticationEnabled
      `Prelude.hashWithSalt` instanceCreateTime
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` originalSnapshotCreateTime
      `Prelude.hashWithSalt` percentProgress
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` snapshotCreateTime
      `Prelude.hashWithSalt` snapshotDatabaseTime
      `Prelude.hashWithSalt` snapshotTarget
      `Prelude.hashWithSalt` snapshotType
      `Prelude.hashWithSalt` sourceDBSnapshotIdentifier
      `Prelude.hashWithSalt` sourceRegion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` storageThroughput
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` tdeCredentialArn
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData DBSnapshot where
  rnf DBSnapshot' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbSnapshotArn
      `Prelude.seq` Prelude.rnf dbSnapshotIdentifier
      `Prelude.seq` Prelude.rnf dbiResourceId
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf iAMDatabaseAuthenticationEnabled
      `Prelude.seq` Prelude.rnf instanceCreateTime
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf
        originalSnapshotCreateTime
      `Prelude.seq` Prelude.rnf percentProgress
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf processorFeatures
      `Prelude.seq` Prelude.rnf
        snapshotCreateTime
      `Prelude.seq` Prelude.rnf
        snapshotDatabaseTime
      `Prelude.seq` Prelude.rnf
        snapshotTarget
      `Prelude.seq` Prelude.rnf
        snapshotType
      `Prelude.seq` Prelude.rnf
        sourceDBSnapshotIdentifier
      `Prelude.seq` Prelude.rnf
        sourceRegion
      `Prelude.seq` Prelude.rnf
        status
      `Prelude.seq` Prelude.rnf
        storageThroughput
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tagList
      `Prelude.seq` Prelude.rnf
        tdeCredentialArn
      `Prelude.seq` Prelude.rnf
        timezone
      `Prelude.seq` Prelude.rnf
        vpcId
