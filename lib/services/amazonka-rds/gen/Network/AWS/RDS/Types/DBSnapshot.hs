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
-- Module      : Network.AWS.RDS.Types.DBSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSnapshot where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.ProcessorFeature
import Network.AWS.RDS.Types.Tag

-- | Contains the details of an Amazon RDS DB snapshot.
--
-- This data type is used as a response element in the
-- @DescribeDBSnapshots@ action.
--
-- /See:/ 'newDBSnapshot' smart constructor.
data DBSnapshot = DBSnapshot'
  { -- | Specifies the status of this DB snapshot.
    status :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time of the CreateDBSnapshot operation in Coordinated
    -- Universal Time (UTC). Doesn\'t change when the snapshot is copied.
    originalSnapshotCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | Specifies the storage type associated with DB snapshot.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the identifier for the DB snapshot.
    dbSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the DB snapshot is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | Provides the option group name for the DB snapshot.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time in Coordinated Universal Time (UTC) when the DB
    -- instance, from which the snapshot was taken, was created.
    instanceCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | The identifier for the source DB instance, which can\'t be changed and
    -- which is unique to an Amazon Web Services Region.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the snapshot was taken in Coordinated Universal Time
    -- (UTC). Changes for the copy when the snapshot is copied.
    snapshotCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | Provides the master username for the DB snapshot.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | If @Encrypted@ is true, the Amazon Web Services KMS key identifier for
    -- the encrypted DB snapshot.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK).
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the DB snapshot.
    dbSnapshotArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the Availability Zone the DB instance was located
    -- in at the time of the DB snapshot.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Specifies the version of the database engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Provides the type of the DB snapshot.
    snapshotType :: Prelude.Maybe Prelude.Text,
    -- | License model information for the restored DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was
    -- copied from. It only has a value in the case of a cross-account or
    -- cross-Region copy.
    sourceDBSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies the DB instance identifier of the DB instance this DB snapshot
    -- was created from.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance when the DB snapshot was created.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | Specifies the port that the database engine was listening on at the time
    -- of the snapshot.
    port :: Prelude.Maybe Prelude.Int,
    -- | The percentage of the estimated data that has been transferred.
    percentProgress :: Prelude.Maybe Prelude.Int,
    -- | Specifies the name of the database engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the DB snapshot. In most cases, the @Timezone@ element
    -- is empty. @Timezone@ content appears only for snapshots taken from
    -- Microsoft SQL Server DB instances that were created with a time zone
    -- specified.
    timezone :: Prelude.Maybe Prelude.Text,
    tagList :: Prelude.Maybe [Tag],
    -- | Provides the VPC ID associated with the DB snapshot.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Specifies the Provisioned IOPS (I\/O operations per second) value of the
    -- DB instance at the time of the snapshot.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services Region that the DB snapshot was created in or
    -- copied from.
    sourceRegion :: Prelude.Maybe Prelude.Text,
    -- | True if mapping of Amazon Web Services Identity and Access Management
    -- (IAM) accounts to database accounts is enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool
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
-- 'status', 'dbSnapshot_status' - Specifies the status of this DB snapshot.
--
-- 'originalSnapshotCreateTime', 'dbSnapshot_originalSnapshotCreateTime' - Specifies the time of the CreateDBSnapshot operation in Coordinated
-- Universal Time (UTC). Doesn\'t change when the snapshot is copied.
--
-- 'storageType', 'dbSnapshot_storageType' - Specifies the storage type associated with DB snapshot.
--
-- 'dbSnapshotIdentifier', 'dbSnapshot_dbSnapshotIdentifier' - Specifies the identifier for the DB snapshot.
--
-- 'encrypted', 'dbSnapshot_encrypted' - Specifies whether the DB snapshot is encrypted.
--
-- 'optionGroupName', 'dbSnapshot_optionGroupName' - Provides the option group name for the DB snapshot.
--
-- 'instanceCreateTime', 'dbSnapshot_instanceCreateTime' - Specifies the time in Coordinated Universal Time (UTC) when the DB
-- instance, from which the snapshot was taken, was created.
--
-- 'dbiResourceId', 'dbSnapshot_dbiResourceId' - The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an Amazon Web Services Region.
--
-- 'snapshotCreateTime', 'dbSnapshot_snapshotCreateTime' - Specifies when the snapshot was taken in Coordinated Universal Time
-- (UTC). Changes for the copy when the snapshot is copied.
--
-- 'masterUsername', 'dbSnapshot_masterUsername' - Provides the master username for the DB snapshot.
--
-- 'kmsKeyId', 'dbSnapshot_kmsKeyId' - If @Encrypted@ is true, the Amazon Web Services KMS key identifier for
-- the encrypted DB snapshot.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
--
-- 'dbSnapshotArn', 'dbSnapshot_dbSnapshotArn' - The Amazon Resource Name (ARN) for the DB snapshot.
--
-- 'availabilityZone', 'dbSnapshot_availabilityZone' - Specifies the name of the Availability Zone the DB instance was located
-- in at the time of the DB snapshot.
--
-- 'engineVersion', 'dbSnapshot_engineVersion' - Specifies the version of the database engine.
--
-- 'snapshotType', 'dbSnapshot_snapshotType' - Provides the type of the DB snapshot.
--
-- 'licenseModel', 'dbSnapshot_licenseModel' - License model information for the restored DB instance.
--
-- 'sourceDBSnapshotIdentifier', 'dbSnapshot_sourceDBSnapshotIdentifier' - The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was
-- copied from. It only has a value in the case of a cross-account or
-- cross-Region copy.
--
-- 'dbInstanceIdentifier', 'dbSnapshot_dbInstanceIdentifier' - Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
--
-- 'processorFeatures', 'dbSnapshot_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance when the DB snapshot was created.
--
-- 'port', 'dbSnapshot_port' - Specifies the port that the database engine was listening on at the time
-- of the snapshot.
--
-- 'percentProgress', 'dbSnapshot_percentProgress' - The percentage of the estimated data that has been transferred.
--
-- 'engine', 'dbSnapshot_engine' - Specifies the name of the database engine.
--
-- 'tdeCredentialArn', 'dbSnapshot_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- 'timezone', 'dbSnapshot_timezone' - The time zone of the DB snapshot. In most cases, the @Timezone@ element
-- is empty. @Timezone@ content appears only for snapshots taken from
-- Microsoft SQL Server DB instances that were created with a time zone
-- specified.
--
-- 'tagList', 'dbSnapshot_tagList' - Undocumented member.
--
-- 'vpcId', 'dbSnapshot_vpcId' - Provides the VPC ID associated with the DB snapshot.
--
-- 'allocatedStorage', 'dbSnapshot_allocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- 'iops', 'dbSnapshot_iops' - Specifies the Provisioned IOPS (I\/O operations per second) value of the
-- DB instance at the time of the snapshot.
--
-- 'sourceRegion', 'dbSnapshot_sourceRegion' - The Amazon Web Services Region that the DB snapshot was created in or
-- copied from.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbSnapshot_iAMDatabaseAuthenticationEnabled' - True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
newDBSnapshot ::
  DBSnapshot
newDBSnapshot =
  DBSnapshot'
    { status = Prelude.Nothing,
      originalSnapshotCreateTime = Prelude.Nothing,
      storageType = Prelude.Nothing,
      dbSnapshotIdentifier = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      dbSnapshotArn = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      snapshotType = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      sourceDBSnapshotIdentifier = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      port = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      engine = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      timezone = Prelude.Nothing,
      tagList = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      iops = Prelude.Nothing,
      sourceRegion = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled = Prelude.Nothing
    }

-- | Specifies the status of this DB snapshot.
dbSnapshot_status :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_status = Lens.lens (\DBSnapshot' {status} -> status) (\s@DBSnapshot' {} a -> s {status = a} :: DBSnapshot)

-- | Specifies the time of the CreateDBSnapshot operation in Coordinated
-- Universal Time (UTC). Doesn\'t change when the snapshot is copied.
dbSnapshot_originalSnapshotCreateTime :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.UTCTime)
dbSnapshot_originalSnapshotCreateTime = Lens.lens (\DBSnapshot' {originalSnapshotCreateTime} -> originalSnapshotCreateTime) (\s@DBSnapshot' {} a -> s {originalSnapshotCreateTime = a} :: DBSnapshot) Prelude.. Lens.mapping Core._Time

-- | Specifies the storage type associated with DB snapshot.
dbSnapshot_storageType :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_storageType = Lens.lens (\DBSnapshot' {storageType} -> storageType) (\s@DBSnapshot' {} a -> s {storageType = a} :: DBSnapshot)

-- | Specifies the identifier for the DB snapshot.
dbSnapshot_dbSnapshotIdentifier :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_dbSnapshotIdentifier = Lens.lens (\DBSnapshot' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@DBSnapshot' {} a -> s {dbSnapshotIdentifier = a} :: DBSnapshot)

-- | Specifies whether the DB snapshot is encrypted.
dbSnapshot_encrypted :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Bool)
dbSnapshot_encrypted = Lens.lens (\DBSnapshot' {encrypted} -> encrypted) (\s@DBSnapshot' {} a -> s {encrypted = a} :: DBSnapshot)

-- | Provides the option group name for the DB snapshot.
dbSnapshot_optionGroupName :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_optionGroupName = Lens.lens (\DBSnapshot' {optionGroupName} -> optionGroupName) (\s@DBSnapshot' {} a -> s {optionGroupName = a} :: DBSnapshot)

-- | Specifies the time in Coordinated Universal Time (UTC) when the DB
-- instance, from which the snapshot was taken, was created.
dbSnapshot_instanceCreateTime :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.UTCTime)
dbSnapshot_instanceCreateTime = Lens.lens (\DBSnapshot' {instanceCreateTime} -> instanceCreateTime) (\s@DBSnapshot' {} a -> s {instanceCreateTime = a} :: DBSnapshot) Prelude.. Lens.mapping Core._Time

-- | The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an Amazon Web Services Region.
dbSnapshot_dbiResourceId :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_dbiResourceId = Lens.lens (\DBSnapshot' {dbiResourceId} -> dbiResourceId) (\s@DBSnapshot' {} a -> s {dbiResourceId = a} :: DBSnapshot)

-- | Specifies when the snapshot was taken in Coordinated Universal Time
-- (UTC). Changes for the copy when the snapshot is copied.
dbSnapshot_snapshotCreateTime :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.UTCTime)
dbSnapshot_snapshotCreateTime = Lens.lens (\DBSnapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@DBSnapshot' {} a -> s {snapshotCreateTime = a} :: DBSnapshot) Prelude.. Lens.mapping Core._Time

-- | Provides the master username for the DB snapshot.
dbSnapshot_masterUsername :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_masterUsername = Lens.lens (\DBSnapshot' {masterUsername} -> masterUsername) (\s@DBSnapshot' {} a -> s {masterUsername = a} :: DBSnapshot)

-- | If @Encrypted@ is true, the Amazon Web Services KMS key identifier for
-- the encrypted DB snapshot.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
dbSnapshot_kmsKeyId :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_kmsKeyId = Lens.lens (\DBSnapshot' {kmsKeyId} -> kmsKeyId) (\s@DBSnapshot' {} a -> s {kmsKeyId = a} :: DBSnapshot)

-- | The Amazon Resource Name (ARN) for the DB snapshot.
dbSnapshot_dbSnapshotArn :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_dbSnapshotArn = Lens.lens (\DBSnapshot' {dbSnapshotArn} -> dbSnapshotArn) (\s@DBSnapshot' {} a -> s {dbSnapshotArn = a} :: DBSnapshot)

-- | Specifies the name of the Availability Zone the DB instance was located
-- in at the time of the DB snapshot.
dbSnapshot_availabilityZone :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_availabilityZone = Lens.lens (\DBSnapshot' {availabilityZone} -> availabilityZone) (\s@DBSnapshot' {} a -> s {availabilityZone = a} :: DBSnapshot)

-- | Specifies the version of the database engine.
dbSnapshot_engineVersion :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_engineVersion = Lens.lens (\DBSnapshot' {engineVersion} -> engineVersion) (\s@DBSnapshot' {} a -> s {engineVersion = a} :: DBSnapshot)

-- | Provides the type of the DB snapshot.
dbSnapshot_snapshotType :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_snapshotType = Lens.lens (\DBSnapshot' {snapshotType} -> snapshotType) (\s@DBSnapshot' {} a -> s {snapshotType = a} :: DBSnapshot)

-- | License model information for the restored DB instance.
dbSnapshot_licenseModel :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_licenseModel = Lens.lens (\DBSnapshot' {licenseModel} -> licenseModel) (\s@DBSnapshot' {} a -> s {licenseModel = a} :: DBSnapshot)

-- | The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was
-- copied from. It only has a value in the case of a cross-account or
-- cross-Region copy.
dbSnapshot_sourceDBSnapshotIdentifier :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_sourceDBSnapshotIdentifier = Lens.lens (\DBSnapshot' {sourceDBSnapshotIdentifier} -> sourceDBSnapshotIdentifier) (\s@DBSnapshot' {} a -> s {sourceDBSnapshotIdentifier = a} :: DBSnapshot)

-- | Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
dbSnapshot_dbInstanceIdentifier :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_dbInstanceIdentifier = Lens.lens (\DBSnapshot' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBSnapshot' {} a -> s {dbInstanceIdentifier = a} :: DBSnapshot)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance when the DB snapshot was created.
dbSnapshot_processorFeatures :: Lens.Lens' DBSnapshot (Prelude.Maybe [ProcessorFeature])
dbSnapshot_processorFeatures = Lens.lens (\DBSnapshot' {processorFeatures} -> processorFeatures) (\s@DBSnapshot' {} a -> s {processorFeatures = a} :: DBSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the port that the database engine was listening on at the time
-- of the snapshot.
dbSnapshot_port :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Int)
dbSnapshot_port = Lens.lens (\DBSnapshot' {port} -> port) (\s@DBSnapshot' {} a -> s {port = a} :: DBSnapshot)

-- | The percentage of the estimated data that has been transferred.
dbSnapshot_percentProgress :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Int)
dbSnapshot_percentProgress = Lens.lens (\DBSnapshot' {percentProgress} -> percentProgress) (\s@DBSnapshot' {} a -> s {percentProgress = a} :: DBSnapshot)

-- | Specifies the name of the database engine.
dbSnapshot_engine :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_engine = Lens.lens (\DBSnapshot' {engine} -> engine) (\s@DBSnapshot' {} a -> s {engine = a} :: DBSnapshot)

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

-- | Undocumented member.
dbSnapshot_tagList :: Lens.Lens' DBSnapshot (Prelude.Maybe [Tag])
dbSnapshot_tagList = Lens.lens (\DBSnapshot' {tagList} -> tagList) (\s@DBSnapshot' {} a -> s {tagList = a} :: DBSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | Provides the VPC ID associated with the DB snapshot.
dbSnapshot_vpcId :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_vpcId = Lens.lens (\DBSnapshot' {vpcId} -> vpcId) (\s@DBSnapshot' {} a -> s {vpcId = a} :: DBSnapshot)

-- | Specifies the allocated storage size in gibibytes (GiB).
dbSnapshot_allocatedStorage :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Int)
dbSnapshot_allocatedStorage = Lens.lens (\DBSnapshot' {allocatedStorage} -> allocatedStorage) (\s@DBSnapshot' {} a -> s {allocatedStorage = a} :: DBSnapshot)

-- | Specifies the Provisioned IOPS (I\/O operations per second) value of the
-- DB instance at the time of the snapshot.
dbSnapshot_iops :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Int)
dbSnapshot_iops = Lens.lens (\DBSnapshot' {iops} -> iops) (\s@DBSnapshot' {} a -> s {iops = a} :: DBSnapshot)

-- | The Amazon Web Services Region that the DB snapshot was created in or
-- copied from.
dbSnapshot_sourceRegion :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Text)
dbSnapshot_sourceRegion = Lens.lens (\DBSnapshot' {sourceRegion} -> sourceRegion) (\s@DBSnapshot' {} a -> s {sourceRegion = a} :: DBSnapshot)

-- | True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
dbSnapshot_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBSnapshot (Prelude.Maybe Prelude.Bool)
dbSnapshot_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBSnapshot' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBSnapshot' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBSnapshot)

instance Core.FromXML DBSnapshot where
  parseXML x =
    DBSnapshot'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "OriginalSnapshotCreateTime")
      Prelude.<*> (x Core..@? "StorageType")
      Prelude.<*> (x Core..@? "DBSnapshotIdentifier")
      Prelude.<*> (x Core..@? "Encrypted")
      Prelude.<*> (x Core..@? "OptionGroupName")
      Prelude.<*> (x Core..@? "InstanceCreateTime")
      Prelude.<*> (x Core..@? "DbiResourceId")
      Prelude.<*> (x Core..@? "SnapshotCreateTime")
      Prelude.<*> (x Core..@? "MasterUsername")
      Prelude.<*> (x Core..@? "KmsKeyId")
      Prelude.<*> (x Core..@? "DBSnapshotArn")
      Prelude.<*> (x Core..@? "AvailabilityZone")
      Prelude.<*> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "SnapshotType")
      Prelude.<*> (x Core..@? "LicenseModel")
      Prelude.<*> (x Core..@? "SourceDBSnapshotIdentifier")
      Prelude.<*> (x Core..@? "DBInstanceIdentifier")
      Prelude.<*> ( x Core..@? "ProcessorFeatures"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "ProcessorFeature")
                  )
      Prelude.<*> (x Core..@? "Port")
      Prelude.<*> (x Core..@? "PercentProgress")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "TdeCredentialArn")
      Prelude.<*> (x Core..@? "Timezone")
      Prelude.<*> ( x Core..@? "TagList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> (x Core..@? "VpcId")
      Prelude.<*> (x Core..@? "AllocatedStorage")
      Prelude.<*> (x Core..@? "Iops")
      Prelude.<*> (x Core..@? "SourceRegion")
      Prelude.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")

instance Prelude.Hashable DBSnapshot

instance Prelude.NFData DBSnapshot
