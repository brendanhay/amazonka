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
    status :: Core.Maybe Core.Text,
    -- | Specifies the storage type associated with DB snapshot.
    storageType :: Core.Maybe Core.Text,
    -- | Specifies the identifier for the DB snapshot.
    dbSnapshotIdentifier :: Core.Maybe Core.Text,
    -- | Specifies whether the DB snapshot is encrypted.
    encrypted :: Core.Maybe Core.Bool,
    -- | Provides the option group name for the DB snapshot.
    optionGroupName :: Core.Maybe Core.Text,
    -- | Specifies the time in Coordinated Universal Time (UTC) when the DB
    -- instance, from which the snapshot was taken, was created.
    instanceCreateTime :: Core.Maybe Core.ISO8601,
    -- | The identifier for the source DB instance, which can\'t be changed and
    -- which is unique to an AWS Region.
    dbiResourceId :: Core.Maybe Core.Text,
    -- | Specifies when the snapshot was taken in Coordinated Universal Time
    -- (UTC).
    snapshotCreateTime :: Core.Maybe Core.ISO8601,
    -- | Provides the master username for the DB snapshot.
    masterUsername :: Core.Maybe Core.Text,
    -- | If @Encrypted@ is true, the AWS KMS key identifier for the encrypted DB
    -- snapshot.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Specifies the name of the Availability Zone the DB instance was located
    -- in at the time of the DB snapshot.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the DB snapshot.
    dbSnapshotArn :: Core.Maybe Core.Text,
    -- | Specifies the version of the database engine.
    engineVersion :: Core.Maybe Core.Text,
    -- | Provides the type of the DB snapshot.
    snapshotType :: Core.Maybe Core.Text,
    -- | The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was
    -- copied from. It only has value in case of cross-customer or cross-region
    -- copy.
    sourceDBSnapshotIdentifier :: Core.Maybe Core.Text,
    -- | License model information for the restored DB instance.
    licenseModel :: Core.Maybe Core.Text,
    -- | Specifies the DB instance identifier of the DB instance this DB snapshot
    -- was created from.
    dbInstanceIdentifier :: Core.Maybe Core.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance when the DB snapshot was created.
    processorFeatures :: Core.Maybe [ProcessorFeature],
    -- | Specifies the port that the database engine was listening on at the time
    -- of the snapshot.
    port :: Core.Maybe Core.Int,
    -- | The percentage of the estimated data that has been transferred.
    percentProgress :: Core.Maybe Core.Int,
    -- | Specifies the name of the database engine.
    engine :: Core.Maybe Core.Text,
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Core.Maybe Core.Text,
    -- | The time zone of the DB snapshot. In most cases, the @Timezone@ element
    -- is empty. @Timezone@ content appears only for snapshots taken from
    -- Microsoft SQL Server DB instances that were created with a time zone
    -- specified.
    timezone :: Core.Maybe Core.Text,
    tagList :: Core.Maybe [Tag],
    -- | Provides the VPC ID associated with the DB snapshot.
    vpcId :: Core.Maybe Core.Text,
    -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Core.Maybe Core.Int,
    -- | The AWS Region that the DB snapshot was created in or copied from.
    sourceRegion :: Core.Maybe Core.Text,
    -- | True if mapping of AWS Identity and Access Management (IAM) accounts to
    -- database accounts is enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool,
    -- | Specifies the Provisioned IOPS (I\/O operations per second) value of the
    -- DB instance at the time of the snapshot.
    iops :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- which is unique to an AWS Region.
--
-- 'snapshotCreateTime', 'dbSnapshot_snapshotCreateTime' - Specifies when the snapshot was taken in Coordinated Universal Time
-- (UTC).
--
-- 'masterUsername', 'dbSnapshot_masterUsername' - Provides the master username for the DB snapshot.
--
-- 'kmsKeyId', 'dbSnapshot_kmsKeyId' - If @Encrypted@ is true, the AWS KMS key identifier for the encrypted DB
-- snapshot.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- 'availabilityZone', 'dbSnapshot_availabilityZone' - Specifies the name of the Availability Zone the DB instance was located
-- in at the time of the DB snapshot.
--
-- 'dbSnapshotArn', 'dbSnapshot_dbSnapshotArn' - The Amazon Resource Name (ARN) for the DB snapshot.
--
-- 'engineVersion', 'dbSnapshot_engineVersion' - Specifies the version of the database engine.
--
-- 'snapshotType', 'dbSnapshot_snapshotType' - Provides the type of the DB snapshot.
--
-- 'sourceDBSnapshotIdentifier', 'dbSnapshot_sourceDBSnapshotIdentifier' - The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was
-- copied from. It only has value in case of cross-customer or cross-region
-- copy.
--
-- 'licenseModel', 'dbSnapshot_licenseModel' - License model information for the restored DB instance.
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
-- 'sourceRegion', 'dbSnapshot_sourceRegion' - The AWS Region that the DB snapshot was created in or copied from.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbSnapshot_iAMDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to
-- database accounts is enabled, and otherwise false.
--
-- 'iops', 'dbSnapshot_iops' - Specifies the Provisioned IOPS (I\/O operations per second) value of the
-- DB instance at the time of the snapshot.
newDBSnapshot ::
  DBSnapshot
newDBSnapshot =
  DBSnapshot'
    { status = Core.Nothing,
      storageType = Core.Nothing,
      dbSnapshotIdentifier = Core.Nothing,
      encrypted = Core.Nothing,
      optionGroupName = Core.Nothing,
      instanceCreateTime = Core.Nothing,
      dbiResourceId = Core.Nothing,
      snapshotCreateTime = Core.Nothing,
      masterUsername = Core.Nothing,
      kmsKeyId = Core.Nothing,
      availabilityZone = Core.Nothing,
      dbSnapshotArn = Core.Nothing,
      engineVersion = Core.Nothing,
      snapshotType = Core.Nothing,
      sourceDBSnapshotIdentifier = Core.Nothing,
      licenseModel = Core.Nothing,
      dbInstanceIdentifier = Core.Nothing,
      processorFeatures = Core.Nothing,
      port = Core.Nothing,
      percentProgress = Core.Nothing,
      engine = Core.Nothing,
      tdeCredentialArn = Core.Nothing,
      timezone = Core.Nothing,
      tagList = Core.Nothing,
      vpcId = Core.Nothing,
      allocatedStorage = Core.Nothing,
      sourceRegion = Core.Nothing,
      iAMDatabaseAuthenticationEnabled = Core.Nothing,
      iops = Core.Nothing
    }

-- | Specifies the status of this DB snapshot.
dbSnapshot_status :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_status = Lens.lens (\DBSnapshot' {status} -> status) (\s@DBSnapshot' {} a -> s {status = a} :: DBSnapshot)

-- | Specifies the storage type associated with DB snapshot.
dbSnapshot_storageType :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_storageType = Lens.lens (\DBSnapshot' {storageType} -> storageType) (\s@DBSnapshot' {} a -> s {storageType = a} :: DBSnapshot)

-- | Specifies the identifier for the DB snapshot.
dbSnapshot_dbSnapshotIdentifier :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_dbSnapshotIdentifier = Lens.lens (\DBSnapshot' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@DBSnapshot' {} a -> s {dbSnapshotIdentifier = a} :: DBSnapshot)

-- | Specifies whether the DB snapshot is encrypted.
dbSnapshot_encrypted :: Lens.Lens' DBSnapshot (Core.Maybe Core.Bool)
dbSnapshot_encrypted = Lens.lens (\DBSnapshot' {encrypted} -> encrypted) (\s@DBSnapshot' {} a -> s {encrypted = a} :: DBSnapshot)

-- | Provides the option group name for the DB snapshot.
dbSnapshot_optionGroupName :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_optionGroupName = Lens.lens (\DBSnapshot' {optionGroupName} -> optionGroupName) (\s@DBSnapshot' {} a -> s {optionGroupName = a} :: DBSnapshot)

-- | Specifies the time in Coordinated Universal Time (UTC) when the DB
-- instance, from which the snapshot was taken, was created.
dbSnapshot_instanceCreateTime :: Lens.Lens' DBSnapshot (Core.Maybe Core.UTCTime)
dbSnapshot_instanceCreateTime = Lens.lens (\DBSnapshot' {instanceCreateTime} -> instanceCreateTime) (\s@DBSnapshot' {} a -> s {instanceCreateTime = a} :: DBSnapshot) Core.. Lens.mapping Core._Time

-- | The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an AWS Region.
dbSnapshot_dbiResourceId :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_dbiResourceId = Lens.lens (\DBSnapshot' {dbiResourceId} -> dbiResourceId) (\s@DBSnapshot' {} a -> s {dbiResourceId = a} :: DBSnapshot)

-- | Specifies when the snapshot was taken in Coordinated Universal Time
-- (UTC).
dbSnapshot_snapshotCreateTime :: Lens.Lens' DBSnapshot (Core.Maybe Core.UTCTime)
dbSnapshot_snapshotCreateTime = Lens.lens (\DBSnapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@DBSnapshot' {} a -> s {snapshotCreateTime = a} :: DBSnapshot) Core.. Lens.mapping Core._Time

-- | Provides the master username for the DB snapshot.
dbSnapshot_masterUsername :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_masterUsername = Lens.lens (\DBSnapshot' {masterUsername} -> masterUsername) (\s@DBSnapshot' {} a -> s {masterUsername = a} :: DBSnapshot)

-- | If @Encrypted@ is true, the AWS KMS key identifier for the encrypted DB
-- snapshot.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
dbSnapshot_kmsKeyId :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_kmsKeyId = Lens.lens (\DBSnapshot' {kmsKeyId} -> kmsKeyId) (\s@DBSnapshot' {} a -> s {kmsKeyId = a} :: DBSnapshot)

-- | Specifies the name of the Availability Zone the DB instance was located
-- in at the time of the DB snapshot.
dbSnapshot_availabilityZone :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_availabilityZone = Lens.lens (\DBSnapshot' {availabilityZone} -> availabilityZone) (\s@DBSnapshot' {} a -> s {availabilityZone = a} :: DBSnapshot)

-- | The Amazon Resource Name (ARN) for the DB snapshot.
dbSnapshot_dbSnapshotArn :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_dbSnapshotArn = Lens.lens (\DBSnapshot' {dbSnapshotArn} -> dbSnapshotArn) (\s@DBSnapshot' {} a -> s {dbSnapshotArn = a} :: DBSnapshot)

-- | Specifies the version of the database engine.
dbSnapshot_engineVersion :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_engineVersion = Lens.lens (\DBSnapshot' {engineVersion} -> engineVersion) (\s@DBSnapshot' {} a -> s {engineVersion = a} :: DBSnapshot)

-- | Provides the type of the DB snapshot.
dbSnapshot_snapshotType :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_snapshotType = Lens.lens (\DBSnapshot' {snapshotType} -> snapshotType) (\s@DBSnapshot' {} a -> s {snapshotType = a} :: DBSnapshot)

-- | The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was
-- copied from. It only has value in case of cross-customer or cross-region
-- copy.
dbSnapshot_sourceDBSnapshotIdentifier :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_sourceDBSnapshotIdentifier = Lens.lens (\DBSnapshot' {sourceDBSnapshotIdentifier} -> sourceDBSnapshotIdentifier) (\s@DBSnapshot' {} a -> s {sourceDBSnapshotIdentifier = a} :: DBSnapshot)

-- | License model information for the restored DB instance.
dbSnapshot_licenseModel :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_licenseModel = Lens.lens (\DBSnapshot' {licenseModel} -> licenseModel) (\s@DBSnapshot' {} a -> s {licenseModel = a} :: DBSnapshot)

-- | Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
dbSnapshot_dbInstanceIdentifier :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_dbInstanceIdentifier = Lens.lens (\DBSnapshot' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBSnapshot' {} a -> s {dbInstanceIdentifier = a} :: DBSnapshot)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance when the DB snapshot was created.
dbSnapshot_processorFeatures :: Lens.Lens' DBSnapshot (Core.Maybe [ProcessorFeature])
dbSnapshot_processorFeatures = Lens.lens (\DBSnapshot' {processorFeatures} -> processorFeatures) (\s@DBSnapshot' {} a -> s {processorFeatures = a} :: DBSnapshot) Core.. Lens.mapping Lens._Coerce

-- | Specifies the port that the database engine was listening on at the time
-- of the snapshot.
dbSnapshot_port :: Lens.Lens' DBSnapshot (Core.Maybe Core.Int)
dbSnapshot_port = Lens.lens (\DBSnapshot' {port} -> port) (\s@DBSnapshot' {} a -> s {port = a} :: DBSnapshot)

-- | The percentage of the estimated data that has been transferred.
dbSnapshot_percentProgress :: Lens.Lens' DBSnapshot (Core.Maybe Core.Int)
dbSnapshot_percentProgress = Lens.lens (\DBSnapshot' {percentProgress} -> percentProgress) (\s@DBSnapshot' {} a -> s {percentProgress = a} :: DBSnapshot)

-- | Specifies the name of the database engine.
dbSnapshot_engine :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_engine = Lens.lens (\DBSnapshot' {engine} -> engine) (\s@DBSnapshot' {} a -> s {engine = a} :: DBSnapshot)

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
dbSnapshot_tdeCredentialArn :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_tdeCredentialArn = Lens.lens (\DBSnapshot' {tdeCredentialArn} -> tdeCredentialArn) (\s@DBSnapshot' {} a -> s {tdeCredentialArn = a} :: DBSnapshot)

-- | The time zone of the DB snapshot. In most cases, the @Timezone@ element
-- is empty. @Timezone@ content appears only for snapshots taken from
-- Microsoft SQL Server DB instances that were created with a time zone
-- specified.
dbSnapshot_timezone :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_timezone = Lens.lens (\DBSnapshot' {timezone} -> timezone) (\s@DBSnapshot' {} a -> s {timezone = a} :: DBSnapshot)

-- | Undocumented member.
dbSnapshot_tagList :: Lens.Lens' DBSnapshot (Core.Maybe [Tag])
dbSnapshot_tagList = Lens.lens (\DBSnapshot' {tagList} -> tagList) (\s@DBSnapshot' {} a -> s {tagList = a} :: DBSnapshot) Core.. Lens.mapping Lens._Coerce

-- | Provides the VPC ID associated with the DB snapshot.
dbSnapshot_vpcId :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_vpcId = Lens.lens (\DBSnapshot' {vpcId} -> vpcId) (\s@DBSnapshot' {} a -> s {vpcId = a} :: DBSnapshot)

-- | Specifies the allocated storage size in gibibytes (GiB).
dbSnapshot_allocatedStorage :: Lens.Lens' DBSnapshot (Core.Maybe Core.Int)
dbSnapshot_allocatedStorage = Lens.lens (\DBSnapshot' {allocatedStorage} -> allocatedStorage) (\s@DBSnapshot' {} a -> s {allocatedStorage = a} :: DBSnapshot)

-- | The AWS Region that the DB snapshot was created in or copied from.
dbSnapshot_sourceRegion :: Lens.Lens' DBSnapshot (Core.Maybe Core.Text)
dbSnapshot_sourceRegion = Lens.lens (\DBSnapshot' {sourceRegion} -> sourceRegion) (\s@DBSnapshot' {} a -> s {sourceRegion = a} :: DBSnapshot)

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to
-- database accounts is enabled, and otherwise false.
dbSnapshot_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBSnapshot (Core.Maybe Core.Bool)
dbSnapshot_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBSnapshot' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBSnapshot' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBSnapshot)

-- | Specifies the Provisioned IOPS (I\/O operations per second) value of the
-- DB instance at the time of the snapshot.
dbSnapshot_iops :: Lens.Lens' DBSnapshot (Core.Maybe Core.Int)
dbSnapshot_iops = Lens.lens (\DBSnapshot' {iops} -> iops) (\s@DBSnapshot' {} a -> s {iops = a} :: DBSnapshot)

instance Core.FromXML DBSnapshot where
  parseXML x =
    DBSnapshot'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "StorageType")
      Core.<*> (x Core..@? "DBSnapshotIdentifier")
      Core.<*> (x Core..@? "Encrypted")
      Core.<*> (x Core..@? "OptionGroupName")
      Core.<*> (x Core..@? "InstanceCreateTime")
      Core.<*> (x Core..@? "DbiResourceId")
      Core.<*> (x Core..@? "SnapshotCreateTime")
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "AvailabilityZone")
      Core.<*> (x Core..@? "DBSnapshotArn")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "SnapshotType")
      Core.<*> (x Core..@? "SourceDBSnapshotIdentifier")
      Core.<*> (x Core..@? "LicenseModel")
      Core.<*> (x Core..@? "DBInstanceIdentifier")
      Core.<*> ( x Core..@? "ProcessorFeatures" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "ProcessorFeature")
               )
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "PercentProgress")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "TdeCredentialArn")
      Core.<*> (x Core..@? "Timezone")
      Core.<*> ( x Core..@? "TagList" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )
      Core.<*> (x Core..@? "VpcId")
      Core.<*> (x Core..@? "AllocatedStorage")
      Core.<*> (x Core..@? "SourceRegion")
      Core.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Core.<*> (x Core..@? "Iops")

instance Core.Hashable DBSnapshot

instance Core.NFData DBSnapshot
