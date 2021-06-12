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
-- Module      : Network.AWS.RDS.Types.DBClusterSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterSnapshot where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.Tag

-- | Contains the details for an Amazon RDS DB cluster snapshot
--
-- This data type is used as a response element in the
-- @DescribeDBClusterSnapshots@ action.
--
-- /See:/ 'newDBClusterSnapshot' smart constructor.
data DBClusterSnapshot = DBClusterSnapshot'
  { -- | Specifies whether the DB cluster snapshot is encrypted.
    storageEncrypted :: Core.Maybe Core.Bool,
    -- | Specifies the status of this DB cluster snapshot.
    status :: Core.Maybe Core.Text,
    -- | Provides the list of Availability Zones (AZs) where instances in the DB
    -- cluster snapshot can be restored.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | Specifies the time when the DB cluster was created, in Universal
    -- Coordinated Time (UTC).
    clusterCreateTime :: Core.Maybe Core.ISO8601,
    -- | Provides the time when the snapshot was taken, in Universal Coordinated
    -- Time (UTC).
    snapshotCreateTime :: Core.Maybe Core.ISO8601,
    -- | Provides the engine mode of the database engine for this DB cluster
    -- snapshot.
    engineMode :: Core.Maybe Core.Text,
    -- | Provides the master username for this DB cluster snapshot.
    masterUsername :: Core.Maybe Core.Text,
    -- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the
    -- encrypted DB cluster snapshot.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Specifies the DB cluster identifier of the DB cluster that this DB
    -- cluster snapshot was created from.
    dbClusterIdentifier :: Core.Maybe Core.Text,
    -- | Provides the version of the database engine for this DB cluster
    -- snapshot.
    engineVersion :: Core.Maybe Core.Text,
    -- | Provides the type of the DB cluster snapshot.
    snapshotType :: Core.Maybe Core.Text,
    -- | Provides the license model information for this DB cluster snapshot.
    licenseModel :: Core.Maybe Core.Text,
    -- | Specifies the port that the DB cluster was listening on at the time of
    -- the snapshot.
    port :: Core.Maybe Core.Int,
    -- | Specifies the percentage of the estimated data that has been
    -- transferred.
    percentProgress :: Core.Maybe Core.Int,
    -- | Specifies the name of the database engine for this DB cluster snapshot.
    engine :: Core.Maybe Core.Text,
    -- | Specifies the identifier for the DB cluster snapshot.
    dbClusterSnapshotIdentifier :: Core.Maybe Core.Text,
    -- | If the DB cluster snapshot was copied from a source DB cluster snapshot,
    -- the Amazon Resource Name (ARN) for the source DB cluster snapshot,
    -- otherwise, a null value.
    sourceDBClusterSnapshotArn :: Core.Maybe Core.Text,
    tagList :: Core.Maybe [Tag],
    -- | Provides the VPC ID associated with the DB cluster snapshot.
    vpcId :: Core.Maybe Core.Text,
    -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Core.Maybe Core.Int,
    -- | True if mapping of AWS Identity and Access Management (IAM) accounts to
    -- database accounts is enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) for the DB cluster snapshot.
    dbClusterSnapshotArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageEncrypted', 'dbClusterSnapshot_storageEncrypted' - Specifies whether the DB cluster snapshot is encrypted.
--
-- 'status', 'dbClusterSnapshot_status' - Specifies the status of this DB cluster snapshot.
--
-- 'availabilityZones', 'dbClusterSnapshot_availabilityZones' - Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster snapshot can be restored.
--
-- 'clusterCreateTime', 'dbClusterSnapshot_clusterCreateTime' - Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
--
-- 'snapshotCreateTime', 'dbClusterSnapshot_snapshotCreateTime' - Provides the time when the snapshot was taken, in Universal Coordinated
-- Time (UTC).
--
-- 'engineMode', 'dbClusterSnapshot_engineMode' - Provides the engine mode of the database engine for this DB cluster
-- snapshot.
--
-- 'masterUsername', 'dbClusterSnapshot_masterUsername' - Provides the master username for this DB cluster snapshot.
--
-- 'kmsKeyId', 'dbClusterSnapshot_kmsKeyId' - If @StorageEncrypted@ is true, the AWS KMS key identifier for the
-- encrypted DB cluster snapshot.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- 'dbClusterIdentifier', 'dbClusterSnapshot_dbClusterIdentifier' - Specifies the DB cluster identifier of the DB cluster that this DB
-- cluster snapshot was created from.
--
-- 'engineVersion', 'dbClusterSnapshot_engineVersion' - Provides the version of the database engine for this DB cluster
-- snapshot.
--
-- 'snapshotType', 'dbClusterSnapshot_snapshotType' - Provides the type of the DB cluster snapshot.
--
-- 'licenseModel', 'dbClusterSnapshot_licenseModel' - Provides the license model information for this DB cluster snapshot.
--
-- 'port', 'dbClusterSnapshot_port' - Specifies the port that the DB cluster was listening on at the time of
-- the snapshot.
--
-- 'percentProgress', 'dbClusterSnapshot_percentProgress' - Specifies the percentage of the estimated data that has been
-- transferred.
--
-- 'engine', 'dbClusterSnapshot_engine' - Specifies the name of the database engine for this DB cluster snapshot.
--
-- 'dbClusterSnapshotIdentifier', 'dbClusterSnapshot_dbClusterSnapshotIdentifier' - Specifies the identifier for the DB cluster snapshot.
--
-- 'sourceDBClusterSnapshotArn', 'dbClusterSnapshot_sourceDBClusterSnapshotArn' - If the DB cluster snapshot was copied from a source DB cluster snapshot,
-- the Amazon Resource Name (ARN) for the source DB cluster snapshot,
-- otherwise, a null value.
--
-- 'tagList', 'dbClusterSnapshot_tagList' - Undocumented member.
--
-- 'vpcId', 'dbClusterSnapshot_vpcId' - Provides the VPC ID associated with the DB cluster snapshot.
--
-- 'allocatedStorage', 'dbClusterSnapshot_allocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbClusterSnapshot_iAMDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to
-- database accounts is enabled, and otherwise false.
--
-- 'dbClusterSnapshotArn', 'dbClusterSnapshot_dbClusterSnapshotArn' - The Amazon Resource Name (ARN) for the DB cluster snapshot.
newDBClusterSnapshot ::
  DBClusterSnapshot
newDBClusterSnapshot =
  DBClusterSnapshot'
    { storageEncrypted = Core.Nothing,
      status = Core.Nothing,
      availabilityZones = Core.Nothing,
      clusterCreateTime = Core.Nothing,
      snapshotCreateTime = Core.Nothing,
      engineMode = Core.Nothing,
      masterUsername = Core.Nothing,
      kmsKeyId = Core.Nothing,
      dbClusterIdentifier = Core.Nothing,
      engineVersion = Core.Nothing,
      snapshotType = Core.Nothing,
      licenseModel = Core.Nothing,
      port = Core.Nothing,
      percentProgress = Core.Nothing,
      engine = Core.Nothing,
      dbClusterSnapshotIdentifier = Core.Nothing,
      sourceDBClusterSnapshotArn = Core.Nothing,
      tagList = Core.Nothing,
      vpcId = Core.Nothing,
      allocatedStorage = Core.Nothing,
      iAMDatabaseAuthenticationEnabled = Core.Nothing,
      dbClusterSnapshotArn = Core.Nothing
    }

-- | Specifies whether the DB cluster snapshot is encrypted.
dbClusterSnapshot_storageEncrypted :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Bool)
dbClusterSnapshot_storageEncrypted = Lens.lens (\DBClusterSnapshot' {storageEncrypted} -> storageEncrypted) (\s@DBClusterSnapshot' {} a -> s {storageEncrypted = a} :: DBClusterSnapshot)

-- | Specifies the status of this DB cluster snapshot.
dbClusterSnapshot_status :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_status = Lens.lens (\DBClusterSnapshot' {status} -> status) (\s@DBClusterSnapshot' {} a -> s {status = a} :: DBClusterSnapshot)

-- | Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster snapshot can be restored.
dbClusterSnapshot_availabilityZones :: Lens.Lens' DBClusterSnapshot (Core.Maybe [Core.Text])
dbClusterSnapshot_availabilityZones = Lens.lens (\DBClusterSnapshot' {availabilityZones} -> availabilityZones) (\s@DBClusterSnapshot' {} a -> s {availabilityZones = a} :: DBClusterSnapshot) Core.. Lens.mapping Lens._Coerce

-- | Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
dbClusterSnapshot_clusterCreateTime :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.UTCTime)
dbClusterSnapshot_clusterCreateTime = Lens.lens (\DBClusterSnapshot' {clusterCreateTime} -> clusterCreateTime) (\s@DBClusterSnapshot' {} a -> s {clusterCreateTime = a} :: DBClusterSnapshot) Core.. Lens.mapping Core._Time

-- | Provides the time when the snapshot was taken, in Universal Coordinated
-- Time (UTC).
dbClusterSnapshot_snapshotCreateTime :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.UTCTime)
dbClusterSnapshot_snapshotCreateTime = Lens.lens (\DBClusterSnapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@DBClusterSnapshot' {} a -> s {snapshotCreateTime = a} :: DBClusterSnapshot) Core.. Lens.mapping Core._Time

-- | Provides the engine mode of the database engine for this DB cluster
-- snapshot.
dbClusterSnapshot_engineMode :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_engineMode = Lens.lens (\DBClusterSnapshot' {engineMode} -> engineMode) (\s@DBClusterSnapshot' {} a -> s {engineMode = a} :: DBClusterSnapshot)

-- | Provides the master username for this DB cluster snapshot.
dbClusterSnapshot_masterUsername :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_masterUsername = Lens.lens (\DBClusterSnapshot' {masterUsername} -> masterUsername) (\s@DBClusterSnapshot' {} a -> s {masterUsername = a} :: DBClusterSnapshot)

-- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the
-- encrypted DB cluster snapshot.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
dbClusterSnapshot_kmsKeyId :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_kmsKeyId = Lens.lens (\DBClusterSnapshot' {kmsKeyId} -> kmsKeyId) (\s@DBClusterSnapshot' {} a -> s {kmsKeyId = a} :: DBClusterSnapshot)

-- | Specifies the DB cluster identifier of the DB cluster that this DB
-- cluster snapshot was created from.
dbClusterSnapshot_dbClusterIdentifier :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_dbClusterIdentifier = Lens.lens (\DBClusterSnapshot' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBClusterSnapshot' {} a -> s {dbClusterIdentifier = a} :: DBClusterSnapshot)

-- | Provides the version of the database engine for this DB cluster
-- snapshot.
dbClusterSnapshot_engineVersion :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_engineVersion = Lens.lens (\DBClusterSnapshot' {engineVersion} -> engineVersion) (\s@DBClusterSnapshot' {} a -> s {engineVersion = a} :: DBClusterSnapshot)

-- | Provides the type of the DB cluster snapshot.
dbClusterSnapshot_snapshotType :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_snapshotType = Lens.lens (\DBClusterSnapshot' {snapshotType} -> snapshotType) (\s@DBClusterSnapshot' {} a -> s {snapshotType = a} :: DBClusterSnapshot)

-- | Provides the license model information for this DB cluster snapshot.
dbClusterSnapshot_licenseModel :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_licenseModel = Lens.lens (\DBClusterSnapshot' {licenseModel} -> licenseModel) (\s@DBClusterSnapshot' {} a -> s {licenseModel = a} :: DBClusterSnapshot)

-- | Specifies the port that the DB cluster was listening on at the time of
-- the snapshot.
dbClusterSnapshot_port :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Int)
dbClusterSnapshot_port = Lens.lens (\DBClusterSnapshot' {port} -> port) (\s@DBClusterSnapshot' {} a -> s {port = a} :: DBClusterSnapshot)

-- | Specifies the percentage of the estimated data that has been
-- transferred.
dbClusterSnapshot_percentProgress :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Int)
dbClusterSnapshot_percentProgress = Lens.lens (\DBClusterSnapshot' {percentProgress} -> percentProgress) (\s@DBClusterSnapshot' {} a -> s {percentProgress = a} :: DBClusterSnapshot)

-- | Specifies the name of the database engine for this DB cluster snapshot.
dbClusterSnapshot_engine :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_engine = Lens.lens (\DBClusterSnapshot' {engine} -> engine) (\s@DBClusterSnapshot' {} a -> s {engine = a} :: DBClusterSnapshot)

-- | Specifies the identifier for the DB cluster snapshot.
dbClusterSnapshot_dbClusterSnapshotIdentifier :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_dbClusterSnapshotIdentifier = Lens.lens (\DBClusterSnapshot' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@DBClusterSnapshot' {} a -> s {dbClusterSnapshotIdentifier = a} :: DBClusterSnapshot)

-- | If the DB cluster snapshot was copied from a source DB cluster snapshot,
-- the Amazon Resource Name (ARN) for the source DB cluster snapshot,
-- otherwise, a null value.
dbClusterSnapshot_sourceDBClusterSnapshotArn :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_sourceDBClusterSnapshotArn = Lens.lens (\DBClusterSnapshot' {sourceDBClusterSnapshotArn} -> sourceDBClusterSnapshotArn) (\s@DBClusterSnapshot' {} a -> s {sourceDBClusterSnapshotArn = a} :: DBClusterSnapshot)

-- | Undocumented member.
dbClusterSnapshot_tagList :: Lens.Lens' DBClusterSnapshot (Core.Maybe [Tag])
dbClusterSnapshot_tagList = Lens.lens (\DBClusterSnapshot' {tagList} -> tagList) (\s@DBClusterSnapshot' {} a -> s {tagList = a} :: DBClusterSnapshot) Core.. Lens.mapping Lens._Coerce

-- | Provides the VPC ID associated with the DB cluster snapshot.
dbClusterSnapshot_vpcId :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_vpcId = Lens.lens (\DBClusterSnapshot' {vpcId} -> vpcId) (\s@DBClusterSnapshot' {} a -> s {vpcId = a} :: DBClusterSnapshot)

-- | Specifies the allocated storage size in gibibytes (GiB).
dbClusterSnapshot_allocatedStorage :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Int)
dbClusterSnapshot_allocatedStorage = Lens.lens (\DBClusterSnapshot' {allocatedStorage} -> allocatedStorage) (\s@DBClusterSnapshot' {} a -> s {allocatedStorage = a} :: DBClusterSnapshot)

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to
-- database accounts is enabled, and otherwise false.
dbClusterSnapshot_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Bool)
dbClusterSnapshot_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBClusterSnapshot' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBClusterSnapshot' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBClusterSnapshot)

-- | The Amazon Resource Name (ARN) for the DB cluster snapshot.
dbClusterSnapshot_dbClusterSnapshotArn :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbClusterSnapshot_dbClusterSnapshotArn = Lens.lens (\DBClusterSnapshot' {dbClusterSnapshotArn} -> dbClusterSnapshotArn) (\s@DBClusterSnapshot' {} a -> s {dbClusterSnapshotArn = a} :: DBClusterSnapshot)

instance Core.FromXML DBClusterSnapshot where
  parseXML x =
    DBClusterSnapshot'
      Core.<$> (x Core..@? "StorageEncrypted")
      Core.<*> (x Core..@? "Status")
      Core.<*> ( x Core..@? "AvailabilityZones" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "AvailabilityZone")
               )
      Core.<*> (x Core..@? "ClusterCreateTime")
      Core.<*> (x Core..@? "SnapshotCreateTime")
      Core.<*> (x Core..@? "EngineMode")
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "DBClusterIdentifier")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "SnapshotType")
      Core.<*> (x Core..@? "LicenseModel")
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "PercentProgress")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "DBClusterSnapshotIdentifier")
      Core.<*> (x Core..@? "SourceDBClusterSnapshotArn")
      Core.<*> ( x Core..@? "TagList" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )
      Core.<*> (x Core..@? "VpcId")
      Core.<*> (x Core..@? "AllocatedStorage")
      Core.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Core.<*> (x Core..@? "DBClusterSnapshotArn")

instance Core.Hashable DBClusterSnapshot

instance Core.NFData DBClusterSnapshot
