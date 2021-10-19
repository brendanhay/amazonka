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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.Tag

-- | Contains the details for an Amazon RDS DB cluster snapshot
--
-- This data type is used as a response element in the
-- @DescribeDBClusterSnapshots@ action.
--
-- /See:/ 'newDBClusterSnapshot' smart constructor.
data DBClusterSnapshot = DBClusterSnapshot'
  { -- | Provides the version of the database engine for this DB cluster
    -- snapshot.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the status of this DB cluster snapshot.
    status :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the DB cluster snapshot is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the DB cluster identifier of the DB cluster that this DB
    -- cluster snapshot was created from.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Provides the master username for this DB cluster snapshot.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | True if mapping of Amazon Web Services Identity and Access Management
    -- (IAM) accounts to database accounts is enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the DB cluster snapshot.
    dbClusterSnapshotArn :: Prelude.Maybe Prelude.Text,
    -- | Provides the VPC ID associated with the DB cluster snapshot.
    vpcId :: Prelude.Maybe Prelude.Text,
    tagList :: Prelude.Maybe [Tag],
    -- | Specifies the identifier for the DB cluster snapshot.
    dbClusterSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the database engine for this DB cluster snapshot.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Provides the license model information for this DB cluster snapshot.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of Availability Zones (AZs) where instances in the DB
    -- cluster snapshot can be restored.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Provides the type of the DB cluster snapshot.
    snapshotType :: Prelude.Maybe Prelude.Text,
    -- | If @StorageEncrypted@ is true, the Amazon Web Services KMS key
    -- identifier for the encrypted DB cluster snapshot.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK).
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Provides the engine mode of the database engine for this DB cluster
    -- snapshot.
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | Provides the time when the snapshot was taken, in Universal Coordinated
    -- Time (UTC).
    snapshotCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | If the DB cluster snapshot was copied from a source DB cluster snapshot,
    -- the Amazon Resource Name (ARN) for the source DB cluster snapshot,
    -- otherwise, a null value.
    sourceDBClusterSnapshotArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time when the DB cluster was created, in Universal
    -- Coordinated Time (UTC).
    clusterCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | Specifies the percentage of the estimated data that has been
    -- transferred.
    percentProgress :: Prelude.Maybe Prelude.Int,
    -- | Specifies the port that the DB cluster was listening on at the time of
    -- the snapshot.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'dbClusterSnapshot_engineVersion' - Provides the version of the database engine for this DB cluster
-- snapshot.
--
-- 'status', 'dbClusterSnapshot_status' - Specifies the status of this DB cluster snapshot.
--
-- 'storageEncrypted', 'dbClusterSnapshot_storageEncrypted' - Specifies whether the DB cluster snapshot is encrypted.
--
-- 'dbClusterIdentifier', 'dbClusterSnapshot_dbClusterIdentifier' - Specifies the DB cluster identifier of the DB cluster that this DB
-- cluster snapshot was created from.
--
-- 'masterUsername', 'dbClusterSnapshot_masterUsername' - Provides the master username for this DB cluster snapshot.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbClusterSnapshot_iAMDatabaseAuthenticationEnabled' - True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- 'dbClusterSnapshotArn', 'dbClusterSnapshot_dbClusterSnapshotArn' - The Amazon Resource Name (ARN) for the DB cluster snapshot.
--
-- 'vpcId', 'dbClusterSnapshot_vpcId' - Provides the VPC ID associated with the DB cluster snapshot.
--
-- 'tagList', 'dbClusterSnapshot_tagList' - Undocumented member.
--
-- 'dbClusterSnapshotIdentifier', 'dbClusterSnapshot_dbClusterSnapshotIdentifier' - Specifies the identifier for the DB cluster snapshot.
--
-- 'engine', 'dbClusterSnapshot_engine' - Specifies the name of the database engine for this DB cluster snapshot.
--
-- 'licenseModel', 'dbClusterSnapshot_licenseModel' - Provides the license model information for this DB cluster snapshot.
--
-- 'availabilityZones', 'dbClusterSnapshot_availabilityZones' - Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster snapshot can be restored.
--
-- 'snapshotType', 'dbClusterSnapshot_snapshotType' - Provides the type of the DB cluster snapshot.
--
-- 'kmsKeyId', 'dbClusterSnapshot_kmsKeyId' - If @StorageEncrypted@ is true, the Amazon Web Services KMS key
-- identifier for the encrypted DB cluster snapshot.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
--
-- 'engineMode', 'dbClusterSnapshot_engineMode' - Provides the engine mode of the database engine for this DB cluster
-- snapshot.
--
-- 'snapshotCreateTime', 'dbClusterSnapshot_snapshotCreateTime' - Provides the time when the snapshot was taken, in Universal Coordinated
-- Time (UTC).
--
-- 'allocatedStorage', 'dbClusterSnapshot_allocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- 'sourceDBClusterSnapshotArn', 'dbClusterSnapshot_sourceDBClusterSnapshotArn' - If the DB cluster snapshot was copied from a source DB cluster snapshot,
-- the Amazon Resource Name (ARN) for the source DB cluster snapshot,
-- otherwise, a null value.
--
-- 'clusterCreateTime', 'dbClusterSnapshot_clusterCreateTime' - Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
--
-- 'percentProgress', 'dbClusterSnapshot_percentProgress' - Specifies the percentage of the estimated data that has been
-- transferred.
--
-- 'port', 'dbClusterSnapshot_port' - Specifies the port that the DB cluster was listening on at the time of
-- the snapshot.
newDBClusterSnapshot ::
  DBClusterSnapshot
newDBClusterSnapshot =
  DBClusterSnapshot'
    { engineVersion = Prelude.Nothing,
      status = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled = Prelude.Nothing,
      dbClusterSnapshotArn = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      tagList = Prelude.Nothing,
      dbClusterSnapshotIdentifier = Prelude.Nothing,
      engine = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      snapshotType = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      engineMode = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      sourceDBClusterSnapshotArn = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | Provides the version of the database engine for this DB cluster
-- snapshot.
dbClusterSnapshot_engineVersion :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_engineVersion = Lens.lens (\DBClusterSnapshot' {engineVersion} -> engineVersion) (\s@DBClusterSnapshot' {} a -> s {engineVersion = a} :: DBClusterSnapshot)

-- | Specifies the status of this DB cluster snapshot.
dbClusterSnapshot_status :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_status = Lens.lens (\DBClusterSnapshot' {status} -> status) (\s@DBClusterSnapshot' {} a -> s {status = a} :: DBClusterSnapshot)

-- | Specifies whether the DB cluster snapshot is encrypted.
dbClusterSnapshot_storageEncrypted :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Bool)
dbClusterSnapshot_storageEncrypted = Lens.lens (\DBClusterSnapshot' {storageEncrypted} -> storageEncrypted) (\s@DBClusterSnapshot' {} a -> s {storageEncrypted = a} :: DBClusterSnapshot)

-- | Specifies the DB cluster identifier of the DB cluster that this DB
-- cluster snapshot was created from.
dbClusterSnapshot_dbClusterIdentifier :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_dbClusterIdentifier = Lens.lens (\DBClusterSnapshot' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBClusterSnapshot' {} a -> s {dbClusterIdentifier = a} :: DBClusterSnapshot)

-- | Provides the master username for this DB cluster snapshot.
dbClusterSnapshot_masterUsername :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_masterUsername = Lens.lens (\DBClusterSnapshot' {masterUsername} -> masterUsername) (\s@DBClusterSnapshot' {} a -> s {masterUsername = a} :: DBClusterSnapshot)

-- | True if mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled, and otherwise false.
dbClusterSnapshot_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Bool)
dbClusterSnapshot_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBClusterSnapshot' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBClusterSnapshot' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBClusterSnapshot)

-- | The Amazon Resource Name (ARN) for the DB cluster snapshot.
dbClusterSnapshot_dbClusterSnapshotArn :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_dbClusterSnapshotArn = Lens.lens (\DBClusterSnapshot' {dbClusterSnapshotArn} -> dbClusterSnapshotArn) (\s@DBClusterSnapshot' {} a -> s {dbClusterSnapshotArn = a} :: DBClusterSnapshot)

-- | Provides the VPC ID associated with the DB cluster snapshot.
dbClusterSnapshot_vpcId :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_vpcId = Lens.lens (\DBClusterSnapshot' {vpcId} -> vpcId) (\s@DBClusterSnapshot' {} a -> s {vpcId = a} :: DBClusterSnapshot)

-- | Undocumented member.
dbClusterSnapshot_tagList :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe [Tag])
dbClusterSnapshot_tagList = Lens.lens (\DBClusterSnapshot' {tagList} -> tagList) (\s@DBClusterSnapshot' {} a -> s {tagList = a} :: DBClusterSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the identifier for the DB cluster snapshot.
dbClusterSnapshot_dbClusterSnapshotIdentifier :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_dbClusterSnapshotIdentifier = Lens.lens (\DBClusterSnapshot' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@DBClusterSnapshot' {} a -> s {dbClusterSnapshotIdentifier = a} :: DBClusterSnapshot)

-- | Specifies the name of the database engine for this DB cluster snapshot.
dbClusterSnapshot_engine :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_engine = Lens.lens (\DBClusterSnapshot' {engine} -> engine) (\s@DBClusterSnapshot' {} a -> s {engine = a} :: DBClusterSnapshot)

-- | Provides the license model information for this DB cluster snapshot.
dbClusterSnapshot_licenseModel :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_licenseModel = Lens.lens (\DBClusterSnapshot' {licenseModel} -> licenseModel) (\s@DBClusterSnapshot' {} a -> s {licenseModel = a} :: DBClusterSnapshot)

-- | Provides the list of Availability Zones (AZs) where instances in the DB
-- cluster snapshot can be restored.
dbClusterSnapshot_availabilityZones :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe [Prelude.Text])
dbClusterSnapshot_availabilityZones = Lens.lens (\DBClusterSnapshot' {availabilityZones} -> availabilityZones) (\s@DBClusterSnapshot' {} a -> s {availabilityZones = a} :: DBClusterSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | Provides the type of the DB cluster snapshot.
dbClusterSnapshot_snapshotType :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_snapshotType = Lens.lens (\DBClusterSnapshot' {snapshotType} -> snapshotType) (\s@DBClusterSnapshot' {} a -> s {snapshotType = a} :: DBClusterSnapshot)

-- | If @StorageEncrypted@ is true, the Amazon Web Services KMS key
-- identifier for the encrypted DB cluster snapshot.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
dbClusterSnapshot_kmsKeyId :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_kmsKeyId = Lens.lens (\DBClusterSnapshot' {kmsKeyId} -> kmsKeyId) (\s@DBClusterSnapshot' {} a -> s {kmsKeyId = a} :: DBClusterSnapshot)

-- | Provides the engine mode of the database engine for this DB cluster
-- snapshot.
dbClusterSnapshot_engineMode :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_engineMode = Lens.lens (\DBClusterSnapshot' {engineMode} -> engineMode) (\s@DBClusterSnapshot' {} a -> s {engineMode = a} :: DBClusterSnapshot)

-- | Provides the time when the snapshot was taken, in Universal Coordinated
-- Time (UTC).
dbClusterSnapshot_snapshotCreateTime :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.UTCTime)
dbClusterSnapshot_snapshotCreateTime = Lens.lens (\DBClusterSnapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@DBClusterSnapshot' {} a -> s {snapshotCreateTime = a} :: DBClusterSnapshot) Prelude.. Lens.mapping Core._Time

-- | Specifies the allocated storage size in gibibytes (GiB).
dbClusterSnapshot_allocatedStorage :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Int)
dbClusterSnapshot_allocatedStorage = Lens.lens (\DBClusterSnapshot' {allocatedStorage} -> allocatedStorage) (\s@DBClusterSnapshot' {} a -> s {allocatedStorage = a} :: DBClusterSnapshot)

-- | If the DB cluster snapshot was copied from a source DB cluster snapshot,
-- the Amazon Resource Name (ARN) for the source DB cluster snapshot,
-- otherwise, a null value.
dbClusterSnapshot_sourceDBClusterSnapshotArn :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_sourceDBClusterSnapshotArn = Lens.lens (\DBClusterSnapshot' {sourceDBClusterSnapshotArn} -> sourceDBClusterSnapshotArn) (\s@DBClusterSnapshot' {} a -> s {sourceDBClusterSnapshotArn = a} :: DBClusterSnapshot)

-- | Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
dbClusterSnapshot_clusterCreateTime :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.UTCTime)
dbClusterSnapshot_clusterCreateTime = Lens.lens (\DBClusterSnapshot' {clusterCreateTime} -> clusterCreateTime) (\s@DBClusterSnapshot' {} a -> s {clusterCreateTime = a} :: DBClusterSnapshot) Prelude.. Lens.mapping Core._Time

-- | Specifies the percentage of the estimated data that has been
-- transferred.
dbClusterSnapshot_percentProgress :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Int)
dbClusterSnapshot_percentProgress = Lens.lens (\DBClusterSnapshot' {percentProgress} -> percentProgress) (\s@DBClusterSnapshot' {} a -> s {percentProgress = a} :: DBClusterSnapshot)

-- | Specifies the port that the DB cluster was listening on at the time of
-- the snapshot.
dbClusterSnapshot_port :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Int)
dbClusterSnapshot_port = Lens.lens (\DBClusterSnapshot' {port} -> port) (\s@DBClusterSnapshot' {} a -> s {port = a} :: DBClusterSnapshot)

instance Core.FromXML DBClusterSnapshot where
  parseXML x =
    DBClusterSnapshot'
      Prelude.<$> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "StorageEncrypted")
      Prelude.<*> (x Core..@? "DBClusterIdentifier")
      Prelude.<*> (x Core..@? "MasterUsername")
      Prelude.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Core..@? "DBClusterSnapshotArn")
      Prelude.<*> (x Core..@? "VpcId")
      Prelude.<*> ( x Core..@? "TagList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> (x Core..@? "DBClusterSnapshotIdentifier")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "LicenseModel")
      Prelude.<*> ( x Core..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Core..@? "SnapshotType")
      Prelude.<*> (x Core..@? "KmsKeyId")
      Prelude.<*> (x Core..@? "EngineMode")
      Prelude.<*> (x Core..@? "SnapshotCreateTime")
      Prelude.<*> (x Core..@? "AllocatedStorage")
      Prelude.<*> (x Core..@? "SourceDBClusterSnapshotArn")
      Prelude.<*> (x Core..@? "ClusterCreateTime")
      Prelude.<*> (x Core..@? "PercentProgress")
      Prelude.<*> (x Core..@? "Port")

instance Prelude.Hashable DBClusterSnapshot

instance Prelude.NFData DBClusterSnapshot
