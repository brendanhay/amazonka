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
-- Module      : Amazonka.DocumentDB.Types.DBClusterSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.DBClusterSnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about a cluster snapshot.
--
-- /See:/ 'newDBClusterSnapshot' smart constructor.
data DBClusterSnapshot = DBClusterSnapshot'
  { -- | Specifies the port that the cluster was listening on at the time of the
    -- snapshot.
    port :: Prelude.Maybe Prelude.Int,
    -- | Specifies the percentage of the estimated data that has been
    -- transferred.
    percentProgress :: Prelude.Maybe Prelude.Int,
    -- | Provides the master user name for the cluster snapshot.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | Specifies the identifier for the cluster snapshot.
    dbClusterSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies the cluster identifier of the cluster that this cluster
    -- snapshot was created from.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of Amazon EC2 Availability Zones that instances in the
    -- cluster snapshot can be restored in.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | If the cluster snapshot was copied from a source cluster snapshot, the
    -- ARN for the source cluster snapshot; otherwise, a null value.
    sourceDBClusterSnapshotArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the status of this cluster snapshot.
    status :: Prelude.Maybe Prelude.Text,
    -- | Provides the time when the snapshot was taken, in UTC.
    snapshotCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | Specifies whether the cluster snapshot is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | If @StorageEncrypted@ is @true@, the KMS key identifier for the
    -- encrypted cluster snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the database engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Provides the virtual private cloud (VPC) ID that is associated with the
    -- cluster snapshot.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time when the cluster was created, in Universal
    -- Coordinated Time (UTC).
    clusterCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) for the cluster snapshot.
    dbClusterSnapshotArn :: Prelude.Maybe Prelude.Text,
    -- | Provides the version of the database engine for this cluster snapshot.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Provides the type of the cluster snapshot.
    snapshotType :: Prelude.Maybe Prelude.Text
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
-- 'port', 'dbClusterSnapshot_port' - Specifies the port that the cluster was listening on at the time of the
-- snapshot.
--
-- 'percentProgress', 'dbClusterSnapshot_percentProgress' - Specifies the percentage of the estimated data that has been
-- transferred.
--
-- 'masterUsername', 'dbClusterSnapshot_masterUsername' - Provides the master user name for the cluster snapshot.
--
-- 'dbClusterSnapshotIdentifier', 'dbClusterSnapshot_dbClusterSnapshotIdentifier' - Specifies the identifier for the cluster snapshot.
--
-- 'dbClusterIdentifier', 'dbClusterSnapshot_dbClusterIdentifier' - Specifies the cluster identifier of the cluster that this cluster
-- snapshot was created from.
--
-- 'availabilityZones', 'dbClusterSnapshot_availabilityZones' - Provides the list of Amazon EC2 Availability Zones that instances in the
-- cluster snapshot can be restored in.
--
-- 'sourceDBClusterSnapshotArn', 'dbClusterSnapshot_sourceDBClusterSnapshotArn' - If the cluster snapshot was copied from a source cluster snapshot, the
-- ARN for the source cluster snapshot; otherwise, a null value.
--
-- 'status', 'dbClusterSnapshot_status' - Specifies the status of this cluster snapshot.
--
-- 'snapshotCreateTime', 'dbClusterSnapshot_snapshotCreateTime' - Provides the time when the snapshot was taken, in UTC.
--
-- 'storageEncrypted', 'dbClusterSnapshot_storageEncrypted' - Specifies whether the cluster snapshot is encrypted.
--
-- 'kmsKeyId', 'dbClusterSnapshot_kmsKeyId' - If @StorageEncrypted@ is @true@, the KMS key identifier for the
-- encrypted cluster snapshot.
--
-- 'engine', 'dbClusterSnapshot_engine' - Specifies the name of the database engine.
--
-- 'vpcId', 'dbClusterSnapshot_vpcId' - Provides the virtual private cloud (VPC) ID that is associated with the
-- cluster snapshot.
--
-- 'clusterCreateTime', 'dbClusterSnapshot_clusterCreateTime' - Specifies the time when the cluster was created, in Universal
-- Coordinated Time (UTC).
--
-- 'dbClusterSnapshotArn', 'dbClusterSnapshot_dbClusterSnapshotArn' - The Amazon Resource Name (ARN) for the cluster snapshot.
--
-- 'engineVersion', 'dbClusterSnapshot_engineVersion' - Provides the version of the database engine for this cluster snapshot.
--
-- 'snapshotType', 'dbClusterSnapshot_snapshotType' - Provides the type of the cluster snapshot.
newDBClusterSnapshot ::
  DBClusterSnapshot
newDBClusterSnapshot =
  DBClusterSnapshot'
    { port = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      dbClusterSnapshotIdentifier = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      sourceDBClusterSnapshotArn = Prelude.Nothing,
      status = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      engine = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      dbClusterSnapshotArn = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      snapshotType = Prelude.Nothing
    }

-- | Specifies the port that the cluster was listening on at the time of the
-- snapshot.
dbClusterSnapshot_port :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Int)
dbClusterSnapshot_port = Lens.lens (\DBClusterSnapshot' {port} -> port) (\s@DBClusterSnapshot' {} a -> s {port = a} :: DBClusterSnapshot)

-- | Specifies the percentage of the estimated data that has been
-- transferred.
dbClusterSnapshot_percentProgress :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Int)
dbClusterSnapshot_percentProgress = Lens.lens (\DBClusterSnapshot' {percentProgress} -> percentProgress) (\s@DBClusterSnapshot' {} a -> s {percentProgress = a} :: DBClusterSnapshot)

-- | Provides the master user name for the cluster snapshot.
dbClusterSnapshot_masterUsername :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_masterUsername = Lens.lens (\DBClusterSnapshot' {masterUsername} -> masterUsername) (\s@DBClusterSnapshot' {} a -> s {masterUsername = a} :: DBClusterSnapshot)

-- | Specifies the identifier for the cluster snapshot.
dbClusterSnapshot_dbClusterSnapshotIdentifier :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_dbClusterSnapshotIdentifier = Lens.lens (\DBClusterSnapshot' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@DBClusterSnapshot' {} a -> s {dbClusterSnapshotIdentifier = a} :: DBClusterSnapshot)

-- | Specifies the cluster identifier of the cluster that this cluster
-- snapshot was created from.
dbClusterSnapshot_dbClusterIdentifier :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_dbClusterIdentifier = Lens.lens (\DBClusterSnapshot' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBClusterSnapshot' {} a -> s {dbClusterIdentifier = a} :: DBClusterSnapshot)

-- | Provides the list of Amazon EC2 Availability Zones that instances in the
-- cluster snapshot can be restored in.
dbClusterSnapshot_availabilityZones :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe [Prelude.Text])
dbClusterSnapshot_availabilityZones = Lens.lens (\DBClusterSnapshot' {availabilityZones} -> availabilityZones) (\s@DBClusterSnapshot' {} a -> s {availabilityZones = a} :: DBClusterSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | If the cluster snapshot was copied from a source cluster snapshot, the
-- ARN for the source cluster snapshot; otherwise, a null value.
dbClusterSnapshot_sourceDBClusterSnapshotArn :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_sourceDBClusterSnapshotArn = Lens.lens (\DBClusterSnapshot' {sourceDBClusterSnapshotArn} -> sourceDBClusterSnapshotArn) (\s@DBClusterSnapshot' {} a -> s {sourceDBClusterSnapshotArn = a} :: DBClusterSnapshot)

-- | Specifies the status of this cluster snapshot.
dbClusterSnapshot_status :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_status = Lens.lens (\DBClusterSnapshot' {status} -> status) (\s@DBClusterSnapshot' {} a -> s {status = a} :: DBClusterSnapshot)

-- | Provides the time when the snapshot was taken, in UTC.
dbClusterSnapshot_snapshotCreateTime :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.UTCTime)
dbClusterSnapshot_snapshotCreateTime = Lens.lens (\DBClusterSnapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@DBClusterSnapshot' {} a -> s {snapshotCreateTime = a} :: DBClusterSnapshot) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the cluster snapshot is encrypted.
dbClusterSnapshot_storageEncrypted :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Bool)
dbClusterSnapshot_storageEncrypted = Lens.lens (\DBClusterSnapshot' {storageEncrypted} -> storageEncrypted) (\s@DBClusterSnapshot' {} a -> s {storageEncrypted = a} :: DBClusterSnapshot)

-- | If @StorageEncrypted@ is @true@, the KMS key identifier for the
-- encrypted cluster snapshot.
dbClusterSnapshot_kmsKeyId :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_kmsKeyId = Lens.lens (\DBClusterSnapshot' {kmsKeyId} -> kmsKeyId) (\s@DBClusterSnapshot' {} a -> s {kmsKeyId = a} :: DBClusterSnapshot)

-- | Specifies the name of the database engine.
dbClusterSnapshot_engine :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_engine = Lens.lens (\DBClusterSnapshot' {engine} -> engine) (\s@DBClusterSnapshot' {} a -> s {engine = a} :: DBClusterSnapshot)

-- | Provides the virtual private cloud (VPC) ID that is associated with the
-- cluster snapshot.
dbClusterSnapshot_vpcId :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_vpcId = Lens.lens (\DBClusterSnapshot' {vpcId} -> vpcId) (\s@DBClusterSnapshot' {} a -> s {vpcId = a} :: DBClusterSnapshot)

-- | Specifies the time when the cluster was created, in Universal
-- Coordinated Time (UTC).
dbClusterSnapshot_clusterCreateTime :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.UTCTime)
dbClusterSnapshot_clusterCreateTime = Lens.lens (\DBClusterSnapshot' {clusterCreateTime} -> clusterCreateTime) (\s@DBClusterSnapshot' {} a -> s {clusterCreateTime = a} :: DBClusterSnapshot) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) for the cluster snapshot.
dbClusterSnapshot_dbClusterSnapshotArn :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_dbClusterSnapshotArn = Lens.lens (\DBClusterSnapshot' {dbClusterSnapshotArn} -> dbClusterSnapshotArn) (\s@DBClusterSnapshot' {} a -> s {dbClusterSnapshotArn = a} :: DBClusterSnapshot)

-- | Provides the version of the database engine for this cluster snapshot.
dbClusterSnapshot_engineVersion :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_engineVersion = Lens.lens (\DBClusterSnapshot' {engineVersion} -> engineVersion) (\s@DBClusterSnapshot' {} a -> s {engineVersion = a} :: DBClusterSnapshot)

-- | Provides the type of the cluster snapshot.
dbClusterSnapshot_snapshotType :: Lens.Lens' DBClusterSnapshot (Prelude.Maybe Prelude.Text)
dbClusterSnapshot_snapshotType = Lens.lens (\DBClusterSnapshot' {snapshotType} -> snapshotType) (\s@DBClusterSnapshot' {} a -> s {snapshotType = a} :: DBClusterSnapshot)

instance Data.FromXML DBClusterSnapshot where
  parseXML x =
    DBClusterSnapshot'
      Prelude.<$> (x Data..@? "Port")
      Prelude.<*> (x Data..@? "PercentProgress")
      Prelude.<*> (x Data..@? "MasterUsername")
      Prelude.<*> (x Data..@? "DBClusterSnapshotIdentifier")
      Prelude.<*> (x Data..@? "DBClusterIdentifier")
      Prelude.<*> ( x Data..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Data..@? "SourceDBClusterSnapshotArn")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "SnapshotCreateTime")
      Prelude.<*> (x Data..@? "StorageEncrypted")
      Prelude.<*> (x Data..@? "KmsKeyId")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "VpcId")
      Prelude.<*> (x Data..@? "ClusterCreateTime")
      Prelude.<*> (x Data..@? "DBClusterSnapshotArn")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "SnapshotType")

instance Prelude.Hashable DBClusterSnapshot where
  hashWithSalt _salt DBClusterSnapshot' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` percentProgress
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` dbClusterSnapshotIdentifier
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` sourceDBClusterSnapshotArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` snapshotCreateTime
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` clusterCreateTime
      `Prelude.hashWithSalt` dbClusterSnapshotArn
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` snapshotType

instance Prelude.NFData DBClusterSnapshot where
  rnf DBClusterSnapshot' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf percentProgress
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf dbClusterSnapshotIdentifier
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf sourceDBClusterSnapshotArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf snapshotCreateTime
      `Prelude.seq` Prelude.rnf storageEncrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf clusterCreateTime
      `Prelude.seq` Prelude.rnf dbClusterSnapshotArn
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf snapshotType
