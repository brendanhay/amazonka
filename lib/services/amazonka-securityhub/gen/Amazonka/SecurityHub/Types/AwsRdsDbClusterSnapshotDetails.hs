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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbClusterSnapshotDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbClusterSnapshotDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an Amazon RDS DB cluster snapshot.
--
-- /See:/ 'newAwsRdsDbClusterSnapshotDetails' smart constructor.
data AwsRdsDbClusterSnapshotDetails = AwsRdsDbClusterSnapshotDetails'
  { -- | The version of the database engine to use.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The status of this DB cluster snapshot.
    status :: Prelude.Maybe Prelude.Text,
    -- | Whether the DB cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The DB cluster identifier.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the master user for the DB cluster.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | Whether mapping of IAM accounts to database accounts is enabled.
    iamDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The VPC ID that is associated with the DB cluster snapshot.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the DB cluster snapshot.
    dbClusterSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the database engine that you want to use for this DB
    -- instance.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The license model information for this DB cluster snapshot.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | A list of Availability Zones where instances in the DB cluster can be
    -- created.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The type of DB cluster snapshot.
    snapshotType :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the KMS master key that is used to encrypt the database
    -- instances in the DB cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the snapshot was taken.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    snapshotCreateTime :: Prelude.Maybe Prelude.Text,
    -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Indicates when the DB cluster was created, in Universal Coordinated Time
    -- (UTC).
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    clusterCreateTime :: Prelude.Maybe Prelude.Text,
    -- | Specifies the percentage of the estimated data that has been
    -- transferred.
    percentProgress :: Prelude.Maybe Prelude.Int,
    -- | The port number on which the DB instances in the DB cluster accept
    -- connections.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbClusterSnapshotDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'awsRdsDbClusterSnapshotDetails_engineVersion' - The version of the database engine to use.
--
-- 'status', 'awsRdsDbClusterSnapshotDetails_status' - The status of this DB cluster snapshot.
--
-- 'storageEncrypted', 'awsRdsDbClusterSnapshotDetails_storageEncrypted' - Whether the DB cluster is encrypted.
--
-- 'dbClusterIdentifier', 'awsRdsDbClusterSnapshotDetails_dbClusterIdentifier' - The DB cluster identifier.
--
-- 'masterUsername', 'awsRdsDbClusterSnapshotDetails_masterUsername' - The name of the master user for the DB cluster.
--
-- 'iamDatabaseAuthenticationEnabled', 'awsRdsDbClusterSnapshotDetails_iamDatabaseAuthenticationEnabled' - Whether mapping of IAM accounts to database accounts is enabled.
--
-- 'vpcId', 'awsRdsDbClusterSnapshotDetails_vpcId' - The VPC ID that is associated with the DB cluster snapshot.
--
-- 'dbClusterSnapshotIdentifier', 'awsRdsDbClusterSnapshotDetails_dbClusterSnapshotIdentifier' - The identifier of the DB cluster snapshot.
--
-- 'engine', 'awsRdsDbClusterSnapshotDetails_engine' - The name of the database engine that you want to use for this DB
-- instance.
--
-- 'licenseModel', 'awsRdsDbClusterSnapshotDetails_licenseModel' - The license model information for this DB cluster snapshot.
--
-- 'availabilityZones', 'awsRdsDbClusterSnapshotDetails_availabilityZones' - A list of Availability Zones where instances in the DB cluster can be
-- created.
--
-- 'snapshotType', 'awsRdsDbClusterSnapshotDetails_snapshotType' - The type of DB cluster snapshot.
--
-- 'kmsKeyId', 'awsRdsDbClusterSnapshotDetails_kmsKeyId' - The ARN of the KMS master key that is used to encrypt the database
-- instances in the DB cluster.
--
-- 'snapshotCreateTime', 'awsRdsDbClusterSnapshotDetails_snapshotCreateTime' - Indicates when the snapshot was taken.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'allocatedStorage', 'awsRdsDbClusterSnapshotDetails_allocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- 'clusterCreateTime', 'awsRdsDbClusterSnapshotDetails_clusterCreateTime' - Indicates when the DB cluster was created, in Universal Coordinated Time
-- (UTC).
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'percentProgress', 'awsRdsDbClusterSnapshotDetails_percentProgress' - Specifies the percentage of the estimated data that has been
-- transferred.
--
-- 'port', 'awsRdsDbClusterSnapshotDetails_port' - The port number on which the DB instances in the DB cluster accept
-- connections.
newAwsRdsDbClusterSnapshotDetails ::
  AwsRdsDbClusterSnapshotDetails
newAwsRdsDbClusterSnapshotDetails =
  AwsRdsDbClusterSnapshotDetails'
    { engineVersion =
        Prelude.Nothing,
      status = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      iamDatabaseAuthenticationEnabled =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      dbClusterSnapshotIdentifier =
        Prelude.Nothing,
      engine = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      snapshotType = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The version of the database engine to use.
awsRdsDbClusterSnapshotDetails_engineVersion :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_engineVersion = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {engineVersion} -> engineVersion) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {engineVersion = a} :: AwsRdsDbClusterSnapshotDetails)

-- | The status of this DB cluster snapshot.
awsRdsDbClusterSnapshotDetails_status :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_status = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {status} -> status) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {status = a} :: AwsRdsDbClusterSnapshotDetails)

-- | Whether the DB cluster is encrypted.
awsRdsDbClusterSnapshotDetails_storageEncrypted :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterSnapshotDetails_storageEncrypted = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {storageEncrypted} -> storageEncrypted) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {storageEncrypted = a} :: AwsRdsDbClusterSnapshotDetails)

-- | The DB cluster identifier.
awsRdsDbClusterSnapshotDetails_dbClusterIdentifier :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_dbClusterIdentifier = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {dbClusterIdentifier = a} :: AwsRdsDbClusterSnapshotDetails)

-- | The name of the master user for the DB cluster.
awsRdsDbClusterSnapshotDetails_masterUsername :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_masterUsername = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {masterUsername} -> masterUsername) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {masterUsername = a} :: AwsRdsDbClusterSnapshotDetails)

-- | Whether mapping of IAM accounts to database accounts is enabled.
awsRdsDbClusterSnapshotDetails_iamDatabaseAuthenticationEnabled :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterSnapshotDetails_iamDatabaseAuthenticationEnabled = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {iamDatabaseAuthenticationEnabled} -> iamDatabaseAuthenticationEnabled) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {iamDatabaseAuthenticationEnabled = a} :: AwsRdsDbClusterSnapshotDetails)

-- | The VPC ID that is associated with the DB cluster snapshot.
awsRdsDbClusterSnapshotDetails_vpcId :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_vpcId = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {vpcId} -> vpcId) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {vpcId = a} :: AwsRdsDbClusterSnapshotDetails)

-- | The identifier of the DB cluster snapshot.
awsRdsDbClusterSnapshotDetails_dbClusterSnapshotIdentifier :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_dbClusterSnapshotIdentifier = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {dbClusterSnapshotIdentifier = a} :: AwsRdsDbClusterSnapshotDetails)

-- | The name of the database engine that you want to use for this DB
-- instance.
awsRdsDbClusterSnapshotDetails_engine :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_engine = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {engine} -> engine) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {engine = a} :: AwsRdsDbClusterSnapshotDetails)

-- | The license model information for this DB cluster snapshot.
awsRdsDbClusterSnapshotDetails_licenseModel :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_licenseModel = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {licenseModel} -> licenseModel) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {licenseModel = a} :: AwsRdsDbClusterSnapshotDetails)

-- | A list of Availability Zones where instances in the DB cluster can be
-- created.
awsRdsDbClusterSnapshotDetails_availabilityZones :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbClusterSnapshotDetails_availabilityZones = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {availabilityZones} -> availabilityZones) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {availabilityZones = a} :: AwsRdsDbClusterSnapshotDetails) Prelude.. Lens.mapping Lens.coerced

-- | The type of DB cluster snapshot.
awsRdsDbClusterSnapshotDetails_snapshotType :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_snapshotType = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {snapshotType} -> snapshotType) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {snapshotType = a} :: AwsRdsDbClusterSnapshotDetails)

-- | The ARN of the KMS master key that is used to encrypt the database
-- instances in the DB cluster.
awsRdsDbClusterSnapshotDetails_kmsKeyId :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_kmsKeyId = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {kmsKeyId = a} :: AwsRdsDbClusterSnapshotDetails)

-- | Indicates when the snapshot was taken.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRdsDbClusterSnapshotDetails_snapshotCreateTime :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_snapshotCreateTime = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {snapshotCreateTime} -> snapshotCreateTime) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {snapshotCreateTime = a} :: AwsRdsDbClusterSnapshotDetails)

-- | Specifies the allocated storage size in gibibytes (GiB).
awsRdsDbClusterSnapshotDetails_allocatedStorage :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbClusterSnapshotDetails_allocatedStorage = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {allocatedStorage} -> allocatedStorage) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {allocatedStorage = a} :: AwsRdsDbClusterSnapshotDetails)

-- | Indicates when the DB cluster was created, in Universal Coordinated Time
-- (UTC).
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRdsDbClusterSnapshotDetails_clusterCreateTime :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterSnapshotDetails_clusterCreateTime = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {clusterCreateTime} -> clusterCreateTime) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {clusterCreateTime = a} :: AwsRdsDbClusterSnapshotDetails)

-- | Specifies the percentage of the estimated data that has been
-- transferred.
awsRdsDbClusterSnapshotDetails_percentProgress :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbClusterSnapshotDetails_percentProgress = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {percentProgress} -> percentProgress) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {percentProgress = a} :: AwsRdsDbClusterSnapshotDetails)

-- | The port number on which the DB instances in the DB cluster accept
-- connections.
awsRdsDbClusterSnapshotDetails_port :: Lens.Lens' AwsRdsDbClusterSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbClusterSnapshotDetails_port = Lens.lens (\AwsRdsDbClusterSnapshotDetails' {port} -> port) (\s@AwsRdsDbClusterSnapshotDetails' {} a -> s {port = a} :: AwsRdsDbClusterSnapshotDetails)

instance Core.FromJSON AwsRdsDbClusterSnapshotDetails where
  parseJSON =
    Core.withObject
      "AwsRdsDbClusterSnapshotDetails"
      ( \x ->
          AwsRdsDbClusterSnapshotDetails'
            Prelude.<$> (x Core..:? "EngineVersion")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "StorageEncrypted")
            Prelude.<*> (x Core..:? "DbClusterIdentifier")
            Prelude.<*> (x Core..:? "MasterUsername")
            Prelude.<*> (x Core..:? "IamDatabaseAuthenticationEnabled")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "DbClusterSnapshotIdentifier")
            Prelude.<*> (x Core..:? "Engine")
            Prelude.<*> (x Core..:? "LicenseModel")
            Prelude.<*> ( x Core..:? "AvailabilityZones"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SnapshotType")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "SnapshotCreateTime")
            Prelude.<*> (x Core..:? "AllocatedStorage")
            Prelude.<*> (x Core..:? "ClusterCreateTime")
            Prelude.<*> (x Core..:? "PercentProgress")
            Prelude.<*> (x Core..:? "Port")
      )

instance
  Prelude.Hashable
    AwsRdsDbClusterSnapshotDetails
  where
  hashWithSalt
    salt'
    AwsRdsDbClusterSnapshotDetails' {..} =
      salt' `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` percentProgress
        `Prelude.hashWithSalt` clusterCreateTime
        `Prelude.hashWithSalt` allocatedStorage
        `Prelude.hashWithSalt` snapshotCreateTime
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` snapshotType
        `Prelude.hashWithSalt` availabilityZones
        `Prelude.hashWithSalt` licenseModel
        `Prelude.hashWithSalt` engine
        `Prelude.hashWithSalt` dbClusterSnapshotIdentifier
        `Prelude.hashWithSalt` vpcId
        `Prelude.hashWithSalt` iamDatabaseAuthenticationEnabled
        `Prelude.hashWithSalt` masterUsername
        `Prelude.hashWithSalt` dbClusterIdentifier
        `Prelude.hashWithSalt` storageEncrypted
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` engineVersion

instance
  Prelude.NFData
    AwsRdsDbClusterSnapshotDetails
  where
  rnf AwsRdsDbClusterSnapshotDetails' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf percentProgress
      `Prelude.seq` Prelude.rnf clusterCreateTime
      `Prelude.seq` Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf snapshotCreateTime
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf snapshotType
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf dbClusterSnapshotIdentifier
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf iamDatabaseAuthenticationEnabled
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf storageEncrypted
      `Prelude.seq` Prelude.rnf status

instance Core.ToJSON AwsRdsDbClusterSnapshotDetails where
  toJSON AwsRdsDbClusterSnapshotDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            ("Status" Core..=) Prelude.<$> status,
            ("StorageEncrypted" Core..=)
              Prelude.<$> storageEncrypted,
            ("DbClusterIdentifier" Core..=)
              Prelude.<$> dbClusterIdentifier,
            ("MasterUsername" Core..=)
              Prelude.<$> masterUsername,
            ("IamDatabaseAuthenticationEnabled" Core..=)
              Prelude.<$> iamDatabaseAuthenticationEnabled,
            ("VpcId" Core..=) Prelude.<$> vpcId,
            ("DbClusterSnapshotIdentifier" Core..=)
              Prelude.<$> dbClusterSnapshotIdentifier,
            ("Engine" Core..=) Prelude.<$> engine,
            ("LicenseModel" Core..=) Prelude.<$> licenseModel,
            ("AvailabilityZones" Core..=)
              Prelude.<$> availabilityZones,
            ("SnapshotType" Core..=) Prelude.<$> snapshotType,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("SnapshotCreateTime" Core..=)
              Prelude.<$> snapshotCreateTime,
            ("AllocatedStorage" Core..=)
              Prelude.<$> allocatedStorage,
            ("ClusterCreateTime" Core..=)
              Prelude.<$> clusterCreateTime,
            ("PercentProgress" Core..=)
              Prelude.<$> percentProgress,
            ("Port" Core..=) Prelude.<$> port
          ]
      )
