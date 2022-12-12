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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbPendingModifiedValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbPendingModifiedValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRdsDbProcessorFeature
import Amazonka.SecurityHub.Types.AwsRdsPendingCloudWatchLogsExports

-- | Changes to a DB instance that are currently pending.
--
-- /See:/ 'newAwsRdsDbPendingModifiedValues' smart constructor.
data AwsRdsDbPendingModifiedValues = AwsRdsDbPendingModifiedValues'
  { -- | The new value of the allocated storage for the DB instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The new backup retention period for the DB instance.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The new CA certificate identifier for the DB instance.
    caCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The new DB instance class for the DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The new DB instance identifier for the DB instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the new subnet group for the DB instance.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The new engine version for the DB instance.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The new provisioned IOPS value for the DB instance.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The new license model value for the DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The new master user password for the DB instance.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | Indicates that a single Availability Zone DB instance is changing to a
    -- multiple Availability Zone deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | A list of log types that are being enabled or disabled.
    pendingCloudWatchLogsExports :: Prelude.Maybe AwsRdsPendingCloudWatchLogsExports,
    -- | The new port for the DB instance.
    port :: Prelude.Maybe Prelude.Int,
    -- | Processor features that are being updated.
    processorFeatures :: Prelude.Maybe [AwsRdsDbProcessorFeature],
    -- | The new storage type for the DB instance.
    storageType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbPendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedStorage', 'awsRdsDbPendingModifiedValues_allocatedStorage' - The new value of the allocated storage for the DB instance.
--
-- 'backupRetentionPeriod', 'awsRdsDbPendingModifiedValues_backupRetentionPeriod' - The new backup retention period for the DB instance.
--
-- 'caCertificateIdentifier', 'awsRdsDbPendingModifiedValues_caCertificateIdentifier' - The new CA certificate identifier for the DB instance.
--
-- 'dbInstanceClass', 'awsRdsDbPendingModifiedValues_dbInstanceClass' - The new DB instance class for the DB instance.
--
-- 'dbInstanceIdentifier', 'awsRdsDbPendingModifiedValues_dbInstanceIdentifier' - The new DB instance identifier for the DB instance.
--
-- 'dbSubnetGroupName', 'awsRdsDbPendingModifiedValues_dbSubnetGroupName' - The name of the new subnet group for the DB instance.
--
-- 'engineVersion', 'awsRdsDbPendingModifiedValues_engineVersion' - The new engine version for the DB instance.
--
-- 'iops', 'awsRdsDbPendingModifiedValues_iops' - The new provisioned IOPS value for the DB instance.
--
-- 'licenseModel', 'awsRdsDbPendingModifiedValues_licenseModel' - The new license model value for the DB instance.
--
-- 'masterUserPassword', 'awsRdsDbPendingModifiedValues_masterUserPassword' - The new master user password for the DB instance.
--
-- 'multiAZ', 'awsRdsDbPendingModifiedValues_multiAZ' - Indicates that a single Availability Zone DB instance is changing to a
-- multiple Availability Zone deployment.
--
-- 'pendingCloudWatchLogsExports', 'awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports' - A list of log types that are being enabled or disabled.
--
-- 'port', 'awsRdsDbPendingModifiedValues_port' - The new port for the DB instance.
--
-- 'processorFeatures', 'awsRdsDbPendingModifiedValues_processorFeatures' - Processor features that are being updated.
--
-- 'storageType', 'awsRdsDbPendingModifiedValues_storageType' - The new storage type for the DB instance.
newAwsRdsDbPendingModifiedValues ::
  AwsRdsDbPendingModifiedValues
newAwsRdsDbPendingModifiedValues =
  AwsRdsDbPendingModifiedValues'
    { allocatedStorage =
        Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      caCertificateIdentifier = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      iops = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      pendingCloudWatchLogsExports =
        Prelude.Nothing,
      port = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | The new value of the allocated storage for the DB instance.
awsRdsDbPendingModifiedValues_allocatedStorage :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_allocatedStorage = Lens.lens (\AwsRdsDbPendingModifiedValues' {allocatedStorage} -> allocatedStorage) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {allocatedStorage = a} :: AwsRdsDbPendingModifiedValues)

-- | The new backup retention period for the DB instance.
awsRdsDbPendingModifiedValues_backupRetentionPeriod :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_backupRetentionPeriod = Lens.lens (\AwsRdsDbPendingModifiedValues' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {backupRetentionPeriod = a} :: AwsRdsDbPendingModifiedValues)

-- | The new CA certificate identifier for the DB instance.
awsRdsDbPendingModifiedValues_caCertificateIdentifier :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_caCertificateIdentifier = Lens.lens (\AwsRdsDbPendingModifiedValues' {caCertificateIdentifier} -> caCertificateIdentifier) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {caCertificateIdentifier = a} :: AwsRdsDbPendingModifiedValues)

-- | The new DB instance class for the DB instance.
awsRdsDbPendingModifiedValues_dbInstanceClass :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_dbInstanceClass = Lens.lens (\AwsRdsDbPendingModifiedValues' {dbInstanceClass} -> dbInstanceClass) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {dbInstanceClass = a} :: AwsRdsDbPendingModifiedValues)

-- | The new DB instance identifier for the DB instance.
awsRdsDbPendingModifiedValues_dbInstanceIdentifier :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_dbInstanceIdentifier = Lens.lens (\AwsRdsDbPendingModifiedValues' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {dbInstanceIdentifier = a} :: AwsRdsDbPendingModifiedValues)

-- | The name of the new subnet group for the DB instance.
awsRdsDbPendingModifiedValues_dbSubnetGroupName :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_dbSubnetGroupName = Lens.lens (\AwsRdsDbPendingModifiedValues' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {dbSubnetGroupName = a} :: AwsRdsDbPendingModifiedValues)

-- | The new engine version for the DB instance.
awsRdsDbPendingModifiedValues_engineVersion :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_engineVersion = Lens.lens (\AwsRdsDbPendingModifiedValues' {engineVersion} -> engineVersion) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {engineVersion = a} :: AwsRdsDbPendingModifiedValues)

-- | The new provisioned IOPS value for the DB instance.
awsRdsDbPendingModifiedValues_iops :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_iops = Lens.lens (\AwsRdsDbPendingModifiedValues' {iops} -> iops) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {iops = a} :: AwsRdsDbPendingModifiedValues)

-- | The new license model value for the DB instance.
awsRdsDbPendingModifiedValues_licenseModel :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_licenseModel = Lens.lens (\AwsRdsDbPendingModifiedValues' {licenseModel} -> licenseModel) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {licenseModel = a} :: AwsRdsDbPendingModifiedValues)

-- | The new master user password for the DB instance.
awsRdsDbPendingModifiedValues_masterUserPassword :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_masterUserPassword = Lens.lens (\AwsRdsDbPendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {masterUserPassword = a} :: AwsRdsDbPendingModifiedValues)

-- | Indicates that a single Availability Zone DB instance is changing to a
-- multiple Availability Zone deployment.
awsRdsDbPendingModifiedValues_multiAZ :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Bool)
awsRdsDbPendingModifiedValues_multiAZ = Lens.lens (\AwsRdsDbPendingModifiedValues' {multiAZ} -> multiAZ) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {multiAZ = a} :: AwsRdsDbPendingModifiedValues)

-- | A list of log types that are being enabled or disabled.
awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe AwsRdsPendingCloudWatchLogsExports)
awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports = Lens.lens (\AwsRdsDbPendingModifiedValues' {pendingCloudWatchLogsExports} -> pendingCloudWatchLogsExports) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {pendingCloudWatchLogsExports = a} :: AwsRdsDbPendingModifiedValues)

-- | The new port for the DB instance.
awsRdsDbPendingModifiedValues_port :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_port = Lens.lens (\AwsRdsDbPendingModifiedValues' {port} -> port) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {port = a} :: AwsRdsDbPendingModifiedValues)

-- | Processor features that are being updated.
awsRdsDbPendingModifiedValues_processorFeatures :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe [AwsRdsDbProcessorFeature])
awsRdsDbPendingModifiedValues_processorFeatures = Lens.lens (\AwsRdsDbPendingModifiedValues' {processorFeatures} -> processorFeatures) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {processorFeatures = a} :: AwsRdsDbPendingModifiedValues) Prelude.. Lens.mapping Lens.coerced

-- | The new storage type for the DB instance.
awsRdsDbPendingModifiedValues_storageType :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_storageType = Lens.lens (\AwsRdsDbPendingModifiedValues' {storageType} -> storageType) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {storageType = a} :: AwsRdsDbPendingModifiedValues)

instance Data.FromJSON AwsRdsDbPendingModifiedValues where
  parseJSON =
    Data.withObject
      "AwsRdsDbPendingModifiedValues"
      ( \x ->
          AwsRdsDbPendingModifiedValues'
            Prelude.<$> (x Data..:? "AllocatedStorage")
            Prelude.<*> (x Data..:? "BackupRetentionPeriod")
            Prelude.<*> (x Data..:? "CaCertificateIdentifier")
            Prelude.<*> (x Data..:? "DbInstanceClass")
            Prelude.<*> (x Data..:? "DbInstanceIdentifier")
            Prelude.<*> (x Data..:? "DbSubnetGroupName")
            Prelude.<*> (x Data..:? "EngineVersion")
            Prelude.<*> (x Data..:? "Iops")
            Prelude.<*> (x Data..:? "LicenseModel")
            Prelude.<*> (x Data..:? "MasterUserPassword")
            Prelude.<*> (x Data..:? "MultiAZ")
            Prelude.<*> (x Data..:? "PendingCloudWatchLogsExports")
            Prelude.<*> (x Data..:? "Port")
            Prelude.<*> ( x Data..:? "ProcessorFeatures"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StorageType")
      )

instance
  Prelude.Hashable
    AwsRdsDbPendingModifiedValues
  where
  hashWithSalt _salt AwsRdsDbPendingModifiedValues' {..} =
    _salt `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` caCertificateIdentifier
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` pendingCloudWatchLogsExports
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` storageType

instance Prelude.NFData AwsRdsDbPendingModifiedValues where
  rnf AwsRdsDbPendingModifiedValues' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf caCertificateIdentifier
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf pendingCloudWatchLogsExports
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf processorFeatures
      `Prelude.seq` Prelude.rnf storageType

instance Data.ToJSON AwsRdsDbPendingModifiedValues where
  toJSON AwsRdsDbPendingModifiedValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllocatedStorage" Data..=)
              Prelude.<$> allocatedStorage,
            ("BackupRetentionPeriod" Data..=)
              Prelude.<$> backupRetentionPeriod,
            ("CaCertificateIdentifier" Data..=)
              Prelude.<$> caCertificateIdentifier,
            ("DbInstanceClass" Data..=)
              Prelude.<$> dbInstanceClass,
            ("DbInstanceIdentifier" Data..=)
              Prelude.<$> dbInstanceIdentifier,
            ("DbSubnetGroupName" Data..=)
              Prelude.<$> dbSubnetGroupName,
            ("EngineVersion" Data..=) Prelude.<$> engineVersion,
            ("Iops" Data..=) Prelude.<$> iops,
            ("LicenseModel" Data..=) Prelude.<$> licenseModel,
            ("MasterUserPassword" Data..=)
              Prelude.<$> masterUserPassword,
            ("MultiAZ" Data..=) Prelude.<$> multiAZ,
            ("PendingCloudWatchLogsExports" Data..=)
              Prelude.<$> pendingCloudWatchLogsExports,
            ("Port" Data..=) Prelude.<$> port,
            ("ProcessorFeatures" Data..=)
              Prelude.<$> processorFeatures,
            ("StorageType" Data..=) Prelude.<$> storageType
          ]
      )
