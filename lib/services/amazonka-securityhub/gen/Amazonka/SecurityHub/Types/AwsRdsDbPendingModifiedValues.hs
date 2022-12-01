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
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRdsDbProcessorFeature
import Amazonka.SecurityHub.Types.AwsRdsPendingCloudWatchLogsExports

-- | Changes to a DB instance that are currently pending.
--
-- /See:/ 'newAwsRdsDbPendingModifiedValues' smart constructor.
data AwsRdsDbPendingModifiedValues = AwsRdsDbPendingModifiedValues'
  { -- | The new port for the DB instance.
    port :: Prelude.Maybe Prelude.Int,
    -- | The new backup retention period for the DB instance.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The new DB instance class for the DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The name of the new subnet group for the DB instance.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The new DB instance identifier for the DB instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A list of log types that are being enabled or disabled.
    pendingCloudWatchLogsExports :: Prelude.Maybe AwsRdsPendingCloudWatchLogsExports,
    -- | The new master user password for the DB instance.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The new storage type for the DB instance.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | Processor features that are being updated.
    processorFeatures :: Prelude.Maybe [AwsRdsDbProcessorFeature],
    -- | The new CA certificate identifier for the DB instance.
    caCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The new value of the allocated storage for the DB instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The new provisioned IOPS value for the DB instance.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The new engine version for the DB instance.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Indicates that a single Availability Zone DB instance is changing to a
    -- multiple Availability Zone deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The new license model value for the DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text
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
-- 'port', 'awsRdsDbPendingModifiedValues_port' - The new port for the DB instance.
--
-- 'backupRetentionPeriod', 'awsRdsDbPendingModifiedValues_backupRetentionPeriod' - The new backup retention period for the DB instance.
--
-- 'dbInstanceClass', 'awsRdsDbPendingModifiedValues_dbInstanceClass' - The new DB instance class for the DB instance.
--
-- 'dbSubnetGroupName', 'awsRdsDbPendingModifiedValues_dbSubnetGroupName' - The name of the new subnet group for the DB instance.
--
-- 'dbInstanceIdentifier', 'awsRdsDbPendingModifiedValues_dbInstanceIdentifier' - The new DB instance identifier for the DB instance.
--
-- 'pendingCloudWatchLogsExports', 'awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports' - A list of log types that are being enabled or disabled.
--
-- 'masterUserPassword', 'awsRdsDbPendingModifiedValues_masterUserPassword' - The new master user password for the DB instance.
--
-- 'storageType', 'awsRdsDbPendingModifiedValues_storageType' - The new storage type for the DB instance.
--
-- 'processorFeatures', 'awsRdsDbPendingModifiedValues_processorFeatures' - Processor features that are being updated.
--
-- 'caCertificateIdentifier', 'awsRdsDbPendingModifiedValues_caCertificateIdentifier' - The new CA certificate identifier for the DB instance.
--
-- 'allocatedStorage', 'awsRdsDbPendingModifiedValues_allocatedStorage' - The new value of the allocated storage for the DB instance.
--
-- 'iops', 'awsRdsDbPendingModifiedValues_iops' - The new provisioned IOPS value for the DB instance.
--
-- 'engineVersion', 'awsRdsDbPendingModifiedValues_engineVersion' - The new engine version for the DB instance.
--
-- 'multiAZ', 'awsRdsDbPendingModifiedValues_multiAZ' - Indicates that a single Availability Zone DB instance is changing to a
-- multiple Availability Zone deployment.
--
-- 'licenseModel', 'awsRdsDbPendingModifiedValues_licenseModel' - The new license model value for the DB instance.
newAwsRdsDbPendingModifiedValues ::
  AwsRdsDbPendingModifiedValues
newAwsRdsDbPendingModifiedValues =
  AwsRdsDbPendingModifiedValues'
    { port =
        Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      pendingCloudWatchLogsExports =
        Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      storageType = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      caCertificateIdentifier = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      iops = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      licenseModel = Prelude.Nothing
    }

-- | The new port for the DB instance.
awsRdsDbPendingModifiedValues_port :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_port = Lens.lens (\AwsRdsDbPendingModifiedValues' {port} -> port) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {port = a} :: AwsRdsDbPendingModifiedValues)

-- | The new backup retention period for the DB instance.
awsRdsDbPendingModifiedValues_backupRetentionPeriod :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_backupRetentionPeriod = Lens.lens (\AwsRdsDbPendingModifiedValues' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {backupRetentionPeriod = a} :: AwsRdsDbPendingModifiedValues)

-- | The new DB instance class for the DB instance.
awsRdsDbPendingModifiedValues_dbInstanceClass :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_dbInstanceClass = Lens.lens (\AwsRdsDbPendingModifiedValues' {dbInstanceClass} -> dbInstanceClass) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {dbInstanceClass = a} :: AwsRdsDbPendingModifiedValues)

-- | The name of the new subnet group for the DB instance.
awsRdsDbPendingModifiedValues_dbSubnetGroupName :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_dbSubnetGroupName = Lens.lens (\AwsRdsDbPendingModifiedValues' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {dbSubnetGroupName = a} :: AwsRdsDbPendingModifiedValues)

-- | The new DB instance identifier for the DB instance.
awsRdsDbPendingModifiedValues_dbInstanceIdentifier :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_dbInstanceIdentifier = Lens.lens (\AwsRdsDbPendingModifiedValues' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {dbInstanceIdentifier = a} :: AwsRdsDbPendingModifiedValues)

-- | A list of log types that are being enabled or disabled.
awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe AwsRdsPendingCloudWatchLogsExports)
awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports = Lens.lens (\AwsRdsDbPendingModifiedValues' {pendingCloudWatchLogsExports} -> pendingCloudWatchLogsExports) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {pendingCloudWatchLogsExports = a} :: AwsRdsDbPendingModifiedValues)

-- | The new master user password for the DB instance.
awsRdsDbPendingModifiedValues_masterUserPassword :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_masterUserPassword = Lens.lens (\AwsRdsDbPendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {masterUserPassword = a} :: AwsRdsDbPendingModifiedValues)

-- | The new storage type for the DB instance.
awsRdsDbPendingModifiedValues_storageType :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_storageType = Lens.lens (\AwsRdsDbPendingModifiedValues' {storageType} -> storageType) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {storageType = a} :: AwsRdsDbPendingModifiedValues)

-- | Processor features that are being updated.
awsRdsDbPendingModifiedValues_processorFeatures :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe [AwsRdsDbProcessorFeature])
awsRdsDbPendingModifiedValues_processorFeatures = Lens.lens (\AwsRdsDbPendingModifiedValues' {processorFeatures} -> processorFeatures) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {processorFeatures = a} :: AwsRdsDbPendingModifiedValues) Prelude.. Lens.mapping Lens.coerced

-- | The new CA certificate identifier for the DB instance.
awsRdsDbPendingModifiedValues_caCertificateIdentifier :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_caCertificateIdentifier = Lens.lens (\AwsRdsDbPendingModifiedValues' {caCertificateIdentifier} -> caCertificateIdentifier) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {caCertificateIdentifier = a} :: AwsRdsDbPendingModifiedValues)

-- | The new value of the allocated storage for the DB instance.
awsRdsDbPendingModifiedValues_allocatedStorage :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_allocatedStorage = Lens.lens (\AwsRdsDbPendingModifiedValues' {allocatedStorage} -> allocatedStorage) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {allocatedStorage = a} :: AwsRdsDbPendingModifiedValues)

-- | The new provisioned IOPS value for the DB instance.
awsRdsDbPendingModifiedValues_iops :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_iops = Lens.lens (\AwsRdsDbPendingModifiedValues' {iops} -> iops) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {iops = a} :: AwsRdsDbPendingModifiedValues)

-- | The new engine version for the DB instance.
awsRdsDbPendingModifiedValues_engineVersion :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_engineVersion = Lens.lens (\AwsRdsDbPendingModifiedValues' {engineVersion} -> engineVersion) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {engineVersion = a} :: AwsRdsDbPendingModifiedValues)

-- | Indicates that a single Availability Zone DB instance is changing to a
-- multiple Availability Zone deployment.
awsRdsDbPendingModifiedValues_multiAZ :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Bool)
awsRdsDbPendingModifiedValues_multiAZ = Lens.lens (\AwsRdsDbPendingModifiedValues' {multiAZ} -> multiAZ) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {multiAZ = a} :: AwsRdsDbPendingModifiedValues)

-- | The new license model value for the DB instance.
awsRdsDbPendingModifiedValues_licenseModel :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_licenseModel = Lens.lens (\AwsRdsDbPendingModifiedValues' {licenseModel} -> licenseModel) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {licenseModel = a} :: AwsRdsDbPendingModifiedValues)

instance Core.FromJSON AwsRdsDbPendingModifiedValues where
  parseJSON =
    Core.withObject
      "AwsRdsDbPendingModifiedValues"
      ( \x ->
          AwsRdsDbPendingModifiedValues'
            Prelude.<$> (x Core..:? "Port")
            Prelude.<*> (x Core..:? "BackupRetentionPeriod")
            Prelude.<*> (x Core..:? "DbInstanceClass")
            Prelude.<*> (x Core..:? "DbSubnetGroupName")
            Prelude.<*> (x Core..:? "DbInstanceIdentifier")
            Prelude.<*> (x Core..:? "PendingCloudWatchLogsExports")
            Prelude.<*> (x Core..:? "MasterUserPassword")
            Prelude.<*> (x Core..:? "StorageType")
            Prelude.<*> ( x Core..:? "ProcessorFeatures"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CaCertificateIdentifier")
            Prelude.<*> (x Core..:? "AllocatedStorage")
            Prelude.<*> (x Core..:? "Iops")
            Prelude.<*> (x Core..:? "EngineVersion")
            Prelude.<*> (x Core..:? "MultiAZ")
            Prelude.<*> (x Core..:? "LicenseModel")
      )

instance
  Prelude.Hashable
    AwsRdsDbPendingModifiedValues
  where
  hashWithSalt _salt AwsRdsDbPendingModifiedValues' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` pendingCloudWatchLogsExports
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` caCertificateIdentifier
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` licenseModel

instance Prelude.NFData AwsRdsDbPendingModifiedValues where
  rnf AwsRdsDbPendingModifiedValues' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf pendingCloudWatchLogsExports
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf processorFeatures
      `Prelude.seq` Prelude.rnf caCertificateIdentifier
      `Prelude.seq` Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf licenseModel

instance Core.ToJSON AwsRdsDbPendingModifiedValues where
  toJSON AwsRdsDbPendingModifiedValues' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Port" Core..=) Prelude.<$> port,
            ("BackupRetentionPeriod" Core..=)
              Prelude.<$> backupRetentionPeriod,
            ("DbInstanceClass" Core..=)
              Prelude.<$> dbInstanceClass,
            ("DbSubnetGroupName" Core..=)
              Prelude.<$> dbSubnetGroupName,
            ("DbInstanceIdentifier" Core..=)
              Prelude.<$> dbInstanceIdentifier,
            ("PendingCloudWatchLogsExports" Core..=)
              Prelude.<$> pendingCloudWatchLogsExports,
            ("MasterUserPassword" Core..=)
              Prelude.<$> masterUserPassword,
            ("StorageType" Core..=) Prelude.<$> storageType,
            ("ProcessorFeatures" Core..=)
              Prelude.<$> processorFeatures,
            ("CaCertificateIdentifier" Core..=)
              Prelude.<$> caCertificateIdentifier,
            ("AllocatedStorage" Core..=)
              Prelude.<$> allocatedStorage,
            ("Iops" Core..=) Prelude.<$> iops,
            ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            ("MultiAZ" Core..=) Prelude.<$> multiAZ,
            ("LicenseModel" Core..=) Prelude.<$> licenseModel
          ]
      )
