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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbPendingModifiedValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRdsDbProcessorFeature
import Amazonka.SecurityHub.Types.AwsRdsPendingCloudWatchLogsExports

-- | Changes to a DB instance that are currently pending.
--
-- /See:/ 'newAwsRdsDbPendingModifiedValues' smart constructor.
data AwsRdsDbPendingModifiedValues = AwsRdsDbPendingModifiedValues'
  { -- | The new engine version for the DB instance.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The new master user password for the DB instance.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The name of the new subnet group for the DB instance.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The new provisioned IOPS value for the DB instance.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The new DB instance class for the DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | Processor features that are being updated.
    processorFeatures :: Prelude.Maybe [AwsRdsDbProcessorFeature],
    -- | The new license model value for the DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The new DB instance identifier for the DB instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The new CA certificate identifier for the DB instance.
    caCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A list of log types that are being enabled or disabled.
    pendingCloudWatchLogsExports :: Prelude.Maybe AwsRdsPendingCloudWatchLogsExports,
    -- | The new backup retention period for the DB instance.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Indicates that a single Availability Zone DB instance is changing to a
    -- multiple Availability Zone deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The new value of the allocated storage for the DB instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The new port for the DB instance.
    port :: Prelude.Maybe Prelude.Int,
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
-- 'engineVersion', 'awsRdsDbPendingModifiedValues_engineVersion' - The new engine version for the DB instance.
--
-- 'masterUserPassword', 'awsRdsDbPendingModifiedValues_masterUserPassword' - The new master user password for the DB instance.
--
-- 'dbSubnetGroupName', 'awsRdsDbPendingModifiedValues_dbSubnetGroupName' - The name of the new subnet group for the DB instance.
--
-- 'iops', 'awsRdsDbPendingModifiedValues_iops' - The new provisioned IOPS value for the DB instance.
--
-- 'dbInstanceClass', 'awsRdsDbPendingModifiedValues_dbInstanceClass' - The new DB instance class for the DB instance.
--
-- 'processorFeatures', 'awsRdsDbPendingModifiedValues_processorFeatures' - Processor features that are being updated.
--
-- 'licenseModel', 'awsRdsDbPendingModifiedValues_licenseModel' - The new license model value for the DB instance.
--
-- 'dbInstanceIdentifier', 'awsRdsDbPendingModifiedValues_dbInstanceIdentifier' - The new DB instance identifier for the DB instance.
--
-- 'caCertificateIdentifier', 'awsRdsDbPendingModifiedValues_caCertificateIdentifier' - The new CA certificate identifier for the DB instance.
--
-- 'pendingCloudWatchLogsExports', 'awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports' - A list of log types that are being enabled or disabled.
--
-- 'backupRetentionPeriod', 'awsRdsDbPendingModifiedValues_backupRetentionPeriod' - The new backup retention period for the DB instance.
--
-- 'multiAZ', 'awsRdsDbPendingModifiedValues_multiAZ' - Indicates that a single Availability Zone DB instance is changing to a
-- multiple Availability Zone deployment.
--
-- 'allocatedStorage', 'awsRdsDbPendingModifiedValues_allocatedStorage' - The new value of the allocated storage for the DB instance.
--
-- 'port', 'awsRdsDbPendingModifiedValues_port' - The new port for the DB instance.
--
-- 'storageType', 'awsRdsDbPendingModifiedValues_storageType' - The new storage type for the DB instance.
newAwsRdsDbPendingModifiedValues ::
  AwsRdsDbPendingModifiedValues
newAwsRdsDbPendingModifiedValues =
  AwsRdsDbPendingModifiedValues'
    { engineVersion =
        Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      iops = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      caCertificateIdentifier = Prelude.Nothing,
      pendingCloudWatchLogsExports =
        Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      port = Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | The new engine version for the DB instance.
awsRdsDbPendingModifiedValues_engineVersion :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_engineVersion = Lens.lens (\AwsRdsDbPendingModifiedValues' {engineVersion} -> engineVersion) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {engineVersion = a} :: AwsRdsDbPendingModifiedValues)

-- | The new master user password for the DB instance.
awsRdsDbPendingModifiedValues_masterUserPassword :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_masterUserPassword = Lens.lens (\AwsRdsDbPendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {masterUserPassword = a} :: AwsRdsDbPendingModifiedValues)

-- | The name of the new subnet group for the DB instance.
awsRdsDbPendingModifiedValues_dbSubnetGroupName :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_dbSubnetGroupName = Lens.lens (\AwsRdsDbPendingModifiedValues' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {dbSubnetGroupName = a} :: AwsRdsDbPendingModifiedValues)

-- | The new provisioned IOPS value for the DB instance.
awsRdsDbPendingModifiedValues_iops :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_iops = Lens.lens (\AwsRdsDbPendingModifiedValues' {iops} -> iops) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {iops = a} :: AwsRdsDbPendingModifiedValues)

-- | The new DB instance class for the DB instance.
awsRdsDbPendingModifiedValues_dbInstanceClass :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_dbInstanceClass = Lens.lens (\AwsRdsDbPendingModifiedValues' {dbInstanceClass} -> dbInstanceClass) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {dbInstanceClass = a} :: AwsRdsDbPendingModifiedValues)

-- | Processor features that are being updated.
awsRdsDbPendingModifiedValues_processorFeatures :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe [AwsRdsDbProcessorFeature])
awsRdsDbPendingModifiedValues_processorFeatures = Lens.lens (\AwsRdsDbPendingModifiedValues' {processorFeatures} -> processorFeatures) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {processorFeatures = a} :: AwsRdsDbPendingModifiedValues) Prelude.. Lens.mapping Lens.coerced

-- | The new license model value for the DB instance.
awsRdsDbPendingModifiedValues_licenseModel :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_licenseModel = Lens.lens (\AwsRdsDbPendingModifiedValues' {licenseModel} -> licenseModel) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {licenseModel = a} :: AwsRdsDbPendingModifiedValues)

-- | The new DB instance identifier for the DB instance.
awsRdsDbPendingModifiedValues_dbInstanceIdentifier :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_dbInstanceIdentifier = Lens.lens (\AwsRdsDbPendingModifiedValues' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {dbInstanceIdentifier = a} :: AwsRdsDbPendingModifiedValues)

-- | The new CA certificate identifier for the DB instance.
awsRdsDbPendingModifiedValues_caCertificateIdentifier :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_caCertificateIdentifier = Lens.lens (\AwsRdsDbPendingModifiedValues' {caCertificateIdentifier} -> caCertificateIdentifier) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {caCertificateIdentifier = a} :: AwsRdsDbPendingModifiedValues)

-- | A list of log types that are being enabled or disabled.
awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe AwsRdsPendingCloudWatchLogsExports)
awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports = Lens.lens (\AwsRdsDbPendingModifiedValues' {pendingCloudWatchLogsExports} -> pendingCloudWatchLogsExports) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {pendingCloudWatchLogsExports = a} :: AwsRdsDbPendingModifiedValues)

-- | The new backup retention period for the DB instance.
awsRdsDbPendingModifiedValues_backupRetentionPeriod :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_backupRetentionPeriod = Lens.lens (\AwsRdsDbPendingModifiedValues' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {backupRetentionPeriod = a} :: AwsRdsDbPendingModifiedValues)

-- | Indicates that a single Availability Zone DB instance is changing to a
-- multiple Availability Zone deployment.
awsRdsDbPendingModifiedValues_multiAZ :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Bool)
awsRdsDbPendingModifiedValues_multiAZ = Lens.lens (\AwsRdsDbPendingModifiedValues' {multiAZ} -> multiAZ) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {multiAZ = a} :: AwsRdsDbPendingModifiedValues)

-- | The new value of the allocated storage for the DB instance.
awsRdsDbPendingModifiedValues_allocatedStorage :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_allocatedStorage = Lens.lens (\AwsRdsDbPendingModifiedValues' {allocatedStorage} -> allocatedStorage) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {allocatedStorage = a} :: AwsRdsDbPendingModifiedValues)

-- | The new port for the DB instance.
awsRdsDbPendingModifiedValues_port :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Int)
awsRdsDbPendingModifiedValues_port = Lens.lens (\AwsRdsDbPendingModifiedValues' {port} -> port) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {port = a} :: AwsRdsDbPendingModifiedValues)

-- | The new storage type for the DB instance.
awsRdsDbPendingModifiedValues_storageType :: Lens.Lens' AwsRdsDbPendingModifiedValues (Prelude.Maybe Prelude.Text)
awsRdsDbPendingModifiedValues_storageType = Lens.lens (\AwsRdsDbPendingModifiedValues' {storageType} -> storageType) (\s@AwsRdsDbPendingModifiedValues' {} a -> s {storageType = a} :: AwsRdsDbPendingModifiedValues)

instance Core.FromJSON AwsRdsDbPendingModifiedValues where
  parseJSON =
    Core.withObject
      "AwsRdsDbPendingModifiedValues"
      ( \x ->
          AwsRdsDbPendingModifiedValues'
            Prelude.<$> (x Core..:? "EngineVersion")
            Prelude.<*> (x Core..:? "MasterUserPassword")
            Prelude.<*> (x Core..:? "DbSubnetGroupName")
            Prelude.<*> (x Core..:? "Iops")
            Prelude.<*> (x Core..:? "DbInstanceClass")
            Prelude.<*> ( x Core..:? "ProcessorFeatures"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LicenseModel")
            Prelude.<*> (x Core..:? "DbInstanceIdentifier")
            Prelude.<*> (x Core..:? "CaCertificateIdentifier")
            Prelude.<*> (x Core..:? "PendingCloudWatchLogsExports")
            Prelude.<*> (x Core..:? "BackupRetentionPeriod")
            Prelude.<*> (x Core..:? "MultiAZ")
            Prelude.<*> (x Core..:? "AllocatedStorage")
            Prelude.<*> (x Core..:? "Port")
            Prelude.<*> (x Core..:? "StorageType")
      )

instance
  Prelude.Hashable
    AwsRdsDbPendingModifiedValues
  where
  hashWithSalt salt' AwsRdsDbPendingModifiedValues' {..} =
    salt' `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` pendingCloudWatchLogsExports
      `Prelude.hashWithSalt` caCertificateIdentifier
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData AwsRdsDbPendingModifiedValues where
  rnf AwsRdsDbPendingModifiedValues' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf pendingCloudWatchLogsExports
      `Prelude.seq` Prelude.rnf caCertificateIdentifier
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf processorFeatures
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf masterUserPassword

instance Core.ToJSON AwsRdsDbPendingModifiedValues where
  toJSON AwsRdsDbPendingModifiedValues' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            ("MasterUserPassword" Core..=)
              Prelude.<$> masterUserPassword,
            ("DbSubnetGroupName" Core..=)
              Prelude.<$> dbSubnetGroupName,
            ("Iops" Core..=) Prelude.<$> iops,
            ("DbInstanceClass" Core..=)
              Prelude.<$> dbInstanceClass,
            ("ProcessorFeatures" Core..=)
              Prelude.<$> processorFeatures,
            ("LicenseModel" Core..=) Prelude.<$> licenseModel,
            ("DbInstanceIdentifier" Core..=)
              Prelude.<$> dbInstanceIdentifier,
            ("CaCertificateIdentifier" Core..=)
              Prelude.<$> caCertificateIdentifier,
            ("PendingCloudWatchLogsExports" Core..=)
              Prelude.<$> pendingCloudWatchLogsExports,
            ("BackupRetentionPeriod" Core..=)
              Prelude.<$> backupRetentionPeriod,
            ("MultiAZ" Core..=) Prelude.<$> multiAZ,
            ("AllocatedStorage" Core..=)
              Prelude.<$> allocatedStorage,
            ("Port" Core..=) Prelude.<$> port,
            ("StorageType" Core..=) Prelude.<$> storageType
          ]
      )
