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
-- Module      : Amazonka.RDS.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.PendingModifiedValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.AutomationMode
import Amazonka.RDS.Types.PendingCloudwatchLogsExports
import Amazonka.RDS.Types.ProcessorFeature

-- | This data type is used as a response element in the @ModifyDBInstance@
-- operation and contains changes that will be applied during the next
-- maintenance window.
--
-- /See:/ 'newPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { -- | The allocated storage size for the DB instance specified in gibibytes
    -- (GiB).
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The automation mode of the RDS Custom DB instance: @full@ or
    -- @all-paused@. If @full@, the DB instance automates monitoring and
    -- instance recovery. If @all-paused@, the instance pauses automation for
    -- the duration set by @--resume-full-automation-mode-minutes@.
    automationMode :: Prelude.Maybe AutomationMode,
    -- | The number of days for which automated backups are retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the CA certificate for the DB instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
    -- in the /Amazon RDS User Guide/ and
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
    -- in the /Amazon Aurora User Guide/.
    cACertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute and memory capacity class for the DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The database identifier for the DB instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The DB subnet group for the DB instance.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Whether mapping of Amazon Web Services Identity and Access Management
    -- (IAM) accounts to database accounts is enabled.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Provisioned IOPS value for the DB instance.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The license model for the DB instance.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ |
    -- @general-public-license@
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The master credentials for the DB instance.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates that the Single-AZ DB instance will change to a
    -- Multi-AZ deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    pendingCloudwatchLogsExports :: Prelude.Maybe PendingCloudwatchLogsExports,
    -- | The port for the DB instance.
    port :: Prelude.Maybe Prelude.Int,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | The number of minutes to pause the automation. When the time period
    -- ends, RDS Custom resumes full automation. The minimum value is 60
    -- (default). The maximum value is 1,440.
    resumeFullAutomationModeTime :: Prelude.Maybe Data.ISO8601,
    -- | The storage throughput of the DB instance.
    storageThroughput :: Prelude.Maybe Prelude.Int,
    -- | The storage type of the DB instance.
    storageType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedStorage', 'pendingModifiedValues_allocatedStorage' - The allocated storage size for the DB instance specified in gibibytes
-- (GiB).
--
-- 'automationMode', 'pendingModifiedValues_automationMode' - The automation mode of the RDS Custom DB instance: @full@ or
-- @all-paused@. If @full@, the DB instance automates monitoring and
-- instance recovery. If @all-paused@, the instance pauses automation for
-- the duration set by @--resume-full-automation-mode-minutes@.
--
-- 'backupRetentionPeriod', 'pendingModifiedValues_backupRetentionPeriod' - The number of days for which automated backups are retained.
--
-- 'cACertificateIdentifier', 'pendingModifiedValues_cACertificateIdentifier' - The identifier of the CA certificate for the DB instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
-- in the /Amazon Aurora User Guide/.
--
-- 'dbInstanceClass', 'pendingModifiedValues_dbInstanceClass' - The name of the compute and memory capacity class for the DB instance.
--
-- 'dbInstanceIdentifier', 'pendingModifiedValues_dbInstanceIdentifier' - The database identifier for the DB instance.
--
-- 'dbSubnetGroupName', 'pendingModifiedValues_dbSubnetGroupName' - The DB subnet group for the DB instance.
--
-- 'engineVersion', 'pendingModifiedValues_engineVersion' - The database engine version.
--
-- 'iAMDatabaseAuthenticationEnabled', 'pendingModifiedValues_iAMDatabaseAuthenticationEnabled' - Whether mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled.
--
-- 'iops', 'pendingModifiedValues_iops' - The Provisioned IOPS value for the DB instance.
--
-- 'licenseModel', 'pendingModifiedValues_licenseModel' - The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
--
-- 'masterUserPassword', 'pendingModifiedValues_masterUserPassword' - The master credentials for the DB instance.
--
-- 'multiAZ', 'pendingModifiedValues_multiAZ' - A value that indicates that the Single-AZ DB instance will change to a
-- Multi-AZ deployment.
--
-- 'pendingCloudwatchLogsExports', 'pendingModifiedValues_pendingCloudwatchLogsExports' - Undocumented member.
--
-- 'port', 'pendingModifiedValues_port' - The port for the DB instance.
--
-- 'processorFeatures', 'pendingModifiedValues_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'resumeFullAutomationModeTime', 'pendingModifiedValues_resumeFullAutomationModeTime' - The number of minutes to pause the automation. When the time period
-- ends, RDS Custom resumes full automation. The minimum value is 60
-- (default). The maximum value is 1,440.
--
-- 'storageThroughput', 'pendingModifiedValues_storageThroughput' - The storage throughput of the DB instance.
--
-- 'storageType', 'pendingModifiedValues_storageType' - The storage type of the DB instance.
newPendingModifiedValues ::
  PendingModifiedValues
newPendingModifiedValues =
  PendingModifiedValues'
    { allocatedStorage =
        Prelude.Nothing,
      automationMode = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      cACertificateIdentifier = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled = Prelude.Nothing,
      iops = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      pendingCloudwatchLogsExports = Prelude.Nothing,
      port = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      resumeFullAutomationModeTime = Prelude.Nothing,
      storageThroughput = Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | The allocated storage size for the DB instance specified in gibibytes
-- (GiB).
pendingModifiedValues_allocatedStorage :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_allocatedStorage = Lens.lens (\PendingModifiedValues' {allocatedStorage} -> allocatedStorage) (\s@PendingModifiedValues' {} a -> s {allocatedStorage = a} :: PendingModifiedValues)

-- | The automation mode of the RDS Custom DB instance: @full@ or
-- @all-paused@. If @full@, the DB instance automates monitoring and
-- instance recovery. If @all-paused@, the instance pauses automation for
-- the duration set by @--resume-full-automation-mode-minutes@.
pendingModifiedValues_automationMode :: Lens.Lens' PendingModifiedValues (Prelude.Maybe AutomationMode)
pendingModifiedValues_automationMode = Lens.lens (\PendingModifiedValues' {automationMode} -> automationMode) (\s@PendingModifiedValues' {} a -> s {automationMode = a} :: PendingModifiedValues)

-- | The number of days for which automated backups are retained.
pendingModifiedValues_backupRetentionPeriod :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_backupRetentionPeriod = Lens.lens (\PendingModifiedValues' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@PendingModifiedValues' {} a -> s {backupRetentionPeriod = a} :: PendingModifiedValues)

-- | The identifier of the CA certificate for the DB instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
-- in the /Amazon Aurora User Guide/.
pendingModifiedValues_cACertificateIdentifier :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_cACertificateIdentifier = Lens.lens (\PendingModifiedValues' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@PendingModifiedValues' {} a -> s {cACertificateIdentifier = a} :: PendingModifiedValues)

-- | The name of the compute and memory capacity class for the DB instance.
pendingModifiedValues_dbInstanceClass :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_dbInstanceClass = Lens.lens (\PendingModifiedValues' {dbInstanceClass} -> dbInstanceClass) (\s@PendingModifiedValues' {} a -> s {dbInstanceClass = a} :: PendingModifiedValues)

-- | The database identifier for the DB instance.
pendingModifiedValues_dbInstanceIdentifier :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_dbInstanceIdentifier = Lens.lens (\PendingModifiedValues' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@PendingModifiedValues' {} a -> s {dbInstanceIdentifier = a} :: PendingModifiedValues)

-- | The DB subnet group for the DB instance.
pendingModifiedValues_dbSubnetGroupName :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_dbSubnetGroupName = Lens.lens (\PendingModifiedValues' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@PendingModifiedValues' {} a -> s {dbSubnetGroupName = a} :: PendingModifiedValues)

-- | The database engine version.
pendingModifiedValues_engineVersion :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_engineVersion = Lens.lens (\PendingModifiedValues' {engineVersion} -> engineVersion) (\s@PendingModifiedValues' {} a -> s {engineVersion = a} :: PendingModifiedValues)

-- | Whether mapping of Amazon Web Services Identity and Access Management
-- (IAM) accounts to database accounts is enabled.
pendingModifiedValues_iAMDatabaseAuthenticationEnabled :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Bool)
pendingModifiedValues_iAMDatabaseAuthenticationEnabled = Lens.lens (\PendingModifiedValues' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@PendingModifiedValues' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: PendingModifiedValues)

-- | The Provisioned IOPS value for the DB instance.
pendingModifiedValues_iops :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_iops = Lens.lens (\PendingModifiedValues' {iops} -> iops) (\s@PendingModifiedValues' {} a -> s {iops = a} :: PendingModifiedValues)

-- | The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
pendingModifiedValues_licenseModel :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_licenseModel = Lens.lens (\PendingModifiedValues' {licenseModel} -> licenseModel) (\s@PendingModifiedValues' {} a -> s {licenseModel = a} :: PendingModifiedValues)

-- | The master credentials for the DB instance.
pendingModifiedValues_masterUserPassword :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_masterUserPassword = Lens.lens (\PendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@PendingModifiedValues' {} a -> s {masterUserPassword = a} :: PendingModifiedValues)

-- | A value that indicates that the Single-AZ DB instance will change to a
-- Multi-AZ deployment.
pendingModifiedValues_multiAZ :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Bool)
pendingModifiedValues_multiAZ = Lens.lens (\PendingModifiedValues' {multiAZ} -> multiAZ) (\s@PendingModifiedValues' {} a -> s {multiAZ = a} :: PendingModifiedValues)

-- | Undocumented member.
pendingModifiedValues_pendingCloudwatchLogsExports :: Lens.Lens' PendingModifiedValues (Prelude.Maybe PendingCloudwatchLogsExports)
pendingModifiedValues_pendingCloudwatchLogsExports = Lens.lens (\PendingModifiedValues' {pendingCloudwatchLogsExports} -> pendingCloudwatchLogsExports) (\s@PendingModifiedValues' {} a -> s {pendingCloudwatchLogsExports = a} :: PendingModifiedValues)

-- | The port for the DB instance.
pendingModifiedValues_port :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_port = Lens.lens (\PendingModifiedValues' {port} -> port) (\s@PendingModifiedValues' {} a -> s {port = a} :: PendingModifiedValues)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
pendingModifiedValues_processorFeatures :: Lens.Lens' PendingModifiedValues (Prelude.Maybe [ProcessorFeature])
pendingModifiedValues_processorFeatures = Lens.lens (\PendingModifiedValues' {processorFeatures} -> processorFeatures) (\s@PendingModifiedValues' {} a -> s {processorFeatures = a} :: PendingModifiedValues) Prelude.. Lens.mapping Lens.coerced

-- | The number of minutes to pause the automation. When the time period
-- ends, RDS Custom resumes full automation. The minimum value is 60
-- (default). The maximum value is 1,440.
pendingModifiedValues_resumeFullAutomationModeTime :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.UTCTime)
pendingModifiedValues_resumeFullAutomationModeTime = Lens.lens (\PendingModifiedValues' {resumeFullAutomationModeTime} -> resumeFullAutomationModeTime) (\s@PendingModifiedValues' {} a -> s {resumeFullAutomationModeTime = a} :: PendingModifiedValues) Prelude.. Lens.mapping Data._Time

-- | The storage throughput of the DB instance.
pendingModifiedValues_storageThroughput :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_storageThroughput = Lens.lens (\PendingModifiedValues' {storageThroughput} -> storageThroughput) (\s@PendingModifiedValues' {} a -> s {storageThroughput = a} :: PendingModifiedValues)

-- | The storage type of the DB instance.
pendingModifiedValues_storageType :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_storageType = Lens.lens (\PendingModifiedValues' {storageType} -> storageType) (\s@PendingModifiedValues' {} a -> s {storageType = a} :: PendingModifiedValues)

instance Data.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Prelude.<$> (x Data..@? "AllocatedStorage")
      Prelude.<*> (x Data..@? "AutomationMode")
      Prelude.<*> (x Data..@? "BackupRetentionPeriod")
      Prelude.<*> (x Data..@? "CACertificateIdentifier")
      Prelude.<*> (x Data..@? "DBInstanceClass")
      Prelude.<*> (x Data..@? "DBInstanceIdentifier")
      Prelude.<*> (x Data..@? "DBSubnetGroupName")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Data..@? "Iops")
      Prelude.<*> (x Data..@? "LicenseModel")
      Prelude.<*> (x Data..@? "MasterUserPassword")
      Prelude.<*> (x Data..@? "MultiAZ")
      Prelude.<*> (x Data..@? "PendingCloudwatchLogsExports")
      Prelude.<*> (x Data..@? "Port")
      Prelude.<*> ( x
                      Data..@? "ProcessorFeatures"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "ProcessorFeature")
                  )
      Prelude.<*> (x Data..@? "ResumeFullAutomationModeTime")
      Prelude.<*> (x Data..@? "StorageThroughput")
      Prelude.<*> (x Data..@? "StorageType")

instance Prelude.Hashable PendingModifiedValues where
  hashWithSalt _salt PendingModifiedValues' {..} =
    _salt
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` automationMode
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` cACertificateIdentifier
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` iAMDatabaseAuthenticationEnabled
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` pendingCloudwatchLogsExports
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` resumeFullAutomationModeTime
      `Prelude.hashWithSalt` storageThroughput
      `Prelude.hashWithSalt` storageType

instance Prelude.NFData PendingModifiedValues where
  rnf PendingModifiedValues' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf automationMode
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf cACertificateIdentifier
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf iAMDatabaseAuthenticationEnabled
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf pendingCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf processorFeatures
      `Prelude.seq` Prelude.rnf
        resumeFullAutomationModeTime
      `Prelude.seq` Prelude.rnf storageThroughput
      `Prelude.seq` Prelude.rnf storageType
