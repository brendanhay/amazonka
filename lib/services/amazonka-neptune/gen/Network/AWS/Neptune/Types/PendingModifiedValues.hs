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
-- Module      : Network.AWS.Neptune.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Neptune.Types.PendingModifiedValues where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Neptune.Types.PendingCloudwatchLogsExports
import qualified Network.AWS.Prelude as Prelude

-- | This data type is used as a response element in the ModifyDBInstance
-- action.
--
-- /See:/ 'newPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { -- | Indicates the database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Not supported by Neptune.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The new DB subnet group for the DB instance.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the new Provisioned IOPS value for the DB instance that will
    -- be applied or is currently being applied.
    iops :: Prelude.Maybe Prelude.Int,
    -- | Contains the new @DBInstanceClass@ for the DB instance that will be
    -- applied or is currently being applied.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | Not supported by Neptune.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | Specifies the identifier of the CA certificate for the DB instance.
    cACertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Contains the new @DBInstanceIdentifier@ for the DB instance that will be
    -- applied or is currently being applied.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | This @PendingCloudwatchLogsExports@ structure specifies pending changes
    -- to which CloudWatch logs are enabled and which are disabled.
    pendingCloudwatchLogsExports :: Prelude.Maybe PendingCloudwatchLogsExports,
    -- | Specifies the pending number of days for which automated backups are
    -- retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ
    -- deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | Contains the new @AllocatedStorage@ size for the DB instance that will
    -- be applied or is currently being applied.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Specifies the pending port for the DB instance.
    port :: Prelude.Maybe Prelude.Int,
    -- | Specifies the storage type to be associated with the DB instance.
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
-- 'engineVersion', 'pendingModifiedValues_engineVersion' - Indicates the database engine version.
--
-- 'masterUserPassword', 'pendingModifiedValues_masterUserPassword' - Not supported by Neptune.
--
-- 'dbSubnetGroupName', 'pendingModifiedValues_dbSubnetGroupName' - The new DB subnet group for the DB instance.
--
-- 'iops', 'pendingModifiedValues_iops' - Specifies the new Provisioned IOPS value for the DB instance that will
-- be applied or is currently being applied.
--
-- 'dbInstanceClass', 'pendingModifiedValues_dbInstanceClass' - Contains the new @DBInstanceClass@ for the DB instance that will be
-- applied or is currently being applied.
--
-- 'licenseModel', 'pendingModifiedValues_licenseModel' - Not supported by Neptune.
--
-- 'cACertificateIdentifier', 'pendingModifiedValues_cACertificateIdentifier' - Specifies the identifier of the CA certificate for the DB instance.
--
-- 'dbInstanceIdentifier', 'pendingModifiedValues_dbInstanceIdentifier' - Contains the new @DBInstanceIdentifier@ for the DB instance that will be
-- applied or is currently being applied.
--
-- 'pendingCloudwatchLogsExports', 'pendingModifiedValues_pendingCloudwatchLogsExports' - This @PendingCloudwatchLogsExports@ structure specifies pending changes
-- to which CloudWatch logs are enabled and which are disabled.
--
-- 'backupRetentionPeriod', 'pendingModifiedValues_backupRetentionPeriod' - Specifies the pending number of days for which automated backups are
-- retained.
--
-- 'multiAZ', 'pendingModifiedValues_multiAZ' - Indicates that the Single-AZ DB instance is to change to a Multi-AZ
-- deployment.
--
-- 'allocatedStorage', 'pendingModifiedValues_allocatedStorage' - Contains the new @AllocatedStorage@ size for the DB instance that will
-- be applied or is currently being applied.
--
-- 'port', 'pendingModifiedValues_port' - Specifies the pending port for the DB instance.
--
-- 'storageType', 'pendingModifiedValues_storageType' - Specifies the storage type to be associated with the DB instance.
newPendingModifiedValues ::
  PendingModifiedValues
newPendingModifiedValues =
  PendingModifiedValues'
    { engineVersion =
        Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      iops = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      cACertificateIdentifier = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      pendingCloudwatchLogsExports = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      port = Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | Indicates the database engine version.
pendingModifiedValues_engineVersion :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_engineVersion = Lens.lens (\PendingModifiedValues' {engineVersion} -> engineVersion) (\s@PendingModifiedValues' {} a -> s {engineVersion = a} :: PendingModifiedValues)

-- | Not supported by Neptune.
pendingModifiedValues_masterUserPassword :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_masterUserPassword = Lens.lens (\PendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@PendingModifiedValues' {} a -> s {masterUserPassword = a} :: PendingModifiedValues)

-- | The new DB subnet group for the DB instance.
pendingModifiedValues_dbSubnetGroupName :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_dbSubnetGroupName = Lens.lens (\PendingModifiedValues' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@PendingModifiedValues' {} a -> s {dbSubnetGroupName = a} :: PendingModifiedValues)

-- | Specifies the new Provisioned IOPS value for the DB instance that will
-- be applied or is currently being applied.
pendingModifiedValues_iops :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_iops = Lens.lens (\PendingModifiedValues' {iops} -> iops) (\s@PendingModifiedValues' {} a -> s {iops = a} :: PendingModifiedValues)

-- | Contains the new @DBInstanceClass@ for the DB instance that will be
-- applied or is currently being applied.
pendingModifiedValues_dbInstanceClass :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_dbInstanceClass = Lens.lens (\PendingModifiedValues' {dbInstanceClass} -> dbInstanceClass) (\s@PendingModifiedValues' {} a -> s {dbInstanceClass = a} :: PendingModifiedValues)

-- | Not supported by Neptune.
pendingModifiedValues_licenseModel :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_licenseModel = Lens.lens (\PendingModifiedValues' {licenseModel} -> licenseModel) (\s@PendingModifiedValues' {} a -> s {licenseModel = a} :: PendingModifiedValues)

-- | Specifies the identifier of the CA certificate for the DB instance.
pendingModifiedValues_cACertificateIdentifier :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_cACertificateIdentifier = Lens.lens (\PendingModifiedValues' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@PendingModifiedValues' {} a -> s {cACertificateIdentifier = a} :: PendingModifiedValues)

-- | Contains the new @DBInstanceIdentifier@ for the DB instance that will be
-- applied or is currently being applied.
pendingModifiedValues_dbInstanceIdentifier :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_dbInstanceIdentifier = Lens.lens (\PendingModifiedValues' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@PendingModifiedValues' {} a -> s {dbInstanceIdentifier = a} :: PendingModifiedValues)

-- | This @PendingCloudwatchLogsExports@ structure specifies pending changes
-- to which CloudWatch logs are enabled and which are disabled.
pendingModifiedValues_pendingCloudwatchLogsExports :: Lens.Lens' PendingModifiedValues (Prelude.Maybe PendingCloudwatchLogsExports)
pendingModifiedValues_pendingCloudwatchLogsExports = Lens.lens (\PendingModifiedValues' {pendingCloudwatchLogsExports} -> pendingCloudwatchLogsExports) (\s@PendingModifiedValues' {} a -> s {pendingCloudwatchLogsExports = a} :: PendingModifiedValues)

-- | Specifies the pending number of days for which automated backups are
-- retained.
pendingModifiedValues_backupRetentionPeriod :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_backupRetentionPeriod = Lens.lens (\PendingModifiedValues' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@PendingModifiedValues' {} a -> s {backupRetentionPeriod = a} :: PendingModifiedValues)

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ
-- deployment.
pendingModifiedValues_multiAZ :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Bool)
pendingModifiedValues_multiAZ = Lens.lens (\PendingModifiedValues' {multiAZ} -> multiAZ) (\s@PendingModifiedValues' {} a -> s {multiAZ = a} :: PendingModifiedValues)

-- | Contains the new @AllocatedStorage@ size for the DB instance that will
-- be applied or is currently being applied.
pendingModifiedValues_allocatedStorage :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_allocatedStorage = Lens.lens (\PendingModifiedValues' {allocatedStorage} -> allocatedStorage) (\s@PendingModifiedValues' {} a -> s {allocatedStorage = a} :: PendingModifiedValues)

-- | Specifies the pending port for the DB instance.
pendingModifiedValues_port :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_port = Lens.lens (\PendingModifiedValues' {port} -> port) (\s@PendingModifiedValues' {} a -> s {port = a} :: PendingModifiedValues)

-- | Specifies the storage type to be associated with the DB instance.
pendingModifiedValues_storageType :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_storageType = Lens.lens (\PendingModifiedValues' {storageType} -> storageType) (\s@PendingModifiedValues' {} a -> s {storageType = a} :: PendingModifiedValues)

instance Core.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Prelude.<$> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "MasterUserPassword")
      Prelude.<*> (x Core..@? "DBSubnetGroupName")
      Prelude.<*> (x Core..@? "Iops")
      Prelude.<*> (x Core..@? "DBInstanceClass")
      Prelude.<*> (x Core..@? "LicenseModel")
      Prelude.<*> (x Core..@? "CACertificateIdentifier")
      Prelude.<*> (x Core..@? "DBInstanceIdentifier")
      Prelude.<*> (x Core..@? "PendingCloudwatchLogsExports")
      Prelude.<*> (x Core..@? "BackupRetentionPeriod")
      Prelude.<*> (x Core..@? "MultiAZ")
      Prelude.<*> (x Core..@? "AllocatedStorage")
      Prelude.<*> (x Core..@? "Port")
      Prelude.<*> (x Core..@? "StorageType")

instance Prelude.Hashable PendingModifiedValues

instance Prelude.NFData PendingModifiedValues
