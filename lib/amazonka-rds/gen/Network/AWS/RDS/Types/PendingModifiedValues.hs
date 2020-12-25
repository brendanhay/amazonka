{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.PendingModifiedValues
  ( PendingModifiedValues (..),

    -- * Smart constructor
    mkPendingModifiedValues,

    -- * Lenses
    pmvAllocatedStorage,
    pmvBackupRetentionPeriod,
    pmvCACertificateIdentifier,
    pmvDBInstanceClass,
    pmvDBInstanceIdentifier,
    pmvDBSubnetGroupName,
    pmvEngineVersion,
    pmvIops,
    pmvLicenseModel,
    pmvMasterUserPassword,
    pmvMultiAZ,
    pmvPendingCloudwatchLogsExports,
    pmvPort,
    pmvProcessorFeatures,
    pmvStorageType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.PendingCloudwatchLogsExports as Types
import qualified Network.AWS.RDS.Types.ProcessorFeature as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | This data type is used as a response element in the @ModifyDBInstance@ action.
--
-- /See:/ 'mkPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { -- | Contains the new @AllocatedStorage@ size for the DB instance that will be applied or is currently being applied.
    allocatedStorage :: Core.Maybe Core.Int,
    -- | Specifies the pending number of days for which automated backups are retained.
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | Specifies the identifier of the CA certificate for the DB instance.
    cACertificateIdentifier :: Core.Maybe Types.String,
    -- | Contains the new @DBInstanceClass@ for the DB instance that will be applied or is currently being applied.
    dBInstanceClass :: Core.Maybe Types.String,
    -- | Contains the new @DBInstanceIdentifier@ for the DB instance that will be applied or is currently being applied.
    dBInstanceIdentifier :: Core.Maybe Types.String,
    -- | The new DB subnet group for the DB instance.
    dBSubnetGroupName :: Core.Maybe Types.String,
    -- | Indicates the database engine version.
    engineVersion :: Core.Maybe Types.String,
    -- | Specifies the new Provisioned IOPS value for the DB instance that will be applied or is currently being applied.
    iops :: Core.Maybe Core.Int,
    -- | The license model for the DB instance.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
    licenseModel :: Core.Maybe Types.String,
    -- | Contains the pending or currently-in-progress change of the master credentials for the DB instance.
    masterUserPassword :: Core.Maybe Types.String,
    -- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ deployment.
    multiAZ :: Core.Maybe Core.Bool,
    pendingCloudwatchLogsExports :: Core.Maybe Types.PendingCloudwatchLogsExports,
    -- | Specifies the pending port for the DB instance.
    port :: Core.Maybe Core.Int,
    -- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
    processorFeatures :: Core.Maybe [Types.ProcessorFeature],
    -- | Specifies the storage type to be associated with the DB instance.
    storageType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PendingModifiedValues' value with any optional fields omitted.
mkPendingModifiedValues ::
  PendingModifiedValues
mkPendingModifiedValues =
  PendingModifiedValues'
    { allocatedStorage = Core.Nothing,
      backupRetentionPeriod = Core.Nothing,
      cACertificateIdentifier = Core.Nothing,
      dBInstanceClass = Core.Nothing,
      dBInstanceIdentifier = Core.Nothing,
      dBSubnetGroupName = Core.Nothing,
      engineVersion = Core.Nothing,
      iops = Core.Nothing,
      licenseModel = Core.Nothing,
      masterUserPassword = Core.Nothing,
      multiAZ = Core.Nothing,
      pendingCloudwatchLogsExports = Core.Nothing,
      port = Core.Nothing,
      processorFeatures = Core.Nothing,
      storageType = Core.Nothing
    }

-- | Contains the new @AllocatedStorage@ size for the DB instance that will be applied or is currently being applied.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvAllocatedStorage :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Int)
pmvAllocatedStorage = Lens.field @"allocatedStorage"
{-# DEPRECATED pmvAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | Specifies the pending number of days for which automated backups are retained.
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvBackupRetentionPeriod :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Int)
pmvBackupRetentionPeriod = Lens.field @"backupRetentionPeriod"
{-# DEPRECATED pmvBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | Specifies the identifier of the CA certificate for the DB instance.
--
-- /Note:/ Consider using 'cACertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvCACertificateIdentifier :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.String)
pmvCACertificateIdentifier = Lens.field @"cACertificateIdentifier"
{-# DEPRECATED pmvCACertificateIdentifier "Use generic-lens or generic-optics with 'cACertificateIdentifier' instead." #-}

-- | Contains the new @DBInstanceClass@ for the DB instance that will be applied or is currently being applied.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvDBInstanceClass :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.String)
pmvDBInstanceClass = Lens.field @"dBInstanceClass"
{-# DEPRECATED pmvDBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead." #-}

-- | Contains the new @DBInstanceIdentifier@ for the DB instance that will be applied or is currently being applied.
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvDBInstanceIdentifier :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.String)
pmvDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED pmvDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

-- | The new DB subnet group for the DB instance.
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvDBSubnetGroupName :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.String)
pmvDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED pmvDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

-- | Indicates the database engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvEngineVersion :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.String)
pmvEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED pmvEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Specifies the new Provisioned IOPS value for the DB instance that will be applied or is currently being applied.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvIops :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Int)
pmvIops = Lens.field @"iops"
{-# DEPRECATED pmvIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvLicenseModel :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.String)
pmvLicenseModel = Lens.field @"licenseModel"
{-# DEPRECATED pmvLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | Contains the pending or currently-in-progress change of the master credentials for the DB instance.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvMasterUserPassword :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.String)
pmvMasterUserPassword = Lens.field @"masterUserPassword"
{-# DEPRECATED pmvMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ deployment.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvMultiAZ :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Bool)
pmvMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED pmvMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pendingCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvPendingCloudwatchLogsExports :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.PendingCloudwatchLogsExports)
pmvPendingCloudwatchLogsExports = Lens.field @"pendingCloudwatchLogsExports"
{-# DEPRECATED pmvPendingCloudwatchLogsExports "Use generic-lens or generic-optics with 'pendingCloudwatchLogsExports' instead." #-}

-- | Specifies the pending port for the DB instance.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvPort :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Int)
pmvPort = Lens.field @"port"
{-# DEPRECATED pmvPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvProcessorFeatures :: Lens.Lens' PendingModifiedValues (Core.Maybe [Types.ProcessorFeature])
pmvProcessorFeatures = Lens.field @"processorFeatures"
{-# DEPRECATED pmvProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvStorageType :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.String)
pmvStorageType = Lens.field @"storageType"
{-# DEPRECATED pmvStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Core.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Core.<$> (x Core..@? "AllocatedStorage")
      Core.<*> (x Core..@? "BackupRetentionPeriod")
      Core.<*> (x Core..@? "CACertificateIdentifier")
      Core.<*> (x Core..@? "DBInstanceClass")
      Core.<*> (x Core..@? "DBInstanceIdentifier")
      Core.<*> (x Core..@? "DBSubnetGroupName")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "Iops")
      Core.<*> (x Core..@? "LicenseModel")
      Core.<*> (x Core..@? "MasterUserPassword")
      Core.<*> (x Core..@? "MultiAZ")
      Core.<*> (x Core..@? "PendingCloudwatchLogsExports")
      Core.<*> (x Core..@? "Port")
      Core.<*> ( x Core..@? "ProcessorFeatures"
                   Core..<@> Core.parseXMLList "ProcessorFeature"
               )
      Core.<*> (x Core..@? "StorageType")
