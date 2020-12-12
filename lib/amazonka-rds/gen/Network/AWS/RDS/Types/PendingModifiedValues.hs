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
    pmvEngineVersion,
    pmvMasterUserPassword,
    pmvDBSubnetGroupName,
    pmvIOPS,
    pmvProcessorFeatures,
    pmvDBInstanceClass,
    pmvLicenseModel,
    pmvCACertificateIdentifier,
    pmvDBInstanceIdentifier,
    pmvPendingCloudwatchLogsExports,
    pmvBackupRetentionPeriod,
    pmvMultiAZ,
    pmvAllocatedStorage,
    pmvPort,
    pmvStorageType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.PendingCloudwatchLogsExports
import Network.AWS.RDS.Types.ProcessorFeature

-- | This data type is used as a response element in the @ModifyDBInstance@ action.
--
-- /See:/ 'mkPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    masterUserPassword :: Lude.Maybe Lude.Text,
    dbSubnetGroupName :: Lude.Maybe Lude.Text,
    iops :: Lude.Maybe Lude.Int,
    processorFeatures ::
      Lude.Maybe [ProcessorFeature],
    dbInstanceClass :: Lude.Maybe Lude.Text,
    licenseModel :: Lude.Maybe Lude.Text,
    cACertificateIdentifier :: Lude.Maybe Lude.Text,
    dbInstanceIdentifier :: Lude.Maybe Lude.Text,
    pendingCloudwatchLogsExports ::
      Lude.Maybe PendingCloudwatchLogsExports,
    backupRetentionPeriod :: Lude.Maybe Lude.Int,
    multiAZ :: Lude.Maybe Lude.Bool,
    allocatedStorage :: Lude.Maybe Lude.Int,
    port :: Lude.Maybe Lude.Int,
    storageType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PendingModifiedValues' with the minimum fields required to make a request.
--
-- * 'allocatedStorage' - Contains the new @AllocatedStorage@ size for the DB instance that will be applied or is currently being applied.
-- * 'backupRetentionPeriod' - Specifies the pending number of days for which automated backups are retained.
-- * 'cACertificateIdentifier' - Specifies the identifier of the CA certificate for the DB instance.
-- * 'dbInstanceClass' - Contains the new @DBInstanceClass@ for the DB instance that will be applied or is currently being applied.
-- * 'dbInstanceIdentifier' - Contains the new @DBInstanceIdentifier@ for the DB instance that will be applied or is currently being applied.
-- * 'dbSubnetGroupName' - The new DB subnet group for the DB instance.
-- * 'engineVersion' - Indicates the database engine version.
-- * 'iops' - Specifies the new Provisioned IOPS value for the DB instance that will be applied or is currently being applied.
-- * 'licenseModel' - The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
-- * 'masterUserPassword' - Contains the pending or currently-in-progress change of the master credentials for the DB instance.
-- * 'multiAZ' - Indicates that the Single-AZ DB instance is to change to a Multi-AZ deployment.
-- * 'pendingCloudwatchLogsExports' - Undocumented field.
-- * 'port' - Specifies the pending port for the DB instance.
-- * 'processorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
-- * 'storageType' - Specifies the storage type to be associated with the DB instance.
mkPendingModifiedValues ::
  PendingModifiedValues
mkPendingModifiedValues =
  PendingModifiedValues'
    { engineVersion = Lude.Nothing,
      masterUserPassword = Lude.Nothing,
      dbSubnetGroupName = Lude.Nothing,
      iops = Lude.Nothing,
      processorFeatures = Lude.Nothing,
      dbInstanceClass = Lude.Nothing,
      licenseModel = Lude.Nothing,
      cACertificateIdentifier = Lude.Nothing,
      dbInstanceIdentifier = Lude.Nothing,
      pendingCloudwatchLogsExports = Lude.Nothing,
      backupRetentionPeriod = Lude.Nothing,
      multiAZ = Lude.Nothing,
      allocatedStorage = Lude.Nothing,
      port = Lude.Nothing,
      storageType = Lude.Nothing
    }

-- | Indicates the database engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvEngineVersion :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvEngineVersion = Lens.lens (engineVersion :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: PendingModifiedValues)
{-# DEPRECATED pmvEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Contains the pending or currently-in-progress change of the master credentials for the DB instance.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvMasterUserPassword :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvMasterUserPassword = Lens.lens (masterUserPassword :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {masterUserPassword = a} :: PendingModifiedValues)
{-# DEPRECATED pmvMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The new DB subnet group for the DB instance.
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvDBSubnetGroupName :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: PendingModifiedValues)
{-# DEPRECATED pmvDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | Specifies the new Provisioned IOPS value for the DB instance that will be applied or is currently being applied.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvIOPS :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Int)
pmvIOPS = Lens.lens (iops :: PendingModifiedValues -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: PendingModifiedValues)
{-# DEPRECATED pmvIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvProcessorFeatures :: Lens.Lens' PendingModifiedValues (Lude.Maybe [ProcessorFeature])
pmvProcessorFeatures = Lens.lens (processorFeatures :: PendingModifiedValues -> Lude.Maybe [ProcessorFeature]) (\s a -> s {processorFeatures = a} :: PendingModifiedValues)
{-# DEPRECATED pmvProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | Contains the new @DBInstanceClass@ for the DB instance that will be applied or is currently being applied.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvDBInstanceClass :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvDBInstanceClass = Lens.lens (dbInstanceClass :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: PendingModifiedValues)
{-# DEPRECATED pmvDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvLicenseModel :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvLicenseModel = Lens.lens (licenseModel :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: PendingModifiedValues)
{-# DEPRECATED pmvLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | Specifies the identifier of the CA certificate for the DB instance.
--
-- /Note:/ Consider using 'cACertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvCACertificateIdentifier :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvCACertificateIdentifier = Lens.lens (cACertificateIdentifier :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {cACertificateIdentifier = a} :: PendingModifiedValues)
{-# DEPRECATED pmvCACertificateIdentifier "Use generic-lens or generic-optics with 'cACertificateIdentifier' instead." #-}

-- | Contains the new @DBInstanceIdentifier@ for the DB instance that will be applied or is currently being applied.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvDBInstanceIdentifier :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: PendingModifiedValues)
{-# DEPRECATED pmvDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pendingCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvPendingCloudwatchLogsExports :: Lens.Lens' PendingModifiedValues (Lude.Maybe PendingCloudwatchLogsExports)
pmvPendingCloudwatchLogsExports = Lens.lens (pendingCloudwatchLogsExports :: PendingModifiedValues -> Lude.Maybe PendingCloudwatchLogsExports) (\s a -> s {pendingCloudwatchLogsExports = a} :: PendingModifiedValues)
{-# DEPRECATED pmvPendingCloudwatchLogsExports "Use generic-lens or generic-optics with 'pendingCloudwatchLogsExports' instead." #-}

-- | Specifies the pending number of days for which automated backups are retained.
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvBackupRetentionPeriod :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Int)
pmvBackupRetentionPeriod = Lens.lens (backupRetentionPeriod :: PendingModifiedValues -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionPeriod = a} :: PendingModifiedValues)
{-# DEPRECATED pmvBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ deployment.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvMultiAZ :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Bool)
pmvMultiAZ = Lens.lens (multiAZ :: PendingModifiedValues -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: PendingModifiedValues)
{-# DEPRECATED pmvMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | Contains the new @AllocatedStorage@ size for the DB instance that will be applied or is currently being applied.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvAllocatedStorage :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Int)
pmvAllocatedStorage = Lens.lens (allocatedStorage :: PendingModifiedValues -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: PendingModifiedValues)
{-# DEPRECATED pmvAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | Specifies the pending port for the DB instance.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvPort :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Int)
pmvPort = Lens.lens (port :: PendingModifiedValues -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: PendingModifiedValues)
{-# DEPRECATED pmvPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvStorageType :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvStorageType = Lens.lens (storageType :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: PendingModifiedValues)
{-# DEPRECATED pmvStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Lude.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "MasterUserPassword")
      Lude.<*> (x Lude..@? "DBSubnetGroupName")
      Lude.<*> (x Lude..@? "Iops")
      Lude.<*> ( x Lude..@? "ProcessorFeatures" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ProcessorFeature")
               )
      Lude.<*> (x Lude..@? "DBInstanceClass")
      Lude.<*> (x Lude..@? "LicenseModel")
      Lude.<*> (x Lude..@? "CACertificateIdentifier")
      Lude.<*> (x Lude..@? "DBInstanceIdentifier")
      Lude.<*> (x Lude..@? "PendingCloudwatchLogsExports")
      Lude.<*> (x Lude..@? "BackupRetentionPeriod")
      Lude.<*> (x Lude..@? "MultiAZ")
      Lude.<*> (x Lude..@? "AllocatedStorage")
      Lude.<*> (x Lude..@? "Port")
      Lude.<*> (x Lude..@? "StorageType")
