{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBInstanceAutomatedBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBInstanceAutomatedBackup
  ( DBInstanceAutomatedBackup (..),

    -- * Smart constructor
    mkDBInstanceAutomatedBackup,

    -- * Lenses
    diabRestoreWindow,
    diabEngineVersion,
    diabStatus,
    diabDBInstanceARN,
    diabMasterUsername,
    diabIAMDatabaseAuthenticationEnabled,
    diabIOPS,
    diabVPCId,
    diabInstanceCreateTime,
    diabEngine,
    diabEncrypted,
    diabLicenseModel,
    diabDBInstanceIdentifier,
    diabKMSKeyId,
    diabAvailabilityZone,
    diabRegion,
    diabAllocatedStorage,
    diabDBiResourceId,
    diabOptionGroupName,
    diabTimezone,
    diabTDECredentialARN,
    diabPort,
    diabStorageType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.RestoreWindow

-- | An automated backup of a DB instance. It it consists of system backups, transaction logs, and the database instance properties that existed at the time you deleted the source instance.
--
-- /See:/ 'mkDBInstanceAutomatedBackup' smart constructor.
data DBInstanceAutomatedBackup = DBInstanceAutomatedBackup'
  { restoreWindow ::
      Lude.Maybe RestoreWindow,
    engineVersion :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    dbInstanceARN :: Lude.Maybe Lude.Text,
    masterUsername :: Lude.Maybe Lude.Text,
    iamDatabaseAuthenticationEnabled ::
      Lude.Maybe Lude.Bool,
    iops :: Lude.Maybe Lude.Int,
    vpcId :: Lude.Maybe Lude.Text,
    instanceCreateTime ::
      Lude.Maybe Lude.DateTime,
    engine :: Lude.Maybe Lude.Text,
    encrypted :: Lude.Maybe Lude.Bool,
    licenseModel :: Lude.Maybe Lude.Text,
    dbInstanceIdentifier ::
      Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    availabilityZone ::
      Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    allocatedStorage :: Lude.Maybe Lude.Int,
    dbiResourceId :: Lude.Maybe Lude.Text,
    optionGroupName :: Lude.Maybe Lude.Text,
    timezone :: Lude.Maybe Lude.Text,
    tdeCredentialARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DBInstanceAutomatedBackup' with the minimum fields required to make a request.
--
-- * 'allocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
-- * 'availabilityZone' - The Availability Zone that the automated backup was created in. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .
-- * 'dbInstanceARN' - The Amazon Resource Name (ARN) for the automated backup.
-- * 'dbInstanceIdentifier' - The customer id of the instance that is/was associated with the automated backup.
-- * 'dbiResourceId' - The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
-- * 'encrypted' - Specifies whether the automated backup is encrypted.
-- * 'engine' - The name of the database engine for this automated backup.
-- * 'engineVersion' - The version of the database engine for the automated backup.
-- * 'iamDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
-- * 'instanceCreateTime' - Provides the date and time that the DB instance was created.
-- * 'iops' - The IOPS (I/O operations per second) value for the automated backup.
-- * 'kmsKeyId' - The AWS KMS key ID for an automated backup. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
-- * 'licenseModel' - License model information for the automated backup.
-- * 'masterUsername' - The license model of an automated backup.
-- * 'optionGroupName' - The option group the automated backup is associated with. If omitted, the default option group for the engine specified is used.
-- * 'port' - The port number that the automated backup used for connections.
--
-- Default: Inherits from the source DB instance
-- Valid Values: @1150-65535@
-- * 'region' - The AWS Region associated with the automated backup.
-- * 'restoreWindow' - Earliest and latest time an instance can be restored to.
-- * 'status' - Provides a list of status information for an automated backup:
--
--
--     * @active@ - automated backups for current instances
--
--
--     * @retained@ - automated backups for deleted instances
--
--
--     * @creating@ - automated backups that are waiting for the first automated snapshot to be available.
--
--
-- * 'storageType' - Specifies the storage type associated with the automated backup.
-- * 'tdeCredentialARN' - The ARN from the key store with which the automated backup is associated for TDE encryption.
-- * 'timezone' - The time zone of the automated backup. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
-- * 'vpcId' - Provides the VPC ID associated with the DB instance
mkDBInstanceAutomatedBackup ::
  DBInstanceAutomatedBackup
mkDBInstanceAutomatedBackup =
  DBInstanceAutomatedBackup'
    { restoreWindow = Lude.Nothing,
      engineVersion = Lude.Nothing,
      status = Lude.Nothing,
      dbInstanceARN = Lude.Nothing,
      masterUsername = Lude.Nothing,
      iamDatabaseAuthenticationEnabled = Lude.Nothing,
      iops = Lude.Nothing,
      vpcId = Lude.Nothing,
      instanceCreateTime = Lude.Nothing,
      engine = Lude.Nothing,
      encrypted = Lude.Nothing,
      licenseModel = Lude.Nothing,
      dbInstanceIdentifier = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      region = Lude.Nothing,
      allocatedStorage = Lude.Nothing,
      dbiResourceId = Lude.Nothing,
      optionGroupName = Lude.Nothing,
      timezone = Lude.Nothing,
      tdeCredentialARN = Lude.Nothing,
      port = Lude.Nothing,
      storageType = Lude.Nothing
    }

-- | Earliest and latest time an instance can be restored to.
--
-- /Note:/ Consider using 'restoreWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabRestoreWindow :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe RestoreWindow)
diabRestoreWindow = Lens.lens (restoreWindow :: DBInstanceAutomatedBackup -> Lude.Maybe RestoreWindow) (\s a -> s {restoreWindow = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabRestoreWindow "Use generic-lens or generic-optics with 'restoreWindow' instead." #-}

-- | The version of the database engine for the automated backup.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabEngineVersion :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabEngineVersion = Lens.lens (engineVersion :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Provides a list of status information for an automated backup:
--
--
--     * @active@ - automated backups for current instances
--
--
--     * @retained@ - automated backups for deleted instances
--
--
--     * @creating@ - automated backups that are waiting for the first automated snapshot to be available.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabStatus :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabStatus = Lens.lens (status :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) for the automated backup.
--
-- /Note:/ Consider using 'dbInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabDBInstanceARN :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabDBInstanceARN = Lens.lens (dbInstanceARN :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceARN = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabDBInstanceARN "Use generic-lens or generic-optics with 'dbInstanceARN' instead." #-}

-- | The license model of an automated backup.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabMasterUsername :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabMasterUsername = Lens.lens (masterUsername :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {masterUsername = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- /Note:/ Consider using 'iamDatabaseAuthenticationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabIAMDatabaseAuthenticationEnabled :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Bool)
diabIAMDatabaseAuthenticationEnabled = Lens.lens (iamDatabaseAuthenticationEnabled :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Bool) (\s a -> s {iamDatabaseAuthenticationEnabled = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabIAMDatabaseAuthenticationEnabled "Use generic-lens or generic-optics with 'iamDatabaseAuthenticationEnabled' instead." #-}

-- | The IOPS (I/O operations per second) value for the automated backup.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabIOPS :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Int)
diabIOPS = Lens.lens (iops :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Provides the VPC ID associated with the DB instance
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabVPCId :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabVPCId = Lens.lens (vpcId :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Provides the date and time that the DB instance was created.
--
-- /Note:/ Consider using 'instanceCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabInstanceCreateTime :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.DateTime)
diabInstanceCreateTime = Lens.lens (instanceCreateTime :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.DateTime) (\s a -> s {instanceCreateTime = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabInstanceCreateTime "Use generic-lens or generic-optics with 'instanceCreateTime' instead." #-}

-- | The name of the database engine for this automated backup.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabEngine :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabEngine = Lens.lens (engine :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Specifies whether the automated backup is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabEncrypted :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Bool)
diabEncrypted = Lens.lens (encrypted :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | License model information for the automated backup.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabLicenseModel :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabLicenseModel = Lens.lens (licenseModel :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | The customer id of the instance that is/was associated with the automated backup.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabDBInstanceIdentifier :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | The AWS KMS key ID for an automated backup. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabKMSKeyId :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabKMSKeyId = Lens.lens (kmsKeyId :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The Availability Zone that the automated backup was created in. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabAvailabilityZone :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabAvailabilityZone = Lens.lens (availabilityZone :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The AWS Region associated with the automated backup.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabRegion :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabRegion = Lens.lens (region :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Specifies the allocated storage size in gibibytes (GiB).
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabAllocatedStorage :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Int)
diabAllocatedStorage = Lens.lens (allocatedStorage :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabDBiResourceId :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabDBiResourceId = Lens.lens (dbiResourceId :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {dbiResourceId = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabDBiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead." #-}

-- | The option group the automated backup is associated with. If omitted, the default option group for the engine specified is used.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabOptionGroupName :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabOptionGroupName = Lens.lens (optionGroupName :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The time zone of the automated backup. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabTimezone :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabTimezone = Lens.lens (timezone :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The ARN from the key store with which the automated backup is associated for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabTDECredentialARN :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabTDECredentialARN = Lens.lens (tdeCredentialARN :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {tdeCredentialARN = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabTDECredentialARN "Use generic-lens or generic-optics with 'tdeCredentialARN' instead." #-}

-- | The port number that the automated backup used for connections.
--
-- Default: Inherits from the source DB instance
-- Valid Values: @1150-65535@
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabPort :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Int)
diabPort = Lens.lens (port :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | Specifies the storage type associated with the automated backup.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diabStorageType :: Lens.Lens' DBInstanceAutomatedBackup (Lude.Maybe Lude.Text)
diabStorageType = Lens.lens (storageType :: DBInstanceAutomatedBackup -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: DBInstanceAutomatedBackup)
{-# DEPRECATED diabStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Lude.FromXML DBInstanceAutomatedBackup where
  parseXML x =
    DBInstanceAutomatedBackup'
      Lude.<$> (x Lude..@? "RestoreWindow")
      Lude.<*> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "DBInstanceArn")
      Lude.<*> (x Lude..@? "MasterUsername")
      Lude.<*> (x Lude..@? "IAMDatabaseAuthenticationEnabled")
      Lude.<*> (x Lude..@? "Iops")
      Lude.<*> (x Lude..@? "VpcId")
      Lude.<*> (x Lude..@? "InstanceCreateTime")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "Encrypted")
      Lude.<*> (x Lude..@? "LicenseModel")
      Lude.<*> (x Lude..@? "DBInstanceIdentifier")
      Lude.<*> (x Lude..@? "KmsKeyId")
      Lude.<*> (x Lude..@? "AvailabilityZone")
      Lude.<*> (x Lude..@? "Region")
      Lude.<*> (x Lude..@? "AllocatedStorage")
      Lude.<*> (x Lude..@? "DbiResourceId")
      Lude.<*> (x Lude..@? "OptionGroupName")
      Lude.<*> (x Lude..@? "Timezone")
      Lude.<*> (x Lude..@? "TdeCredentialArn")
      Lude.<*> (x Lude..@? "Port")
      Lude.<*> (x Lude..@? "StorageType")
