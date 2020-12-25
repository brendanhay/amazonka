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
    dbiabAllocatedStorage,
    dbiabAvailabilityZone,
    dbiabDBInstanceArn,
    dbiabDBInstanceIdentifier,
    dbiabDbiResourceId,
    dbiabEncrypted,
    dbiabEngine,
    dbiabEngineVersion,
    dbiabIAMDatabaseAuthenticationEnabled,
    dbiabInstanceCreateTime,
    dbiabIops,
    dbiabKmsKeyId,
    dbiabLicenseModel,
    dbiabMasterUsername,
    dbiabOptionGroupName,
    dbiabPort,
    dbiabRegion,
    dbiabRestoreWindow,
    dbiabStatus,
    dbiabStorageType,
    dbiabTdeCredentialArn,
    dbiabTimezone,
    dbiabVpcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.RestoreWindow as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | An automated backup of a DB instance. It it consists of system backups, transaction logs, and the database instance properties that existed at the time you deleted the source instance.
--
-- /See:/ 'mkDBInstanceAutomatedBackup' smart constructor.
data DBInstanceAutomatedBackup = DBInstanceAutomatedBackup'
  { -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Core.Maybe Core.Int,
    -- | The Availability Zone that the automated backup was created in. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .
    availabilityZone :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) for the automated backup.
    dBInstanceArn :: Core.Maybe Types.String,
    -- | The customer id of the instance that is/was associated with the automated backup.
    dBInstanceIdentifier :: Core.Maybe Types.String,
    -- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
    dbiResourceId :: Core.Maybe Types.String,
    -- | Specifies whether the automated backup is encrypted.
    encrypted :: Core.Maybe Core.Bool,
    -- | The name of the database engine for this automated backup.
    engine :: Core.Maybe Types.String,
    -- | The version of the database engine for the automated backup.
    engineVersion :: Core.Maybe Types.String,
    -- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool,
    -- | Provides the date and time that the DB instance was created.
    instanceCreateTime :: Core.Maybe Core.UTCTime,
    -- | The IOPS (I/O operations per second) value for the automated backup.
    iops :: Core.Maybe Core.Int,
    -- | The AWS KMS key ID for an automated backup. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
    kmsKeyId :: Core.Maybe Types.String,
    -- | License model information for the automated backup.
    licenseModel :: Core.Maybe Types.String,
    -- | The license model of an automated backup.
    masterUsername :: Core.Maybe Types.String,
    -- | The option group the automated backup is associated with. If omitted, the default option group for the engine specified is used.
    optionGroupName :: Core.Maybe Types.String,
    -- | The port number that the automated backup used for connections.
    --
    -- Default: Inherits from the source DB instance
    -- Valid Values: @1150-65535@
    port :: Core.Maybe Core.Int,
    -- | The AWS Region associated with the automated backup.
    region :: Core.Maybe Types.String,
    -- | Earliest and latest time an instance can be restored to.
    restoreWindow :: Core.Maybe Types.RestoreWindow,
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
    status :: Core.Maybe Types.String,
    -- | Specifies the storage type associated with the automated backup.
    storageType :: Core.Maybe Types.String,
    -- | The ARN from the key store with which the automated backup is associated for TDE encryption.
    tdeCredentialArn :: Core.Maybe Types.String,
    -- | The time zone of the automated backup. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
    timezone :: Core.Maybe Types.String,
    -- | Provides the VPC ID associated with the DB instance
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DBInstanceAutomatedBackup' value with any optional fields omitted.
mkDBInstanceAutomatedBackup ::
  DBInstanceAutomatedBackup
mkDBInstanceAutomatedBackup =
  DBInstanceAutomatedBackup'
    { allocatedStorage = Core.Nothing,
      availabilityZone = Core.Nothing,
      dBInstanceArn = Core.Nothing,
      dBInstanceIdentifier = Core.Nothing,
      dbiResourceId = Core.Nothing,
      encrypted = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      iAMDatabaseAuthenticationEnabled = Core.Nothing,
      instanceCreateTime = Core.Nothing,
      iops = Core.Nothing,
      kmsKeyId = Core.Nothing,
      licenseModel = Core.Nothing,
      masterUsername = Core.Nothing,
      optionGroupName = Core.Nothing,
      port = Core.Nothing,
      region = Core.Nothing,
      restoreWindow = Core.Nothing,
      status = Core.Nothing,
      storageType = Core.Nothing,
      tdeCredentialArn = Core.Nothing,
      timezone = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | Specifies the allocated storage size in gibibytes (GiB).
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabAllocatedStorage :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Int)
dbiabAllocatedStorage = Lens.field @"allocatedStorage"
{-# DEPRECATED dbiabAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | The Availability Zone that the automated backup was created in. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabAvailabilityZone :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED dbiabAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The Amazon Resource Name (ARN) for the automated backup.
--
-- /Note:/ Consider using 'dBInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabDBInstanceArn :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabDBInstanceArn = Lens.field @"dBInstanceArn"
{-# DEPRECATED dbiabDBInstanceArn "Use generic-lens or generic-optics with 'dBInstanceArn' instead." #-}

-- | The customer id of the instance that is/was associated with the automated backup.
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabDBInstanceIdentifier :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED dbiabDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

-- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabDbiResourceId :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabDbiResourceId = Lens.field @"dbiResourceId"
{-# DEPRECATED dbiabDbiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead." #-}

-- | Specifies whether the automated backup is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabEncrypted :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Bool)
dbiabEncrypted = Lens.field @"encrypted"
{-# DEPRECATED dbiabEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The name of the database engine for this automated backup.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabEngine :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabEngine = Lens.field @"engine"
{-# DEPRECATED dbiabEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The version of the database engine for the automated backup.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabEngineVersion :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED dbiabEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- /Note:/ Consider using 'iAMDatabaseAuthenticationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabIAMDatabaseAuthenticationEnabled :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Bool)
dbiabIAMDatabaseAuthenticationEnabled = Lens.field @"iAMDatabaseAuthenticationEnabled"
{-# DEPRECATED dbiabIAMDatabaseAuthenticationEnabled "Use generic-lens or generic-optics with 'iAMDatabaseAuthenticationEnabled' instead." #-}

-- | Provides the date and time that the DB instance was created.
--
-- /Note:/ Consider using 'instanceCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabInstanceCreateTime :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.UTCTime)
dbiabInstanceCreateTime = Lens.field @"instanceCreateTime"
{-# DEPRECATED dbiabInstanceCreateTime "Use generic-lens or generic-optics with 'instanceCreateTime' instead." #-}

-- | The IOPS (I/O operations per second) value for the automated backup.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabIops :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Int)
dbiabIops = Lens.field @"iops"
{-# DEPRECATED dbiabIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The AWS KMS key ID for an automated backup. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabKmsKeyId :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED dbiabKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | License model information for the automated backup.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabLicenseModel :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabLicenseModel = Lens.field @"licenseModel"
{-# DEPRECATED dbiabLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | The license model of an automated backup.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabMasterUsername :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabMasterUsername = Lens.field @"masterUsername"
{-# DEPRECATED dbiabMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The option group the automated backup is associated with. If omitted, the default option group for the engine specified is used.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabOptionGroupName :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED dbiabOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The port number that the automated backup used for connections.
--
-- Default: Inherits from the source DB instance
-- Valid Values: @1150-65535@
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabPort :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Int)
dbiabPort = Lens.field @"port"
{-# DEPRECATED dbiabPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The AWS Region associated with the automated backup.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabRegion :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabRegion = Lens.field @"region"
{-# DEPRECATED dbiabRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Earliest and latest time an instance can be restored to.
--
-- /Note:/ Consider using 'restoreWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabRestoreWindow :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.RestoreWindow)
dbiabRestoreWindow = Lens.field @"restoreWindow"
{-# DEPRECATED dbiabRestoreWindow "Use generic-lens or generic-optics with 'restoreWindow' instead." #-}

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
dbiabStatus :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabStatus = Lens.field @"status"
{-# DEPRECATED dbiabStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the storage type associated with the automated backup.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabStorageType :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabStorageType = Lens.field @"storageType"
{-# DEPRECATED dbiabStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | The ARN from the key store with which the automated backup is associated for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabTdeCredentialArn :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabTdeCredentialArn = Lens.field @"tdeCredentialArn"
{-# DEPRECATED dbiabTdeCredentialArn "Use generic-lens or generic-optics with 'tdeCredentialArn' instead." #-}

-- | The time zone of the automated backup. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabTimezone :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabTimezone = Lens.field @"timezone"
{-# DEPRECATED dbiabTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | Provides the VPC ID associated with the DB instance
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabVpcId :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.String)
dbiabVpcId = Lens.field @"vpcId"
{-# DEPRECATED dbiabVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML DBInstanceAutomatedBackup where
  parseXML x =
    DBInstanceAutomatedBackup'
      Core.<$> (x Core..@? "AllocatedStorage")
      Core.<*> (x Core..@? "AvailabilityZone")
      Core.<*> (x Core..@? "DBInstanceArn")
      Core.<*> (x Core..@? "DBInstanceIdentifier")
      Core.<*> (x Core..@? "DbiResourceId")
      Core.<*> (x Core..@? "Encrypted")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Core.<*> (x Core..@? "InstanceCreateTime")
      Core.<*> (x Core..@? "Iops")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "LicenseModel")
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "OptionGroupName")
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "Region")
      Core.<*> (x Core..@? "RestoreWindow")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "StorageType")
      Core.<*> (x Core..@? "TdeCredentialArn")
      Core.<*> (x Core..@? "Timezone")
      Core.<*> (x Core..@? "VpcId")
