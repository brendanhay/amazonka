{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBInstanceAutomatedBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBInstanceAutomatedBackup
  ( DBInstanceAutomatedBackup (..)
  -- * Smart constructor
  , mkDBInstanceAutomatedBackup
  -- * Lenses
  , dbiabAllocatedStorage
  , dbiabAvailabilityZone
  , dbiabDBInstanceArn
  , dbiabDBInstanceIdentifier
  , dbiabDbiResourceId
  , dbiabEncrypted
  , dbiabEngine
  , dbiabEngineVersion
  , dbiabIAMDatabaseAuthenticationEnabled
  , dbiabInstanceCreateTime
  , dbiabIops
  , dbiabKmsKeyId
  , dbiabLicenseModel
  , dbiabMasterUsername
  , dbiabOptionGroupName
  , dbiabPort
  , dbiabRegion
  , dbiabRestoreWindow
  , dbiabStatus
  , dbiabStorageType
  , dbiabTdeCredentialArn
  , dbiabTimezone
  , dbiabVpcId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.RestoreWindow as Types

-- | An automated backup of a DB instance. It it consists of system backups, transaction logs, and the database instance properties that existed at the time you deleted the source instance. 
--
-- /See:/ 'mkDBInstanceAutomatedBackup' smart constructor.
data DBInstanceAutomatedBackup = DBInstanceAutomatedBackup'
  { allocatedStorage :: Core.Maybe Core.Int
    -- ^ Specifies the allocated storage size in gibibytes (GiB).
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone that the automated backup was created in. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .
  , dBInstanceArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the automated backup.
  , dBInstanceIdentifier :: Core.Maybe Core.Text
    -- ^ The customer id of the instance that is/was associated with the automated backup. 
  , dbiResourceId :: Core.Maybe Core.Text
    -- ^ The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Specifies whether the automated backup is encrypted.
  , engine :: Core.Maybe Core.Text
    -- ^ The name of the database engine for this automated backup.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The version of the database engine for the automated backup.
  , iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool
    -- ^ True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
  , instanceCreateTime :: Core.Maybe Core.UTCTime
    -- ^ Provides the date and time that the DB instance was created. 
  , iops :: Core.Maybe Core.Int
    -- ^ The IOPS (I/O operations per second) value for the automated backup. 
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS KMS key ID for an automated backup. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key. 
  , licenseModel :: Core.Maybe Core.Text
    -- ^ License model information for the automated backup.
  , masterUsername :: Core.Maybe Core.Text
    -- ^ The license model of an automated backup.
  , optionGroupName :: Core.Maybe Core.Text
    -- ^ The option group the automated backup is associated with. If omitted, the default option group for the engine specified is used.
  , port :: Core.Maybe Core.Int
    -- ^ The port number that the automated backup used for connections.
--
-- Default: Inherits from the source DB instance
-- Valid Values: @1150-65535@ 
  , region :: Core.Maybe Core.Text
    -- ^ The AWS Region associated with the automated backup.
  , restoreWindow :: Core.Maybe Types.RestoreWindow
    -- ^ Earliest and latest time an instance can be restored to.
  , status :: Core.Maybe Core.Text
    -- ^ Provides a list of status information for an automated backup:
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
  , storageType :: Core.Maybe Core.Text
    -- ^ Specifies the storage type associated with the automated backup.
  , tdeCredentialArn :: Core.Maybe Core.Text
    -- ^ The ARN from the key store with which the automated backup is associated for TDE encryption.
  , timezone :: Core.Maybe Core.Text
    -- ^ The time zone of the automated backup. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
  , vpcId :: Core.Maybe Core.Text
    -- ^ Provides the VPC ID associated with the DB instance
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DBInstanceAutomatedBackup' value with any optional fields omitted.
mkDBInstanceAutomatedBackup
    :: DBInstanceAutomatedBackup
mkDBInstanceAutomatedBackup
  = DBInstanceAutomatedBackup'{allocatedStorage = Core.Nothing,
                               availabilityZone = Core.Nothing, dBInstanceArn = Core.Nothing,
                               dBInstanceIdentifier = Core.Nothing, dbiResourceId = Core.Nothing,
                               encrypted = Core.Nothing, engine = Core.Nothing,
                               engineVersion = Core.Nothing,
                               iAMDatabaseAuthenticationEnabled = Core.Nothing,
                               instanceCreateTime = Core.Nothing, iops = Core.Nothing,
                               kmsKeyId = Core.Nothing, licenseModel = Core.Nothing,
                               masterUsername = Core.Nothing, optionGroupName = Core.Nothing,
                               port = Core.Nothing, region = Core.Nothing,
                               restoreWindow = Core.Nothing, status = Core.Nothing,
                               storageType = Core.Nothing, tdeCredentialArn = Core.Nothing,
                               timezone = Core.Nothing, vpcId = Core.Nothing}

-- | Specifies the allocated storage size in gibibytes (GiB).
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabAllocatedStorage :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Int)
dbiabAllocatedStorage = Lens.field @"allocatedStorage"
{-# INLINEABLE dbiabAllocatedStorage #-}
{-# DEPRECATED allocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead"  #-}

-- | The Availability Zone that the automated backup was created in. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabAvailabilityZone :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE dbiabAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The Amazon Resource Name (ARN) for the automated backup.
--
-- /Note:/ Consider using 'dBInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabDBInstanceArn :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabDBInstanceArn = Lens.field @"dBInstanceArn"
{-# INLINEABLE dbiabDBInstanceArn #-}
{-# DEPRECATED dBInstanceArn "Use generic-lens or generic-optics with 'dBInstanceArn' instead"  #-}

-- | The customer id of the instance that is/was associated with the automated backup. 
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabDBInstanceIdentifier :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE dbiabDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabDbiResourceId :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabDbiResourceId = Lens.field @"dbiResourceId"
{-# INLINEABLE dbiabDbiResourceId #-}
{-# DEPRECATED dbiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead"  #-}

-- | Specifies whether the automated backup is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabEncrypted :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Bool)
dbiabEncrypted = Lens.field @"encrypted"
{-# INLINEABLE dbiabEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The name of the database engine for this automated backup.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabEngine :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabEngine = Lens.field @"engine"
{-# INLINEABLE dbiabEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The version of the database engine for the automated backup.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabEngineVersion :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE dbiabEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- /Note:/ Consider using 'iAMDatabaseAuthenticationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabIAMDatabaseAuthenticationEnabled :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Bool)
dbiabIAMDatabaseAuthenticationEnabled = Lens.field @"iAMDatabaseAuthenticationEnabled"
{-# INLINEABLE dbiabIAMDatabaseAuthenticationEnabled #-}
{-# DEPRECATED iAMDatabaseAuthenticationEnabled "Use generic-lens or generic-optics with 'iAMDatabaseAuthenticationEnabled' instead"  #-}

-- | Provides the date and time that the DB instance was created. 
--
-- /Note:/ Consider using 'instanceCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabInstanceCreateTime :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.UTCTime)
dbiabInstanceCreateTime = Lens.field @"instanceCreateTime"
{-# INLINEABLE dbiabInstanceCreateTime #-}
{-# DEPRECATED instanceCreateTime "Use generic-lens or generic-optics with 'instanceCreateTime' instead"  #-}

-- | The IOPS (I/O operations per second) value for the automated backup. 
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabIops :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Int)
dbiabIops = Lens.field @"iops"
{-# INLINEABLE dbiabIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | The AWS KMS key ID for an automated backup. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key. 
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabKmsKeyId :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE dbiabKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | License model information for the automated backup.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabLicenseModel :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabLicenseModel = Lens.field @"licenseModel"
{-# INLINEABLE dbiabLicenseModel #-}
{-# DEPRECATED licenseModel "Use generic-lens or generic-optics with 'licenseModel' instead"  #-}

-- | The license model of an automated backup.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabMasterUsername :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabMasterUsername = Lens.field @"masterUsername"
{-# INLINEABLE dbiabMasterUsername #-}
{-# DEPRECATED masterUsername "Use generic-lens or generic-optics with 'masterUsername' instead"  #-}

-- | The option group the automated backup is associated with. If omitted, the default option group for the engine specified is used.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabOptionGroupName :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE dbiabOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

-- | The port number that the automated backup used for connections.
--
-- Default: Inherits from the source DB instance
-- Valid Values: @1150-65535@ 
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabPort :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Int)
dbiabPort = Lens.field @"port"
{-# INLINEABLE dbiabPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The AWS Region associated with the automated backup.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabRegion :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabRegion = Lens.field @"region"
{-# INLINEABLE dbiabRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | Earliest and latest time an instance can be restored to.
--
-- /Note:/ Consider using 'restoreWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabRestoreWindow :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Types.RestoreWindow)
dbiabRestoreWindow = Lens.field @"restoreWindow"
{-# INLINEABLE dbiabRestoreWindow #-}
{-# DEPRECATED restoreWindow "Use generic-lens or generic-optics with 'restoreWindow' instead"  #-}

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
dbiabStatus :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabStatus = Lens.field @"status"
{-# INLINEABLE dbiabStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Specifies the storage type associated with the automated backup.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabStorageType :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabStorageType = Lens.field @"storageType"
{-# INLINEABLE dbiabStorageType #-}
{-# DEPRECATED storageType "Use generic-lens or generic-optics with 'storageType' instead"  #-}

-- | The ARN from the key store with which the automated backup is associated for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabTdeCredentialArn :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabTdeCredentialArn = Lens.field @"tdeCredentialArn"
{-# INLINEABLE dbiabTdeCredentialArn #-}
{-# DEPRECATED tdeCredentialArn "Use generic-lens or generic-optics with 'tdeCredentialArn' instead"  #-}

-- | The time zone of the automated backup. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabTimezone :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabTimezone = Lens.field @"timezone"
{-# INLINEABLE dbiabTimezone #-}
{-# DEPRECATED timezone "Use generic-lens or generic-optics with 'timezone' instead"  #-}

-- | Provides the VPC ID associated with the DB instance
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiabVpcId :: Lens.Lens' DBInstanceAutomatedBackup (Core.Maybe Core.Text)
dbiabVpcId = Lens.field @"vpcId"
{-# INLINEABLE dbiabVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML DBInstanceAutomatedBackup where
        parseXML x
          = DBInstanceAutomatedBackup' Core.<$>
              (x Core..@? "AllocatedStorage") Core.<*>
                x Core..@? "AvailabilityZone"
                Core.<*> x Core..@? "DBInstanceArn"
                Core.<*> x Core..@? "DBInstanceIdentifier"
                Core.<*> x Core..@? "DbiResourceId"
                Core.<*> x Core..@? "Encrypted"
                Core.<*> x Core..@? "Engine"
                Core.<*> x Core..@? "EngineVersion"
                Core.<*> x Core..@? "IAMDatabaseAuthenticationEnabled"
                Core.<*> x Core..@? "InstanceCreateTime"
                Core.<*> x Core..@? "Iops"
                Core.<*> x Core..@? "KmsKeyId"
                Core.<*> x Core..@? "LicenseModel"
                Core.<*> x Core..@? "MasterUsername"
                Core.<*> x Core..@? "OptionGroupName"
                Core.<*> x Core..@? "Port"
                Core.<*> x Core..@? "Region"
                Core.<*> x Core..@? "RestoreWindow"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "StorageType"
                Core.<*> x Core..@? "TdeCredentialArn"
                Core.<*> x Core..@? "Timezone"
                Core.<*> x Core..@? "VpcId"
