{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSnapshot
  ( DBSnapshot (..),

    -- * Smart constructor
    mkDBSnapshot,

    -- * Lenses
    dbsAllocatedStorage,
    dbsAvailabilityZone,
    dbsDBInstanceIdentifier,
    dbsDBSnapshotArn,
    dbsDBSnapshotIdentifier,
    dbsDbiResourceId,
    dbsEncrypted,
    dbsEngine,
    dbsEngineVersion,
    dbsIAMDatabaseAuthenticationEnabled,
    dbsInstanceCreateTime,
    dbsIops,
    dbsKmsKeyId,
    dbsLicenseModel,
    dbsMasterUsername,
    dbsOptionGroupName,
    dbsPercentProgress,
    dbsPort,
    dbsProcessorFeatures,
    dbsSnapshotCreateTime,
    dbsSnapshotType,
    dbsSourceDBSnapshotIdentifier,
    dbsSourceRegion,
    dbsStatus,
    dbsStorageType,
    dbsTagList,
    dbsTdeCredentialArn,
    dbsTimezone,
    dbsVpcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.DBInstanceIdentifier as Types
import qualified Network.AWS.RDS.Types.DBSnapshotArn as Types
import qualified Network.AWS.RDS.Types.DBSnapshotIdentifier as Types
import qualified Network.AWS.RDS.Types.DbiResourceId as Types
import qualified Network.AWS.RDS.Types.Engine as Types
import qualified Network.AWS.RDS.Types.EngineVersion as Types
import qualified Network.AWS.RDS.Types.KmsKeyId as Types
import qualified Network.AWS.RDS.Types.LicenseModel as Types
import qualified Network.AWS.RDS.Types.MasterUsername as Types
import qualified Network.AWS.RDS.Types.OptionGroupName as Types
import qualified Network.AWS.RDS.Types.ProcessorFeature as Types
import qualified Network.AWS.RDS.Types.SnapshotType as Types
import qualified Network.AWS.RDS.Types.SourceDBSnapshotIdentifier as Types
import qualified Network.AWS.RDS.Types.Status as Types
import qualified Network.AWS.RDS.Types.StorageType as Types
import qualified Network.AWS.RDS.Types.String as Types
import qualified Network.AWS.RDS.Types.Tag as Types
import qualified Network.AWS.RDS.Types.TdeCredentialArn as Types
import qualified Network.AWS.RDS.Types.VpcId as Types

-- | Contains the details of an Amazon RDS DB snapshot.
--
-- This data type is used as a response element in the @DescribeDBSnapshots@ action.
--
-- /See:/ 'mkDBSnapshot' smart constructor.
data DBSnapshot = DBSnapshot'
  { -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Core.Maybe Core.Int,
    -- | Specifies the name of the Availability Zone the DB instance was located in at the time of the DB snapshot.
    availabilityZone :: Core.Maybe Types.String,
    -- | Specifies the DB instance identifier of the DB instance this DB snapshot was created from.
    dBInstanceIdentifier :: Core.Maybe Types.DBInstanceIdentifier,
    -- | The Amazon Resource Name (ARN) for the DB snapshot.
    dBSnapshotArn :: Core.Maybe Types.DBSnapshotArn,
    -- | Specifies the identifier for the DB snapshot.
    dBSnapshotIdentifier :: Core.Maybe Types.DBSnapshotIdentifier,
    -- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
    dbiResourceId :: Core.Maybe Types.DbiResourceId,
    -- | Specifies whether the DB snapshot is encrypted.
    encrypted :: Core.Maybe Core.Bool,
    -- | Specifies the name of the database engine.
    engine :: Core.Maybe Types.Engine,
    -- | Specifies the version of the database engine.
    engineVersion :: Core.Maybe Types.EngineVersion,
    -- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool,
    -- | Specifies the time in Coordinated Universal Time (UTC) when the DB instance, from which the snapshot was taken, was created.
    instanceCreateTime :: Core.Maybe Core.UTCTime,
    -- | Specifies the Provisioned IOPS (I/O operations per second) value of the DB instance at the time of the snapshot.
    iops :: Core.Maybe Core.Int,
    -- | If @Encrypted@ is true, the AWS KMS key identifier for the encrypted DB snapshot.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | License model information for the restored DB instance.
    licenseModel :: Core.Maybe Types.LicenseModel,
    -- | Provides the master username for the DB snapshot.
    masterUsername :: Core.Maybe Types.MasterUsername,
    -- | Provides the option group name for the DB snapshot.
    optionGroupName :: Core.Maybe Types.OptionGroupName,
    -- | The percentage of the estimated data that has been transferred.
    percentProgress :: Core.Maybe Core.Int,
    -- | Specifies the port that the database engine was listening on at the time of the snapshot.
    port :: Core.Maybe Core.Int,
    -- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance when the DB snapshot was created.
    processorFeatures :: Core.Maybe [Types.ProcessorFeature],
    -- | Specifies when the snapshot was taken in Coordinated Universal Time (UTC).
    snapshotCreateTime :: Core.Maybe Core.UTCTime,
    -- | Provides the type of the DB snapshot.
    snapshotType :: Core.Maybe Types.SnapshotType,
    -- | The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was copied from. It only has value in case of cross-customer or cross-region copy.
    sourceDBSnapshotIdentifier :: Core.Maybe Types.SourceDBSnapshotIdentifier,
    -- | The AWS Region that the DB snapshot was created in or copied from.
    sourceRegion :: Core.Maybe Types.String,
    -- | Specifies the status of this DB snapshot.
    status :: Core.Maybe Types.Status,
    -- | Specifies the storage type associated with DB snapshot.
    storageType :: Core.Maybe Types.StorageType,
    tagList :: Core.Maybe [Types.Tag],
    -- | The ARN from the key store with which to associate the instance for TDE encryption.
    tdeCredentialArn :: Core.Maybe Types.TdeCredentialArn,
    -- | The time zone of the DB snapshot. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for snapshots taken from Microsoft SQL Server DB instances that were created with a time zone specified.
    timezone :: Core.Maybe Types.String,
    -- | Provides the VPC ID associated with the DB snapshot.
    vpcId :: Core.Maybe Types.VpcId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DBSnapshot' value with any optional fields omitted.
mkDBSnapshot ::
  DBSnapshot
mkDBSnapshot =
  DBSnapshot'
    { allocatedStorage = Core.Nothing,
      availabilityZone = Core.Nothing,
      dBInstanceIdentifier = Core.Nothing,
      dBSnapshotArn = Core.Nothing,
      dBSnapshotIdentifier = Core.Nothing,
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
      percentProgress = Core.Nothing,
      port = Core.Nothing,
      processorFeatures = Core.Nothing,
      snapshotCreateTime = Core.Nothing,
      snapshotType = Core.Nothing,
      sourceDBSnapshotIdentifier = Core.Nothing,
      sourceRegion = Core.Nothing,
      status = Core.Nothing,
      storageType = Core.Nothing,
      tagList = Core.Nothing,
      tdeCredentialArn = Core.Nothing,
      timezone = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | Specifies the allocated storage size in gibibytes (GiB).
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsAllocatedStorage :: Lens.Lens' DBSnapshot (Core.Maybe Core.Int)
dbsAllocatedStorage = Lens.field @"allocatedStorage"
{-# DEPRECATED dbsAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | Specifies the name of the Availability Zone the DB instance was located in at the time of the DB snapshot.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsAvailabilityZone :: Lens.Lens' DBSnapshot (Core.Maybe Types.String)
dbsAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED dbsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Specifies the DB instance identifier of the DB instance this DB snapshot was created from.
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsDBInstanceIdentifier :: Lens.Lens' DBSnapshot (Core.Maybe Types.DBInstanceIdentifier)
dbsDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED dbsDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) for the DB snapshot.
--
-- /Note:/ Consider using 'dBSnapshotArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsDBSnapshotArn :: Lens.Lens' DBSnapshot (Core.Maybe Types.DBSnapshotArn)
dbsDBSnapshotArn = Lens.field @"dBSnapshotArn"
{-# DEPRECATED dbsDBSnapshotArn "Use generic-lens or generic-optics with 'dBSnapshotArn' instead." #-}

-- | Specifies the identifier for the DB snapshot.
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsDBSnapshotIdentifier :: Lens.Lens' DBSnapshot (Core.Maybe Types.DBSnapshotIdentifier)
dbsDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# DEPRECATED dbsDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead." #-}

-- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsDbiResourceId :: Lens.Lens' DBSnapshot (Core.Maybe Types.DbiResourceId)
dbsDbiResourceId = Lens.field @"dbiResourceId"
{-# DEPRECATED dbsDbiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead." #-}

-- | Specifies whether the DB snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsEncrypted :: Lens.Lens' DBSnapshot (Core.Maybe Core.Bool)
dbsEncrypted = Lens.field @"encrypted"
{-# DEPRECATED dbsEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | Specifies the name of the database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsEngine :: Lens.Lens' DBSnapshot (Core.Maybe Types.Engine)
dbsEngine = Lens.field @"engine"
{-# DEPRECATED dbsEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Specifies the version of the database engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsEngineVersion :: Lens.Lens' DBSnapshot (Core.Maybe Types.EngineVersion)
dbsEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED dbsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- /Note:/ Consider using 'iAMDatabaseAuthenticationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsIAMDatabaseAuthenticationEnabled :: Lens.Lens' DBSnapshot (Core.Maybe Core.Bool)
dbsIAMDatabaseAuthenticationEnabled = Lens.field @"iAMDatabaseAuthenticationEnabled"
{-# DEPRECATED dbsIAMDatabaseAuthenticationEnabled "Use generic-lens or generic-optics with 'iAMDatabaseAuthenticationEnabled' instead." #-}

-- | Specifies the time in Coordinated Universal Time (UTC) when the DB instance, from which the snapshot was taken, was created.
--
-- /Note:/ Consider using 'instanceCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsInstanceCreateTime :: Lens.Lens' DBSnapshot (Core.Maybe Core.UTCTime)
dbsInstanceCreateTime = Lens.field @"instanceCreateTime"
{-# DEPRECATED dbsInstanceCreateTime "Use generic-lens or generic-optics with 'instanceCreateTime' instead." #-}

-- | Specifies the Provisioned IOPS (I/O operations per second) value of the DB instance at the time of the snapshot.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsIops :: Lens.Lens' DBSnapshot (Core.Maybe Core.Int)
dbsIops = Lens.field @"iops"
{-# DEPRECATED dbsIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | If @Encrypted@ is true, the AWS KMS key identifier for the encrypted DB snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsKmsKeyId :: Lens.Lens' DBSnapshot (Core.Maybe Types.KmsKeyId)
dbsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED dbsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | License model information for the restored DB instance.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsLicenseModel :: Lens.Lens' DBSnapshot (Core.Maybe Types.LicenseModel)
dbsLicenseModel = Lens.field @"licenseModel"
{-# DEPRECATED dbsLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | Provides the master username for the DB snapshot.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsMasterUsername :: Lens.Lens' DBSnapshot (Core.Maybe Types.MasterUsername)
dbsMasterUsername = Lens.field @"masterUsername"
{-# DEPRECATED dbsMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | Provides the option group name for the DB snapshot.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsOptionGroupName :: Lens.Lens' DBSnapshot (Core.Maybe Types.OptionGroupName)
dbsOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED dbsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The percentage of the estimated data that has been transferred.
--
-- /Note:/ Consider using 'percentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsPercentProgress :: Lens.Lens' DBSnapshot (Core.Maybe Core.Int)
dbsPercentProgress = Lens.field @"percentProgress"
{-# DEPRECATED dbsPercentProgress "Use generic-lens or generic-optics with 'percentProgress' instead." #-}

-- | Specifies the port that the database engine was listening on at the time of the snapshot.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsPort :: Lens.Lens' DBSnapshot (Core.Maybe Core.Int)
dbsPort = Lens.field @"port"
{-# DEPRECATED dbsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance when the DB snapshot was created.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsProcessorFeatures :: Lens.Lens' DBSnapshot (Core.Maybe [Types.ProcessorFeature])
dbsProcessorFeatures = Lens.field @"processorFeatures"
{-# DEPRECATED dbsProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | Specifies when the snapshot was taken in Coordinated Universal Time (UTC).
--
-- /Note:/ Consider using 'snapshotCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsSnapshotCreateTime :: Lens.Lens' DBSnapshot (Core.Maybe Core.UTCTime)
dbsSnapshotCreateTime = Lens.field @"snapshotCreateTime"
{-# DEPRECATED dbsSnapshotCreateTime "Use generic-lens or generic-optics with 'snapshotCreateTime' instead." #-}

-- | Provides the type of the DB snapshot.
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsSnapshotType :: Lens.Lens' DBSnapshot (Core.Maybe Types.SnapshotType)
dbsSnapshotType = Lens.field @"snapshotType"
{-# DEPRECATED dbsSnapshotType "Use generic-lens or generic-optics with 'snapshotType' instead." #-}

-- | The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was copied from. It only has value in case of cross-customer or cross-region copy.
--
-- /Note:/ Consider using 'sourceDBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsSourceDBSnapshotIdentifier :: Lens.Lens' DBSnapshot (Core.Maybe Types.SourceDBSnapshotIdentifier)
dbsSourceDBSnapshotIdentifier = Lens.field @"sourceDBSnapshotIdentifier"
{-# DEPRECATED dbsSourceDBSnapshotIdentifier "Use generic-lens or generic-optics with 'sourceDBSnapshotIdentifier' instead." #-}

-- | The AWS Region that the DB snapshot was created in or copied from.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsSourceRegion :: Lens.Lens' DBSnapshot (Core.Maybe Types.String)
dbsSourceRegion = Lens.field @"sourceRegion"
{-# DEPRECATED dbsSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | Specifies the status of this DB snapshot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsStatus :: Lens.Lens' DBSnapshot (Core.Maybe Types.Status)
dbsStatus = Lens.field @"status"
{-# DEPRECATED dbsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the storage type associated with DB snapshot.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsStorageType :: Lens.Lens' DBSnapshot (Core.Maybe Types.StorageType)
dbsStorageType = Lens.field @"storageType"
{-# DEPRECATED dbsStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsTagList :: Lens.Lens' DBSnapshot (Core.Maybe [Types.Tag])
dbsTagList = Lens.field @"tagList"
{-# DEPRECATED dbsTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The ARN from the key store with which to associate the instance for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsTdeCredentialArn :: Lens.Lens' DBSnapshot (Core.Maybe Types.TdeCredentialArn)
dbsTdeCredentialArn = Lens.field @"tdeCredentialArn"
{-# DEPRECATED dbsTdeCredentialArn "Use generic-lens or generic-optics with 'tdeCredentialArn' instead." #-}

-- | The time zone of the DB snapshot. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for snapshots taken from Microsoft SQL Server DB instances that were created with a time zone specified.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsTimezone :: Lens.Lens' DBSnapshot (Core.Maybe Types.String)
dbsTimezone = Lens.field @"timezone"
{-# DEPRECATED dbsTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | Provides the VPC ID associated with the DB snapshot.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsVpcId :: Lens.Lens' DBSnapshot (Core.Maybe Types.VpcId)
dbsVpcId = Lens.field @"vpcId"
{-# DEPRECATED dbsVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML DBSnapshot where
  parseXML x =
    DBSnapshot'
      Core.<$> (x Core..@? "AllocatedStorage")
      Core.<*> (x Core..@? "AvailabilityZone")
      Core.<*> (x Core..@? "DBInstanceIdentifier")
      Core.<*> (x Core..@? "DBSnapshotArn")
      Core.<*> (x Core..@? "DBSnapshotIdentifier")
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
      Core.<*> (x Core..@? "PercentProgress")
      Core.<*> (x Core..@? "Port")
      Core.<*> ( x Core..@? "ProcessorFeatures"
                   Core..<@> Core.parseXMLList "ProcessorFeature"
               )
      Core.<*> (x Core..@? "SnapshotCreateTime")
      Core.<*> (x Core..@? "SnapshotType")
      Core.<*> (x Core..@? "SourceDBSnapshotIdentifier")
      Core.<*> (x Core..@? "SourceRegion")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "StorageType")
      Core.<*> (x Core..@? "TagList" Core..<@> Core.parseXMLList "Tag")
      Core.<*> (x Core..@? "TdeCredentialArn")
      Core.<*> (x Core..@? "Timezone")
      Core.<*> (x Core..@? "VpcId")
