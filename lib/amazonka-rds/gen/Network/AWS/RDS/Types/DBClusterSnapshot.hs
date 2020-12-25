{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterSnapshot
  ( DBClusterSnapshot (..),

    -- * Smart constructor
    mkDBClusterSnapshot,

    -- * Lenses
    dbcsAllocatedStorage,
    dbcsAvailabilityZones,
    dbcsClusterCreateTime,
    dbcsDBClusterIdentifier,
    dbcsDBClusterSnapshotArn,
    dbcsDBClusterSnapshotIdentifier,
    dbcsEngine,
    dbcsEngineVersion,
    dbcsIAMDatabaseAuthenticationEnabled,
    dbcsKmsKeyId,
    dbcsLicenseModel,
    dbcsMasterUsername,
    dbcsPercentProgress,
    dbcsPort,
    dbcsSnapshotCreateTime,
    dbcsSnapshotType,
    dbcsSourceDBClusterSnapshotArn,
    dbcsStatus,
    dbcsStorageEncrypted,
    dbcsTagList,
    dbcsVpcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types
import qualified Network.AWS.RDS.Types.Tag as Types

-- | Contains the details for an Amazon RDS DB cluster snapshot
--
-- This data type is used as a response element in the @DescribeDBClusterSnapshots@ action.
--
-- /See:/ 'mkDBClusterSnapshot' smart constructor.
data DBClusterSnapshot = DBClusterSnapshot'
  { -- | Specifies the allocated storage size in gibibytes (GiB).
    allocatedStorage :: Core.Maybe Core.Int,
    -- | Provides the list of Availability Zones (AZs) where instances in the DB cluster snapshot can be restored.
    availabilityZones :: Core.Maybe [Types.String],
    -- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
    clusterCreateTime :: Core.Maybe Core.UTCTime,
    -- | Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
    dBClusterIdentifier :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) for the DB cluster snapshot.
    dBClusterSnapshotArn :: Core.Maybe Types.String,
    -- | Specifies the identifier for the DB cluster snapshot.
    dBClusterSnapshotIdentifier :: Core.Maybe Types.String,
    -- | Specifies the name of the database engine.
    engine :: Core.Maybe Types.String,
    -- | Provides the version of the database engine for this DB cluster snapshot.
    engineVersion :: Core.Maybe Types.String,
    -- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool,
    -- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster snapshot.
    kmsKeyId :: Core.Maybe Types.String,
    -- | Provides the license model information for this DB cluster snapshot.
    licenseModel :: Core.Maybe Types.String,
    -- | Provides the master username for the DB cluster snapshot.
    masterUsername :: Core.Maybe Types.String,
    -- | Specifies the percentage of the estimated data that has been transferred.
    percentProgress :: Core.Maybe Core.Int,
    -- | Specifies the port that the DB cluster was listening on at the time of the snapshot.
    port :: Core.Maybe Core.Int,
    -- | Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
    snapshotCreateTime :: Core.Maybe Core.UTCTime,
    -- | Provides the type of the DB cluster snapshot.
    snapshotType :: Core.Maybe Types.String,
    -- | If the DB cluster snapshot was copied from a source DB cluster snapshot, the Amazon Resource Name (ARN) for the source DB cluster snapshot, otherwise, a null value.
    sourceDBClusterSnapshotArn :: Core.Maybe Types.String,
    -- | Specifies the status of this DB cluster snapshot.
    status :: Core.Maybe Types.String,
    -- | Specifies whether the DB cluster snapshot is encrypted.
    storageEncrypted :: Core.Maybe Core.Bool,
    tagList :: Core.Maybe [Types.Tag],
    -- | Provides the VPC ID associated with the DB cluster snapshot.
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DBClusterSnapshot' value with any optional fields omitted.
mkDBClusterSnapshot ::
  DBClusterSnapshot
mkDBClusterSnapshot =
  DBClusterSnapshot'
    { allocatedStorage = Core.Nothing,
      availabilityZones = Core.Nothing,
      clusterCreateTime = Core.Nothing,
      dBClusterIdentifier = Core.Nothing,
      dBClusterSnapshotArn = Core.Nothing,
      dBClusterSnapshotIdentifier = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      iAMDatabaseAuthenticationEnabled = Core.Nothing,
      kmsKeyId = Core.Nothing,
      licenseModel = Core.Nothing,
      masterUsername = Core.Nothing,
      percentProgress = Core.Nothing,
      port = Core.Nothing,
      snapshotCreateTime = Core.Nothing,
      snapshotType = Core.Nothing,
      sourceDBClusterSnapshotArn = Core.Nothing,
      status = Core.Nothing,
      storageEncrypted = Core.Nothing,
      tagList = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | Specifies the allocated storage size in gibibytes (GiB).
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsAllocatedStorage :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Int)
dbcsAllocatedStorage = Lens.field @"allocatedStorage"
{-# DEPRECATED dbcsAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | Provides the list of Availability Zones (AZs) where instances in the DB cluster snapshot can be restored.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsAvailabilityZones :: Lens.Lens' DBClusterSnapshot (Core.Maybe [Types.String])
dbcsAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED dbcsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'clusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsClusterCreateTime :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.UTCTime)
dbcsClusterCreateTime = Lens.field @"clusterCreateTime"
{-# DEPRECATED dbcsClusterCreateTime "Use generic-lens or generic-optics with 'clusterCreateTime' instead." #-}

-- | Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsDBClusterIdentifier :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED dbcsDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) for the DB cluster snapshot.
--
-- /Note:/ Consider using 'dBClusterSnapshotArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsDBClusterSnapshotArn :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsDBClusterSnapshotArn = Lens.field @"dBClusterSnapshotArn"
{-# DEPRECATED dbcsDBClusterSnapshotArn "Use generic-lens or generic-optics with 'dBClusterSnapshotArn' instead." #-}

-- | Specifies the identifier for the DB cluster snapshot.
--
-- /Note:/ Consider using 'dBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsDBClusterSnapshotIdentifier :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsDBClusterSnapshotIdentifier = Lens.field @"dBClusterSnapshotIdentifier"
{-# DEPRECATED dbcsDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dBClusterSnapshotIdentifier' instead." #-}

-- | Specifies the name of the database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsEngine :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsEngine = Lens.field @"engine"
{-# DEPRECATED dbcsEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Provides the version of the database engine for this DB cluster snapshot.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsEngineVersion :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED dbcsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- /Note:/ Consider using 'iAMDatabaseAuthenticationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsIAMDatabaseAuthenticationEnabled :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Bool)
dbcsIAMDatabaseAuthenticationEnabled = Lens.field @"iAMDatabaseAuthenticationEnabled"
{-# DEPRECATED dbcsIAMDatabaseAuthenticationEnabled "Use generic-lens or generic-optics with 'iAMDatabaseAuthenticationEnabled' instead." #-}

-- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsKmsKeyId :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED dbcsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Provides the license model information for this DB cluster snapshot.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsLicenseModel :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsLicenseModel = Lens.field @"licenseModel"
{-# DEPRECATED dbcsLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | Provides the master username for the DB cluster snapshot.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsMasterUsername :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsMasterUsername = Lens.field @"masterUsername"
{-# DEPRECATED dbcsMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | Specifies the percentage of the estimated data that has been transferred.
--
-- /Note:/ Consider using 'percentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsPercentProgress :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Int)
dbcsPercentProgress = Lens.field @"percentProgress"
{-# DEPRECATED dbcsPercentProgress "Use generic-lens or generic-optics with 'percentProgress' instead." #-}

-- | Specifies the port that the DB cluster was listening on at the time of the snapshot.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsPort :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Int)
dbcsPort = Lens.field @"port"
{-# DEPRECATED dbcsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'snapshotCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsSnapshotCreateTime :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.UTCTime)
dbcsSnapshotCreateTime = Lens.field @"snapshotCreateTime"
{-# DEPRECATED dbcsSnapshotCreateTime "Use generic-lens or generic-optics with 'snapshotCreateTime' instead." #-}

-- | Provides the type of the DB cluster snapshot.
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsSnapshotType :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsSnapshotType = Lens.field @"snapshotType"
{-# DEPRECATED dbcsSnapshotType "Use generic-lens or generic-optics with 'snapshotType' instead." #-}

-- | If the DB cluster snapshot was copied from a source DB cluster snapshot, the Amazon Resource Name (ARN) for the source DB cluster snapshot, otherwise, a null value.
--
-- /Note:/ Consider using 'sourceDBClusterSnapshotArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsSourceDBClusterSnapshotArn :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsSourceDBClusterSnapshotArn = Lens.field @"sourceDBClusterSnapshotArn"
{-# DEPRECATED dbcsSourceDBClusterSnapshotArn "Use generic-lens or generic-optics with 'sourceDBClusterSnapshotArn' instead." #-}

-- | Specifies the status of this DB cluster snapshot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsStatus :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsStatus = Lens.field @"status"
{-# DEPRECATED dbcsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies whether the DB cluster snapshot is encrypted.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsStorageEncrypted :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Bool)
dbcsStorageEncrypted = Lens.field @"storageEncrypted"
{-# DEPRECATED dbcsStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsTagList :: Lens.Lens' DBClusterSnapshot (Core.Maybe [Types.Tag])
dbcsTagList = Lens.field @"tagList"
{-# DEPRECATED dbcsTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | Provides the VPC ID associated with the DB cluster snapshot.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsVpcId :: Lens.Lens' DBClusterSnapshot (Core.Maybe Types.String)
dbcsVpcId = Lens.field @"vpcId"
{-# DEPRECATED dbcsVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML DBClusterSnapshot where
  parseXML x =
    DBClusterSnapshot'
      Core.<$> (x Core..@? "AllocatedStorage")
      Core.<*> ( x Core..@? "AvailabilityZones"
                   Core..<@> Core.parseXMLList "AvailabilityZone"
               )
      Core.<*> (x Core..@? "ClusterCreateTime")
      Core.<*> (x Core..@? "DBClusterIdentifier")
      Core.<*> (x Core..@? "DBClusterSnapshotArn")
      Core.<*> (x Core..@? "DBClusterSnapshotIdentifier")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "LicenseModel")
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "PercentProgress")
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "SnapshotCreateTime")
      Core.<*> (x Core..@? "SnapshotType")
      Core.<*> (x Core..@? "SourceDBClusterSnapshotArn")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "StorageEncrypted")
      Core.<*> (x Core..@? "TagList" Core..<@> Core.parseXMLList "Tag")
      Core.<*> (x Core..@? "VpcId")
