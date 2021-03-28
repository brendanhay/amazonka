{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBClusterSnapshot
  ( DBClusterSnapshot (..)
  -- * Smart constructor
  , mkDBClusterSnapshot
  -- * Lenses
  , dbcsAllocatedStorage
  , dbcsAvailabilityZones
  , dbcsClusterCreateTime
  , dbcsDBClusterIdentifier
  , dbcsDBClusterSnapshotArn
  , dbcsDBClusterSnapshotIdentifier
  , dbcsEngine
  , dbcsEngineVersion
  , dbcsIAMDatabaseAuthenticationEnabled
  , dbcsKmsKeyId
  , dbcsLicenseModel
  , dbcsMasterUsername
  , dbcsPercentProgress
  , dbcsPort
  , dbcsSnapshotCreateTime
  , dbcsSnapshotType
  , dbcsSourceDBClusterSnapshotArn
  , dbcsStatus
  , dbcsStorageEncrypted
  , dbcsTagList
  , dbcsVpcId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.Tag as Types

-- | Contains the details for an Amazon RDS DB cluster snapshot 
--
-- This data type is used as a response element in the @DescribeDBClusterSnapshots@ action. 
--
-- /See:/ 'mkDBClusterSnapshot' smart constructor.
data DBClusterSnapshot = DBClusterSnapshot'
  { allocatedStorage :: Core.Maybe Core.Int
    -- ^ Specifies the allocated storage size in gibibytes (GiB).
  , availabilityZones :: Core.Maybe [Core.Text]
    -- ^ Provides the list of Availability Zones (AZs) where instances in the DB cluster snapshot can be restored.
  , clusterCreateTime :: Core.Maybe Core.UTCTime
    -- ^ Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
  , dBClusterIdentifier :: Core.Maybe Core.Text
    -- ^ Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
  , dBClusterSnapshotArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the DB cluster snapshot.
  , dBClusterSnapshotIdentifier :: Core.Maybe Core.Text
    -- ^ Specifies the identifier for the DB cluster snapshot.
  , engine :: Core.Maybe Core.Text
    -- ^ Specifies the name of the database engine.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ Provides the version of the database engine for this DB cluster snapshot.
  , iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool
    -- ^ True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster snapshot.
  , licenseModel :: Core.Maybe Core.Text
    -- ^ Provides the license model information for this DB cluster snapshot.
  , masterUsername :: Core.Maybe Core.Text
    -- ^ Provides the master username for the DB cluster snapshot.
  , percentProgress :: Core.Maybe Core.Int
    -- ^ Specifies the percentage of the estimated data that has been transferred.
  , port :: Core.Maybe Core.Int
    -- ^ Specifies the port that the DB cluster was listening on at the time of the snapshot.
  , snapshotCreateTime :: Core.Maybe Core.UTCTime
    -- ^ Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
  , snapshotType :: Core.Maybe Core.Text
    -- ^ Provides the type of the DB cluster snapshot.
  , sourceDBClusterSnapshotArn :: Core.Maybe Core.Text
    -- ^ If the DB cluster snapshot was copied from a source DB cluster snapshot, the Amazon Resource Name (ARN) for the source DB cluster snapshot, otherwise, a null value.
  , status :: Core.Maybe Core.Text
    -- ^ Specifies the status of this DB cluster snapshot.
  , storageEncrypted :: Core.Maybe Core.Bool
    -- ^ Specifies whether the DB cluster snapshot is encrypted.
  , tagList :: Core.Maybe [Types.Tag]
  , vpcId :: Core.Maybe Core.Text
    -- ^ Provides the VPC ID associated with the DB cluster snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DBClusterSnapshot' value with any optional fields omitted.
mkDBClusterSnapshot
    :: DBClusterSnapshot
mkDBClusterSnapshot
  = DBClusterSnapshot'{allocatedStorage = Core.Nothing,
                       availabilityZones = Core.Nothing, clusterCreateTime = Core.Nothing,
                       dBClusterIdentifier = Core.Nothing,
                       dBClusterSnapshotArn = Core.Nothing,
                       dBClusterSnapshotIdentifier = Core.Nothing, engine = Core.Nothing,
                       engineVersion = Core.Nothing,
                       iAMDatabaseAuthenticationEnabled = Core.Nothing,
                       kmsKeyId = Core.Nothing, licenseModel = Core.Nothing,
                       masterUsername = Core.Nothing, percentProgress = Core.Nothing,
                       port = Core.Nothing, snapshotCreateTime = Core.Nothing,
                       snapshotType = Core.Nothing,
                       sourceDBClusterSnapshotArn = Core.Nothing, status = Core.Nothing,
                       storageEncrypted = Core.Nothing, tagList = Core.Nothing,
                       vpcId = Core.Nothing}

-- | Specifies the allocated storage size in gibibytes (GiB).
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsAllocatedStorage :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Int)
dbcsAllocatedStorage = Lens.field @"allocatedStorage"
{-# INLINEABLE dbcsAllocatedStorage #-}
{-# DEPRECATED allocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead"  #-}

-- | Provides the list of Availability Zones (AZs) where instances in the DB cluster snapshot can be restored.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsAvailabilityZones :: Lens.Lens' DBClusterSnapshot (Core.Maybe [Core.Text])
dbcsAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE dbcsAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'clusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsClusterCreateTime :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.UTCTime)
dbcsClusterCreateTime = Lens.field @"clusterCreateTime"
{-# INLINEABLE dbcsClusterCreateTime #-}
{-# DEPRECATED clusterCreateTime "Use generic-lens or generic-optics with 'clusterCreateTime' instead"  #-}

-- | Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsDBClusterIdentifier :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE dbcsDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | The Amazon Resource Name (ARN) for the DB cluster snapshot.
--
-- /Note:/ Consider using 'dBClusterSnapshotArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsDBClusterSnapshotArn :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsDBClusterSnapshotArn = Lens.field @"dBClusterSnapshotArn"
{-# INLINEABLE dbcsDBClusterSnapshotArn #-}
{-# DEPRECATED dBClusterSnapshotArn "Use generic-lens or generic-optics with 'dBClusterSnapshotArn' instead"  #-}

-- | Specifies the identifier for the DB cluster snapshot.
--
-- /Note:/ Consider using 'dBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsDBClusterSnapshotIdentifier :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsDBClusterSnapshotIdentifier = Lens.field @"dBClusterSnapshotIdentifier"
{-# INLINEABLE dbcsDBClusterSnapshotIdentifier #-}
{-# DEPRECATED dBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dBClusterSnapshotIdentifier' instead"  #-}

-- | Specifies the name of the database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsEngine :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsEngine = Lens.field @"engine"
{-# INLINEABLE dbcsEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | Provides the version of the database engine for this DB cluster snapshot.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsEngineVersion :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE dbcsEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- /Note:/ Consider using 'iAMDatabaseAuthenticationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsIAMDatabaseAuthenticationEnabled :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Bool)
dbcsIAMDatabaseAuthenticationEnabled = Lens.field @"iAMDatabaseAuthenticationEnabled"
{-# INLINEABLE dbcsIAMDatabaseAuthenticationEnabled #-}
{-# DEPRECATED iAMDatabaseAuthenticationEnabled "Use generic-lens or generic-optics with 'iAMDatabaseAuthenticationEnabled' instead"  #-}

-- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsKmsKeyId :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE dbcsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | Provides the license model information for this DB cluster snapshot.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsLicenseModel :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsLicenseModel = Lens.field @"licenseModel"
{-# INLINEABLE dbcsLicenseModel #-}
{-# DEPRECATED licenseModel "Use generic-lens or generic-optics with 'licenseModel' instead"  #-}

-- | Provides the master username for the DB cluster snapshot.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsMasterUsername :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsMasterUsername = Lens.field @"masterUsername"
{-# INLINEABLE dbcsMasterUsername #-}
{-# DEPRECATED masterUsername "Use generic-lens or generic-optics with 'masterUsername' instead"  #-}

-- | Specifies the percentage of the estimated data that has been transferred.
--
-- /Note:/ Consider using 'percentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsPercentProgress :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Int)
dbcsPercentProgress = Lens.field @"percentProgress"
{-# INLINEABLE dbcsPercentProgress #-}
{-# DEPRECATED percentProgress "Use generic-lens or generic-optics with 'percentProgress' instead"  #-}

-- | Specifies the port that the DB cluster was listening on at the time of the snapshot.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsPort :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Int)
dbcsPort = Lens.field @"port"
{-# INLINEABLE dbcsPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'snapshotCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsSnapshotCreateTime :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.UTCTime)
dbcsSnapshotCreateTime = Lens.field @"snapshotCreateTime"
{-# INLINEABLE dbcsSnapshotCreateTime #-}
{-# DEPRECATED snapshotCreateTime "Use generic-lens or generic-optics with 'snapshotCreateTime' instead"  #-}

-- | Provides the type of the DB cluster snapshot.
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsSnapshotType :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsSnapshotType = Lens.field @"snapshotType"
{-# INLINEABLE dbcsSnapshotType #-}
{-# DEPRECATED snapshotType "Use generic-lens or generic-optics with 'snapshotType' instead"  #-}

-- | If the DB cluster snapshot was copied from a source DB cluster snapshot, the Amazon Resource Name (ARN) for the source DB cluster snapshot, otherwise, a null value.
--
-- /Note:/ Consider using 'sourceDBClusterSnapshotArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsSourceDBClusterSnapshotArn :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsSourceDBClusterSnapshotArn = Lens.field @"sourceDBClusterSnapshotArn"
{-# INLINEABLE dbcsSourceDBClusterSnapshotArn #-}
{-# DEPRECATED sourceDBClusterSnapshotArn "Use generic-lens or generic-optics with 'sourceDBClusterSnapshotArn' instead"  #-}

-- | Specifies the status of this DB cluster snapshot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsStatus :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsStatus = Lens.field @"status"
{-# INLINEABLE dbcsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Specifies whether the DB cluster snapshot is encrypted.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsStorageEncrypted :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Bool)
dbcsStorageEncrypted = Lens.field @"storageEncrypted"
{-# INLINEABLE dbcsStorageEncrypted #-}
{-# DEPRECATED storageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsTagList :: Lens.Lens' DBClusterSnapshot (Core.Maybe [Types.Tag])
dbcsTagList = Lens.field @"tagList"
{-# INLINEABLE dbcsTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

-- | Provides the VPC ID associated with the DB cluster snapshot.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsVpcId :: Lens.Lens' DBClusterSnapshot (Core.Maybe Core.Text)
dbcsVpcId = Lens.field @"vpcId"
{-# INLINEABLE dbcsVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML DBClusterSnapshot where
        parseXML x
          = DBClusterSnapshot' Core.<$>
              (x Core..@? "AllocatedStorage") Core.<*>
                x Core..@? "AvailabilityZones" Core..<@>
                  Core.parseXMLList "AvailabilityZone"
                Core.<*> x Core..@? "ClusterCreateTime"
                Core.<*> x Core..@? "DBClusterIdentifier"
                Core.<*> x Core..@? "DBClusterSnapshotArn"
                Core.<*> x Core..@? "DBClusterSnapshotIdentifier"
                Core.<*> x Core..@? "Engine"
                Core.<*> x Core..@? "EngineVersion"
                Core.<*> x Core..@? "IAMDatabaseAuthenticationEnabled"
                Core.<*> x Core..@? "KmsKeyId"
                Core.<*> x Core..@? "LicenseModel"
                Core.<*> x Core..@? "MasterUsername"
                Core.<*> x Core..@? "PercentProgress"
                Core.<*> x Core..@? "Port"
                Core.<*> x Core..@? "SnapshotCreateTime"
                Core.<*> x Core..@? "SnapshotType"
                Core.<*> x Core..@? "SourceDBClusterSnapshotArn"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "StorageEncrypted"
                Core.<*> x Core..@? "TagList" Core..<@> Core.parseXMLList "Tag"
                Core.<*> x Core..@? "VpcId"
