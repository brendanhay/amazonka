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
    dcsEngineVersion,
    dcsStatus,
    dcsStorageEncrypted,
    dcsDBClusterIdentifier,
    dcsMasterUsername,
    dcsIAMDatabaseAuthenticationEnabled,
    dcsDBClusterSnapshotARN,
    dcsVPCId,
    dcsTagList,
    dcsDBClusterSnapshotIdentifier,
    dcsEngine,
    dcsLicenseModel,
    dcsAvailabilityZones,
    dcsSnapshotType,
    dcsKMSKeyId,
    dcsSnapshotCreateTime,
    dcsAllocatedStorage,
    dcsSourceDBClusterSnapshotARN,
    dcsClusterCreateTime,
    dcsPercentProgress,
    dcsPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.Tag

-- | Contains the details for an Amazon RDS DB cluster snapshot
--
-- This data type is used as a response element in the @DescribeDBClusterSnapshots@ action.
--
-- /See:/ 'mkDBClusterSnapshot' smart constructor.
data DBClusterSnapshot = DBClusterSnapshot'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    storageEncrypted :: Lude.Maybe Lude.Bool,
    dbClusterIdentifier :: Lude.Maybe Lude.Text,
    masterUsername :: Lude.Maybe Lude.Text,
    iamDatabaseAuthenticationEnabled ::
      Lude.Maybe Lude.Bool,
    dbClusterSnapshotARN :: Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    tagList :: Lude.Maybe [Tag],
    dbClusterSnapshotIdentifier :: Lude.Maybe Lude.Text,
    engine :: Lude.Maybe Lude.Text,
    licenseModel :: Lude.Maybe Lude.Text,
    availabilityZones :: Lude.Maybe [Lude.Text],
    snapshotType :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    snapshotCreateTime :: Lude.Maybe Lude.DateTime,
    allocatedStorage :: Lude.Maybe Lude.Int,
    sourceDBClusterSnapshotARN :: Lude.Maybe Lude.Text,
    clusterCreateTime :: Lude.Maybe Lude.DateTime,
    percentProgress :: Lude.Maybe Lude.Int,
    port :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBClusterSnapshot' with the minimum fields required to make a request.
--
-- * 'allocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
-- * 'availabilityZones' - Provides the list of Availability Zones (AZs) where instances in the DB cluster snapshot can be restored.
-- * 'clusterCreateTime' - Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
-- * 'dbClusterIdentifier' - Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
-- * 'dbClusterSnapshotARN' - The Amazon Resource Name (ARN) for the DB cluster snapshot.
-- * 'dbClusterSnapshotIdentifier' - Specifies the identifier for the DB cluster snapshot.
-- * 'engine' - Specifies the name of the database engine.
-- * 'engineVersion' - Provides the version of the database engine for this DB cluster snapshot.
-- * 'iamDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
-- * 'kmsKeyId' - If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster snapshot.
-- * 'licenseModel' - Provides the license model information for this DB cluster snapshot.
-- * 'masterUsername' - Provides the master username for the DB cluster snapshot.
-- * 'percentProgress' - Specifies the percentage of the estimated data that has been transferred.
-- * 'port' - Specifies the port that the DB cluster was listening on at the time of the snapshot.
-- * 'snapshotCreateTime' - Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
-- * 'snapshotType' - Provides the type of the DB cluster snapshot.
-- * 'sourceDBClusterSnapshotARN' - If the DB cluster snapshot was copied from a source DB cluster snapshot, the Amazon Resource Name (ARN) for the source DB cluster snapshot, otherwise, a null value.
-- * 'status' - Specifies the status of this DB cluster snapshot.
-- * 'storageEncrypted' - Specifies whether the DB cluster snapshot is encrypted.
-- * 'tagList' - Undocumented field.
-- * 'vpcId' - Provides the VPC ID associated with the DB cluster snapshot.
mkDBClusterSnapshot ::
  DBClusterSnapshot
mkDBClusterSnapshot =
  DBClusterSnapshot'
    { engineVersion = Lude.Nothing,
      status = Lude.Nothing,
      storageEncrypted = Lude.Nothing,
      dbClusterIdentifier = Lude.Nothing,
      masterUsername = Lude.Nothing,
      iamDatabaseAuthenticationEnabled = Lude.Nothing,
      dbClusterSnapshotARN = Lude.Nothing,
      vpcId = Lude.Nothing,
      tagList = Lude.Nothing,
      dbClusterSnapshotIdentifier = Lude.Nothing,
      engine = Lude.Nothing,
      licenseModel = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      snapshotType = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      snapshotCreateTime = Lude.Nothing,
      allocatedStorage = Lude.Nothing,
      sourceDBClusterSnapshotARN = Lude.Nothing,
      clusterCreateTime = Lude.Nothing,
      percentProgress = Lude.Nothing,
      port = Lude.Nothing
    }

-- | Provides the version of the database engine for this DB cluster snapshot.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsEngineVersion :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsEngineVersion = Lens.lens (engineVersion :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Specifies the status of this DB cluster snapshot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsStatus :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsStatus = Lens.lens (status :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies whether the DB cluster snapshot is encrypted.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsStorageEncrypted :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Bool)
dcsStorageEncrypted = Lens.lens (storageEncrypted :: DBClusterSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {storageEncrypted = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsDBClusterIdentifier :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | Provides the master username for the DB cluster snapshot.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsMasterUsername :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsMasterUsername = Lens.lens (masterUsername :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {masterUsername = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- /Note:/ Consider using 'iamDatabaseAuthenticationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsIAMDatabaseAuthenticationEnabled :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Bool)
dcsIAMDatabaseAuthenticationEnabled = Lens.lens (iamDatabaseAuthenticationEnabled :: DBClusterSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {iamDatabaseAuthenticationEnabled = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsIAMDatabaseAuthenticationEnabled "Use generic-lens or generic-optics with 'iamDatabaseAuthenticationEnabled' instead." #-}

-- | The Amazon Resource Name (ARN) for the DB cluster snapshot.
--
-- /Note:/ Consider using 'dbClusterSnapshotARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsDBClusterSnapshotARN :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsDBClusterSnapshotARN = Lens.lens (dbClusterSnapshotARN :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterSnapshotARN = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsDBClusterSnapshotARN "Use generic-lens or generic-optics with 'dbClusterSnapshotARN' instead." #-}

-- | Provides the VPC ID associated with the DB cluster snapshot.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsVPCId :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsVPCId = Lens.lens (vpcId :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsTagList :: Lens.Lens' DBClusterSnapshot (Lude.Maybe [Tag])
dcsTagList = Lens.lens (tagList :: DBClusterSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | Specifies the identifier for the DB cluster snapshot.
--
-- /Note:/ Consider using 'dbClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsDBClusterSnapshotIdentifier :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsDBClusterSnapshotIdentifier = Lens.lens (dbClusterSnapshotIdentifier :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterSnapshotIdentifier = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dbClusterSnapshotIdentifier' instead." #-}

-- | Specifies the name of the database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsEngine :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsEngine = Lens.lens (engine :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Provides the license model information for this DB cluster snapshot.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsLicenseModel :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsLicenseModel = Lens.lens (licenseModel :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | Provides the list of Availability Zones (AZs) where instances in the DB cluster snapshot can be restored.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsAvailabilityZones :: Lens.Lens' DBClusterSnapshot (Lude.Maybe [Lude.Text])
dcsAvailabilityZones = Lens.lens (availabilityZones :: DBClusterSnapshot -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | Provides the type of the DB cluster snapshot.
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsSnapshotType :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsSnapshotType = Lens.lens (snapshotType :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotType = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsSnapshotType "Use generic-lens or generic-optics with 'snapshotType' instead." #-}

-- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsKMSKeyId :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsKMSKeyId = Lens.lens (kmsKeyId :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'snapshotCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsSnapshotCreateTime :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.DateTime)
dcsSnapshotCreateTime = Lens.lens (snapshotCreateTime :: DBClusterSnapshot -> Lude.Maybe Lude.DateTime) (\s a -> s {snapshotCreateTime = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsSnapshotCreateTime "Use generic-lens or generic-optics with 'snapshotCreateTime' instead." #-}

-- | Specifies the allocated storage size in gibibytes (GiB).
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsAllocatedStorage :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Int)
dcsAllocatedStorage = Lens.lens (allocatedStorage :: DBClusterSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | If the DB cluster snapshot was copied from a source DB cluster snapshot, the Amazon Resource Name (ARN) for the source DB cluster snapshot, otherwise, a null value.
--
-- /Note:/ Consider using 'sourceDBClusterSnapshotARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsSourceDBClusterSnapshotARN :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Text)
dcsSourceDBClusterSnapshotARN = Lens.lens (sourceDBClusterSnapshotARN :: DBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {sourceDBClusterSnapshotARN = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsSourceDBClusterSnapshotARN "Use generic-lens or generic-optics with 'sourceDBClusterSnapshotARN' instead." #-}

-- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'clusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsClusterCreateTime :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.DateTime)
dcsClusterCreateTime = Lens.lens (clusterCreateTime :: DBClusterSnapshot -> Lude.Maybe Lude.DateTime) (\s a -> s {clusterCreateTime = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsClusterCreateTime "Use generic-lens or generic-optics with 'clusterCreateTime' instead." #-}

-- | Specifies the percentage of the estimated data that has been transferred.
--
-- /Note:/ Consider using 'percentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsPercentProgress :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Int)
dcsPercentProgress = Lens.lens (percentProgress :: DBClusterSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {percentProgress = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsPercentProgress "Use generic-lens or generic-optics with 'percentProgress' instead." #-}

-- | Specifies the port that the DB cluster was listening on at the time of the snapshot.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsPort :: Lens.Lens' DBClusterSnapshot (Lude.Maybe Lude.Int)
dcsPort = Lens.lens (port :: DBClusterSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: DBClusterSnapshot)
{-# DEPRECATED dcsPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromXML DBClusterSnapshot where
  parseXML x =
    DBClusterSnapshot'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "StorageEncrypted")
      Lude.<*> (x Lude..@? "DBClusterIdentifier")
      Lude.<*> (x Lude..@? "MasterUsername")
      Lude.<*> (x Lude..@? "IAMDatabaseAuthenticationEnabled")
      Lude.<*> (x Lude..@? "DBClusterSnapshotArn")
      Lude.<*> (x Lude..@? "VpcId")
      Lude.<*> ( x Lude..@? "TagList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
      Lude.<*> (x Lude..@? "DBClusterSnapshotIdentifier")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "LicenseModel")
      Lude.<*> ( x Lude..@? "AvailabilityZones" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AvailabilityZone")
               )
      Lude.<*> (x Lude..@? "SnapshotType")
      Lude.<*> (x Lude..@? "KmsKeyId")
      Lude.<*> (x Lude..@? "SnapshotCreateTime")
      Lude.<*> (x Lude..@? "AllocatedStorage")
      Lude.<*> (x Lude..@? "SourceDBClusterSnapshotArn")
      Lude.<*> (x Lude..@? "ClusterCreateTime")
      Lude.<*> (x Lude..@? "PercentProgress")
      Lude.<*> (x Lude..@? "Port")
