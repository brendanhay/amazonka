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
    dsEngineVersion,
    dsStatus,
    dsDBSnapshotARN,
    dsMasterUsername,
    dsSourceRegion,
    dsIAMDatabaseAuthenticationEnabled,
    dsIOPS,
    dsVPCId,
    dsInstanceCreateTime,
    dsTagList,
    dsEngine,
    dsEncrypted,
    dsDBSnapshotIdentifier,
    dsProcessorFeatures,
    dsLicenseModel,
    dsSourceDBSnapshotIdentifier,
    dsSnapshotType,
    dsDBInstanceIdentifier,
    dsKMSKeyId,
    dsAvailabilityZone,
    dsSnapshotCreateTime,
    dsAllocatedStorage,
    dsDBiResourceId,
    dsOptionGroupName,
    dsTimezone,
    dsTDECredentialARN,
    dsPercentProgress,
    dsPort,
    dsStorageType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.ProcessorFeature
import Network.AWS.RDS.Types.Tag

-- | Contains the details of an Amazon RDS DB snapshot.
--
-- This data type is used as a response element in the @DescribeDBSnapshots@ action.
--
-- /See:/ 'mkDBSnapshot' smart constructor.
data DBSnapshot = DBSnapshot'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    dbSnapshotARN :: Lude.Maybe Lude.Text,
    masterUsername :: Lude.Maybe Lude.Text,
    sourceRegion :: Lude.Maybe Lude.Text,
    iamDatabaseAuthenticationEnabled :: Lude.Maybe Lude.Bool,
    iops :: Lude.Maybe Lude.Int,
    vpcId :: Lude.Maybe Lude.Text,
    instanceCreateTime :: Lude.Maybe Lude.ISO8601,
    tagList :: Lude.Maybe [Tag],
    engine :: Lude.Maybe Lude.Text,
    encrypted :: Lude.Maybe Lude.Bool,
    dbSnapshotIdentifier :: Lude.Maybe Lude.Text,
    processorFeatures :: Lude.Maybe [ProcessorFeature],
    licenseModel :: Lude.Maybe Lude.Text,
    sourceDBSnapshotIdentifier :: Lude.Maybe Lude.Text,
    snapshotType :: Lude.Maybe Lude.Text,
    dbInstanceIdentifier :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    snapshotCreateTime :: Lude.Maybe Lude.ISO8601,
    allocatedStorage :: Lude.Maybe Lude.Int,
    dbiResourceId :: Lude.Maybe Lude.Text,
    optionGroupName :: Lude.Maybe Lude.Text,
    timezone :: Lude.Maybe Lude.Text,
    tdeCredentialARN :: Lude.Maybe Lude.Text,
    percentProgress :: Lude.Maybe Lude.Int,
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

-- | Creates a value of 'DBSnapshot' with the minimum fields required to make a request.
--
-- * 'allocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
-- * 'availabilityZone' - Specifies the name of the Availability Zone the DB instance was located in at the time of the DB snapshot.
-- * 'dbInstanceIdentifier' - Specifies the DB instance identifier of the DB instance this DB snapshot was created from.
-- * 'dbSnapshotARN' - The Amazon Resource Name (ARN) for the DB snapshot.
-- * 'dbSnapshotIdentifier' - Specifies the identifier for the DB snapshot.
-- * 'dbiResourceId' - The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
-- * 'encrypted' - Specifies whether the DB snapshot is encrypted.
-- * 'engine' - Specifies the name of the database engine.
-- * 'engineVersion' - Specifies the version of the database engine.
-- * 'iamDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
-- * 'instanceCreateTime' - Specifies the time in Coordinated Universal Time (UTC) when the DB instance, from which the snapshot was taken, was created.
-- * 'iops' - Specifies the Provisioned IOPS (I/O operations per second) value of the DB instance at the time of the snapshot.
-- * 'kmsKeyId' - If @Encrypted@ is true, the AWS KMS key identifier for the encrypted DB snapshot.
-- * 'licenseModel' - License model information for the restored DB instance.
-- * 'masterUsername' - Provides the master username for the DB snapshot.
-- * 'optionGroupName' - Provides the option group name for the DB snapshot.
-- * 'percentProgress' - The percentage of the estimated data that has been transferred.
-- * 'port' - Specifies the port that the database engine was listening on at the time of the snapshot.
-- * 'processorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance when the DB snapshot was created.
-- * 'snapshotCreateTime' - Specifies when the snapshot was taken in Coordinated Universal Time (UTC).
-- * 'snapshotType' - Provides the type of the DB snapshot.
-- * 'sourceDBSnapshotIdentifier' - The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was copied from. It only has value in case of cross-customer or cross-region copy.
-- * 'sourceRegion' - The AWS Region that the DB snapshot was created in or copied from.
-- * 'status' - Specifies the status of this DB snapshot.
-- * 'storageType' - Specifies the storage type associated with DB snapshot.
-- * 'tagList' - Undocumented field.
-- * 'tdeCredentialARN' - The ARN from the key store with which to associate the instance for TDE encryption.
-- * 'timezone' - The time zone of the DB snapshot. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for snapshots taken from Microsoft SQL Server DB instances that were created with a time zone specified.
-- * 'vpcId' - Provides the VPC ID associated with the DB snapshot.
mkDBSnapshot ::
  DBSnapshot
mkDBSnapshot =
  DBSnapshot'
    { engineVersion = Lude.Nothing,
      status = Lude.Nothing,
      dbSnapshotARN = Lude.Nothing,
      masterUsername = Lude.Nothing,
      sourceRegion = Lude.Nothing,
      iamDatabaseAuthenticationEnabled = Lude.Nothing,
      iops = Lude.Nothing,
      vpcId = Lude.Nothing,
      instanceCreateTime = Lude.Nothing,
      tagList = Lude.Nothing,
      engine = Lude.Nothing,
      encrypted = Lude.Nothing,
      dbSnapshotIdentifier = Lude.Nothing,
      processorFeatures = Lude.Nothing,
      licenseModel = Lude.Nothing,
      sourceDBSnapshotIdentifier = Lude.Nothing,
      snapshotType = Lude.Nothing,
      dbInstanceIdentifier = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      snapshotCreateTime = Lude.Nothing,
      allocatedStorage = Lude.Nothing,
      dbiResourceId = Lude.Nothing,
      optionGroupName = Lude.Nothing,
      timezone = Lude.Nothing,
      tdeCredentialARN = Lude.Nothing,
      percentProgress = Lude.Nothing,
      port = Lude.Nothing,
      storageType = Lude.Nothing
    }

-- | Specifies the version of the database engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsEngineVersion :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsEngineVersion = Lens.lens (engineVersion :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: DBSnapshot)
{-# DEPRECATED dsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Specifies the status of this DB snapshot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStatus :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsStatus = Lens.lens (status :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBSnapshot)
{-# DEPRECATED dsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) for the DB snapshot.
--
-- /Note:/ Consider using 'dbSnapshotARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDBSnapshotARN :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsDBSnapshotARN = Lens.lens (dbSnapshotARN :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbSnapshotARN = a} :: DBSnapshot)
{-# DEPRECATED dsDBSnapshotARN "Use generic-lens or generic-optics with 'dbSnapshotARN' instead." #-}

-- | Provides the master username for the DB snapshot.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMasterUsername :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsMasterUsername = Lens.lens (masterUsername :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {masterUsername = a} :: DBSnapshot)
{-# DEPRECATED dsMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The AWS Region that the DB snapshot was created in or copied from.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSourceRegion :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsSourceRegion = Lens.lens (sourceRegion :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {sourceRegion = a} :: DBSnapshot)
{-# DEPRECATED dsSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- /Note:/ Consider using 'iamDatabaseAuthenticationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsIAMDatabaseAuthenticationEnabled :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Bool)
dsIAMDatabaseAuthenticationEnabled = Lens.lens (iamDatabaseAuthenticationEnabled :: DBSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {iamDatabaseAuthenticationEnabled = a} :: DBSnapshot)
{-# DEPRECATED dsIAMDatabaseAuthenticationEnabled "Use generic-lens or generic-optics with 'iamDatabaseAuthenticationEnabled' instead." #-}

-- | Specifies the Provisioned IOPS (I/O operations per second) value of the DB instance at the time of the snapshot.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsIOPS :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Int)
dsIOPS = Lens.lens (iops :: DBSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: DBSnapshot)
{-# DEPRECATED dsIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Provides the VPC ID associated with the DB snapshot.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsVPCId :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsVPCId = Lens.lens (vpcId :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DBSnapshot)
{-# DEPRECATED dsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Specifies the time in Coordinated Universal Time (UTC) when the DB instance, from which the snapshot was taken, was created.
--
-- /Note:/ Consider using 'instanceCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsInstanceCreateTime :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.ISO8601)
dsInstanceCreateTime = Lens.lens (instanceCreateTime :: DBSnapshot -> Lude.Maybe Lude.ISO8601) (\s a -> s {instanceCreateTime = a} :: DBSnapshot)
{-# DEPRECATED dsInstanceCreateTime "Use generic-lens or generic-optics with 'instanceCreateTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsTagList :: Lens.Lens' DBSnapshot (Lude.Maybe [Tag])
dsTagList = Lens.lens (tagList :: DBSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: DBSnapshot)
{-# DEPRECATED dsTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | Specifies the name of the database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsEngine :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsEngine = Lens.lens (engine :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: DBSnapshot)
{-# DEPRECATED dsEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Specifies whether the DB snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsEncrypted :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Bool)
dsEncrypted = Lens.lens (encrypted :: DBSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: DBSnapshot)
{-# DEPRECATED dsEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | Specifies the identifier for the DB snapshot.
--
-- /Note:/ Consider using 'dbSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDBSnapshotIdentifier :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsDBSnapshotIdentifier = Lens.lens (dbSnapshotIdentifier :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbSnapshotIdentifier = a} :: DBSnapshot)
{-# DEPRECATED dsDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dbSnapshotIdentifier' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance when the DB snapshot was created.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsProcessorFeatures :: Lens.Lens' DBSnapshot (Lude.Maybe [ProcessorFeature])
dsProcessorFeatures = Lens.lens (processorFeatures :: DBSnapshot -> Lude.Maybe [ProcessorFeature]) (\s a -> s {processorFeatures = a} :: DBSnapshot)
{-# DEPRECATED dsProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | License model information for the restored DB instance.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLicenseModel :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsLicenseModel = Lens.lens (licenseModel :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: DBSnapshot)
{-# DEPRECATED dsLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was copied from. It only has value in case of cross-customer or cross-region copy.
--
-- /Note:/ Consider using 'sourceDBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSourceDBSnapshotIdentifier :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsSourceDBSnapshotIdentifier = Lens.lens (sourceDBSnapshotIdentifier :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {sourceDBSnapshotIdentifier = a} :: DBSnapshot)
{-# DEPRECATED dsSourceDBSnapshotIdentifier "Use generic-lens or generic-optics with 'sourceDBSnapshotIdentifier' instead." #-}

-- | Provides the type of the DB snapshot.
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSnapshotType :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsSnapshotType = Lens.lens (snapshotType :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotType = a} :: DBSnapshot)
{-# DEPRECATED dsSnapshotType "Use generic-lens or generic-optics with 'snapshotType' instead." #-}

-- | Specifies the DB instance identifier of the DB instance this DB snapshot was created from.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDBInstanceIdentifier :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: DBSnapshot)
{-# DEPRECATED dsDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | If @Encrypted@ is true, the AWS KMS key identifier for the encrypted DB snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsKMSKeyId :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsKMSKeyId = Lens.lens (kmsKeyId :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DBSnapshot)
{-# DEPRECATED dsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies the name of the Availability Zone the DB instance was located in at the time of the DB snapshot.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAvailabilityZone :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsAvailabilityZone = Lens.lens (availabilityZone :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DBSnapshot)
{-# DEPRECATED dsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Specifies when the snapshot was taken in Coordinated Universal Time (UTC).
--
-- /Note:/ Consider using 'snapshotCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSnapshotCreateTime :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.ISO8601)
dsSnapshotCreateTime = Lens.lens (snapshotCreateTime :: DBSnapshot -> Lude.Maybe Lude.ISO8601) (\s a -> s {snapshotCreateTime = a} :: DBSnapshot)
{-# DEPRECATED dsSnapshotCreateTime "Use generic-lens or generic-optics with 'snapshotCreateTime' instead." #-}

-- | Specifies the allocated storage size in gibibytes (GiB).
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAllocatedStorage :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Int)
dsAllocatedStorage = Lens.lens (allocatedStorage :: DBSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: DBSnapshot)
{-# DEPRECATED dsAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDBiResourceId :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsDBiResourceId = Lens.lens (dbiResourceId :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbiResourceId = a} :: DBSnapshot)
{-# DEPRECATED dsDBiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead." #-}

-- | Provides the option group name for the DB snapshot.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsOptionGroupName :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsOptionGroupName = Lens.lens (optionGroupName :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: DBSnapshot)
{-# DEPRECATED dsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The time zone of the DB snapshot. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for snapshots taken from Microsoft SQL Server DB instances that were created with a time zone specified.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsTimezone :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsTimezone = Lens.lens (timezone :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: DBSnapshot)
{-# DEPRECATED dsTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The ARN from the key store with which to associate the instance for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsTDECredentialARN :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsTDECredentialARN = Lens.lens (tdeCredentialARN :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {tdeCredentialARN = a} :: DBSnapshot)
{-# DEPRECATED dsTDECredentialARN "Use generic-lens or generic-optics with 'tdeCredentialARN' instead." #-}

-- | The percentage of the estimated data that has been transferred.
--
-- /Note:/ Consider using 'percentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsPercentProgress :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Int)
dsPercentProgress = Lens.lens (percentProgress :: DBSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {percentProgress = a} :: DBSnapshot)
{-# DEPRECATED dsPercentProgress "Use generic-lens or generic-optics with 'percentProgress' instead." #-}

-- | Specifies the port that the database engine was listening on at the time of the snapshot.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsPort :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Int)
dsPort = Lens.lens (port :: DBSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: DBSnapshot)
{-# DEPRECATED dsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | Specifies the storage type associated with DB snapshot.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStorageType :: Lens.Lens' DBSnapshot (Lude.Maybe Lude.Text)
dsStorageType = Lens.lens (storageType :: DBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: DBSnapshot)
{-# DEPRECATED dsStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Lude.FromXML DBSnapshot where
  parseXML x =
    DBSnapshot'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "DBSnapshotArn")
      Lude.<*> (x Lude..@? "MasterUsername")
      Lude.<*> (x Lude..@? "SourceRegion")
      Lude.<*> (x Lude..@? "IAMDatabaseAuthenticationEnabled")
      Lude.<*> (x Lude..@? "Iops")
      Lude.<*> (x Lude..@? "VpcId")
      Lude.<*> (x Lude..@? "InstanceCreateTime")
      Lude.<*> ( x Lude..@? "TagList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "Encrypted")
      Lude.<*> (x Lude..@? "DBSnapshotIdentifier")
      Lude.<*> ( x Lude..@? "ProcessorFeatures" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ProcessorFeature")
               )
      Lude.<*> (x Lude..@? "LicenseModel")
      Lude.<*> (x Lude..@? "SourceDBSnapshotIdentifier")
      Lude.<*> (x Lude..@? "SnapshotType")
      Lude.<*> (x Lude..@? "DBInstanceIdentifier")
      Lude.<*> (x Lude..@? "KmsKeyId")
      Lude.<*> (x Lude..@? "AvailabilityZone")
      Lude.<*> (x Lude..@? "SnapshotCreateTime")
      Lude.<*> (x Lude..@? "AllocatedStorage")
      Lude.<*> (x Lude..@? "DbiResourceId")
      Lude.<*> (x Lude..@? "OptionGroupName")
      Lude.<*> (x Lude..@? "Timezone")
      Lude.<*> (x Lude..@? "TdeCredentialArn")
      Lude.<*> (x Lude..@? "PercentProgress")
      Lude.<*> (x Lude..@? "Port")
      Lude.<*> (x Lude..@? "StorageType")
