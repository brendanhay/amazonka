{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecurityHub.Types.AwsRdsDbSnapshotDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsRdsDbSnapshotDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsRdsDbProcessorFeature

-- | Provides details about an Amazon RDS DB cluster snapshot.
--
-- /See:/ 'newAwsRdsDbSnapshotDetails' smart constructor.
data AwsRdsDbSnapshotDetails = AwsRdsDbSnapshotDetails'
  { -- | The version of the database engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The status of this DB snapshot.
    status :: Prelude.Maybe Prelude.Text,
    -- | The master user name for the DB snapshot.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region that the DB snapshot was created in or
    -- copied from.
    sourceRegion :: Prelude.Maybe Prelude.Text,
    -- | The provisioned IOPS (I\/O operations per second) value of the DB
    -- instance at the time of the snapshot.
    iops :: Prelude.Maybe Prelude.Int,
    -- | Whether mapping of IAM accounts to database accounts is enabled.
    iamDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The VPC ID associated with the DB snapshot.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time in Coordinated Universal Time (UTC) when the DB
    -- instance, from which the snapshot was taken, was created.
    instanceCreateTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the database engine to use for this DB instance.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Whether the DB snapshot is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The name or ARN of the DB snapshot that is used to restore the DB
    -- instance.
    dbSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [AwsRdsDbProcessorFeature],
    -- | License model information for the restored DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | A name for the DB instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The type of the DB snapshot.
    snapshotType :: Prelude.Maybe Prelude.Text,
    -- | The DB snapshot ARN that the DB snapshot was copied from.
    sourceDbSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | If @Encrypted@ is @true@, the KMS key identifier for the encrypted DB
    -- snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the Availability Zone in which the DB instance was
    -- located at the time of the DB snapshot.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | When the snapshot was taken in Coordinated Universal Time (UTC).
    snapshotCreateTime :: Prelude.Maybe Prelude.Text,
    -- | The amount of storage (in gigabytes) to be initially allocated for the
    -- database instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the source DB instance.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | The option group name for the DB snapshot.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the DB snapshot.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The percentage of the estimated data that has been transferred.
    percentProgress :: Prelude.Maybe Prelude.Int,
    -- | The port that the database engine was listening on at the time of the
    -- snapshot.
    port :: Prelude.Maybe Prelude.Int,
    -- | The storage type associated with the DB snapshot.
    storageType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbSnapshotDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'awsRdsDbSnapshotDetails_engineVersion' - The version of the database engine.
--
-- 'status', 'awsRdsDbSnapshotDetails_status' - The status of this DB snapshot.
--
-- 'masterUsername', 'awsRdsDbSnapshotDetails_masterUsername' - The master user name for the DB snapshot.
--
-- 'sourceRegion', 'awsRdsDbSnapshotDetails_sourceRegion' - The Amazon Web Services Region that the DB snapshot was created in or
-- copied from.
--
-- 'iops', 'awsRdsDbSnapshotDetails_iops' - The provisioned IOPS (I\/O operations per second) value of the DB
-- instance at the time of the snapshot.
--
-- 'iamDatabaseAuthenticationEnabled', 'awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled' - Whether mapping of IAM accounts to database accounts is enabled.
--
-- 'vpcId', 'awsRdsDbSnapshotDetails_vpcId' - The VPC ID associated with the DB snapshot.
--
-- 'instanceCreateTime', 'awsRdsDbSnapshotDetails_instanceCreateTime' - Specifies the time in Coordinated Universal Time (UTC) when the DB
-- instance, from which the snapshot was taken, was created.
--
-- 'engine', 'awsRdsDbSnapshotDetails_engine' - The name of the database engine to use for this DB instance.
--
-- 'encrypted', 'awsRdsDbSnapshotDetails_encrypted' - Whether the DB snapshot is encrypted.
--
-- 'dbSnapshotIdentifier', 'awsRdsDbSnapshotDetails_dbSnapshotIdentifier' - The name or ARN of the DB snapshot that is used to restore the DB
-- instance.
--
-- 'processorFeatures', 'awsRdsDbSnapshotDetails_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'licenseModel', 'awsRdsDbSnapshotDetails_licenseModel' - License model information for the restored DB instance.
--
-- 'dbInstanceIdentifier', 'awsRdsDbSnapshotDetails_dbInstanceIdentifier' - A name for the DB instance.
--
-- 'snapshotType', 'awsRdsDbSnapshotDetails_snapshotType' - The type of the DB snapshot.
--
-- 'sourceDbSnapshotIdentifier', 'awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier' - The DB snapshot ARN that the DB snapshot was copied from.
--
-- 'kmsKeyId', 'awsRdsDbSnapshotDetails_kmsKeyId' - If @Encrypted@ is @true@, the KMS key identifier for the encrypted DB
-- snapshot.
--
-- 'availabilityZone', 'awsRdsDbSnapshotDetails_availabilityZone' - Specifies the name of the Availability Zone in which the DB instance was
-- located at the time of the DB snapshot.
--
-- 'snapshotCreateTime', 'awsRdsDbSnapshotDetails_snapshotCreateTime' - When the snapshot was taken in Coordinated Universal Time (UTC).
--
-- 'allocatedStorage', 'awsRdsDbSnapshotDetails_allocatedStorage' - The amount of storage (in gigabytes) to be initially allocated for the
-- database instance.
--
-- 'dbiResourceId', 'awsRdsDbSnapshotDetails_dbiResourceId' - The identifier for the source DB instance.
--
-- 'optionGroupName', 'awsRdsDbSnapshotDetails_optionGroupName' - The option group name for the DB snapshot.
--
-- 'timezone', 'awsRdsDbSnapshotDetails_timezone' - The time zone of the DB snapshot.
--
-- 'tdeCredentialArn', 'awsRdsDbSnapshotDetails_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- 'percentProgress', 'awsRdsDbSnapshotDetails_percentProgress' - The percentage of the estimated data that has been transferred.
--
-- 'port', 'awsRdsDbSnapshotDetails_port' - The port that the database engine was listening on at the time of the
-- snapshot.
--
-- 'storageType', 'awsRdsDbSnapshotDetails_storageType' - The storage type associated with the DB snapshot.
newAwsRdsDbSnapshotDetails ::
  AwsRdsDbSnapshotDetails
newAwsRdsDbSnapshotDetails =
  AwsRdsDbSnapshotDetails'
    { engineVersion =
        Prelude.Nothing,
      status = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      sourceRegion = Prelude.Nothing,
      iops = Prelude.Nothing,
      iamDatabaseAuthenticationEnabled = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      engine = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      dbSnapshotIdentifier = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      snapshotType = Prelude.Nothing,
      sourceDbSnapshotIdentifier = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      timezone = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      port = Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | The version of the database engine.
awsRdsDbSnapshotDetails_engineVersion :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_engineVersion = Lens.lens (\AwsRdsDbSnapshotDetails' {engineVersion} -> engineVersion) (\s@AwsRdsDbSnapshotDetails' {} a -> s {engineVersion = a} :: AwsRdsDbSnapshotDetails)

-- | The status of this DB snapshot.
awsRdsDbSnapshotDetails_status :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_status = Lens.lens (\AwsRdsDbSnapshotDetails' {status} -> status) (\s@AwsRdsDbSnapshotDetails' {} a -> s {status = a} :: AwsRdsDbSnapshotDetails)

-- | The master user name for the DB snapshot.
awsRdsDbSnapshotDetails_masterUsername :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_masterUsername = Lens.lens (\AwsRdsDbSnapshotDetails' {masterUsername} -> masterUsername) (\s@AwsRdsDbSnapshotDetails' {} a -> s {masterUsername = a} :: AwsRdsDbSnapshotDetails)

-- | The Amazon Web Services Region that the DB snapshot was created in or
-- copied from.
awsRdsDbSnapshotDetails_sourceRegion :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_sourceRegion = Lens.lens (\AwsRdsDbSnapshotDetails' {sourceRegion} -> sourceRegion) (\s@AwsRdsDbSnapshotDetails' {} a -> s {sourceRegion = a} :: AwsRdsDbSnapshotDetails)

-- | The provisioned IOPS (I\/O operations per second) value of the DB
-- instance at the time of the snapshot.
awsRdsDbSnapshotDetails_iops :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_iops = Lens.lens (\AwsRdsDbSnapshotDetails' {iops} -> iops) (\s@AwsRdsDbSnapshotDetails' {} a -> s {iops = a} :: AwsRdsDbSnapshotDetails)

-- | Whether mapping of IAM accounts to database accounts is enabled.
awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled = Lens.lens (\AwsRdsDbSnapshotDetails' {iamDatabaseAuthenticationEnabled} -> iamDatabaseAuthenticationEnabled) (\s@AwsRdsDbSnapshotDetails' {} a -> s {iamDatabaseAuthenticationEnabled = a} :: AwsRdsDbSnapshotDetails)

-- | The VPC ID associated with the DB snapshot.
awsRdsDbSnapshotDetails_vpcId :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_vpcId = Lens.lens (\AwsRdsDbSnapshotDetails' {vpcId} -> vpcId) (\s@AwsRdsDbSnapshotDetails' {} a -> s {vpcId = a} :: AwsRdsDbSnapshotDetails)

-- | Specifies the time in Coordinated Universal Time (UTC) when the DB
-- instance, from which the snapshot was taken, was created.
awsRdsDbSnapshotDetails_instanceCreateTime :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_instanceCreateTime = Lens.lens (\AwsRdsDbSnapshotDetails' {instanceCreateTime} -> instanceCreateTime) (\s@AwsRdsDbSnapshotDetails' {} a -> s {instanceCreateTime = a} :: AwsRdsDbSnapshotDetails)

-- | The name of the database engine to use for this DB instance.
awsRdsDbSnapshotDetails_engine :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_engine = Lens.lens (\AwsRdsDbSnapshotDetails' {engine} -> engine) (\s@AwsRdsDbSnapshotDetails' {} a -> s {engine = a} :: AwsRdsDbSnapshotDetails)

-- | Whether the DB snapshot is encrypted.
awsRdsDbSnapshotDetails_encrypted :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbSnapshotDetails_encrypted = Lens.lens (\AwsRdsDbSnapshotDetails' {encrypted} -> encrypted) (\s@AwsRdsDbSnapshotDetails' {} a -> s {encrypted = a} :: AwsRdsDbSnapshotDetails)

-- | The name or ARN of the DB snapshot that is used to restore the DB
-- instance.
awsRdsDbSnapshotDetails_dbSnapshotIdentifier :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_dbSnapshotIdentifier = Lens.lens (\AwsRdsDbSnapshotDetails' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@AwsRdsDbSnapshotDetails' {} a -> s {dbSnapshotIdentifier = a} :: AwsRdsDbSnapshotDetails)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
awsRdsDbSnapshotDetails_processorFeatures :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe [AwsRdsDbProcessorFeature])
awsRdsDbSnapshotDetails_processorFeatures = Lens.lens (\AwsRdsDbSnapshotDetails' {processorFeatures} -> processorFeatures) (\s@AwsRdsDbSnapshotDetails' {} a -> s {processorFeatures = a} :: AwsRdsDbSnapshotDetails) Prelude.. Lens.mapping Lens.coerced

-- | License model information for the restored DB instance.
awsRdsDbSnapshotDetails_licenseModel :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_licenseModel = Lens.lens (\AwsRdsDbSnapshotDetails' {licenseModel} -> licenseModel) (\s@AwsRdsDbSnapshotDetails' {} a -> s {licenseModel = a} :: AwsRdsDbSnapshotDetails)

-- | A name for the DB instance.
awsRdsDbSnapshotDetails_dbInstanceIdentifier :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_dbInstanceIdentifier = Lens.lens (\AwsRdsDbSnapshotDetails' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@AwsRdsDbSnapshotDetails' {} a -> s {dbInstanceIdentifier = a} :: AwsRdsDbSnapshotDetails)

-- | The type of the DB snapshot.
awsRdsDbSnapshotDetails_snapshotType :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_snapshotType = Lens.lens (\AwsRdsDbSnapshotDetails' {snapshotType} -> snapshotType) (\s@AwsRdsDbSnapshotDetails' {} a -> s {snapshotType = a} :: AwsRdsDbSnapshotDetails)

-- | The DB snapshot ARN that the DB snapshot was copied from.
awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier = Lens.lens (\AwsRdsDbSnapshotDetails' {sourceDbSnapshotIdentifier} -> sourceDbSnapshotIdentifier) (\s@AwsRdsDbSnapshotDetails' {} a -> s {sourceDbSnapshotIdentifier = a} :: AwsRdsDbSnapshotDetails)

-- | If @Encrypted@ is @true@, the KMS key identifier for the encrypted DB
-- snapshot.
awsRdsDbSnapshotDetails_kmsKeyId :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_kmsKeyId = Lens.lens (\AwsRdsDbSnapshotDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsRdsDbSnapshotDetails' {} a -> s {kmsKeyId = a} :: AwsRdsDbSnapshotDetails)

-- | Specifies the name of the Availability Zone in which the DB instance was
-- located at the time of the DB snapshot.
awsRdsDbSnapshotDetails_availabilityZone :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_availabilityZone = Lens.lens (\AwsRdsDbSnapshotDetails' {availabilityZone} -> availabilityZone) (\s@AwsRdsDbSnapshotDetails' {} a -> s {availabilityZone = a} :: AwsRdsDbSnapshotDetails)

-- | When the snapshot was taken in Coordinated Universal Time (UTC).
awsRdsDbSnapshotDetails_snapshotCreateTime :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_snapshotCreateTime = Lens.lens (\AwsRdsDbSnapshotDetails' {snapshotCreateTime} -> snapshotCreateTime) (\s@AwsRdsDbSnapshotDetails' {} a -> s {snapshotCreateTime = a} :: AwsRdsDbSnapshotDetails)

-- | The amount of storage (in gigabytes) to be initially allocated for the
-- database instance.
awsRdsDbSnapshotDetails_allocatedStorage :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_allocatedStorage = Lens.lens (\AwsRdsDbSnapshotDetails' {allocatedStorage} -> allocatedStorage) (\s@AwsRdsDbSnapshotDetails' {} a -> s {allocatedStorage = a} :: AwsRdsDbSnapshotDetails)

-- | The identifier for the source DB instance.
awsRdsDbSnapshotDetails_dbiResourceId :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_dbiResourceId = Lens.lens (\AwsRdsDbSnapshotDetails' {dbiResourceId} -> dbiResourceId) (\s@AwsRdsDbSnapshotDetails' {} a -> s {dbiResourceId = a} :: AwsRdsDbSnapshotDetails)

-- | The option group name for the DB snapshot.
awsRdsDbSnapshotDetails_optionGroupName :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_optionGroupName = Lens.lens (\AwsRdsDbSnapshotDetails' {optionGroupName} -> optionGroupName) (\s@AwsRdsDbSnapshotDetails' {} a -> s {optionGroupName = a} :: AwsRdsDbSnapshotDetails)

-- | The time zone of the DB snapshot.
awsRdsDbSnapshotDetails_timezone :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_timezone = Lens.lens (\AwsRdsDbSnapshotDetails' {timezone} -> timezone) (\s@AwsRdsDbSnapshotDetails' {} a -> s {timezone = a} :: AwsRdsDbSnapshotDetails)

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
awsRdsDbSnapshotDetails_tdeCredentialArn :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_tdeCredentialArn = Lens.lens (\AwsRdsDbSnapshotDetails' {tdeCredentialArn} -> tdeCredentialArn) (\s@AwsRdsDbSnapshotDetails' {} a -> s {tdeCredentialArn = a} :: AwsRdsDbSnapshotDetails)

-- | The percentage of the estimated data that has been transferred.
awsRdsDbSnapshotDetails_percentProgress :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_percentProgress = Lens.lens (\AwsRdsDbSnapshotDetails' {percentProgress} -> percentProgress) (\s@AwsRdsDbSnapshotDetails' {} a -> s {percentProgress = a} :: AwsRdsDbSnapshotDetails)

-- | The port that the database engine was listening on at the time of the
-- snapshot.
awsRdsDbSnapshotDetails_port :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_port = Lens.lens (\AwsRdsDbSnapshotDetails' {port} -> port) (\s@AwsRdsDbSnapshotDetails' {} a -> s {port = a} :: AwsRdsDbSnapshotDetails)

-- | The storage type associated with the DB snapshot.
awsRdsDbSnapshotDetails_storageType :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_storageType = Lens.lens (\AwsRdsDbSnapshotDetails' {storageType} -> storageType) (\s@AwsRdsDbSnapshotDetails' {} a -> s {storageType = a} :: AwsRdsDbSnapshotDetails)

instance Core.FromJSON AwsRdsDbSnapshotDetails where
  parseJSON =
    Core.withObject
      "AwsRdsDbSnapshotDetails"
      ( \x ->
          AwsRdsDbSnapshotDetails'
            Prelude.<$> (x Core..:? "EngineVersion")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "MasterUsername")
            Prelude.<*> (x Core..:? "SourceRegion")
            Prelude.<*> (x Core..:? "Iops")
            Prelude.<*> (x Core..:? "IamDatabaseAuthenticationEnabled")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "InstanceCreateTime")
            Prelude.<*> (x Core..:? "Engine")
            Prelude.<*> (x Core..:? "Encrypted")
            Prelude.<*> (x Core..:? "DbSnapshotIdentifier")
            Prelude.<*> ( x Core..:? "ProcessorFeatures"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LicenseModel")
            Prelude.<*> (x Core..:? "DbInstanceIdentifier")
            Prelude.<*> (x Core..:? "SnapshotType")
            Prelude.<*> (x Core..:? "SourceDbSnapshotIdentifier")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "SnapshotCreateTime")
            Prelude.<*> (x Core..:? "AllocatedStorage")
            Prelude.<*> (x Core..:? "DbiResourceId")
            Prelude.<*> (x Core..:? "OptionGroupName")
            Prelude.<*> (x Core..:? "Timezone")
            Prelude.<*> (x Core..:? "TdeCredentialArn")
            Prelude.<*> (x Core..:? "PercentProgress")
            Prelude.<*> (x Core..:? "Port")
            Prelude.<*> (x Core..:? "StorageType")
      )

instance Prelude.Hashable AwsRdsDbSnapshotDetails

instance Prelude.NFData AwsRdsDbSnapshotDetails

instance Core.ToJSON AwsRdsDbSnapshotDetails where
  toJSON AwsRdsDbSnapshotDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            ("Status" Core..=) Prelude.<$> status,
            ("MasterUsername" Core..=)
              Prelude.<$> masterUsername,
            ("SourceRegion" Core..=) Prelude.<$> sourceRegion,
            ("Iops" Core..=) Prelude.<$> iops,
            ("IamDatabaseAuthenticationEnabled" Core..=)
              Prelude.<$> iamDatabaseAuthenticationEnabled,
            ("VpcId" Core..=) Prelude.<$> vpcId,
            ("InstanceCreateTime" Core..=)
              Prelude.<$> instanceCreateTime,
            ("Engine" Core..=) Prelude.<$> engine,
            ("Encrypted" Core..=) Prelude.<$> encrypted,
            ("DbSnapshotIdentifier" Core..=)
              Prelude.<$> dbSnapshotIdentifier,
            ("ProcessorFeatures" Core..=)
              Prelude.<$> processorFeatures,
            ("LicenseModel" Core..=) Prelude.<$> licenseModel,
            ("DbInstanceIdentifier" Core..=)
              Prelude.<$> dbInstanceIdentifier,
            ("SnapshotType" Core..=) Prelude.<$> snapshotType,
            ("SourceDbSnapshotIdentifier" Core..=)
              Prelude.<$> sourceDbSnapshotIdentifier,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("AvailabilityZone" Core..=)
              Prelude.<$> availabilityZone,
            ("SnapshotCreateTime" Core..=)
              Prelude.<$> snapshotCreateTime,
            ("AllocatedStorage" Core..=)
              Prelude.<$> allocatedStorage,
            ("DbiResourceId" Core..=) Prelude.<$> dbiResourceId,
            ("OptionGroupName" Core..=)
              Prelude.<$> optionGroupName,
            ("Timezone" Core..=) Prelude.<$> timezone,
            ("TdeCredentialArn" Core..=)
              Prelude.<$> tdeCredentialArn,
            ("PercentProgress" Core..=)
              Prelude.<$> percentProgress,
            ("Port" Core..=) Prelude.<$> port,
            ("StorageType" Core..=) Prelude.<$> storageType
          ]
      )
