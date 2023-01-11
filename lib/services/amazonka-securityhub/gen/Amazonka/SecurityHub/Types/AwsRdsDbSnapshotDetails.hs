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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbSnapshotDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbSnapshotDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRdsDbProcessorFeature

-- | Provides details about an Amazon RDS DB cluster snapshot.
--
-- /See:/ 'newAwsRdsDbSnapshotDetails' smart constructor.
data AwsRdsDbSnapshotDetails = AwsRdsDbSnapshotDetails'
  { -- | The amount of storage (in gigabytes) to be initially allocated for the
    -- database instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Specifies the name of the Availability Zone in which the DB instance was
    -- located at the time of the DB snapshot.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | A name for the DB instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the DB snapshot that is used to restore the DB
    -- instance.
    dbSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the source DB instance.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | Whether the DB snapshot is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The name of the database engine to use for this DB instance. Valid
    -- values are as follows:
    --
    -- -   @aurora@
    --
    -- -   @aurora-mysql@
    --
    -- -   @aurora-postgresql@
    --
    -- -   @c@
    --
    -- -   @mariadb@
    --
    -- -   @mysql@
    --
    -- -   @oracle-ee@
    --
    -- -   @oracle-se@
    --
    -- -   @oracle-se1@
    --
    -- -   @oracle-se2@
    --
    -- -   @sqlserver-ee@
    --
    -- -   @sqlserver-ex@
    --
    -- -   @sqlserver-se@
    --
    -- -   @sqlserver-web@
    engine :: Prelude.Maybe Prelude.Text,
    -- | The version of the database engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Whether mapping of IAM accounts to database accounts is enabled.
    iamDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the time in Coordinated Universal Time (UTC) when the DB
    -- instance, from which the snapshot was taken, was created.
    instanceCreateTime :: Prelude.Maybe Prelude.Text,
    -- | The provisioned IOPS (I\/O operations per second) value of the DB
    -- instance at the time of the snapshot.
    iops :: Prelude.Maybe Prelude.Int,
    -- | If @Encrypted@ is @true@, the KMS key identifier for the encrypted DB
    -- snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | License model information for the restored DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The master user name for the DB snapshot.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The option group name for the DB snapshot.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The percentage of the estimated data that has been transferred.
    percentProgress :: Prelude.Maybe Prelude.Int,
    -- | The port that the database engine was listening on at the time of the
    -- snapshot.
    port :: Prelude.Maybe Prelude.Int,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [AwsRdsDbProcessorFeature],
    -- | When the snapshot was taken in Coordinated Universal Time (UTC).
    snapshotCreateTime :: Prelude.Maybe Prelude.Text,
    -- | The type of the DB snapshot.
    snapshotType :: Prelude.Maybe Prelude.Text,
    -- | The DB snapshot ARN that the DB snapshot was copied from.
    sourceDbSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region that the DB snapshot was created in or
    -- copied from.
    sourceRegion :: Prelude.Maybe Prelude.Text,
    -- | The status of this DB snapshot.
    status :: Prelude.Maybe Prelude.Text,
    -- | The storage type associated with the DB snapshot. Valid values are as
    -- follows:
    --
    -- -   @gp2@
    --
    -- -   @io1@
    --
    -- -   @standard@
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the DB snapshot.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The VPC ID associated with the DB snapshot.
    vpcId :: Prelude.Maybe Prelude.Text
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
-- 'allocatedStorage', 'awsRdsDbSnapshotDetails_allocatedStorage' - The amount of storage (in gigabytes) to be initially allocated for the
-- database instance.
--
-- 'availabilityZone', 'awsRdsDbSnapshotDetails_availabilityZone' - Specifies the name of the Availability Zone in which the DB instance was
-- located at the time of the DB snapshot.
--
-- 'dbInstanceIdentifier', 'awsRdsDbSnapshotDetails_dbInstanceIdentifier' - A name for the DB instance.
--
-- 'dbSnapshotIdentifier', 'awsRdsDbSnapshotDetails_dbSnapshotIdentifier' - The name or ARN of the DB snapshot that is used to restore the DB
-- instance.
--
-- 'dbiResourceId', 'awsRdsDbSnapshotDetails_dbiResourceId' - The identifier for the source DB instance.
--
-- 'encrypted', 'awsRdsDbSnapshotDetails_encrypted' - Whether the DB snapshot is encrypted.
--
-- 'engine', 'awsRdsDbSnapshotDetails_engine' - The name of the database engine to use for this DB instance. Valid
-- values are as follows:
--
-- -   @aurora@
--
-- -   @aurora-mysql@
--
-- -   @aurora-postgresql@
--
-- -   @c@
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-se@
--
-- -   @oracle-se1@
--
-- -   @oracle-se2@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-web@
--
-- 'engineVersion', 'awsRdsDbSnapshotDetails_engineVersion' - The version of the database engine.
--
-- 'iamDatabaseAuthenticationEnabled', 'awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled' - Whether mapping of IAM accounts to database accounts is enabled.
--
-- 'instanceCreateTime', 'awsRdsDbSnapshotDetails_instanceCreateTime' - Specifies the time in Coordinated Universal Time (UTC) when the DB
-- instance, from which the snapshot was taken, was created.
--
-- 'iops', 'awsRdsDbSnapshotDetails_iops' - The provisioned IOPS (I\/O operations per second) value of the DB
-- instance at the time of the snapshot.
--
-- 'kmsKeyId', 'awsRdsDbSnapshotDetails_kmsKeyId' - If @Encrypted@ is @true@, the KMS key identifier for the encrypted DB
-- snapshot.
--
-- 'licenseModel', 'awsRdsDbSnapshotDetails_licenseModel' - License model information for the restored DB instance.
--
-- 'masterUsername', 'awsRdsDbSnapshotDetails_masterUsername' - The master user name for the DB snapshot.
--
-- 'optionGroupName', 'awsRdsDbSnapshotDetails_optionGroupName' - The option group name for the DB snapshot.
--
-- 'percentProgress', 'awsRdsDbSnapshotDetails_percentProgress' - The percentage of the estimated data that has been transferred.
--
-- 'port', 'awsRdsDbSnapshotDetails_port' - The port that the database engine was listening on at the time of the
-- snapshot.
--
-- 'processorFeatures', 'awsRdsDbSnapshotDetails_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'snapshotCreateTime', 'awsRdsDbSnapshotDetails_snapshotCreateTime' - When the snapshot was taken in Coordinated Universal Time (UTC).
--
-- 'snapshotType', 'awsRdsDbSnapshotDetails_snapshotType' - The type of the DB snapshot.
--
-- 'sourceDbSnapshotIdentifier', 'awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier' - The DB snapshot ARN that the DB snapshot was copied from.
--
-- 'sourceRegion', 'awsRdsDbSnapshotDetails_sourceRegion' - The Amazon Web Services Region that the DB snapshot was created in or
-- copied from.
--
-- 'status', 'awsRdsDbSnapshotDetails_status' - The status of this DB snapshot.
--
-- 'storageType', 'awsRdsDbSnapshotDetails_storageType' - The storage type associated with the DB snapshot. Valid values are as
-- follows:
--
-- -   @gp2@
--
-- -   @io1@
--
-- -   @standard@
--
-- 'tdeCredentialArn', 'awsRdsDbSnapshotDetails_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- 'timezone', 'awsRdsDbSnapshotDetails_timezone' - The time zone of the DB snapshot.
--
-- 'vpcId', 'awsRdsDbSnapshotDetails_vpcId' - The VPC ID associated with the DB snapshot.
newAwsRdsDbSnapshotDetails ::
  AwsRdsDbSnapshotDetails
newAwsRdsDbSnapshotDetails =
  AwsRdsDbSnapshotDetails'
    { allocatedStorage =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      dbSnapshotIdentifier = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      iamDatabaseAuthenticationEnabled = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      iops = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      port = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      snapshotType = Prelude.Nothing,
      sourceDbSnapshotIdentifier = Prelude.Nothing,
      sourceRegion = Prelude.Nothing,
      status = Prelude.Nothing,
      storageType = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      timezone = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The amount of storage (in gigabytes) to be initially allocated for the
-- database instance.
awsRdsDbSnapshotDetails_allocatedStorage :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_allocatedStorage = Lens.lens (\AwsRdsDbSnapshotDetails' {allocatedStorage} -> allocatedStorage) (\s@AwsRdsDbSnapshotDetails' {} a -> s {allocatedStorage = a} :: AwsRdsDbSnapshotDetails)

-- | Specifies the name of the Availability Zone in which the DB instance was
-- located at the time of the DB snapshot.
awsRdsDbSnapshotDetails_availabilityZone :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_availabilityZone = Lens.lens (\AwsRdsDbSnapshotDetails' {availabilityZone} -> availabilityZone) (\s@AwsRdsDbSnapshotDetails' {} a -> s {availabilityZone = a} :: AwsRdsDbSnapshotDetails)

-- | A name for the DB instance.
awsRdsDbSnapshotDetails_dbInstanceIdentifier :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_dbInstanceIdentifier = Lens.lens (\AwsRdsDbSnapshotDetails' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@AwsRdsDbSnapshotDetails' {} a -> s {dbInstanceIdentifier = a} :: AwsRdsDbSnapshotDetails)

-- | The name or ARN of the DB snapshot that is used to restore the DB
-- instance.
awsRdsDbSnapshotDetails_dbSnapshotIdentifier :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_dbSnapshotIdentifier = Lens.lens (\AwsRdsDbSnapshotDetails' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@AwsRdsDbSnapshotDetails' {} a -> s {dbSnapshotIdentifier = a} :: AwsRdsDbSnapshotDetails)

-- | The identifier for the source DB instance.
awsRdsDbSnapshotDetails_dbiResourceId :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_dbiResourceId = Lens.lens (\AwsRdsDbSnapshotDetails' {dbiResourceId} -> dbiResourceId) (\s@AwsRdsDbSnapshotDetails' {} a -> s {dbiResourceId = a} :: AwsRdsDbSnapshotDetails)

-- | Whether the DB snapshot is encrypted.
awsRdsDbSnapshotDetails_encrypted :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbSnapshotDetails_encrypted = Lens.lens (\AwsRdsDbSnapshotDetails' {encrypted} -> encrypted) (\s@AwsRdsDbSnapshotDetails' {} a -> s {encrypted = a} :: AwsRdsDbSnapshotDetails)

-- | The name of the database engine to use for this DB instance. Valid
-- values are as follows:
--
-- -   @aurora@
--
-- -   @aurora-mysql@
--
-- -   @aurora-postgresql@
--
-- -   @c@
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-se@
--
-- -   @oracle-se1@
--
-- -   @oracle-se2@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-web@
awsRdsDbSnapshotDetails_engine :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_engine = Lens.lens (\AwsRdsDbSnapshotDetails' {engine} -> engine) (\s@AwsRdsDbSnapshotDetails' {} a -> s {engine = a} :: AwsRdsDbSnapshotDetails)

-- | The version of the database engine.
awsRdsDbSnapshotDetails_engineVersion :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_engineVersion = Lens.lens (\AwsRdsDbSnapshotDetails' {engineVersion} -> engineVersion) (\s@AwsRdsDbSnapshotDetails' {} a -> s {engineVersion = a} :: AwsRdsDbSnapshotDetails)

-- | Whether mapping of IAM accounts to database accounts is enabled.
awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled = Lens.lens (\AwsRdsDbSnapshotDetails' {iamDatabaseAuthenticationEnabled} -> iamDatabaseAuthenticationEnabled) (\s@AwsRdsDbSnapshotDetails' {} a -> s {iamDatabaseAuthenticationEnabled = a} :: AwsRdsDbSnapshotDetails)

-- | Specifies the time in Coordinated Universal Time (UTC) when the DB
-- instance, from which the snapshot was taken, was created.
awsRdsDbSnapshotDetails_instanceCreateTime :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_instanceCreateTime = Lens.lens (\AwsRdsDbSnapshotDetails' {instanceCreateTime} -> instanceCreateTime) (\s@AwsRdsDbSnapshotDetails' {} a -> s {instanceCreateTime = a} :: AwsRdsDbSnapshotDetails)

-- | The provisioned IOPS (I\/O operations per second) value of the DB
-- instance at the time of the snapshot.
awsRdsDbSnapshotDetails_iops :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_iops = Lens.lens (\AwsRdsDbSnapshotDetails' {iops} -> iops) (\s@AwsRdsDbSnapshotDetails' {} a -> s {iops = a} :: AwsRdsDbSnapshotDetails)

-- | If @Encrypted@ is @true@, the KMS key identifier for the encrypted DB
-- snapshot.
awsRdsDbSnapshotDetails_kmsKeyId :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_kmsKeyId = Lens.lens (\AwsRdsDbSnapshotDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsRdsDbSnapshotDetails' {} a -> s {kmsKeyId = a} :: AwsRdsDbSnapshotDetails)

-- | License model information for the restored DB instance.
awsRdsDbSnapshotDetails_licenseModel :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_licenseModel = Lens.lens (\AwsRdsDbSnapshotDetails' {licenseModel} -> licenseModel) (\s@AwsRdsDbSnapshotDetails' {} a -> s {licenseModel = a} :: AwsRdsDbSnapshotDetails)

-- | The master user name for the DB snapshot.
awsRdsDbSnapshotDetails_masterUsername :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_masterUsername = Lens.lens (\AwsRdsDbSnapshotDetails' {masterUsername} -> masterUsername) (\s@AwsRdsDbSnapshotDetails' {} a -> s {masterUsername = a} :: AwsRdsDbSnapshotDetails)

-- | The option group name for the DB snapshot.
awsRdsDbSnapshotDetails_optionGroupName :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_optionGroupName = Lens.lens (\AwsRdsDbSnapshotDetails' {optionGroupName} -> optionGroupName) (\s@AwsRdsDbSnapshotDetails' {} a -> s {optionGroupName = a} :: AwsRdsDbSnapshotDetails)

-- | The percentage of the estimated data that has been transferred.
awsRdsDbSnapshotDetails_percentProgress :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_percentProgress = Lens.lens (\AwsRdsDbSnapshotDetails' {percentProgress} -> percentProgress) (\s@AwsRdsDbSnapshotDetails' {} a -> s {percentProgress = a} :: AwsRdsDbSnapshotDetails)

-- | The port that the database engine was listening on at the time of the
-- snapshot.
awsRdsDbSnapshotDetails_port :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_port = Lens.lens (\AwsRdsDbSnapshotDetails' {port} -> port) (\s@AwsRdsDbSnapshotDetails' {} a -> s {port = a} :: AwsRdsDbSnapshotDetails)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
awsRdsDbSnapshotDetails_processorFeatures :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe [AwsRdsDbProcessorFeature])
awsRdsDbSnapshotDetails_processorFeatures = Lens.lens (\AwsRdsDbSnapshotDetails' {processorFeatures} -> processorFeatures) (\s@AwsRdsDbSnapshotDetails' {} a -> s {processorFeatures = a} :: AwsRdsDbSnapshotDetails) Prelude.. Lens.mapping Lens.coerced

-- | When the snapshot was taken in Coordinated Universal Time (UTC).
awsRdsDbSnapshotDetails_snapshotCreateTime :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_snapshotCreateTime = Lens.lens (\AwsRdsDbSnapshotDetails' {snapshotCreateTime} -> snapshotCreateTime) (\s@AwsRdsDbSnapshotDetails' {} a -> s {snapshotCreateTime = a} :: AwsRdsDbSnapshotDetails)

-- | The type of the DB snapshot.
awsRdsDbSnapshotDetails_snapshotType :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_snapshotType = Lens.lens (\AwsRdsDbSnapshotDetails' {snapshotType} -> snapshotType) (\s@AwsRdsDbSnapshotDetails' {} a -> s {snapshotType = a} :: AwsRdsDbSnapshotDetails)

-- | The DB snapshot ARN that the DB snapshot was copied from.
awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier = Lens.lens (\AwsRdsDbSnapshotDetails' {sourceDbSnapshotIdentifier} -> sourceDbSnapshotIdentifier) (\s@AwsRdsDbSnapshotDetails' {} a -> s {sourceDbSnapshotIdentifier = a} :: AwsRdsDbSnapshotDetails)

-- | The Amazon Web Services Region that the DB snapshot was created in or
-- copied from.
awsRdsDbSnapshotDetails_sourceRegion :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_sourceRegion = Lens.lens (\AwsRdsDbSnapshotDetails' {sourceRegion} -> sourceRegion) (\s@AwsRdsDbSnapshotDetails' {} a -> s {sourceRegion = a} :: AwsRdsDbSnapshotDetails)

-- | The status of this DB snapshot.
awsRdsDbSnapshotDetails_status :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_status = Lens.lens (\AwsRdsDbSnapshotDetails' {status} -> status) (\s@AwsRdsDbSnapshotDetails' {} a -> s {status = a} :: AwsRdsDbSnapshotDetails)

-- | The storage type associated with the DB snapshot. Valid values are as
-- follows:
--
-- -   @gp2@
--
-- -   @io1@
--
-- -   @standard@
awsRdsDbSnapshotDetails_storageType :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_storageType = Lens.lens (\AwsRdsDbSnapshotDetails' {storageType} -> storageType) (\s@AwsRdsDbSnapshotDetails' {} a -> s {storageType = a} :: AwsRdsDbSnapshotDetails)

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
awsRdsDbSnapshotDetails_tdeCredentialArn :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_tdeCredentialArn = Lens.lens (\AwsRdsDbSnapshotDetails' {tdeCredentialArn} -> tdeCredentialArn) (\s@AwsRdsDbSnapshotDetails' {} a -> s {tdeCredentialArn = a} :: AwsRdsDbSnapshotDetails)

-- | The time zone of the DB snapshot.
awsRdsDbSnapshotDetails_timezone :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_timezone = Lens.lens (\AwsRdsDbSnapshotDetails' {timezone} -> timezone) (\s@AwsRdsDbSnapshotDetails' {} a -> s {timezone = a} :: AwsRdsDbSnapshotDetails)

-- | The VPC ID associated with the DB snapshot.
awsRdsDbSnapshotDetails_vpcId :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_vpcId = Lens.lens (\AwsRdsDbSnapshotDetails' {vpcId} -> vpcId) (\s@AwsRdsDbSnapshotDetails' {} a -> s {vpcId = a} :: AwsRdsDbSnapshotDetails)

instance Data.FromJSON AwsRdsDbSnapshotDetails where
  parseJSON =
    Data.withObject
      "AwsRdsDbSnapshotDetails"
      ( \x ->
          AwsRdsDbSnapshotDetails'
            Prelude.<$> (x Data..:? "AllocatedStorage")
            Prelude.<*> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "DbInstanceIdentifier")
            Prelude.<*> (x Data..:? "DbSnapshotIdentifier")
            Prelude.<*> (x Data..:? "DbiResourceId")
            Prelude.<*> (x Data..:? "Encrypted")
            Prelude.<*> (x Data..:? "Engine")
            Prelude.<*> (x Data..:? "EngineVersion")
            Prelude.<*> (x Data..:? "IamDatabaseAuthenticationEnabled")
            Prelude.<*> (x Data..:? "InstanceCreateTime")
            Prelude.<*> (x Data..:? "Iops")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "LicenseModel")
            Prelude.<*> (x Data..:? "MasterUsername")
            Prelude.<*> (x Data..:? "OptionGroupName")
            Prelude.<*> (x Data..:? "PercentProgress")
            Prelude.<*> (x Data..:? "Port")
            Prelude.<*> ( x Data..:? "ProcessorFeatures"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SnapshotCreateTime")
            Prelude.<*> (x Data..:? "SnapshotType")
            Prelude.<*> (x Data..:? "SourceDbSnapshotIdentifier")
            Prelude.<*> (x Data..:? "SourceRegion")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StorageType")
            Prelude.<*> (x Data..:? "TdeCredentialArn")
            Prelude.<*> (x Data..:? "Timezone")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable AwsRdsDbSnapshotDetails where
  hashWithSalt _salt AwsRdsDbSnapshotDetails' {..} =
    _salt `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbSnapshotIdentifier
      `Prelude.hashWithSalt` dbiResourceId
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` iamDatabaseAuthenticationEnabled
      `Prelude.hashWithSalt` instanceCreateTime
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` percentProgress
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` snapshotCreateTime
      `Prelude.hashWithSalt` snapshotType
      `Prelude.hashWithSalt` sourceDbSnapshotIdentifier
      `Prelude.hashWithSalt` sourceRegion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tdeCredentialArn
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AwsRdsDbSnapshotDetails where
  rnf AwsRdsDbSnapshotDetails' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbSnapshotIdentifier
      `Prelude.seq` Prelude.rnf dbiResourceId
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf iamDatabaseAuthenticationEnabled
      `Prelude.seq` Prelude.rnf instanceCreateTime
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf percentProgress
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf processorFeatures
      `Prelude.seq` Prelude.rnf snapshotCreateTime
      `Prelude.seq` Prelude.rnf snapshotType
      `Prelude.seq` Prelude.rnf
        sourceDbSnapshotIdentifier
      `Prelude.seq` Prelude.rnf sourceRegion
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tdeCredentialArn
      `Prelude.seq` Prelude.rnf
        timezone
      `Prelude.seq` Prelude.rnf
        vpcId

instance Data.ToJSON AwsRdsDbSnapshotDetails where
  toJSON AwsRdsDbSnapshotDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllocatedStorage" Data..=)
              Prelude.<$> allocatedStorage,
            ("AvailabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("DbInstanceIdentifier" Data..=)
              Prelude.<$> dbInstanceIdentifier,
            ("DbSnapshotIdentifier" Data..=)
              Prelude.<$> dbSnapshotIdentifier,
            ("DbiResourceId" Data..=) Prelude.<$> dbiResourceId,
            ("Encrypted" Data..=) Prelude.<$> encrypted,
            ("Engine" Data..=) Prelude.<$> engine,
            ("EngineVersion" Data..=) Prelude.<$> engineVersion,
            ("IamDatabaseAuthenticationEnabled" Data..=)
              Prelude.<$> iamDatabaseAuthenticationEnabled,
            ("InstanceCreateTime" Data..=)
              Prelude.<$> instanceCreateTime,
            ("Iops" Data..=) Prelude.<$> iops,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("LicenseModel" Data..=) Prelude.<$> licenseModel,
            ("MasterUsername" Data..=)
              Prelude.<$> masterUsername,
            ("OptionGroupName" Data..=)
              Prelude.<$> optionGroupName,
            ("PercentProgress" Data..=)
              Prelude.<$> percentProgress,
            ("Port" Data..=) Prelude.<$> port,
            ("ProcessorFeatures" Data..=)
              Prelude.<$> processorFeatures,
            ("SnapshotCreateTime" Data..=)
              Prelude.<$> snapshotCreateTime,
            ("SnapshotType" Data..=) Prelude.<$> snapshotType,
            ("SourceDbSnapshotIdentifier" Data..=)
              Prelude.<$> sourceDbSnapshotIdentifier,
            ("SourceRegion" Data..=) Prelude.<$> sourceRegion,
            ("Status" Data..=) Prelude.<$> status,
            ("StorageType" Data..=) Prelude.<$> storageType,
            ("TdeCredentialArn" Data..=)
              Prelude.<$> tdeCredentialArn,
            ("Timezone" Data..=) Prelude.<$> timezone,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )
