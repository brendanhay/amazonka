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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbSnapshotDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRdsDbProcessorFeature

-- | Provides details about an Amazon RDS DB cluster snapshot.
--
-- /See:/ 'newAwsRdsDbSnapshotDetails' smart constructor.
data AwsRdsDbSnapshotDetails = AwsRdsDbSnapshotDetails'
  { -- | The port that the database engine was listening on at the time of the
    -- snapshot.
    port :: Prelude.Maybe Prelude.Int,
    -- | The percentage of the estimated data that has been transferred.
    percentProgress :: Prelude.Maybe Prelude.Int,
    -- | The master user name for the DB snapshot.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region that the DB snapshot was created in or
    -- copied from.
    sourceRegion :: Prelude.Maybe Prelude.Text,
    -- | A name for the DB instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the DB snapshot that is used to restore the DB
    -- instance.
    dbSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The DB snapshot ARN that the DB snapshot was copied from.
    sourceDbSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The option group name for the DB snapshot.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the DB snapshot.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time in Coordinated Universal Time (UTC) when the DB
    -- instance, from which the snapshot was taken, was created.
    instanceCreateTime :: Prelude.Maybe Prelude.Text,
    -- | The status of this DB snapshot.
    status :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the Availability Zone in which the DB instance was
    -- located at the time of the DB snapshot.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | When the snapshot was taken in Coordinated Universal Time (UTC).
    snapshotCreateTime :: Prelude.Maybe Prelude.Text,
    -- | The storage type associated with the DB snapshot. Valid values are as
    -- follows:
    --
    -- -   @gp2@
    --
    -- -   @io1@
    --
    -- -   @standard@
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [AwsRdsDbProcessorFeature],
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the DB snapshot is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | If @Encrypted@ is @true@, the KMS key identifier for the encrypted DB
    -- snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
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
    -- | The amount of storage (in gigabytes) to be initially allocated for the
    -- database instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Whether mapping of IAM accounts to database accounts is enabled.
    iamDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The VPC ID associated with the DB snapshot.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the source DB instance.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | The provisioned IOPS (I\/O operations per second) value of the DB
    -- instance at the time of the snapshot.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The version of the database engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | License model information for the restored DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The type of the DB snapshot.
    snapshotType :: Prelude.Maybe Prelude.Text
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
-- 'port', 'awsRdsDbSnapshotDetails_port' - The port that the database engine was listening on at the time of the
-- snapshot.
--
-- 'percentProgress', 'awsRdsDbSnapshotDetails_percentProgress' - The percentage of the estimated data that has been transferred.
--
-- 'masterUsername', 'awsRdsDbSnapshotDetails_masterUsername' - The master user name for the DB snapshot.
--
-- 'sourceRegion', 'awsRdsDbSnapshotDetails_sourceRegion' - The Amazon Web Services Region that the DB snapshot was created in or
-- copied from.
--
-- 'dbInstanceIdentifier', 'awsRdsDbSnapshotDetails_dbInstanceIdentifier' - A name for the DB instance.
--
-- 'dbSnapshotIdentifier', 'awsRdsDbSnapshotDetails_dbSnapshotIdentifier' - The name or ARN of the DB snapshot that is used to restore the DB
-- instance.
--
-- 'sourceDbSnapshotIdentifier', 'awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier' - The DB snapshot ARN that the DB snapshot was copied from.
--
-- 'optionGroupName', 'awsRdsDbSnapshotDetails_optionGroupName' - The option group name for the DB snapshot.
--
-- 'timezone', 'awsRdsDbSnapshotDetails_timezone' - The time zone of the DB snapshot.
--
-- 'instanceCreateTime', 'awsRdsDbSnapshotDetails_instanceCreateTime' - Specifies the time in Coordinated Universal Time (UTC) when the DB
-- instance, from which the snapshot was taken, was created.
--
-- 'status', 'awsRdsDbSnapshotDetails_status' - The status of this DB snapshot.
--
-- 'availabilityZone', 'awsRdsDbSnapshotDetails_availabilityZone' - Specifies the name of the Availability Zone in which the DB instance was
-- located at the time of the DB snapshot.
--
-- 'snapshotCreateTime', 'awsRdsDbSnapshotDetails_snapshotCreateTime' - When the snapshot was taken in Coordinated Universal Time (UTC).
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
-- 'processorFeatures', 'awsRdsDbSnapshotDetails_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'tdeCredentialArn', 'awsRdsDbSnapshotDetails_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- 'encrypted', 'awsRdsDbSnapshotDetails_encrypted' - Whether the DB snapshot is encrypted.
--
-- 'kmsKeyId', 'awsRdsDbSnapshotDetails_kmsKeyId' - If @Encrypted@ is @true@, the KMS key identifier for the encrypted DB
-- snapshot.
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
-- 'allocatedStorage', 'awsRdsDbSnapshotDetails_allocatedStorage' - The amount of storage (in gigabytes) to be initially allocated for the
-- database instance.
--
-- 'iamDatabaseAuthenticationEnabled', 'awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled' - Whether mapping of IAM accounts to database accounts is enabled.
--
-- 'vpcId', 'awsRdsDbSnapshotDetails_vpcId' - The VPC ID associated with the DB snapshot.
--
-- 'dbiResourceId', 'awsRdsDbSnapshotDetails_dbiResourceId' - The identifier for the source DB instance.
--
-- 'iops', 'awsRdsDbSnapshotDetails_iops' - The provisioned IOPS (I\/O operations per second) value of the DB
-- instance at the time of the snapshot.
--
-- 'engineVersion', 'awsRdsDbSnapshotDetails_engineVersion' - The version of the database engine.
--
-- 'licenseModel', 'awsRdsDbSnapshotDetails_licenseModel' - License model information for the restored DB instance.
--
-- 'snapshotType', 'awsRdsDbSnapshotDetails_snapshotType' - The type of the DB snapshot.
newAwsRdsDbSnapshotDetails ::
  AwsRdsDbSnapshotDetails
newAwsRdsDbSnapshotDetails =
  AwsRdsDbSnapshotDetails'
    { port = Prelude.Nothing,
      percentProgress = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      sourceRegion = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      dbSnapshotIdentifier = Prelude.Nothing,
      sourceDbSnapshotIdentifier = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      timezone = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      status = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      storageType = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      engine = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      iamDatabaseAuthenticationEnabled = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      iops = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      snapshotType = Prelude.Nothing
    }

-- | The port that the database engine was listening on at the time of the
-- snapshot.
awsRdsDbSnapshotDetails_port :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_port = Lens.lens (\AwsRdsDbSnapshotDetails' {port} -> port) (\s@AwsRdsDbSnapshotDetails' {} a -> s {port = a} :: AwsRdsDbSnapshotDetails)

-- | The percentage of the estimated data that has been transferred.
awsRdsDbSnapshotDetails_percentProgress :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_percentProgress = Lens.lens (\AwsRdsDbSnapshotDetails' {percentProgress} -> percentProgress) (\s@AwsRdsDbSnapshotDetails' {} a -> s {percentProgress = a} :: AwsRdsDbSnapshotDetails)

-- | The master user name for the DB snapshot.
awsRdsDbSnapshotDetails_masterUsername :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_masterUsername = Lens.lens (\AwsRdsDbSnapshotDetails' {masterUsername} -> masterUsername) (\s@AwsRdsDbSnapshotDetails' {} a -> s {masterUsername = a} :: AwsRdsDbSnapshotDetails)

-- | The Amazon Web Services Region that the DB snapshot was created in or
-- copied from.
awsRdsDbSnapshotDetails_sourceRegion :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_sourceRegion = Lens.lens (\AwsRdsDbSnapshotDetails' {sourceRegion} -> sourceRegion) (\s@AwsRdsDbSnapshotDetails' {} a -> s {sourceRegion = a} :: AwsRdsDbSnapshotDetails)

-- | A name for the DB instance.
awsRdsDbSnapshotDetails_dbInstanceIdentifier :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_dbInstanceIdentifier = Lens.lens (\AwsRdsDbSnapshotDetails' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@AwsRdsDbSnapshotDetails' {} a -> s {dbInstanceIdentifier = a} :: AwsRdsDbSnapshotDetails)

-- | The name or ARN of the DB snapshot that is used to restore the DB
-- instance.
awsRdsDbSnapshotDetails_dbSnapshotIdentifier :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_dbSnapshotIdentifier = Lens.lens (\AwsRdsDbSnapshotDetails' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@AwsRdsDbSnapshotDetails' {} a -> s {dbSnapshotIdentifier = a} :: AwsRdsDbSnapshotDetails)

-- | The DB snapshot ARN that the DB snapshot was copied from.
awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier = Lens.lens (\AwsRdsDbSnapshotDetails' {sourceDbSnapshotIdentifier} -> sourceDbSnapshotIdentifier) (\s@AwsRdsDbSnapshotDetails' {} a -> s {sourceDbSnapshotIdentifier = a} :: AwsRdsDbSnapshotDetails)

-- | The option group name for the DB snapshot.
awsRdsDbSnapshotDetails_optionGroupName :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_optionGroupName = Lens.lens (\AwsRdsDbSnapshotDetails' {optionGroupName} -> optionGroupName) (\s@AwsRdsDbSnapshotDetails' {} a -> s {optionGroupName = a} :: AwsRdsDbSnapshotDetails)

-- | The time zone of the DB snapshot.
awsRdsDbSnapshotDetails_timezone :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_timezone = Lens.lens (\AwsRdsDbSnapshotDetails' {timezone} -> timezone) (\s@AwsRdsDbSnapshotDetails' {} a -> s {timezone = a} :: AwsRdsDbSnapshotDetails)

-- | Specifies the time in Coordinated Universal Time (UTC) when the DB
-- instance, from which the snapshot was taken, was created.
awsRdsDbSnapshotDetails_instanceCreateTime :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_instanceCreateTime = Lens.lens (\AwsRdsDbSnapshotDetails' {instanceCreateTime} -> instanceCreateTime) (\s@AwsRdsDbSnapshotDetails' {} a -> s {instanceCreateTime = a} :: AwsRdsDbSnapshotDetails)

-- | The status of this DB snapshot.
awsRdsDbSnapshotDetails_status :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_status = Lens.lens (\AwsRdsDbSnapshotDetails' {status} -> status) (\s@AwsRdsDbSnapshotDetails' {} a -> s {status = a} :: AwsRdsDbSnapshotDetails)

-- | Specifies the name of the Availability Zone in which the DB instance was
-- located at the time of the DB snapshot.
awsRdsDbSnapshotDetails_availabilityZone :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_availabilityZone = Lens.lens (\AwsRdsDbSnapshotDetails' {availabilityZone} -> availabilityZone) (\s@AwsRdsDbSnapshotDetails' {} a -> s {availabilityZone = a} :: AwsRdsDbSnapshotDetails)

-- | When the snapshot was taken in Coordinated Universal Time (UTC).
awsRdsDbSnapshotDetails_snapshotCreateTime :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_snapshotCreateTime = Lens.lens (\AwsRdsDbSnapshotDetails' {snapshotCreateTime} -> snapshotCreateTime) (\s@AwsRdsDbSnapshotDetails' {} a -> s {snapshotCreateTime = a} :: AwsRdsDbSnapshotDetails)

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

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
awsRdsDbSnapshotDetails_processorFeatures :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe [AwsRdsDbProcessorFeature])
awsRdsDbSnapshotDetails_processorFeatures = Lens.lens (\AwsRdsDbSnapshotDetails' {processorFeatures} -> processorFeatures) (\s@AwsRdsDbSnapshotDetails' {} a -> s {processorFeatures = a} :: AwsRdsDbSnapshotDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
awsRdsDbSnapshotDetails_tdeCredentialArn :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_tdeCredentialArn = Lens.lens (\AwsRdsDbSnapshotDetails' {tdeCredentialArn} -> tdeCredentialArn) (\s@AwsRdsDbSnapshotDetails' {} a -> s {tdeCredentialArn = a} :: AwsRdsDbSnapshotDetails)

-- | Whether the DB snapshot is encrypted.
awsRdsDbSnapshotDetails_encrypted :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbSnapshotDetails_encrypted = Lens.lens (\AwsRdsDbSnapshotDetails' {encrypted} -> encrypted) (\s@AwsRdsDbSnapshotDetails' {} a -> s {encrypted = a} :: AwsRdsDbSnapshotDetails)

-- | If @Encrypted@ is @true@, the KMS key identifier for the encrypted DB
-- snapshot.
awsRdsDbSnapshotDetails_kmsKeyId :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_kmsKeyId = Lens.lens (\AwsRdsDbSnapshotDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsRdsDbSnapshotDetails' {} a -> s {kmsKeyId = a} :: AwsRdsDbSnapshotDetails)

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

-- | The amount of storage (in gigabytes) to be initially allocated for the
-- database instance.
awsRdsDbSnapshotDetails_allocatedStorage :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_allocatedStorage = Lens.lens (\AwsRdsDbSnapshotDetails' {allocatedStorage} -> allocatedStorage) (\s@AwsRdsDbSnapshotDetails' {} a -> s {allocatedStorage = a} :: AwsRdsDbSnapshotDetails)

-- | Whether mapping of IAM accounts to database accounts is enabled.
awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled = Lens.lens (\AwsRdsDbSnapshotDetails' {iamDatabaseAuthenticationEnabled} -> iamDatabaseAuthenticationEnabled) (\s@AwsRdsDbSnapshotDetails' {} a -> s {iamDatabaseAuthenticationEnabled = a} :: AwsRdsDbSnapshotDetails)

-- | The VPC ID associated with the DB snapshot.
awsRdsDbSnapshotDetails_vpcId :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_vpcId = Lens.lens (\AwsRdsDbSnapshotDetails' {vpcId} -> vpcId) (\s@AwsRdsDbSnapshotDetails' {} a -> s {vpcId = a} :: AwsRdsDbSnapshotDetails)

-- | The identifier for the source DB instance.
awsRdsDbSnapshotDetails_dbiResourceId :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_dbiResourceId = Lens.lens (\AwsRdsDbSnapshotDetails' {dbiResourceId} -> dbiResourceId) (\s@AwsRdsDbSnapshotDetails' {} a -> s {dbiResourceId = a} :: AwsRdsDbSnapshotDetails)

-- | The provisioned IOPS (I\/O operations per second) value of the DB
-- instance at the time of the snapshot.
awsRdsDbSnapshotDetails_iops :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Int)
awsRdsDbSnapshotDetails_iops = Lens.lens (\AwsRdsDbSnapshotDetails' {iops} -> iops) (\s@AwsRdsDbSnapshotDetails' {} a -> s {iops = a} :: AwsRdsDbSnapshotDetails)

-- | The version of the database engine.
awsRdsDbSnapshotDetails_engineVersion :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_engineVersion = Lens.lens (\AwsRdsDbSnapshotDetails' {engineVersion} -> engineVersion) (\s@AwsRdsDbSnapshotDetails' {} a -> s {engineVersion = a} :: AwsRdsDbSnapshotDetails)

-- | License model information for the restored DB instance.
awsRdsDbSnapshotDetails_licenseModel :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_licenseModel = Lens.lens (\AwsRdsDbSnapshotDetails' {licenseModel} -> licenseModel) (\s@AwsRdsDbSnapshotDetails' {} a -> s {licenseModel = a} :: AwsRdsDbSnapshotDetails)

-- | The type of the DB snapshot.
awsRdsDbSnapshotDetails_snapshotType :: Lens.Lens' AwsRdsDbSnapshotDetails (Prelude.Maybe Prelude.Text)
awsRdsDbSnapshotDetails_snapshotType = Lens.lens (\AwsRdsDbSnapshotDetails' {snapshotType} -> snapshotType) (\s@AwsRdsDbSnapshotDetails' {} a -> s {snapshotType = a} :: AwsRdsDbSnapshotDetails)

instance Core.FromJSON AwsRdsDbSnapshotDetails where
  parseJSON =
    Core.withObject
      "AwsRdsDbSnapshotDetails"
      ( \x ->
          AwsRdsDbSnapshotDetails'
            Prelude.<$> (x Core..:? "Port")
            Prelude.<*> (x Core..:? "PercentProgress")
            Prelude.<*> (x Core..:? "MasterUsername")
            Prelude.<*> (x Core..:? "SourceRegion")
            Prelude.<*> (x Core..:? "DbInstanceIdentifier")
            Prelude.<*> (x Core..:? "DbSnapshotIdentifier")
            Prelude.<*> (x Core..:? "SourceDbSnapshotIdentifier")
            Prelude.<*> (x Core..:? "OptionGroupName")
            Prelude.<*> (x Core..:? "Timezone")
            Prelude.<*> (x Core..:? "InstanceCreateTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "SnapshotCreateTime")
            Prelude.<*> (x Core..:? "StorageType")
            Prelude.<*> ( x Core..:? "ProcessorFeatures"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "TdeCredentialArn")
            Prelude.<*> (x Core..:? "Encrypted")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "Engine")
            Prelude.<*> (x Core..:? "AllocatedStorage")
            Prelude.<*> (x Core..:? "IamDatabaseAuthenticationEnabled")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "DbiResourceId")
            Prelude.<*> (x Core..:? "Iops")
            Prelude.<*> (x Core..:? "EngineVersion")
            Prelude.<*> (x Core..:? "LicenseModel")
            Prelude.<*> (x Core..:? "SnapshotType")
      )

instance Prelude.Hashable AwsRdsDbSnapshotDetails where
  hashWithSalt _salt AwsRdsDbSnapshotDetails' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` percentProgress
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` sourceRegion
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbSnapshotIdentifier
      `Prelude.hashWithSalt` sourceDbSnapshotIdentifier
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` instanceCreateTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` snapshotCreateTime
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` tdeCredentialArn
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` iamDatabaseAuthenticationEnabled
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` dbiResourceId
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` snapshotType

instance Prelude.NFData AwsRdsDbSnapshotDetails where
  rnf AwsRdsDbSnapshotDetails' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf percentProgress
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf sourceRegion
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbSnapshotIdentifier
      `Prelude.seq` Prelude.rnf sourceDbSnapshotIdentifier
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf instanceCreateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf snapshotCreateTime
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf processorFeatures
      `Prelude.seq` Prelude.rnf tdeCredentialArn
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf
        iamDatabaseAuthenticationEnabled
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf
        dbiResourceId
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        licenseModel
      `Prelude.seq` Prelude.rnf
        snapshotType

instance Core.ToJSON AwsRdsDbSnapshotDetails where
  toJSON AwsRdsDbSnapshotDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Port" Core..=) Prelude.<$> port,
            ("PercentProgress" Core..=)
              Prelude.<$> percentProgress,
            ("MasterUsername" Core..=)
              Prelude.<$> masterUsername,
            ("SourceRegion" Core..=) Prelude.<$> sourceRegion,
            ("DbInstanceIdentifier" Core..=)
              Prelude.<$> dbInstanceIdentifier,
            ("DbSnapshotIdentifier" Core..=)
              Prelude.<$> dbSnapshotIdentifier,
            ("SourceDbSnapshotIdentifier" Core..=)
              Prelude.<$> sourceDbSnapshotIdentifier,
            ("OptionGroupName" Core..=)
              Prelude.<$> optionGroupName,
            ("Timezone" Core..=) Prelude.<$> timezone,
            ("InstanceCreateTime" Core..=)
              Prelude.<$> instanceCreateTime,
            ("Status" Core..=) Prelude.<$> status,
            ("AvailabilityZone" Core..=)
              Prelude.<$> availabilityZone,
            ("SnapshotCreateTime" Core..=)
              Prelude.<$> snapshotCreateTime,
            ("StorageType" Core..=) Prelude.<$> storageType,
            ("ProcessorFeatures" Core..=)
              Prelude.<$> processorFeatures,
            ("TdeCredentialArn" Core..=)
              Prelude.<$> tdeCredentialArn,
            ("Encrypted" Core..=) Prelude.<$> encrypted,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("Engine" Core..=) Prelude.<$> engine,
            ("AllocatedStorage" Core..=)
              Prelude.<$> allocatedStorage,
            ("IamDatabaseAuthenticationEnabled" Core..=)
              Prelude.<$> iamDatabaseAuthenticationEnabled,
            ("VpcId" Core..=) Prelude.<$> vpcId,
            ("DbiResourceId" Core..=) Prelude.<$> dbiResourceId,
            ("Iops" Core..=) Prelude.<$> iops,
            ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            ("LicenseModel" Core..=) Prelude.<$> licenseModel,
            ("SnapshotType" Core..=) Prelude.<$> snapshotType
          ]
      )
