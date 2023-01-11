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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters for a block device for an Amazon Elastic Block Store (Amazon
-- EBS) volume in an Amazon EC2 launch template.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' smart constructor.
data AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails = AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails'
  { -- | Indicates whether the EBS volume is deleted on instance termination.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the EBS volume is encrypted. Encrypted volumes can
    -- only be attached to instances that support Amazon EBS encryption. If
    -- you\'re creating a volume from a snapshot, you can\'t specify an
    -- encryption value.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The number of I\/O operations per second (IOPS).
    iops :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the symmetric Key Management Service
    -- (KMS) customer managed key used for encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the EBS snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The throughput to provision for a gp3 volume, with a maximum of 1,000
    -- MiB\/s.
    throughput :: Prelude.Maybe Prelude.Int,
    -- | The size of the volume, in GiBs. You must specify either a snapshot ID
    -- or a volume size.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | The volume type.
    volumeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteOnTermination', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_deleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
--
-- 'encrypted', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_encrypted' - Indicates whether the EBS volume is encrypted. Encrypted volumes can
-- only be attached to instances that support Amazon EBS encryption. If
-- you\'re creating a volume from a snapshot, you can\'t specify an
-- encryption value.
--
-- 'iops', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_iops' - The number of I\/O operations per second (IOPS).
--
-- 'kmsKeyId', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_kmsKeyId' - The Amazon Resource Name (ARN) of the symmetric Key Management Service
-- (KMS) customer managed key used for encryption.
--
-- 'snapshotId', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_snapshotId' - The ID of the EBS snapshot.
--
-- 'throughput', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_throughput' - The throughput to provision for a gp3 volume, with a maximum of 1,000
-- MiB\/s.
--
-- 'volumeSize', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_volumeSize' - The size of the volume, in GiBs. You must specify either a snapshot ID
-- or a volume size.
--
-- 'volumeType', 'awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_volumeType' - The volume type.
newAwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails ::
  AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails
newAwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails =
  AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails'
    { deleteOnTermination =
        Prelude.Nothing,
      encrypted =
        Prelude.Nothing,
      iops =
        Prelude.Nothing,
      kmsKeyId =
        Prelude.Nothing,
      snapshotId =
        Prelude.Nothing,
      throughput =
        Prelude.Nothing,
      volumeSize =
        Prelude.Nothing,
      volumeType =
        Prelude.Nothing
    }

-- | Indicates whether the EBS volume is deleted on instance termination.
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_deleteOnTermination :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_deleteOnTermination = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {deleteOnTermination} -> deleteOnTermination) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {} a -> s {deleteOnTermination = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails)

-- | Indicates whether the EBS volume is encrypted. Encrypted volumes can
-- only be attached to instances that support Amazon EBS encryption. If
-- you\'re creating a volume from a snapshot, you can\'t specify an
-- encryption value.
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_encrypted :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_encrypted = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {encrypted} -> encrypted) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {} a -> s {encrypted = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails)

-- | The number of I\/O operations per second (IOPS).
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_iops :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_iops = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {iops} -> iops) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {} a -> s {iops = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails)

-- | The Amazon Resource Name (ARN) of the symmetric Key Management Service
-- (KMS) customer managed key used for encryption.
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_kmsKeyId :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_kmsKeyId = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {} a -> s {kmsKeyId = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails)

-- | The ID of the EBS snapshot.
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_snapshotId :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_snapshotId = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {snapshotId} -> snapshotId) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {} a -> s {snapshotId = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails)

-- | The throughput to provision for a gp3 volume, with a maximum of 1,000
-- MiB\/s.
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_throughput :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_throughput = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {throughput} -> throughput) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {} a -> s {throughput = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails)

-- | The size of the volume, in GiBs. You must specify either a snapshot ID
-- or a volume size.
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_volumeSize :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_volumeSize = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {volumeSize} -> volumeSize) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {} a -> s {volumeSize = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails)

-- | The volume type.
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_volumeType :: Lens.Lens' AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_volumeType = Lens.lens (\AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {volumeType} -> volumeType) (\s@AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {} a -> s {volumeType = a} :: AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails'
            Prelude.<$> (x Data..:? "DeleteOnTermination")
              Prelude.<*> (x Data..:? "Encrypted")
              Prelude.<*> (x Data..:? "Iops")
              Prelude.<*> (x Data..:? "KmsKeyId")
              Prelude.<*> (x Data..:? "SnapshotId")
              Prelude.<*> (x Data..:? "Throughput")
              Prelude.<*> (x Data..:? "VolumeSize")
              Prelude.<*> (x Data..:? "VolumeType")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {..} =
      _salt `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` encrypted
        `Prelude.hashWithSalt` iops
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` snapshotId
        `Prelude.hashWithSalt` throughput
        `Prelude.hashWithSalt` volumeSize
        `Prelude.hashWithSalt` volumeType

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails
  where
  rnf
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {..} =
      Prelude.rnf deleteOnTermination
        `Prelude.seq` Prelude.rnf encrypted
        `Prelude.seq` Prelude.rnf iops
        `Prelude.seq` Prelude.rnf kmsKeyId
        `Prelude.seq` Prelude.rnf snapshotId
        `Prelude.seq` Prelude.rnf throughput
        `Prelude.seq` Prelude.rnf volumeSize
        `Prelude.seq` Prelude.rnf volumeType

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DeleteOnTermination" Data..=)
                Prelude.<$> deleteOnTermination,
              ("Encrypted" Data..=) Prelude.<$> encrypted,
              ("Iops" Data..=) Prelude.<$> iops,
              ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
              ("SnapshotId" Data..=) Prelude.<$> snapshotId,
              ("Throughput" Data..=) Prelude.<$> throughput,
              ("VolumeSize" Data..=) Prelude.<$> volumeSize,
              ("VolumeType" Data..=) Prelude.<$> volumeType
            ]
        )
