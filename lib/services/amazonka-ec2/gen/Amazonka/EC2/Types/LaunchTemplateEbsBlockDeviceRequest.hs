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
-- Module      : Amazonka.EC2.Types.LaunchTemplateEbsBlockDeviceRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateEbsBlockDeviceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VolumeType
import qualified Amazonka.Prelude as Prelude

-- | The parameters for a block device for an EBS volume.
--
-- /See:/ 'newLaunchTemplateEbsBlockDeviceRequest' smart constructor.
data LaunchTemplateEbsBlockDeviceRequest = LaunchTemplateEbsBlockDeviceRequest'
  { -- | Indicates whether the EBS volume is deleted on instance termination.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the EBS volume is encrypted. Encrypted volumes can
    -- only be attached to instances that support Amazon EBS encryption. If you
    -- are creating a volume from a snapshot, you can\'t specify an encryption
    -- value.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
    -- @io2@ volumes, this represents the number of IOPS that are provisioned
    -- for the volume. For @gp2@ volumes, this represents the baseline
    -- performance of the volume and the rate at which the volume accumulates
    -- I\/O credits for bursting.
    --
    -- The following are the supported values for each volume type:
    --
    -- -   @gp3@: 3,000-16,000 IOPS
    --
    -- -   @io1@: 100-64,000 IOPS
    --
    -- -   @io2@: 100-64,000 IOPS
    --
    -- For @io1@ and @io2@ volumes, we guarantee 64,000 IOPS only for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>.
    -- Other instance families guarantee performance up to 32,000 IOPS.
    --
    -- This parameter is supported for @io1@, @io2@, and @gp3@ volumes only.
    -- This parameter is not supported for @gp2@, @st1@, @sc1@, or @standard@
    -- volumes.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the symmetric Key Management Service (KMS) CMK used for
    -- encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The throughput to provision for a @gp3@ volume, with a maximum of 1,000
    -- MiB\/s.
    --
    -- Valid Range: Minimum value of 125. Maximum value of 1000.
    throughput :: Prelude.Maybe Prelude.Int,
    -- | The size of the volume, in GiBs. You must specify either a snapshot ID
    -- or a volume size. The following are the supported volumes sizes for each
    -- volume type:
    --
    -- -   @gp2@ and @gp3@: 1-16,384
    --
    -- -   @io1@ and @io2@: 4-16,384
    --
    -- -   @st1@ and @sc1@: 125-16,384
    --
    -- -   @standard@: 1-1,024
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | The volume type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    volumeType :: Prelude.Maybe VolumeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateEbsBlockDeviceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteOnTermination', 'launchTemplateEbsBlockDeviceRequest_deleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
--
-- 'encrypted', 'launchTemplateEbsBlockDeviceRequest_encrypted' - Indicates whether the EBS volume is encrypted. Encrypted volumes can
-- only be attached to instances that support Amazon EBS encryption. If you
-- are creating a volume from a snapshot, you can\'t specify an encryption
-- value.
--
-- 'iops', 'launchTemplateEbsBlockDeviceRequest_iops' - The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
-- @io2@ volumes, this represents the number of IOPS that are provisioned
-- for the volume. For @gp2@ volumes, this represents the baseline
-- performance of the volume and the rate at which the volume accumulates
-- I\/O credits for bursting.
--
-- The following are the supported values for each volume type:
--
-- -   @gp3@: 3,000-16,000 IOPS
--
-- -   @io1@: 100-64,000 IOPS
--
-- -   @io2@: 100-64,000 IOPS
--
-- For @io1@ and @io2@ volumes, we guarantee 64,000 IOPS only for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>.
-- Other instance families guarantee performance up to 32,000 IOPS.
--
-- This parameter is supported for @io1@, @io2@, and @gp3@ volumes only.
-- This parameter is not supported for @gp2@, @st1@, @sc1@, or @standard@
-- volumes.
--
-- 'kmsKeyId', 'launchTemplateEbsBlockDeviceRequest_kmsKeyId' - The ARN of the symmetric Key Management Service (KMS) CMK used for
-- encryption.
--
-- 'snapshotId', 'launchTemplateEbsBlockDeviceRequest_snapshotId' - The ID of the snapshot.
--
-- 'throughput', 'launchTemplateEbsBlockDeviceRequest_throughput' - The throughput to provision for a @gp3@ volume, with a maximum of 1,000
-- MiB\/s.
--
-- Valid Range: Minimum value of 125. Maximum value of 1000.
--
-- 'volumeSize', 'launchTemplateEbsBlockDeviceRequest_volumeSize' - The size of the volume, in GiBs. You must specify either a snapshot ID
-- or a volume size. The following are the supported volumes sizes for each
-- volume type:
--
-- -   @gp2@ and @gp3@: 1-16,384
--
-- -   @io1@ and @io2@: 4-16,384
--
-- -   @st1@ and @sc1@: 125-16,384
--
-- -   @standard@: 1-1,024
--
-- 'volumeType', 'launchTemplateEbsBlockDeviceRequest_volumeType' - The volume type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
newLaunchTemplateEbsBlockDeviceRequest ::
  LaunchTemplateEbsBlockDeviceRequest
newLaunchTemplateEbsBlockDeviceRequest =
  LaunchTemplateEbsBlockDeviceRequest'
    { deleteOnTermination =
        Prelude.Nothing,
      encrypted = Prelude.Nothing,
      iops = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      throughput = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | Indicates whether the EBS volume is deleted on instance termination.
launchTemplateEbsBlockDeviceRequest_deleteOnTermination :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Prelude.Maybe Prelude.Bool)
launchTemplateEbsBlockDeviceRequest_deleteOnTermination = Lens.lens (\LaunchTemplateEbsBlockDeviceRequest' {deleteOnTermination} -> deleteOnTermination) (\s@LaunchTemplateEbsBlockDeviceRequest' {} a -> s {deleteOnTermination = a} :: LaunchTemplateEbsBlockDeviceRequest)

-- | Indicates whether the EBS volume is encrypted. Encrypted volumes can
-- only be attached to instances that support Amazon EBS encryption. If you
-- are creating a volume from a snapshot, you can\'t specify an encryption
-- value.
launchTemplateEbsBlockDeviceRequest_encrypted :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Prelude.Maybe Prelude.Bool)
launchTemplateEbsBlockDeviceRequest_encrypted = Lens.lens (\LaunchTemplateEbsBlockDeviceRequest' {encrypted} -> encrypted) (\s@LaunchTemplateEbsBlockDeviceRequest' {} a -> s {encrypted = a} :: LaunchTemplateEbsBlockDeviceRequest)

-- | The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
-- @io2@ volumes, this represents the number of IOPS that are provisioned
-- for the volume. For @gp2@ volumes, this represents the baseline
-- performance of the volume and the rate at which the volume accumulates
-- I\/O credits for bursting.
--
-- The following are the supported values for each volume type:
--
-- -   @gp3@: 3,000-16,000 IOPS
--
-- -   @io1@: 100-64,000 IOPS
--
-- -   @io2@: 100-64,000 IOPS
--
-- For @io1@ and @io2@ volumes, we guarantee 64,000 IOPS only for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>.
-- Other instance families guarantee performance up to 32,000 IOPS.
--
-- This parameter is supported for @io1@, @io2@, and @gp3@ volumes only.
-- This parameter is not supported for @gp2@, @st1@, @sc1@, or @standard@
-- volumes.
launchTemplateEbsBlockDeviceRequest_iops :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Prelude.Maybe Prelude.Int)
launchTemplateEbsBlockDeviceRequest_iops = Lens.lens (\LaunchTemplateEbsBlockDeviceRequest' {iops} -> iops) (\s@LaunchTemplateEbsBlockDeviceRequest' {} a -> s {iops = a} :: LaunchTemplateEbsBlockDeviceRequest)

-- | The ARN of the symmetric Key Management Service (KMS) CMK used for
-- encryption.
launchTemplateEbsBlockDeviceRequest_kmsKeyId :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Prelude.Maybe Prelude.Text)
launchTemplateEbsBlockDeviceRequest_kmsKeyId = Lens.lens (\LaunchTemplateEbsBlockDeviceRequest' {kmsKeyId} -> kmsKeyId) (\s@LaunchTemplateEbsBlockDeviceRequest' {} a -> s {kmsKeyId = a} :: LaunchTemplateEbsBlockDeviceRequest)

-- | The ID of the snapshot.
launchTemplateEbsBlockDeviceRequest_snapshotId :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Prelude.Maybe Prelude.Text)
launchTemplateEbsBlockDeviceRequest_snapshotId = Lens.lens (\LaunchTemplateEbsBlockDeviceRequest' {snapshotId} -> snapshotId) (\s@LaunchTemplateEbsBlockDeviceRequest' {} a -> s {snapshotId = a} :: LaunchTemplateEbsBlockDeviceRequest)

-- | The throughput to provision for a @gp3@ volume, with a maximum of 1,000
-- MiB\/s.
--
-- Valid Range: Minimum value of 125. Maximum value of 1000.
launchTemplateEbsBlockDeviceRequest_throughput :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Prelude.Maybe Prelude.Int)
launchTemplateEbsBlockDeviceRequest_throughput = Lens.lens (\LaunchTemplateEbsBlockDeviceRequest' {throughput} -> throughput) (\s@LaunchTemplateEbsBlockDeviceRequest' {} a -> s {throughput = a} :: LaunchTemplateEbsBlockDeviceRequest)

-- | The size of the volume, in GiBs. You must specify either a snapshot ID
-- or a volume size. The following are the supported volumes sizes for each
-- volume type:
--
-- -   @gp2@ and @gp3@: 1-16,384
--
-- -   @io1@ and @io2@: 4-16,384
--
-- -   @st1@ and @sc1@: 125-16,384
--
-- -   @standard@: 1-1,024
launchTemplateEbsBlockDeviceRequest_volumeSize :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Prelude.Maybe Prelude.Int)
launchTemplateEbsBlockDeviceRequest_volumeSize = Lens.lens (\LaunchTemplateEbsBlockDeviceRequest' {volumeSize} -> volumeSize) (\s@LaunchTemplateEbsBlockDeviceRequest' {} a -> s {volumeSize = a} :: LaunchTemplateEbsBlockDeviceRequest)

-- | The volume type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
launchTemplateEbsBlockDeviceRequest_volumeType :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Prelude.Maybe VolumeType)
launchTemplateEbsBlockDeviceRequest_volumeType = Lens.lens (\LaunchTemplateEbsBlockDeviceRequest' {volumeType} -> volumeType) (\s@LaunchTemplateEbsBlockDeviceRequest' {} a -> s {volumeType = a} :: LaunchTemplateEbsBlockDeviceRequest)

instance
  Prelude.Hashable
    LaunchTemplateEbsBlockDeviceRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateEbsBlockDeviceRequest' {..} =
      _salt
        `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` encrypted
        `Prelude.hashWithSalt` iops
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` snapshotId
        `Prelude.hashWithSalt` throughput
        `Prelude.hashWithSalt` volumeSize
        `Prelude.hashWithSalt` volumeType

instance
  Prelude.NFData
    LaunchTemplateEbsBlockDeviceRequest
  where
  rnf LaunchTemplateEbsBlockDeviceRequest' {..} =
    Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf throughput
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf volumeType

instance
  Data.ToQuery
    LaunchTemplateEbsBlockDeviceRequest
  where
  toQuery LaunchTemplateEbsBlockDeviceRequest' {..} =
    Prelude.mconcat
      [ "DeleteOnTermination" Data.=: deleteOnTermination,
        "Encrypted" Data.=: encrypted,
        "Iops" Data.=: iops,
        "KmsKeyId" Data.=: kmsKeyId,
        "SnapshotId" Data.=: snapshotId,
        "Throughput" Data.=: throughput,
        "VolumeSize" Data.=: volumeSize,
        "VolumeType" Data.=: volumeType
      ]
