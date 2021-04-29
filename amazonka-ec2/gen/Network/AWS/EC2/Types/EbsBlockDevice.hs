{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.EbsBlockDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EbsBlockDevice where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a block device for an EBS volume.
--
-- /See:/ 'newEbsBlockDevice' smart constructor.
data EbsBlockDevice = EbsBlockDevice'
  { -- | Indicates whether the encryption state of an EBS volume is changed while
    -- being restored from a backing snapshot. The effect of setting the
    -- encryption state to @true@ depends on the volume origin (new or from a
    -- snapshot), starting encryption state, ownership, and whether encryption
    -- by default is enabled. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-parameters Amazon EBS encryption>
    -- in the /Amazon EC2 User Guide/.
    --
    -- In no case can you remove encryption from an encrypted volume.
    --
    -- Encrypted volumes can only be attached to instances that support Amazon
    -- EBS encryption. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types>.
    --
    -- This parameter is not returned by .
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the Outpost on which the snapshot is stored.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The throughput that the volume supports, in MiB\/s.
    --
    -- This parameter is valid only for @gp3@ volumes.
    --
    -- Valid Range: Minimum value of 125. Maximum value of 1000.
    throughput :: Prelude.Maybe Prelude.Int,
    -- | Identifier (key ID, key alias, ID ARN, or alias ARN) for a customer
    -- managed CMK under which the EBS volume is encrypted.
    --
    -- This parameter is only supported on @BlockDeviceMapping@ objects called
    -- by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>,
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html RequestSpotFleet>,
    -- and
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances>.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the EBS volume is deleted on instance termination. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/terminating-instances.html#preserving-volumes-on-termination Preserving Amazon EBS volumes on instance termination>
    -- in the /Amazon EC2 User Guide/.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The volume type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
    -- in the /Amazon EC2 User Guide/. If the volume type is @io1@ or @io2@,
    -- you must specify the IOPS that the volume supports.
    volumeType :: Prelude.Maybe VolumeType,
    -- | The size of the volume, in GiBs. You must specify either a snapshot ID
    -- or a volume size. If you specify a snapshot, the default is the snapshot
    -- size. You can specify a volume size that is equal to or larger than the
    -- snapshot size.
    --
    -- The following are the supported volumes sizes for each volume type:
    --
    -- -   @gp2@ and @gp3@:1-16,384
    --
    -- -   @io1@ and @io2@: 4-16,384
    --
    -- -   @st1@ and @sc1@: 125-16,384
    --
    -- -   @standard@: 1-1,024
    volumeSize :: Prelude.Maybe Prelude.Int,
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
    -- This parameter is required for @io1@ and @io2@ volumes. The default for
    -- @gp3@ volumes is 3,000 IOPS. This parameter is not supported for @gp2@,
    -- @st1@, @sc1@, or @standard@ volumes.
    iops :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EbsBlockDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encrypted', 'ebsBlockDevice_encrypted' - Indicates whether the encryption state of an EBS volume is changed while
-- being restored from a backing snapshot. The effect of setting the
-- encryption state to @true@ depends on the volume origin (new or from a
-- snapshot), starting encryption state, ownership, and whether encryption
-- by default is enabled. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-parameters Amazon EBS encryption>
-- in the /Amazon EC2 User Guide/.
--
-- In no case can you remove encryption from an encrypted volume.
--
-- Encrypted volumes can only be attached to instances that support Amazon
-- EBS encryption. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types>.
--
-- This parameter is not returned by .
--
-- 'outpostArn', 'ebsBlockDevice_outpostArn' - The ARN of the Outpost on which the snapshot is stored.
--
-- 'throughput', 'ebsBlockDevice_throughput' - The throughput that the volume supports, in MiB\/s.
--
-- This parameter is valid only for @gp3@ volumes.
--
-- Valid Range: Minimum value of 125. Maximum value of 1000.
--
-- 'kmsKeyId', 'ebsBlockDevice_kmsKeyId' - Identifier (key ID, key alias, ID ARN, or alias ARN) for a customer
-- managed CMK under which the EBS volume is encrypted.
--
-- This parameter is only supported on @BlockDeviceMapping@ objects called
-- by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>,
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html RequestSpotFleet>,
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances>.
--
-- 'deleteOnTermination', 'ebsBlockDevice_deleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/terminating-instances.html#preserving-volumes-on-termination Preserving Amazon EBS volumes on instance termination>
-- in the /Amazon EC2 User Guide/.
--
-- 'snapshotId', 'ebsBlockDevice_snapshotId' - The ID of the snapshot.
--
-- 'volumeType', 'ebsBlockDevice_volumeType' - The volume type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon EC2 User Guide/. If the volume type is @io1@ or @io2@,
-- you must specify the IOPS that the volume supports.
--
-- 'volumeSize', 'ebsBlockDevice_volumeSize' - The size of the volume, in GiBs. You must specify either a snapshot ID
-- or a volume size. If you specify a snapshot, the default is the snapshot
-- size. You can specify a volume size that is equal to or larger than the
-- snapshot size.
--
-- The following are the supported volumes sizes for each volume type:
--
-- -   @gp2@ and @gp3@:1-16,384
--
-- -   @io1@ and @io2@: 4-16,384
--
-- -   @st1@ and @sc1@: 125-16,384
--
-- -   @standard@: 1-1,024
--
-- 'iops', 'ebsBlockDevice_iops' - The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
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
-- This parameter is required for @io1@ and @io2@ volumes. The default for
-- @gp3@ volumes is 3,000 IOPS. This parameter is not supported for @gp2@,
-- @st1@, @sc1@, or @standard@ volumes.
newEbsBlockDevice ::
  EbsBlockDevice
newEbsBlockDevice =
  EbsBlockDevice'
    { encrypted = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      throughput = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      iops = Prelude.Nothing
    }

-- | Indicates whether the encryption state of an EBS volume is changed while
-- being restored from a backing snapshot. The effect of setting the
-- encryption state to @true@ depends on the volume origin (new or from a
-- snapshot), starting encryption state, ownership, and whether encryption
-- by default is enabled. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-parameters Amazon EBS encryption>
-- in the /Amazon EC2 User Guide/.
--
-- In no case can you remove encryption from an encrypted volume.
--
-- Encrypted volumes can only be attached to instances that support Amazon
-- EBS encryption. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types>.
--
-- This parameter is not returned by .
ebsBlockDevice_encrypted :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Bool)
ebsBlockDevice_encrypted = Lens.lens (\EbsBlockDevice' {encrypted} -> encrypted) (\s@EbsBlockDevice' {} a -> s {encrypted = a} :: EbsBlockDevice)

-- | The ARN of the Outpost on which the snapshot is stored.
ebsBlockDevice_outpostArn :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Text)
ebsBlockDevice_outpostArn = Lens.lens (\EbsBlockDevice' {outpostArn} -> outpostArn) (\s@EbsBlockDevice' {} a -> s {outpostArn = a} :: EbsBlockDevice)

-- | The throughput that the volume supports, in MiB\/s.
--
-- This parameter is valid only for @gp3@ volumes.
--
-- Valid Range: Minimum value of 125. Maximum value of 1000.
ebsBlockDevice_throughput :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Int)
ebsBlockDevice_throughput = Lens.lens (\EbsBlockDevice' {throughput} -> throughput) (\s@EbsBlockDevice' {} a -> s {throughput = a} :: EbsBlockDevice)

-- | Identifier (key ID, key alias, ID ARN, or alias ARN) for a customer
-- managed CMK under which the EBS volume is encrypted.
--
-- This parameter is only supported on @BlockDeviceMapping@ objects called
-- by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>,
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html RequestSpotFleet>,
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances>.
ebsBlockDevice_kmsKeyId :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Text)
ebsBlockDevice_kmsKeyId = Lens.lens (\EbsBlockDevice' {kmsKeyId} -> kmsKeyId) (\s@EbsBlockDevice' {} a -> s {kmsKeyId = a} :: EbsBlockDevice)

-- | Indicates whether the EBS volume is deleted on instance termination. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/terminating-instances.html#preserving-volumes-on-termination Preserving Amazon EBS volumes on instance termination>
-- in the /Amazon EC2 User Guide/.
ebsBlockDevice_deleteOnTermination :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Bool)
ebsBlockDevice_deleteOnTermination = Lens.lens (\EbsBlockDevice' {deleteOnTermination} -> deleteOnTermination) (\s@EbsBlockDevice' {} a -> s {deleteOnTermination = a} :: EbsBlockDevice)

-- | The ID of the snapshot.
ebsBlockDevice_snapshotId :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Text)
ebsBlockDevice_snapshotId = Lens.lens (\EbsBlockDevice' {snapshotId} -> snapshotId) (\s@EbsBlockDevice' {} a -> s {snapshotId = a} :: EbsBlockDevice)

-- | The volume type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon EC2 User Guide/. If the volume type is @io1@ or @io2@,
-- you must specify the IOPS that the volume supports.
ebsBlockDevice_volumeType :: Lens.Lens' EbsBlockDevice (Prelude.Maybe VolumeType)
ebsBlockDevice_volumeType = Lens.lens (\EbsBlockDevice' {volumeType} -> volumeType) (\s@EbsBlockDevice' {} a -> s {volumeType = a} :: EbsBlockDevice)

-- | The size of the volume, in GiBs. You must specify either a snapshot ID
-- or a volume size. If you specify a snapshot, the default is the snapshot
-- size. You can specify a volume size that is equal to or larger than the
-- snapshot size.
--
-- The following are the supported volumes sizes for each volume type:
--
-- -   @gp2@ and @gp3@:1-16,384
--
-- -   @io1@ and @io2@: 4-16,384
--
-- -   @st1@ and @sc1@: 125-16,384
--
-- -   @standard@: 1-1,024
ebsBlockDevice_volumeSize :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Int)
ebsBlockDevice_volumeSize = Lens.lens (\EbsBlockDevice' {volumeSize} -> volumeSize) (\s@EbsBlockDevice' {} a -> s {volumeSize = a} :: EbsBlockDevice)

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
-- This parameter is required for @io1@ and @io2@ volumes. The default for
-- @gp3@ volumes is 3,000 IOPS. This parameter is not supported for @gp2@,
-- @st1@, @sc1@, or @standard@ volumes.
ebsBlockDevice_iops :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Int)
ebsBlockDevice_iops = Lens.lens (\EbsBlockDevice' {iops} -> iops) (\s@EbsBlockDevice' {} a -> s {iops = a} :: EbsBlockDevice)

instance Prelude.FromXML EbsBlockDevice where
  parseXML x =
    EbsBlockDevice'
      Prelude.<$> (x Prelude..@? "encrypted")
      Prelude.<*> (x Prelude..@? "outpostArn")
      Prelude.<*> (x Prelude..@? "throughput")
      Prelude.<*> (x Prelude..@? "KmsKeyId")
      Prelude.<*> (x Prelude..@? "deleteOnTermination")
      Prelude.<*> (x Prelude..@? "snapshotId")
      Prelude.<*> (x Prelude..@? "volumeType")
      Prelude.<*> (x Prelude..@? "volumeSize")
      Prelude.<*> (x Prelude..@? "iops")

instance Prelude.Hashable EbsBlockDevice

instance Prelude.NFData EbsBlockDevice

instance Prelude.ToQuery EbsBlockDevice where
  toQuery EbsBlockDevice' {..} =
    Prelude.mconcat
      [ "Encrypted" Prelude.=: encrypted,
        "OutpostArn" Prelude.=: outpostArn,
        "Throughput" Prelude.=: throughput,
        "KmsKeyId" Prelude.=: kmsKeyId,
        "DeleteOnTermination" Prelude.=: deleteOnTermination,
        "SnapshotId" Prelude.=: snapshotId,
        "VolumeType" Prelude.=: volumeType,
        "VolumeSize" Prelude.=: volumeSize,
        "Iops" Prelude.=: iops
      ]
