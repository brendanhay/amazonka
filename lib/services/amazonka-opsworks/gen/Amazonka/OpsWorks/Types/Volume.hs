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
-- Module      : Amazonka.OpsWorks.Types.Volume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.Volume where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance\'s Amazon EBS volume.
--
-- /See:/ 'newVolume' smart constructor.
data Volume = Volume'
  { -- | The volume Availability Zone. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The device name.
    device :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 volume ID.
    ec2VolumeId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether an Amazon EBS volume is encrypted. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | For PIOPS volumes, the IOPS per disk.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The volume mount point. For example, \"\/mnt\/disk1\".
    mountPoint :: Prelude.Maybe Prelude.Text,
    -- | The volume name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The RAID array ID.
    raidArrayId :: Prelude.Maybe Prelude.Text,
    -- | The AWS region. For more information about AWS regions, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    region :: Prelude.Maybe Prelude.Text,
    -- | The volume size.
    size :: Prelude.Maybe Prelude.Int,
    -- | The value returned by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes>.
    status :: Prelude.Maybe Prelude.Text,
    -- | The volume ID.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | The volume type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>.
    --
    -- -   @standard@ - Magnetic. Magnetic volumes must have a minimum size of
    --     1 GiB and a maximum size of 1024 GiB.
    --
    -- -   @io1@ - Provisioned IOPS (SSD). PIOPS volumes must have a minimum
    --     size of 4 GiB and a maximum size of 16384 GiB.
    --
    -- -   @gp2@ - General Purpose (SSD). General purpose volumes must have a
    --     minimum size of 1 GiB and a maximum size of 16384 GiB.
    --
    -- -   @st1@ - Throughput Optimized hard disk drive (HDD). Throughput
    --     optimized HDD volumes must have a minimum size of 500 GiB and a
    --     maximum size of 16384 GiB.
    --
    -- -   @sc1@ - Cold HDD. Cold HDD volumes must have a minimum size of 500
    --     GiB and a maximum size of 16384 GiB.
    volumeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Volume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'volume_availabilityZone' - The volume Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'device', 'volume_device' - The device name.
--
-- 'ec2VolumeId', 'volume_ec2VolumeId' - The Amazon EC2 volume ID.
--
-- 'encrypted', 'volume_encrypted' - Specifies whether an Amazon EBS volume is encrypted. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
--
-- 'instanceId', 'volume_instanceId' - The instance ID.
--
-- 'iops', 'volume_iops' - For PIOPS volumes, the IOPS per disk.
--
-- 'mountPoint', 'volume_mountPoint' - The volume mount point. For example, \"\/mnt\/disk1\".
--
-- 'name', 'volume_name' - The volume name.
--
-- 'raidArrayId', 'volume_raidArrayId' - The RAID array ID.
--
-- 'region', 'volume_region' - The AWS region. For more information about AWS regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'size', 'volume_size' - The volume size.
--
-- 'status', 'volume_status' - The value returned by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes>.
--
-- 'volumeId', 'volume_volumeId' - The volume ID.
--
-- 'volumeType', 'volume_volumeType' - The volume type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>.
--
-- -   @standard@ - Magnetic. Magnetic volumes must have a minimum size of
--     1 GiB and a maximum size of 1024 GiB.
--
-- -   @io1@ - Provisioned IOPS (SSD). PIOPS volumes must have a minimum
--     size of 4 GiB and a maximum size of 16384 GiB.
--
-- -   @gp2@ - General Purpose (SSD). General purpose volumes must have a
--     minimum size of 1 GiB and a maximum size of 16384 GiB.
--
-- -   @st1@ - Throughput Optimized hard disk drive (HDD). Throughput
--     optimized HDD volumes must have a minimum size of 500 GiB and a
--     maximum size of 16384 GiB.
--
-- -   @sc1@ - Cold HDD. Cold HDD volumes must have a minimum size of 500
--     GiB and a maximum size of 16384 GiB.
newVolume ::
  Volume
newVolume =
  Volume'
    { availabilityZone = Prelude.Nothing,
      device = Prelude.Nothing,
      ec2VolumeId = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      iops = Prelude.Nothing,
      mountPoint = Prelude.Nothing,
      name = Prelude.Nothing,
      raidArrayId = Prelude.Nothing,
      region = Prelude.Nothing,
      size = Prelude.Nothing,
      status = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | The volume Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
volume_availabilityZone :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_availabilityZone = Lens.lens (\Volume' {availabilityZone} -> availabilityZone) (\s@Volume' {} a -> s {availabilityZone = a} :: Volume)

-- | The device name.
volume_device :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_device = Lens.lens (\Volume' {device} -> device) (\s@Volume' {} a -> s {device = a} :: Volume)

-- | The Amazon EC2 volume ID.
volume_ec2VolumeId :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_ec2VolumeId = Lens.lens (\Volume' {ec2VolumeId} -> ec2VolumeId) (\s@Volume' {} a -> s {ec2VolumeId = a} :: Volume)

-- | Specifies whether an Amazon EBS volume is encrypted. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
volume_encrypted :: Lens.Lens' Volume (Prelude.Maybe Prelude.Bool)
volume_encrypted = Lens.lens (\Volume' {encrypted} -> encrypted) (\s@Volume' {} a -> s {encrypted = a} :: Volume)

-- | The instance ID.
volume_instanceId :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_instanceId = Lens.lens (\Volume' {instanceId} -> instanceId) (\s@Volume' {} a -> s {instanceId = a} :: Volume)

-- | For PIOPS volumes, the IOPS per disk.
volume_iops :: Lens.Lens' Volume (Prelude.Maybe Prelude.Int)
volume_iops = Lens.lens (\Volume' {iops} -> iops) (\s@Volume' {} a -> s {iops = a} :: Volume)

-- | The volume mount point. For example, \"\/mnt\/disk1\".
volume_mountPoint :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_mountPoint = Lens.lens (\Volume' {mountPoint} -> mountPoint) (\s@Volume' {} a -> s {mountPoint = a} :: Volume)

-- | The volume name.
volume_name :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_name = Lens.lens (\Volume' {name} -> name) (\s@Volume' {} a -> s {name = a} :: Volume)

-- | The RAID array ID.
volume_raidArrayId :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_raidArrayId = Lens.lens (\Volume' {raidArrayId} -> raidArrayId) (\s@Volume' {} a -> s {raidArrayId = a} :: Volume)

-- | The AWS region. For more information about AWS regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
volume_region :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_region = Lens.lens (\Volume' {region} -> region) (\s@Volume' {} a -> s {region = a} :: Volume)

-- | The volume size.
volume_size :: Lens.Lens' Volume (Prelude.Maybe Prelude.Int)
volume_size = Lens.lens (\Volume' {size} -> size) (\s@Volume' {} a -> s {size = a} :: Volume)

-- | The value returned by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes>.
volume_status :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_status = Lens.lens (\Volume' {status} -> status) (\s@Volume' {} a -> s {status = a} :: Volume)

-- | The volume ID.
volume_volumeId :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_volumeId = Lens.lens (\Volume' {volumeId} -> volumeId) (\s@Volume' {} a -> s {volumeId = a} :: Volume)

-- | The volume type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types>.
--
-- -   @standard@ - Magnetic. Magnetic volumes must have a minimum size of
--     1 GiB and a maximum size of 1024 GiB.
--
-- -   @io1@ - Provisioned IOPS (SSD). PIOPS volumes must have a minimum
--     size of 4 GiB and a maximum size of 16384 GiB.
--
-- -   @gp2@ - General Purpose (SSD). General purpose volumes must have a
--     minimum size of 1 GiB and a maximum size of 16384 GiB.
--
-- -   @st1@ - Throughput Optimized hard disk drive (HDD). Throughput
--     optimized HDD volumes must have a minimum size of 500 GiB and a
--     maximum size of 16384 GiB.
--
-- -   @sc1@ - Cold HDD. Cold HDD volumes must have a minimum size of 500
--     GiB and a maximum size of 16384 GiB.
volume_volumeType :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_volumeType = Lens.lens (\Volume' {volumeType} -> volumeType) (\s@Volume' {} a -> s {volumeType = a} :: Volume)

instance Data.FromJSON Volume where
  parseJSON =
    Data.withObject
      "Volume"
      ( \x ->
          Volume'
            Prelude.<$> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "Device")
            Prelude.<*> (x Data..:? "Ec2VolumeId")
            Prelude.<*> (x Data..:? "Encrypted")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "Iops")
            Prelude.<*> (x Data..:? "MountPoint")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RaidArrayId")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "Size")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "VolumeId")
            Prelude.<*> (x Data..:? "VolumeType")
      )

instance Prelude.Hashable Volume where
  hashWithSalt _salt Volume' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` device
      `Prelude.hashWithSalt` ec2VolumeId
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` mountPoint
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` raidArrayId
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData Volume where
  rnf Volume' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf device
      `Prelude.seq` Prelude.rnf ec2VolumeId
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf mountPoint
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf raidArrayId
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf volumeType
