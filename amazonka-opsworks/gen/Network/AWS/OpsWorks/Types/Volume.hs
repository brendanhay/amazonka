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
-- Module      : Network.AWS.OpsWorks.Types.Volume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Volume where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an instance\'s Amazon EBS volume.
--
-- /See:/ 'newVolume' smart constructor.
data Volume = Volume'
  { -- | The value returned by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes>.
    status :: Core.Maybe Core.Text,
    -- | The instance ID.
    instanceId :: Core.Maybe Core.Text,
    -- | The Amazon EC2 volume ID.
    ec2VolumeId :: Core.Maybe Core.Text,
    -- | Specifies whether an Amazon EBS volume is encrypted. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
    encrypted :: Core.Maybe Core.Bool,
    -- | The device name.
    device :: Core.Maybe Core.Text,
    -- | The volume ID.
    volumeId :: Core.Maybe Core.Text,
    -- | The RAID array ID.
    raidArrayId :: Core.Maybe Core.Text,
    -- | The volume Availability Zone. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The volume name.
    name :: Core.Maybe Core.Text,
    -- | The volume mount point. For example, \"\/mnt\/disk1\".
    mountPoint :: Core.Maybe Core.Text,
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
    volumeType :: Core.Maybe Core.Text,
    -- | The AWS region. For more information about AWS regions, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    region :: Core.Maybe Core.Text,
    -- | For PIOPS volumes, the IOPS per disk.
    iops :: Core.Maybe Core.Int,
    -- | The volume size.
    size :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Volume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'volume_status' - The value returned by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes>.
--
-- 'instanceId', 'volume_instanceId' - The instance ID.
--
-- 'ec2VolumeId', 'volume_ec2VolumeId' - The Amazon EC2 volume ID.
--
-- 'encrypted', 'volume_encrypted' - Specifies whether an Amazon EBS volume is encrypted. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
--
-- 'device', 'volume_device' - The device name.
--
-- 'volumeId', 'volume_volumeId' - The volume ID.
--
-- 'raidArrayId', 'volume_raidArrayId' - The RAID array ID.
--
-- 'availabilityZone', 'volume_availabilityZone' - The volume Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'name', 'volume_name' - The volume name.
--
-- 'mountPoint', 'volume_mountPoint' - The volume mount point. For example, \"\/mnt\/disk1\".
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
--
-- 'region', 'volume_region' - The AWS region. For more information about AWS regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'iops', 'volume_iops' - For PIOPS volumes, the IOPS per disk.
--
-- 'size', 'volume_size' - The volume size.
newVolume ::
  Volume
newVolume =
  Volume'
    { status = Core.Nothing,
      instanceId = Core.Nothing,
      ec2VolumeId = Core.Nothing,
      encrypted = Core.Nothing,
      device = Core.Nothing,
      volumeId = Core.Nothing,
      raidArrayId = Core.Nothing,
      availabilityZone = Core.Nothing,
      name = Core.Nothing,
      mountPoint = Core.Nothing,
      volumeType = Core.Nothing,
      region = Core.Nothing,
      iops = Core.Nothing,
      size = Core.Nothing
    }

-- | The value returned by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes>.
volume_status :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_status = Lens.lens (\Volume' {status} -> status) (\s@Volume' {} a -> s {status = a} :: Volume)

-- | The instance ID.
volume_instanceId :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_instanceId = Lens.lens (\Volume' {instanceId} -> instanceId) (\s@Volume' {} a -> s {instanceId = a} :: Volume)

-- | The Amazon EC2 volume ID.
volume_ec2VolumeId :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_ec2VolumeId = Lens.lens (\Volume' {ec2VolumeId} -> ec2VolumeId) (\s@Volume' {} a -> s {ec2VolumeId = a} :: Volume)

-- | Specifies whether an Amazon EBS volume is encrypted. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
volume_encrypted :: Lens.Lens' Volume (Core.Maybe Core.Bool)
volume_encrypted = Lens.lens (\Volume' {encrypted} -> encrypted) (\s@Volume' {} a -> s {encrypted = a} :: Volume)

-- | The device name.
volume_device :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_device = Lens.lens (\Volume' {device} -> device) (\s@Volume' {} a -> s {device = a} :: Volume)

-- | The volume ID.
volume_volumeId :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_volumeId = Lens.lens (\Volume' {volumeId} -> volumeId) (\s@Volume' {} a -> s {volumeId = a} :: Volume)

-- | The RAID array ID.
volume_raidArrayId :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_raidArrayId = Lens.lens (\Volume' {raidArrayId} -> raidArrayId) (\s@Volume' {} a -> s {raidArrayId = a} :: Volume)

-- | The volume Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
volume_availabilityZone :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_availabilityZone = Lens.lens (\Volume' {availabilityZone} -> availabilityZone) (\s@Volume' {} a -> s {availabilityZone = a} :: Volume)

-- | The volume name.
volume_name :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_name = Lens.lens (\Volume' {name} -> name) (\s@Volume' {} a -> s {name = a} :: Volume)

-- | The volume mount point. For example, \"\/mnt\/disk1\".
volume_mountPoint :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_mountPoint = Lens.lens (\Volume' {mountPoint} -> mountPoint) (\s@Volume' {} a -> s {mountPoint = a} :: Volume)

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
volume_volumeType :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_volumeType = Lens.lens (\Volume' {volumeType} -> volumeType) (\s@Volume' {} a -> s {volumeType = a} :: Volume)

-- | The AWS region. For more information about AWS regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
volume_region :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_region = Lens.lens (\Volume' {region} -> region) (\s@Volume' {} a -> s {region = a} :: Volume)

-- | For PIOPS volumes, the IOPS per disk.
volume_iops :: Lens.Lens' Volume (Core.Maybe Core.Int)
volume_iops = Lens.lens (\Volume' {iops} -> iops) (\s@Volume' {} a -> s {iops = a} :: Volume)

-- | The volume size.
volume_size :: Lens.Lens' Volume (Core.Maybe Core.Int)
volume_size = Lens.lens (\Volume' {size} -> size) (\s@Volume' {} a -> s {size = a} :: Volume)

instance Core.FromJSON Volume where
  parseJSON =
    Core.withObject
      "Volume"
      ( \x ->
          Volume'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "Ec2VolumeId")
            Core.<*> (x Core..:? "Encrypted")
            Core.<*> (x Core..:? "Device")
            Core.<*> (x Core..:? "VolumeId")
            Core.<*> (x Core..:? "RaidArrayId")
            Core.<*> (x Core..:? "AvailabilityZone")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "MountPoint")
            Core.<*> (x Core..:? "VolumeType")
            Core.<*> (x Core..:? "Region")
            Core.<*> (x Core..:? "Iops")
            Core.<*> (x Core..:? "Size")
      )

instance Core.Hashable Volume

instance Core.NFData Volume
