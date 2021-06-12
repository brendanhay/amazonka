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
-- Module      : Network.AWS.OpsWorks.Types.VolumeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.VolumeConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an Amazon EBS volume configuration.
--
-- /See:/ 'newVolumeConfiguration' smart constructor.
data VolumeConfiguration = VolumeConfiguration'
  { -- | Specifies whether an Amazon EBS volume is encrypted. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
    encrypted :: Core.Maybe Core.Bool,
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
    -- | The volume
    -- <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
    raidLevel :: Core.Maybe Core.Int,
    -- | For PIOPS volumes, the IOPS per disk.
    iops :: Core.Maybe Core.Int,
    -- | The volume mount point. For example \"\/dev\/sdh\".
    mountPoint :: Core.Text,
    -- | The number of disks in the volume.
    numberOfDisks :: Core.Int,
    -- | The volume size.
    size :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encrypted', 'volumeConfiguration_encrypted' - Specifies whether an Amazon EBS volume is encrypted. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
--
-- 'volumeType', 'volumeConfiguration_volumeType' - The volume type. For more information, see
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
-- 'raidLevel', 'volumeConfiguration_raidLevel' - The volume
-- <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
--
-- 'iops', 'volumeConfiguration_iops' - For PIOPS volumes, the IOPS per disk.
--
-- 'mountPoint', 'volumeConfiguration_mountPoint' - The volume mount point. For example \"\/dev\/sdh\".
--
-- 'numberOfDisks', 'volumeConfiguration_numberOfDisks' - The number of disks in the volume.
--
-- 'size', 'volumeConfiguration_size' - The volume size.
newVolumeConfiguration ::
  -- | 'mountPoint'
  Core.Text ->
  -- | 'numberOfDisks'
  Core.Int ->
  -- | 'size'
  Core.Int ->
  VolumeConfiguration
newVolumeConfiguration
  pMountPoint_
  pNumberOfDisks_
  pSize_ =
    VolumeConfiguration'
      { encrypted = Core.Nothing,
        volumeType = Core.Nothing,
        raidLevel = Core.Nothing,
        iops = Core.Nothing,
        mountPoint = pMountPoint_,
        numberOfDisks = pNumberOfDisks_,
        size = pSize_
      }

-- | Specifies whether an Amazon EBS volume is encrypted. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>.
volumeConfiguration_encrypted :: Lens.Lens' VolumeConfiguration (Core.Maybe Core.Bool)
volumeConfiguration_encrypted = Lens.lens (\VolumeConfiguration' {encrypted} -> encrypted) (\s@VolumeConfiguration' {} a -> s {encrypted = a} :: VolumeConfiguration)

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
volumeConfiguration_volumeType :: Lens.Lens' VolumeConfiguration (Core.Maybe Core.Text)
volumeConfiguration_volumeType = Lens.lens (\VolumeConfiguration' {volumeType} -> volumeType) (\s@VolumeConfiguration' {} a -> s {volumeType = a} :: VolumeConfiguration)

-- | The volume
-- <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
volumeConfiguration_raidLevel :: Lens.Lens' VolumeConfiguration (Core.Maybe Core.Int)
volumeConfiguration_raidLevel = Lens.lens (\VolumeConfiguration' {raidLevel} -> raidLevel) (\s@VolumeConfiguration' {} a -> s {raidLevel = a} :: VolumeConfiguration)

-- | For PIOPS volumes, the IOPS per disk.
volumeConfiguration_iops :: Lens.Lens' VolumeConfiguration (Core.Maybe Core.Int)
volumeConfiguration_iops = Lens.lens (\VolumeConfiguration' {iops} -> iops) (\s@VolumeConfiguration' {} a -> s {iops = a} :: VolumeConfiguration)

-- | The volume mount point. For example \"\/dev\/sdh\".
volumeConfiguration_mountPoint :: Lens.Lens' VolumeConfiguration Core.Text
volumeConfiguration_mountPoint = Lens.lens (\VolumeConfiguration' {mountPoint} -> mountPoint) (\s@VolumeConfiguration' {} a -> s {mountPoint = a} :: VolumeConfiguration)

-- | The number of disks in the volume.
volumeConfiguration_numberOfDisks :: Lens.Lens' VolumeConfiguration Core.Int
volumeConfiguration_numberOfDisks = Lens.lens (\VolumeConfiguration' {numberOfDisks} -> numberOfDisks) (\s@VolumeConfiguration' {} a -> s {numberOfDisks = a} :: VolumeConfiguration)

-- | The volume size.
volumeConfiguration_size :: Lens.Lens' VolumeConfiguration Core.Int
volumeConfiguration_size = Lens.lens (\VolumeConfiguration' {size} -> size) (\s@VolumeConfiguration' {} a -> s {size = a} :: VolumeConfiguration)

instance Core.FromJSON VolumeConfiguration where
  parseJSON =
    Core.withObject
      "VolumeConfiguration"
      ( \x ->
          VolumeConfiguration'
            Core.<$> (x Core..:? "Encrypted")
            Core.<*> (x Core..:? "VolumeType")
            Core.<*> (x Core..:? "RaidLevel")
            Core.<*> (x Core..:? "Iops")
            Core.<*> (x Core..: "MountPoint")
            Core.<*> (x Core..: "NumberOfDisks")
            Core.<*> (x Core..: "Size")
      )

instance Core.Hashable VolumeConfiguration

instance Core.NFData VolumeConfiguration

instance Core.ToJSON VolumeConfiguration where
  toJSON VolumeConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Encrypted" Core..=) Core.<$> encrypted,
            ("VolumeType" Core..=) Core.<$> volumeType,
            ("RaidLevel" Core..=) Core.<$> raidLevel,
            ("Iops" Core..=) Core.<$> iops,
            Core.Just ("MountPoint" Core..= mountPoint),
            Core.Just ("NumberOfDisks" Core..= numberOfDisks),
            Core.Just ("Size" Core..= size)
          ]
      )
