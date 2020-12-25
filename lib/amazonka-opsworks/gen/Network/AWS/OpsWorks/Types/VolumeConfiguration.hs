{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.VolumeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.VolumeConfiguration
  ( VolumeConfiguration (..),

    -- * Smart constructor
    mkVolumeConfiguration,

    -- * Lenses
    vcMountPoint,
    vcNumberOfDisks,
    vcSize,
    vcEncrypted,
    vcIops,
    vcRaidLevel,
    vcVolumeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon EBS volume configuration.
--
-- /See:/ 'mkVolumeConfiguration' smart constructor.
data VolumeConfiguration = VolumeConfiguration'
  { -- | The volume mount point. For example "/dev/sdh".
    mountPoint :: Types.String,
    -- | The number of disks in the volume.
    numberOfDisks :: Core.Int,
    -- | The volume size.
    size :: Core.Int,
    -- | Specifies whether an Amazon EBS volume is encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
    encrypted :: Core.Maybe Core.Bool,
    -- | For PIOPS volumes, the IOPS per disk.
    iops :: Core.Maybe Core.Int,
    -- | The volume <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
    raidLevel :: Core.Maybe Core.Int,
    -- | The volume type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .
    --
    --
    --     * @standard@ - Magnetic. Magnetic volumes must have a minimum size of 1 GiB and a maximum size of 1024 GiB.
    --
    --
    --     * @io1@ - Provisioned IOPS (SSD). PIOPS volumes must have a minimum size of 4 GiB and a maximum size of 16384 GiB.
    --
    --
    --     * @gp2@ - General Purpose (SSD). General purpose volumes must have a minimum size of 1 GiB and a maximum size of 16384 GiB.
    --
    --
    --     * @st1@ - Throughput Optimized hard disk drive (HDD). Throughput optimized HDD volumes must have a minimum size of 500 GiB and a maximum size of 16384 GiB.
    --
    --
    --     * @sc1@ - Cold HDD. Cold HDD volumes must have a minimum size of 500 GiB and a maximum size of 16384 GiB.
    volumeType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VolumeConfiguration' value with any optional fields omitted.
mkVolumeConfiguration ::
  -- | 'mountPoint'
  Types.String ->
  -- | 'numberOfDisks'
  Core.Int ->
  -- | 'size'
  Core.Int ->
  VolumeConfiguration
mkVolumeConfiguration mountPoint numberOfDisks size =
  VolumeConfiguration'
    { mountPoint,
      numberOfDisks,
      size,
      encrypted = Core.Nothing,
      iops = Core.Nothing,
      raidLevel = Core.Nothing,
      volumeType = Core.Nothing
    }

-- | The volume mount point. For example "/dev/sdh".
--
-- /Note:/ Consider using 'mountPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcMountPoint :: Lens.Lens' VolumeConfiguration Types.String
vcMountPoint = Lens.field @"mountPoint"
{-# DEPRECATED vcMountPoint "Use generic-lens or generic-optics with 'mountPoint' instead." #-}

-- | The number of disks in the volume.
--
-- /Note:/ Consider using 'numberOfDisks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcNumberOfDisks :: Lens.Lens' VolumeConfiguration Core.Int
vcNumberOfDisks = Lens.field @"numberOfDisks"
{-# DEPRECATED vcNumberOfDisks "Use generic-lens or generic-optics with 'numberOfDisks' instead." #-}

-- | The volume size.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSize :: Lens.Lens' VolumeConfiguration Core.Int
vcSize = Lens.field @"size"
{-# DEPRECATED vcSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | Specifies whether an Amazon EBS volume is encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcEncrypted :: Lens.Lens' VolumeConfiguration (Core.Maybe Core.Bool)
vcEncrypted = Lens.field @"encrypted"
{-# DEPRECATED vcEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | For PIOPS volumes, the IOPS per disk.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcIops :: Lens.Lens' VolumeConfiguration (Core.Maybe Core.Int)
vcIops = Lens.field @"iops"
{-# DEPRECATED vcIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The volume <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
--
-- /Note:/ Consider using 'raidLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcRaidLevel :: Lens.Lens' VolumeConfiguration (Core.Maybe Core.Int)
vcRaidLevel = Lens.field @"raidLevel"
{-# DEPRECATED vcRaidLevel "Use generic-lens or generic-optics with 'raidLevel' instead." #-}

-- | The volume type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .
--
--
--     * @standard@ - Magnetic. Magnetic volumes must have a minimum size of 1 GiB and a maximum size of 1024 GiB.
--
--
--     * @io1@ - Provisioned IOPS (SSD). PIOPS volumes must have a minimum size of 4 GiB and a maximum size of 16384 GiB.
--
--
--     * @gp2@ - General Purpose (SSD). General purpose volumes must have a minimum size of 1 GiB and a maximum size of 16384 GiB.
--
--
--     * @st1@ - Throughput Optimized hard disk drive (HDD). Throughput optimized HDD volumes must have a minimum size of 500 GiB and a maximum size of 16384 GiB.
--
--
--     * @sc1@ - Cold HDD. Cold HDD volumes must have a minimum size of 500 GiB and a maximum size of 16384 GiB.
--
--
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcVolumeType :: Lens.Lens' VolumeConfiguration (Core.Maybe Types.String)
vcVolumeType = Lens.field @"volumeType"
{-# DEPRECATED vcVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

instance Core.FromJSON VolumeConfiguration where
  toJSON VolumeConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MountPoint" Core..= mountPoint),
            Core.Just ("NumberOfDisks" Core..= numberOfDisks),
            Core.Just ("Size" Core..= size),
            ("Encrypted" Core..=) Core.<$> encrypted,
            ("Iops" Core..=) Core.<$> iops,
            ("RaidLevel" Core..=) Core.<$> raidLevel,
            ("VolumeType" Core..=) Core.<$> volumeType
          ]
      )

instance Core.FromJSON VolumeConfiguration where
  parseJSON =
    Core.withObject "VolumeConfiguration" Core.$
      \x ->
        VolumeConfiguration'
          Core.<$> (x Core..: "MountPoint")
          Core.<*> (x Core..: "NumberOfDisks")
          Core.<*> (x Core..: "Size")
          Core.<*> (x Core..:? "Encrypted")
          Core.<*> (x Core..:? "Iops")
          Core.<*> (x Core..:? "RaidLevel")
          Core.<*> (x Core..:? "VolumeType")
