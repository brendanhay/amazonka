{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.VolumeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.VolumeSpecification
  ( VolumeSpecification (..),

    -- * Smart constructor
    mkVolumeSpecification,

    -- * Lenses
    vsVolumeType,
    vsSizeInGB,
    vsIops,
  )
where

import qualified Network.AWS.EMR.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
--
-- /See:/ 'mkVolumeSpecification' smart constructor.
data VolumeSpecification = VolumeSpecification'
  { -- | The volume type. Volume types supported are gp2, io1, standard.
    volumeType :: Types.String,
    -- | The volume size, in gibibytes (GiB). This can be a number from 1 - 1024. If the volume type is EBS-optimized, the minimum value is 10.
    sizeInGB :: Core.Int,
    -- | The number of I/O operations per second (IOPS) that the volume supports.
    iops :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VolumeSpecification' value with any optional fields omitted.
mkVolumeSpecification ::
  -- | 'volumeType'
  Types.String ->
  -- | 'sizeInGB'
  Core.Int ->
  VolumeSpecification
mkVolumeSpecification volumeType sizeInGB =
  VolumeSpecification' {volumeType, sizeInGB, iops = Core.Nothing}

-- | The volume type. Volume types supported are gp2, io1, standard.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsVolumeType :: Lens.Lens' VolumeSpecification Types.String
vsVolumeType = Lens.field @"volumeType"
{-# DEPRECATED vsVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The volume size, in gibibytes (GiB). This can be a number from 1 - 1024. If the volume type is EBS-optimized, the minimum value is 10.
--
-- /Note:/ Consider using 'sizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSizeInGB :: Lens.Lens' VolumeSpecification Core.Int
vsSizeInGB = Lens.field @"sizeInGB"
{-# DEPRECATED vsSizeInGB "Use generic-lens or generic-optics with 'sizeInGB' instead." #-}

-- | The number of I/O operations per second (IOPS) that the volume supports.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsIops :: Lens.Lens' VolumeSpecification (Core.Maybe Core.Int)
vsIops = Lens.field @"iops"
{-# DEPRECATED vsIops "Use generic-lens or generic-optics with 'iops' instead." #-}

instance Core.FromJSON VolumeSpecification where
  toJSON VolumeSpecification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VolumeType" Core..= volumeType),
            Core.Just ("SizeInGB" Core..= sizeInGB),
            ("Iops" Core..=) Core.<$> iops
          ]
      )

instance Core.FromJSON VolumeSpecification where
  parseJSON =
    Core.withObject "VolumeSpecification" Core.$
      \x ->
        VolumeSpecification'
          Core.<$> (x Core..: "VolumeType")
          Core.<*> (x Core..: "SizeInGB")
          Core.<*> (x Core..:? "Iops")
