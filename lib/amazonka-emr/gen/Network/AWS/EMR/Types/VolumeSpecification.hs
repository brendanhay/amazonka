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
    vsIOPS,
    vsSizeInGB,
    vsVolumeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
--
-- /See:/ 'mkVolumeSpecification' smart constructor.
data VolumeSpecification = VolumeSpecification'
  { -- | The number of I/O operations per second (IOPS) that the volume supports.
    iops :: Lude.Maybe Lude.Int,
    -- | The volume size, in gibibytes (GiB). This can be a number from 1 - 1024. If the volume type is EBS-optimized, the minimum value is 10.
    sizeInGB :: Lude.Int,
    -- | The volume type. Volume types supported are gp2, io1, standard.
    volumeType :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeSpecification' with the minimum fields required to make a request.
--
-- * 'iops' - The number of I/O operations per second (IOPS) that the volume supports.
-- * 'sizeInGB' - The volume size, in gibibytes (GiB). This can be a number from 1 - 1024. If the volume type is EBS-optimized, the minimum value is 10.
-- * 'volumeType' - The volume type. Volume types supported are gp2, io1, standard.
mkVolumeSpecification ::
  -- | 'sizeInGB'
  Lude.Int ->
  -- | 'volumeType'
  Lude.Text ->
  VolumeSpecification
mkVolumeSpecification pSizeInGB_ pVolumeType_ =
  VolumeSpecification'
    { iops = Lude.Nothing,
      sizeInGB = pSizeInGB_,
      volumeType = pVolumeType_
    }

-- | The number of I/O operations per second (IOPS) that the volume supports.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsIOPS :: Lens.Lens' VolumeSpecification (Lude.Maybe Lude.Int)
vsIOPS = Lens.lens (iops :: VolumeSpecification -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: VolumeSpecification)
{-# DEPRECATED vsIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The volume size, in gibibytes (GiB). This can be a number from 1 - 1024. If the volume type is EBS-optimized, the minimum value is 10.
--
-- /Note:/ Consider using 'sizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSizeInGB :: Lens.Lens' VolumeSpecification Lude.Int
vsSizeInGB = Lens.lens (sizeInGB :: VolumeSpecification -> Lude.Int) (\s a -> s {sizeInGB = a} :: VolumeSpecification)
{-# DEPRECATED vsSizeInGB "Use generic-lens or generic-optics with 'sizeInGB' instead." #-}

-- | The volume type. Volume types supported are gp2, io1, standard.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsVolumeType :: Lens.Lens' VolumeSpecification Lude.Text
vsVolumeType = Lens.lens (volumeType :: VolumeSpecification -> Lude.Text) (\s a -> s {volumeType = a} :: VolumeSpecification)
{-# DEPRECATED vsVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

instance Lude.FromJSON VolumeSpecification where
  parseJSON =
    Lude.withObject
      "VolumeSpecification"
      ( \x ->
          VolumeSpecification'
            Lude.<$> (x Lude..:? "Iops")
            Lude.<*> (x Lude..: "SizeInGB")
            Lude.<*> (x Lude..: "VolumeType")
      )

instance Lude.ToJSON VolumeSpecification where
  toJSON VolumeSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Iops" Lude..=) Lude.<$> iops,
            Lude.Just ("SizeInGB" Lude..= sizeInGB),
            Lude.Just ("VolumeType" Lude..= volumeType)
          ]
      )
