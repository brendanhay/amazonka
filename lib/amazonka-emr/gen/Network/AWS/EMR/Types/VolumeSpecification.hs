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
    vsVolumeType,
    vsSizeInGB,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
--
-- /See:/ 'mkVolumeSpecification' smart constructor.
data VolumeSpecification = VolumeSpecification'
  { iops ::
      Lude.Maybe Lude.Int,
    volumeType :: Lude.Text,
    sizeInGB :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeSpecification' with the minimum fields required to make a request.
--
-- * 'iops' - The number of I/O operations per second (IOPS) that the volume supports.
-- * 'sizeInGB' - The volume size, in gibibytes (GiB). This can be a number from 1 - 1024. If the volume type is EBS-optimized, the minimum value is 10.
-- * 'volumeType' - The volume type. Volume types supported are gp2, io1, standard.
mkVolumeSpecification ::
  -- | 'volumeType'
  Lude.Text ->
  -- | 'sizeInGB'
  Lude.Int ->
  VolumeSpecification
mkVolumeSpecification pVolumeType_ pSizeInGB_ =
  VolumeSpecification'
    { iops = Lude.Nothing,
      volumeType = pVolumeType_,
      sizeInGB = pSizeInGB_
    }

-- | The number of I/O operations per second (IOPS) that the volume supports.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsIOPS :: Lens.Lens' VolumeSpecification (Lude.Maybe Lude.Int)
vsIOPS = Lens.lens (iops :: VolumeSpecification -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: VolumeSpecification)
{-# DEPRECATED vsIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The volume type. Volume types supported are gp2, io1, standard.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsVolumeType :: Lens.Lens' VolumeSpecification Lude.Text
vsVolumeType = Lens.lens (volumeType :: VolumeSpecification -> Lude.Text) (\s a -> s {volumeType = a} :: VolumeSpecification)
{-# DEPRECATED vsVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The volume size, in gibibytes (GiB). This can be a number from 1 - 1024. If the volume type is EBS-optimized, the minimum value is 10.
--
-- /Note:/ Consider using 'sizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSizeInGB :: Lens.Lens' VolumeSpecification Lude.Int
vsSizeInGB = Lens.lens (sizeInGB :: VolumeSpecification -> Lude.Int) (\s a -> s {sizeInGB = a} :: VolumeSpecification)
{-# DEPRECATED vsSizeInGB "Use generic-lens or generic-optics with 'sizeInGB' instead." #-}

instance Lude.FromJSON VolumeSpecification where
  parseJSON =
    Lude.withObject
      "VolumeSpecification"
      ( \x ->
          VolumeSpecification'
            Lude.<$> (x Lude..:? "Iops")
            Lude.<*> (x Lude..: "VolumeType")
            Lude.<*> (x Lude..: "SizeInGB")
      )

instance Lude.ToJSON VolumeSpecification where
  toJSON VolumeSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Iops" Lude..=) Lude.<$> iops,
            Lude.Just ("VolumeType" Lude..= volumeType),
            Lude.Just ("SizeInGB" Lude..= sizeInGB)
          ]
      )
