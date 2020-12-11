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
    vcIOPS,
    vcRAIDLevel,
    vcEncrypted,
    vcVolumeType,
    vcMountPoint,
    vcNumberOfDisks,
    vcSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon EBS volume configuration.
--
-- /See:/ 'mkVolumeConfiguration' smart constructor.
data VolumeConfiguration = VolumeConfiguration'
  { iops ::
      Lude.Maybe Lude.Int,
    raidLevel :: Lude.Maybe Lude.Int,
    encrypted :: Lude.Maybe Lude.Bool,
    volumeType :: Lude.Maybe Lude.Text,
    mountPoint :: Lude.Text,
    numberOfDisks :: Lude.Int,
    size :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeConfiguration' with the minimum fields required to make a request.
--
-- * 'encrypted' - Specifies whether an Amazon EBS volume is encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
-- * 'iops' - For PIOPS volumes, the IOPS per disk.
-- * 'mountPoint' - The volume mount point. For example "/dev/sdh".
-- * 'numberOfDisks' - The number of disks in the volume.
-- * 'raidLevel' - The volume <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
-- * 'size' - The volume size.
-- * 'volumeType' - The volume type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .
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
mkVolumeConfiguration ::
  -- | 'mountPoint'
  Lude.Text ->
  -- | 'numberOfDisks'
  Lude.Int ->
  -- | 'size'
  Lude.Int ->
  VolumeConfiguration
mkVolumeConfiguration pMountPoint_ pNumberOfDisks_ pSize_ =
  VolumeConfiguration'
    { iops = Lude.Nothing,
      raidLevel = Lude.Nothing,
      encrypted = Lude.Nothing,
      volumeType = Lude.Nothing,
      mountPoint = pMountPoint_,
      numberOfDisks = pNumberOfDisks_,
      size = pSize_
    }

-- | For PIOPS volumes, the IOPS per disk.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcIOPS :: Lens.Lens' VolumeConfiguration (Lude.Maybe Lude.Int)
vcIOPS = Lens.lens (iops :: VolumeConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: VolumeConfiguration)
{-# DEPRECATED vcIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The volume <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
--
-- /Note:/ Consider using 'raidLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcRAIDLevel :: Lens.Lens' VolumeConfiguration (Lude.Maybe Lude.Int)
vcRAIDLevel = Lens.lens (raidLevel :: VolumeConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {raidLevel = a} :: VolumeConfiguration)
{-# DEPRECATED vcRAIDLevel "Use generic-lens or generic-optics with 'raidLevel' instead." #-}

-- | Specifies whether an Amazon EBS volume is encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcEncrypted :: Lens.Lens' VolumeConfiguration (Lude.Maybe Lude.Bool)
vcEncrypted = Lens.lens (encrypted :: VolumeConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: VolumeConfiguration)
{-# DEPRECATED vcEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

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
vcVolumeType :: Lens.Lens' VolumeConfiguration (Lude.Maybe Lude.Text)
vcVolumeType = Lens.lens (volumeType :: VolumeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {volumeType = a} :: VolumeConfiguration)
{-# DEPRECATED vcVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The volume mount point. For example "/dev/sdh".
--
-- /Note:/ Consider using 'mountPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcMountPoint :: Lens.Lens' VolumeConfiguration Lude.Text
vcMountPoint = Lens.lens (mountPoint :: VolumeConfiguration -> Lude.Text) (\s a -> s {mountPoint = a} :: VolumeConfiguration)
{-# DEPRECATED vcMountPoint "Use generic-lens or generic-optics with 'mountPoint' instead." #-}

-- | The number of disks in the volume.
--
-- /Note:/ Consider using 'numberOfDisks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcNumberOfDisks :: Lens.Lens' VolumeConfiguration Lude.Int
vcNumberOfDisks = Lens.lens (numberOfDisks :: VolumeConfiguration -> Lude.Int) (\s a -> s {numberOfDisks = a} :: VolumeConfiguration)
{-# DEPRECATED vcNumberOfDisks "Use generic-lens or generic-optics with 'numberOfDisks' instead." #-}

-- | The volume size.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSize :: Lens.Lens' VolumeConfiguration Lude.Int
vcSize = Lens.lens (size :: VolumeConfiguration -> Lude.Int) (\s a -> s {size = a} :: VolumeConfiguration)
{-# DEPRECATED vcSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Lude.FromJSON VolumeConfiguration where
  parseJSON =
    Lude.withObject
      "VolumeConfiguration"
      ( \x ->
          VolumeConfiguration'
            Lude.<$> (x Lude..:? "Iops")
            Lude.<*> (x Lude..:? "RaidLevel")
            Lude.<*> (x Lude..:? "Encrypted")
            Lude.<*> (x Lude..:? "VolumeType")
            Lude.<*> (x Lude..: "MountPoint")
            Lude.<*> (x Lude..: "NumberOfDisks")
            Lude.<*> (x Lude..: "Size")
      )

instance Lude.ToJSON VolumeConfiguration where
  toJSON VolumeConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Iops" Lude..=) Lude.<$> iops,
            ("RaidLevel" Lude..=) Lude.<$> raidLevel,
            ("Encrypted" Lude..=) Lude.<$> encrypted,
            ("VolumeType" Lude..=) Lude.<$> volumeType,
            Lude.Just ("MountPoint" Lude..= mountPoint),
            Lude.Just ("NumberOfDisks" Lude..= numberOfDisks),
            Lude.Just ("Size" Lude..= size)
          ]
      )
