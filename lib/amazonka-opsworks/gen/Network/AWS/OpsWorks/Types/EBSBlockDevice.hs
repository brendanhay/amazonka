-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.EBSBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.EBSBlockDevice
  ( EBSBlockDevice (..),

    -- * Smart constructor
    mkEBSBlockDevice,

    -- * Lenses
    ebdDeleteOnTermination,
    ebdVolumeSize,
    ebdIOPS,
    ebdVolumeType,
    ebdSnapshotId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.VolumeType
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon EBS volume. This data type maps directly to the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> data type.
--
-- /See:/ 'mkEBSBlockDevice' smart constructor.
data EBSBlockDevice = EBSBlockDevice'
  { deleteOnTermination ::
      Lude.Maybe Lude.Bool,
    volumeSize :: Lude.Maybe Lude.Int,
    iops :: Lude.Maybe Lude.Int,
    volumeType :: Lude.Maybe VolumeType,
    snapshotId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EBSBlockDevice' with the minimum fields required to make a request.
--
-- * 'deleteOnTermination' - Whether the volume is deleted on instance termination.
-- * 'iops' - The number of I/O operations per second (IOPS) that the volume supports. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
-- * 'snapshotId' - The snapshot ID.
-- * 'volumeSize' - The volume size, in GiB. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
-- * 'volumeType' - The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes.
--
-- If you specify the @io1@ volume type, you must also specify a value for the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB) specified in the AMI attributes to set IOPS to 50 x (volume size).
mkEBSBlockDevice ::
  EBSBlockDevice
mkEBSBlockDevice =
  EBSBlockDevice'
    { deleteOnTermination = Lude.Nothing,
      volumeSize = Lude.Nothing,
      iops = Lude.Nothing,
      volumeType = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | Whether the volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdDeleteOnTermination :: Lens.Lens' EBSBlockDevice (Lude.Maybe Lude.Bool)
ebdDeleteOnTermination = Lens.lens (deleteOnTermination :: EBSBlockDevice -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: EBSBlockDevice)
{-# DEPRECATED ebdDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The volume size, in GiB. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdVolumeSize :: Lens.Lens' EBSBlockDevice (Lude.Maybe Lude.Int)
ebdVolumeSize = Lens.lens (volumeSize :: EBSBlockDevice -> Lude.Maybe Lude.Int) (\s a -> s {volumeSize = a} :: EBSBlockDevice)
{-# DEPRECATED ebdVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

-- | The number of I/O operations per second (IOPS) that the volume supports. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdIOPS :: Lens.Lens' EBSBlockDevice (Lude.Maybe Lude.Int)
ebdIOPS = Lens.lens (iops :: EBSBlockDevice -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: EBSBlockDevice)
{-# DEPRECATED ebdIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes.
--
-- If you specify the @io1@ volume type, you must also specify a value for the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB) specified in the AMI attributes to set IOPS to 50 x (volume size).
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdVolumeType :: Lens.Lens' EBSBlockDevice (Lude.Maybe VolumeType)
ebdVolumeType = Lens.lens (volumeType :: EBSBlockDevice -> Lude.Maybe VolumeType) (\s a -> s {volumeType = a} :: EBSBlockDevice)
{-# DEPRECATED ebdVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The snapshot ID.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdSnapshotId :: Lens.Lens' EBSBlockDevice (Lude.Maybe Lude.Text)
ebdSnapshotId = Lens.lens (snapshotId :: EBSBlockDevice -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: EBSBlockDevice)
{-# DEPRECATED ebdSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromJSON EBSBlockDevice where
  parseJSON =
    Lude.withObject
      "EBSBlockDevice"
      ( \x ->
          EBSBlockDevice'
            Lude.<$> (x Lude..:? "DeleteOnTermination")
            Lude.<*> (x Lude..:? "VolumeSize")
            Lude.<*> (x Lude..:? "Iops")
            Lude.<*> (x Lude..:? "VolumeType")
            Lude.<*> (x Lude..:? "SnapshotId")
      )

instance Lude.ToJSON EBSBlockDevice where
  toJSON EBSBlockDevice' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeleteOnTermination" Lude..=) Lude.<$> deleteOnTermination,
            ("VolumeSize" Lude..=) Lude.<$> volumeSize,
            ("Iops" Lude..=) Lude.<$> iops,
            ("VolumeType" Lude..=) Lude.<$> volumeType,
            ("SnapshotId" Lude..=) Lude.<$> snapshotId
          ]
      )
