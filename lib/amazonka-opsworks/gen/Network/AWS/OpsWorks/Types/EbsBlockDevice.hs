{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.EbsBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.EbsBlockDevice
  ( EbsBlockDevice (..)
  -- * Smart constructor
  , mkEbsBlockDevice
  -- * Lenses
  , ebdDeleteOnTermination
  , ebdIops
  , ebdSnapshotId
  , ebdVolumeSize
  , ebdVolumeType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.VolumeType as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon EBS volume. This data type maps directly to the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> data type.
--
-- /See:/ 'mkEbsBlockDevice' smart constructor.
data EbsBlockDevice = EbsBlockDevice'
  { deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ Whether the volume is deleted on instance termination.
  , iops :: Core.Maybe Core.Int
    -- ^ The number of I/O operations per second (IOPS) that the volume supports. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
  , snapshotId :: Core.Maybe Core.Text
    -- ^ The snapshot ID.
  , volumeSize :: Core.Maybe Core.Int
    -- ^ The volume size, in GiB. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
  , volumeType :: Core.Maybe Types.VolumeType
    -- ^ The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes.
--
-- If you specify the @io1@ volume type, you must also specify a value for the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB) specified in the AMI attributes to set IOPS to 50 x (volume size).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EbsBlockDevice' value with any optional fields omitted.
mkEbsBlockDevice
    :: EbsBlockDevice
mkEbsBlockDevice
  = EbsBlockDevice'{deleteOnTermination = Core.Nothing,
                    iops = Core.Nothing, snapshotId = Core.Nothing,
                    volumeSize = Core.Nothing, volumeType = Core.Nothing}

-- | Whether the volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdDeleteOnTermination :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Bool)
ebdDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE ebdDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | The number of I/O operations per second (IOPS) that the volume supports. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdIops :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Int)
ebdIops = Lens.field @"iops"
{-# INLINEABLE ebdIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | The snapshot ID.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdSnapshotId :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Text)
ebdSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE ebdSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The volume size, in GiB. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdVolumeSize :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Int)
ebdVolumeSize = Lens.field @"volumeSize"
{-# INLINEABLE ebdVolumeSize #-}
{-# DEPRECATED volumeSize "Use generic-lens or generic-optics with 'volumeSize' instead"  #-}

-- | The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes.
--
-- If you specify the @io1@ volume type, you must also specify a value for the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB) specified in the AMI attributes to set IOPS to 50 x (volume size).
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdVolumeType :: Lens.Lens' EbsBlockDevice (Core.Maybe Types.VolumeType)
ebdVolumeType = Lens.field @"volumeType"
{-# INLINEABLE ebdVolumeType #-}
{-# DEPRECATED volumeType "Use generic-lens or generic-optics with 'volumeType' instead"  #-}

instance Core.FromJSON EbsBlockDevice where
        toJSON EbsBlockDevice{..}
          = Core.object
              (Core.catMaybes
                 [("DeleteOnTermination" Core..=) Core.<$> deleteOnTermination,
                  ("Iops" Core..=) Core.<$> iops,
                  ("SnapshotId" Core..=) Core.<$> snapshotId,
                  ("VolumeSize" Core..=) Core.<$> volumeSize,
                  ("VolumeType" Core..=) Core.<$> volumeType])

instance Core.FromJSON EbsBlockDevice where
        parseJSON
          = Core.withObject "EbsBlockDevice" Core.$
              \ x ->
                EbsBlockDevice' Core.<$>
                  (x Core..:? "DeleteOnTermination") Core.<*> x Core..:? "Iops"
                    Core.<*> x Core..:? "SnapshotId"
                    Core.<*> x Core..:? "VolumeSize"
                    Core.<*> x Core..:? "VolumeType"
