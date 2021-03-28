{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Volume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Volume
  ( Volume (..)
  -- * Smart constructor
  , mkVolume
  -- * Lenses
  , vAvailabilityZone
  , vDevice
  , vEc2VolumeId
  , vEncrypted
  , vInstanceId
  , vIops
  , vMountPoint
  , vName
  , vRaidArrayId
  , vRegion
  , vSize
  , vStatus
  , vVolumeId
  , vVolumeType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance's Amazon EBS volume.
--
-- /See:/ 'mkVolume' smart constructor.
data Volume = Volume'
  { availabilityZone :: Core.Maybe Core.Text
    -- ^ The volume Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
  , device :: Core.Maybe Core.Text
    -- ^ The device name.
  , ec2VolumeId :: Core.Maybe Core.Text
    -- ^ The Amazon EC2 volume ID.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Specifies whether an Amazon EBS volume is encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
  , instanceId :: Core.Maybe Core.Text
    -- ^ The instance ID.
  , iops :: Core.Maybe Core.Int
    -- ^ For PIOPS volumes, the IOPS per disk.
  , mountPoint :: Core.Maybe Core.Text
    -- ^ The volume mount point. For example, "/mnt/disk1".
  , name :: Core.Maybe Core.Text
    -- ^ The volume name.
  , raidArrayId :: Core.Maybe Core.Text
    -- ^ The RAID array ID.
  , region :: Core.Maybe Core.Text
    -- ^ The AWS region. For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
  , size :: Core.Maybe Core.Int
    -- ^ The volume size.
  , status :: Core.Maybe Core.Text
    -- ^ The value returned by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes> .
  , volumeId :: Core.Maybe Core.Text
    -- ^ The volume ID.
  , volumeType :: Core.Maybe Core.Text
    -- ^ The volume type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Volume' value with any optional fields omitted.
mkVolume
    :: Volume
mkVolume
  = Volume'{availabilityZone = Core.Nothing, device = Core.Nothing,
            ec2VolumeId = Core.Nothing, encrypted = Core.Nothing,
            instanceId = Core.Nothing, iops = Core.Nothing,
            mountPoint = Core.Nothing, name = Core.Nothing,
            raidArrayId = Core.Nothing, region = Core.Nothing,
            size = Core.Nothing, status = Core.Nothing,
            volumeId = Core.Nothing, volumeType = Core.Nothing}

-- | The volume Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vAvailabilityZone :: Lens.Lens' Volume (Core.Maybe Core.Text)
vAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE vAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The device name.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vDevice :: Lens.Lens' Volume (Core.Maybe Core.Text)
vDevice = Lens.field @"device"
{-# INLINEABLE vDevice #-}
{-# DEPRECATED device "Use generic-lens or generic-optics with 'device' instead"  #-}

-- | The Amazon EC2 volume ID.
--
-- /Note:/ Consider using 'ec2VolumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vEc2VolumeId :: Lens.Lens' Volume (Core.Maybe Core.Text)
vEc2VolumeId = Lens.field @"ec2VolumeId"
{-# INLINEABLE vEc2VolumeId #-}
{-# DEPRECATED ec2VolumeId "Use generic-lens or generic-optics with 'ec2VolumeId' instead"  #-}

-- | Specifies whether an Amazon EBS volume is encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vEncrypted :: Lens.Lens' Volume (Core.Maybe Core.Bool)
vEncrypted = Lens.field @"encrypted"
{-# INLINEABLE vEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vInstanceId :: Lens.Lens' Volume (Core.Maybe Core.Text)
vInstanceId = Lens.field @"instanceId"
{-# INLINEABLE vInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | For PIOPS volumes, the IOPS per disk.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vIops :: Lens.Lens' Volume (Core.Maybe Core.Int)
vIops = Lens.field @"iops"
{-# INLINEABLE vIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | The volume mount point. For example, "/mnt/disk1".
--
-- /Note:/ Consider using 'mountPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vMountPoint :: Lens.Lens' Volume (Core.Maybe Core.Text)
vMountPoint = Lens.field @"mountPoint"
{-# INLINEABLE vMountPoint #-}
{-# DEPRECATED mountPoint "Use generic-lens or generic-optics with 'mountPoint' instead"  #-}

-- | The volume name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vName :: Lens.Lens' Volume (Core.Maybe Core.Text)
vName = Lens.field @"name"
{-# INLINEABLE vName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The RAID array ID.
--
-- /Note:/ Consider using 'raidArrayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vRaidArrayId :: Lens.Lens' Volume (Core.Maybe Core.Text)
vRaidArrayId = Lens.field @"raidArrayId"
{-# INLINEABLE vRaidArrayId #-}
{-# DEPRECATED raidArrayId "Use generic-lens or generic-optics with 'raidArrayId' instead"  #-}

-- | The AWS region. For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vRegion :: Lens.Lens' Volume (Core.Maybe Core.Text)
vRegion = Lens.field @"region"
{-# INLINEABLE vRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The volume size.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSize :: Lens.Lens' Volume (Core.Maybe Core.Int)
vSize = Lens.field @"size"
{-# INLINEABLE vSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

-- | The value returned by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes> .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vStatus :: Lens.Lens' Volume (Core.Maybe Core.Text)
vStatus = Lens.field @"status"
{-# INLINEABLE vStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vVolumeId :: Lens.Lens' Volume (Core.Maybe Core.Text)
vVolumeId = Lens.field @"volumeId"
{-# INLINEABLE vVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

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
vVolumeType :: Lens.Lens' Volume (Core.Maybe Core.Text)
vVolumeType = Lens.field @"volumeType"
{-# INLINEABLE vVolumeType #-}
{-# DEPRECATED volumeType "Use generic-lens or generic-optics with 'volumeType' instead"  #-}

instance Core.FromJSON Volume where
        parseJSON
          = Core.withObject "Volume" Core.$
              \ x ->
                Volume' Core.<$>
                  (x Core..:? "AvailabilityZone") Core.<*> x Core..:? "Device"
                    Core.<*> x Core..:? "Ec2VolumeId"
                    Core.<*> x Core..:? "Encrypted"
                    Core.<*> x Core..:? "InstanceId"
                    Core.<*> x Core..:? "Iops"
                    Core.<*> x Core..:? "MountPoint"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "RaidArrayId"
                    Core.<*> x Core..:? "Region"
                    Core.<*> x Core..:? "Size"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "VolumeId"
                    Core.<*> x Core..:? "VolumeType"
