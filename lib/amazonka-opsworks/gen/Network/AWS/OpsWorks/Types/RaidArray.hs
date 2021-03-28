{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.RaidArray
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.RaidArray
  ( RaidArray (..)
  -- * Smart constructor
  , mkRaidArray
  -- * Lenses
  , raAvailabilityZone
  , raCreatedAt
  , raDevice
  , raInstanceId
  , raIops
  , raMountPoint
  , raName
  , raNumberOfDisks
  , raRaidArrayId
  , raRaidLevel
  , raSize
  , raStackId
  , raVolumeType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.CreatedAt as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an instance's RAID array.
--
-- /See:/ 'mkRaidArray' smart constructor.
data RaidArray = RaidArray'
  { availabilityZone :: Core.Maybe Core.Text
    -- ^ The array's Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
  , createdAt :: Core.Maybe Types.CreatedAt
    -- ^ When the RAID array was created.
  , device :: Core.Maybe Core.Text
    -- ^ The array's Linux device. For example /dev/mdadm0.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The instance ID.
  , iops :: Core.Maybe Core.Int
    -- ^ For PIOPS volumes, the IOPS per disk.
  , mountPoint :: Core.Maybe Core.Text
    -- ^ The array's mount point.
  , name :: Core.Maybe Core.Text
    -- ^ The array name.
  , numberOfDisks :: Core.Maybe Core.Int
    -- ^ The number of disks in the array.
  , raidArrayId :: Core.Maybe Core.Text
    -- ^ The array ID.
  , raidLevel :: Core.Maybe Core.Int
    -- ^ The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
  , size :: Core.Maybe Core.Int
    -- ^ The array's size.
  , stackId :: Core.Maybe Core.Text
    -- ^ The stack ID.
  , volumeType :: Core.Maybe Core.Text
    -- ^ The volume type, standard or PIOPS.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RaidArray' value with any optional fields omitted.
mkRaidArray
    :: RaidArray
mkRaidArray
  = RaidArray'{availabilityZone = Core.Nothing,
               createdAt = Core.Nothing, device = Core.Nothing,
               instanceId = Core.Nothing, iops = Core.Nothing,
               mountPoint = Core.Nothing, name = Core.Nothing,
               numberOfDisks = Core.Nothing, raidArrayId = Core.Nothing,
               raidLevel = Core.Nothing, size = Core.Nothing,
               stackId = Core.Nothing, volumeType = Core.Nothing}

-- | The array's Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAvailabilityZone :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE raAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | When the RAID array was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raCreatedAt :: Lens.Lens' RaidArray (Core.Maybe Types.CreatedAt)
raCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE raCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The array's Linux device. For example /dev/mdadm0.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raDevice :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raDevice = Lens.field @"device"
{-# INLINEABLE raDevice #-}
{-# DEPRECATED device "Use generic-lens or generic-optics with 'device' instead"  #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raInstanceId :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raInstanceId = Lens.field @"instanceId"
{-# INLINEABLE raInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | For PIOPS volumes, the IOPS per disk.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raIops :: Lens.Lens' RaidArray (Core.Maybe Core.Int)
raIops = Lens.field @"iops"
{-# INLINEABLE raIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | The array's mount point.
--
-- /Note:/ Consider using 'mountPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raMountPoint :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raMountPoint = Lens.field @"mountPoint"
{-# INLINEABLE raMountPoint #-}
{-# DEPRECATED mountPoint "Use generic-lens or generic-optics with 'mountPoint' instead"  #-}

-- | The array name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raName :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raName = Lens.field @"name"
{-# INLINEABLE raName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The number of disks in the array.
--
-- /Note:/ Consider using 'numberOfDisks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raNumberOfDisks :: Lens.Lens' RaidArray (Core.Maybe Core.Int)
raNumberOfDisks = Lens.field @"numberOfDisks"
{-# INLINEABLE raNumberOfDisks #-}
{-# DEPRECATED numberOfDisks "Use generic-lens or generic-optics with 'numberOfDisks' instead"  #-}

-- | The array ID.
--
-- /Note:/ Consider using 'raidArrayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raRaidArrayId :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raRaidArrayId = Lens.field @"raidArrayId"
{-# INLINEABLE raRaidArrayId #-}
{-# DEPRECATED raidArrayId "Use generic-lens or generic-optics with 'raidArrayId' instead"  #-}

-- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
--
-- /Note:/ Consider using 'raidLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raRaidLevel :: Lens.Lens' RaidArray (Core.Maybe Core.Int)
raRaidLevel = Lens.field @"raidLevel"
{-# INLINEABLE raRaidLevel #-}
{-# DEPRECATED raidLevel "Use generic-lens or generic-optics with 'raidLevel' instead"  #-}

-- | The array's size.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raSize :: Lens.Lens' RaidArray (Core.Maybe Core.Int)
raSize = Lens.field @"size"
{-# INLINEABLE raSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raStackId :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raStackId = Lens.field @"stackId"
{-# INLINEABLE raStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The volume type, standard or PIOPS.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raVolumeType :: Lens.Lens' RaidArray (Core.Maybe Core.Text)
raVolumeType = Lens.field @"volumeType"
{-# INLINEABLE raVolumeType #-}
{-# DEPRECATED volumeType "Use generic-lens or generic-optics with 'volumeType' instead"  #-}

instance Core.FromJSON RaidArray where
        parseJSON
          = Core.withObject "RaidArray" Core.$
              \ x ->
                RaidArray' Core.<$>
                  (x Core..:? "AvailabilityZone") Core.<*> x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "Device"
                    Core.<*> x Core..:? "InstanceId"
                    Core.<*> x Core..:? "Iops"
                    Core.<*> x Core..:? "MountPoint"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "NumberOfDisks"
                    Core.<*> x Core..:? "RaidArrayId"
                    Core.<*> x Core..:? "RaidLevel"
                    Core.<*> x Core..:? "Size"
                    Core.<*> x Core..:? "StackId"
                    Core.<*> x Core..:? "VolumeType"
