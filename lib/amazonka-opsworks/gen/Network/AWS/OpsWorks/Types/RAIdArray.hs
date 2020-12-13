{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.RAIdArray
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.RAIdArray
  ( RAIDArray (..),

    -- * Smart constructor
    mkRAIDArray,

    -- * Lenses
    raiaInstanceId,
    raiaSize,
    raiaIOPS,
    raiaCreatedAt,
    raiaRAIDLevel,
    raiaDevice,
    raiaNumberOfDisks,
    raiaAvailabilityZone,
    raiaName,
    raiaRAIDArrayId,
    raiaVolumeType,
    raiaStackId,
    raiaMountPoint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance's RAID array.
--
-- /See:/ 'mkRAIDArray' smart constructor.
data RAIDArray = RAIDArray'
  { -- | The instance ID.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The array's size.
    size :: Lude.Maybe Lude.Int,
    -- | For PIOPS volumes, the IOPS per disk.
    iops :: Lude.Maybe Lude.Int,
    -- | When the RAID array was created.
    createdAt :: Lude.Maybe Lude.Text,
    -- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
    raidLevel :: Lude.Maybe Lude.Int,
    -- | The array's Linux device. For example /dev/mdadm0.
    device :: Lude.Maybe Lude.Text,
    -- | The number of disks in the array.
    numberOfDisks :: Lude.Maybe Lude.Int,
    -- | The array's Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The array name.
    name :: Lude.Maybe Lude.Text,
    -- | The array ID.
    raidArrayId :: Lude.Maybe Lude.Text,
    -- | The volume type, standard or PIOPS.
    volumeType :: Lude.Maybe Lude.Text,
    -- | The stack ID.
    stackId :: Lude.Maybe Lude.Text,
    -- | The array's mount point.
    mountPoint :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RAIDArray' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID.
-- * 'size' - The array's size.
-- * 'iops' - For PIOPS volumes, the IOPS per disk.
-- * 'createdAt' - When the RAID array was created.
-- * 'raidLevel' - The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
-- * 'device' - The array's Linux device. For example /dev/mdadm0.
-- * 'numberOfDisks' - The number of disks in the array.
-- * 'availabilityZone' - The array's Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
-- * 'name' - The array name.
-- * 'raidArrayId' - The array ID.
-- * 'volumeType' - The volume type, standard or PIOPS.
-- * 'stackId' - The stack ID.
-- * 'mountPoint' - The array's mount point.
mkRAIDArray ::
  RAIDArray
mkRAIDArray =
  RAIDArray'
    { instanceId = Lude.Nothing,
      size = Lude.Nothing,
      iops = Lude.Nothing,
      createdAt = Lude.Nothing,
      raidLevel = Lude.Nothing,
      device = Lude.Nothing,
      numberOfDisks = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      name = Lude.Nothing,
      raidArrayId = Lude.Nothing,
      volumeType = Lude.Nothing,
      stackId = Lude.Nothing,
      mountPoint = Lude.Nothing
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaInstanceId :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Text)
raiaInstanceId = Lens.lens (instanceId :: RAIDArray -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: RAIDArray)
{-# DEPRECATED raiaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The array's size.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaSize :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Int)
raiaSize = Lens.lens (size :: RAIDArray -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: RAIDArray)
{-# DEPRECATED raiaSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | For PIOPS volumes, the IOPS per disk.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaIOPS :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Int)
raiaIOPS = Lens.lens (iops :: RAIDArray -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: RAIDArray)
{-# DEPRECATED raiaIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | When the RAID array was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaCreatedAt :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Text)
raiaCreatedAt = Lens.lens (createdAt :: RAIDArray -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: RAIDArray)
{-# DEPRECATED raiaCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
--
-- /Note:/ Consider using 'raidLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaRAIDLevel :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Int)
raiaRAIDLevel = Lens.lens (raidLevel :: RAIDArray -> Lude.Maybe Lude.Int) (\s a -> s {raidLevel = a} :: RAIDArray)
{-# DEPRECATED raiaRAIDLevel "Use generic-lens or generic-optics with 'raidLevel' instead." #-}

-- | The array's Linux device. For example /dev/mdadm0.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaDevice :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Text)
raiaDevice = Lens.lens (device :: RAIDArray -> Lude.Maybe Lude.Text) (\s a -> s {device = a} :: RAIDArray)
{-# DEPRECATED raiaDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The number of disks in the array.
--
-- /Note:/ Consider using 'numberOfDisks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaNumberOfDisks :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Int)
raiaNumberOfDisks = Lens.lens (numberOfDisks :: RAIDArray -> Lude.Maybe Lude.Int) (\s a -> s {numberOfDisks = a} :: RAIDArray)
{-# DEPRECATED raiaNumberOfDisks "Use generic-lens or generic-optics with 'numberOfDisks' instead." #-}

-- | The array's Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaAvailabilityZone :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Text)
raiaAvailabilityZone = Lens.lens (availabilityZone :: RAIDArray -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: RAIDArray)
{-# DEPRECATED raiaAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The array name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaName :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Text)
raiaName = Lens.lens (name :: RAIDArray -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RAIDArray)
{-# DEPRECATED raiaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The array ID.
--
-- /Note:/ Consider using 'raidArrayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaRAIDArrayId :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Text)
raiaRAIDArrayId = Lens.lens (raidArrayId :: RAIDArray -> Lude.Maybe Lude.Text) (\s a -> s {raidArrayId = a} :: RAIDArray)
{-# DEPRECATED raiaRAIDArrayId "Use generic-lens or generic-optics with 'raidArrayId' instead." #-}

-- | The volume type, standard or PIOPS.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaVolumeType :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Text)
raiaVolumeType = Lens.lens (volumeType :: RAIDArray -> Lude.Maybe Lude.Text) (\s a -> s {volumeType = a} :: RAIDArray)
{-# DEPRECATED raiaVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaStackId :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Text)
raiaStackId = Lens.lens (stackId :: RAIDArray -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: RAIDArray)
{-# DEPRECATED raiaStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The array's mount point.
--
-- /Note:/ Consider using 'mountPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raiaMountPoint :: Lens.Lens' RAIDArray (Lude.Maybe Lude.Text)
raiaMountPoint = Lens.lens (mountPoint :: RAIDArray -> Lude.Maybe Lude.Text) (\s a -> s {mountPoint = a} :: RAIDArray)
{-# DEPRECATED raiaMountPoint "Use generic-lens or generic-optics with 'mountPoint' instead." #-}

instance Lude.FromJSON RAIDArray where
  parseJSON =
    Lude.withObject
      "RAIDArray"
      ( \x ->
          RAIDArray'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "Size")
            Lude.<*> (x Lude..:? "Iops")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "RaidLevel")
            Lude.<*> (x Lude..:? "Device")
            Lude.<*> (x Lude..:? "NumberOfDisks")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "RaidArrayId")
            Lude.<*> (x Lude..:? "VolumeType")
            Lude.<*> (x Lude..:? "StackId")
            Lude.<*> (x Lude..:? "MountPoint")
      )
