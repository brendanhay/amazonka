{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Volume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Volume
  ( Volume (..),

    -- * Smart constructor
    mkVolume,

    -- * Lenses
    vInstanceId,
    vStatus,
    vSize,
    vIOPS,
    vDevice,
    vEncrypted,
    vAvailabilityZone,
    vName,
    vRAIDArrayId,
    vVolumeId,
    vRegion,
    vVolumeType,
    vEC2VolumeId,
    vMountPoint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance's Amazon EBS volume.
--
-- /See:/ 'mkVolume' smart constructor.
data Volume = Volume'
  { instanceId :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    size :: Lude.Maybe Lude.Int,
    iops :: Lude.Maybe Lude.Int,
    device :: Lude.Maybe Lude.Text,
    encrypted :: Lude.Maybe Lude.Bool,
    availabilityZone :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    raidArrayId :: Lude.Maybe Lude.Text,
    volumeId :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    volumeType :: Lude.Maybe Lude.Text,
    ec2VolumeId :: Lude.Maybe Lude.Text,
    mountPoint :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The volume Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
-- * 'device' - The device name.
-- * 'ec2VolumeId' - The Amazon EC2 volume ID.
-- * 'encrypted' - Specifies whether an Amazon EBS volume is encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
-- * 'instanceId' - The instance ID.
-- * 'iops' - For PIOPS volumes, the IOPS per disk.
-- * 'mountPoint' - The volume mount point. For example, "/mnt/disk1".
-- * 'name' - The volume name.
-- * 'raidArrayId' - The RAID array ID.
-- * 'region' - The AWS region. For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
-- * 'size' - The volume size.
-- * 'status' - The value returned by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes> .
-- * 'volumeId' - The volume ID.
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
mkVolume ::
  Volume
mkVolume =
  Volume'
    { instanceId = Lude.Nothing,
      status = Lude.Nothing,
      size = Lude.Nothing,
      iops = Lude.Nothing,
      device = Lude.Nothing,
      encrypted = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      name = Lude.Nothing,
      raidArrayId = Lude.Nothing,
      volumeId = Lude.Nothing,
      region = Lude.Nothing,
      volumeType = Lude.Nothing,
      ec2VolumeId = Lude.Nothing,
      mountPoint = Lude.Nothing
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vInstanceId :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vInstanceId = Lens.lens (instanceId :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: Volume)
{-# DEPRECATED vInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The value returned by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes> .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vStatus :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vStatus = Lens.lens (status :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Volume)
{-# DEPRECATED vStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The volume size.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSize :: Lens.Lens' Volume (Lude.Maybe Lude.Int)
vSize = Lens.lens (size :: Volume -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: Volume)
{-# DEPRECATED vSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | For PIOPS volumes, the IOPS per disk.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vIOPS :: Lens.Lens' Volume (Lude.Maybe Lude.Int)
vIOPS = Lens.lens (iops :: Volume -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: Volume)
{-# DEPRECATED vIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The device name.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vDevice :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vDevice = Lens.lens (device :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {device = a} :: Volume)
{-# DEPRECATED vDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | Specifies whether an Amazon EBS volume is encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vEncrypted :: Lens.Lens' Volume (Lude.Maybe Lude.Bool)
vEncrypted = Lens.lens (encrypted :: Volume -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: Volume)
{-# DEPRECATED vEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The volume Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vAvailabilityZone :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vAvailabilityZone = Lens.lens (availabilityZone :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: Volume)
{-# DEPRECATED vAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The volume name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vName :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vName = Lens.lens (name :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Volume)
{-# DEPRECATED vName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The RAID array ID.
--
-- /Note:/ Consider using 'raidArrayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vRAIDArrayId :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vRAIDArrayId = Lens.lens (raidArrayId :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {raidArrayId = a} :: Volume)
{-# DEPRECATED vRAIDArrayId "Use generic-lens or generic-optics with 'raidArrayId' instead." #-}

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vVolumeId :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vVolumeId = Lens.lens (volumeId :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: Volume)
{-# DEPRECATED vVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The AWS region. For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vRegion :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vRegion = Lens.lens (region :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: Volume)
{-# DEPRECATED vRegion "Use generic-lens or generic-optics with 'region' instead." #-}

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
vVolumeType :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vVolumeType = Lens.lens (volumeType :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {volumeType = a} :: Volume)
{-# DEPRECATED vVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The Amazon EC2 volume ID.
--
-- /Note:/ Consider using 'ec2VolumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vEC2VolumeId :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vEC2VolumeId = Lens.lens (ec2VolumeId :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {ec2VolumeId = a} :: Volume)
{-# DEPRECATED vEC2VolumeId "Use generic-lens or generic-optics with 'ec2VolumeId' instead." #-}

-- | The volume mount point. For example, "/mnt/disk1".
--
-- /Note:/ Consider using 'mountPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vMountPoint :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vMountPoint = Lens.lens (mountPoint :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {mountPoint = a} :: Volume)
{-# DEPRECATED vMountPoint "Use generic-lens or generic-optics with 'mountPoint' instead." #-}

instance Lude.FromJSON Volume where
  parseJSON =
    Lude.withObject
      "Volume"
      ( \x ->
          Volume'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Size")
            Lude.<*> (x Lude..:? "Iops")
            Lude.<*> (x Lude..:? "Device")
            Lude.<*> (x Lude..:? "Encrypted")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "RaidArrayId")
            Lude.<*> (x Lude..:? "VolumeId")
            Lude.<*> (x Lude..:? "Region")
            Lude.<*> (x Lude..:? "VolumeType")
            Lude.<*> (x Lude..:? "Ec2VolumeId")
            Lude.<*> (x Lude..:? "MountPoint")
      )
