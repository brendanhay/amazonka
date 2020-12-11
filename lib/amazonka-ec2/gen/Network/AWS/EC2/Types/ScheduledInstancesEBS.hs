-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesEBS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesEBS
  ( ScheduledInstancesEBS (..),

    -- * Smart constructor
    mkScheduledInstancesEBS,

    -- * Lenses
    sieDeleteOnTermination,
    sieVolumeSize,
    sieIOPS,
    sieEncrypted,
    sieVolumeType,
    sieSnapshotId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an EBS volume for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesEBS' smart constructor.
data ScheduledInstancesEBS = ScheduledInstancesEBS'
  { deleteOnTermination ::
      Lude.Maybe Lude.Bool,
    volumeSize :: Lude.Maybe Lude.Int,
    iops :: Lude.Maybe Lude.Int,
    encrypted :: Lude.Maybe Lude.Bool,
    volumeType :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ScheduledInstancesEBS' with the minimum fields required to make a request.
--
-- * 'deleteOnTermination' - Indicates whether the volume is deleted on instance termination.
-- * 'encrypted' - Indicates whether the volume is encrypted. You can attached encrypted volumes only to instances that support them.
-- * 'iops' - The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
-- * 'snapshotId' - The ID of the snapshot.
-- * 'volumeSize' - The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
-- * 'volumeType' - The volume type. @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, Throughput Optimized HDD for @st1@ , Cold HDD for @sc1@ , or @standard@ for Magnetic.
--
-- Default: @gp2@
mkScheduledInstancesEBS ::
  ScheduledInstancesEBS
mkScheduledInstancesEBS =
  ScheduledInstancesEBS'
    { deleteOnTermination = Lude.Nothing,
      volumeSize = Lude.Nothing,
      iops = Lude.Nothing,
      encrypted = Lude.Nothing,
      volumeType = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | Indicates whether the volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieDeleteOnTermination :: Lens.Lens' ScheduledInstancesEBS (Lude.Maybe Lude.Bool)
sieDeleteOnTermination = Lens.lens (deleteOnTermination :: ScheduledInstancesEBS -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: ScheduledInstancesEBS)
{-# DEPRECATED sieDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieVolumeSize :: Lens.Lens' ScheduledInstancesEBS (Lude.Maybe Lude.Int)
sieVolumeSize = Lens.lens (volumeSize :: ScheduledInstancesEBS -> Lude.Maybe Lude.Int) (\s a -> s {volumeSize = a} :: ScheduledInstancesEBS)
{-# DEPRECATED sieVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

-- | The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieIOPS :: Lens.Lens' ScheduledInstancesEBS (Lude.Maybe Lude.Int)
sieIOPS = Lens.lens (iops :: ScheduledInstancesEBS -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: ScheduledInstancesEBS)
{-# DEPRECATED sieIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Indicates whether the volume is encrypted. You can attached encrypted volumes only to instances that support them.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieEncrypted :: Lens.Lens' ScheduledInstancesEBS (Lude.Maybe Lude.Bool)
sieEncrypted = Lens.lens (encrypted :: ScheduledInstancesEBS -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: ScheduledInstancesEBS)
{-# DEPRECATED sieEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The volume type. @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, Throughput Optimized HDD for @st1@ , Cold HDD for @sc1@ , or @standard@ for Magnetic.
--
-- Default: @gp2@
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieVolumeType :: Lens.Lens' ScheduledInstancesEBS (Lude.Maybe Lude.Text)
sieVolumeType = Lens.lens (volumeType :: ScheduledInstancesEBS -> Lude.Maybe Lude.Text) (\s a -> s {volumeType = a} :: ScheduledInstancesEBS)
{-# DEPRECATED sieVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieSnapshotId :: Lens.Lens' ScheduledInstancesEBS (Lude.Maybe Lude.Text)
sieSnapshotId = Lens.lens (snapshotId :: ScheduledInstancesEBS -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: ScheduledInstancesEBS)
{-# DEPRECATED sieSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.ToQuery ScheduledInstancesEBS where
  toQuery ScheduledInstancesEBS' {..} =
    Lude.mconcat
      [ "DeleteOnTermination" Lude.=: deleteOnTermination,
        "VolumeSize" Lude.=: volumeSize,
        "Iops" Lude.=: iops,
        "Encrypted" Lude.=: encrypted,
        "VolumeType" Lude.=: volumeType,
        "SnapshotId" Lude.=: snapshotId
      ]
