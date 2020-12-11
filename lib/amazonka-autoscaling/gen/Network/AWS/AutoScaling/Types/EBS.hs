-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.EBS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.EBS
  ( EBS (..),

    -- * Smart constructor
    mkEBS,

    -- * Lenses
    ebsDeleteOnTermination,
    ebsVolumeSize,
    ebsIOPS,
    ebsEncrypted,
    ebsVolumeType,
    ebsSnapshotId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes information used to set up an Amazon EBS volume specified in a block device mapping.
--
-- /See:/ 'mkEBS' smart constructor.
data EBS = EBS'
  { deleteOnTermination :: Lude.Maybe Lude.Bool,
    volumeSize :: Lude.Maybe Lude.Natural,
    iops :: Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'EBS' with the minimum fields required to make a request.
--
-- * 'deleteOnTermination' - Indicates whether the volume is deleted on instance termination. For Amazon EC2 Auto Scaling, the default value is @true@ .
-- * 'encrypted' - Specifies whether the volume should be encrypted. Encrypted EBS volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported Instance Types> . If your AMI uses encrypted volumes, you can also only launch it on supported instance types.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIEncryption.html Using Encryption with EBS-Backed AMIs> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/key-policy-requirements-EBS-encryption.html Required CMK key policy for use with encrypted volumes> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'iops' - The number of I/O operations per second (IOPS) to provision for the volume. The maximum ratio of IOPS to volume size (in GiB) is 50:1. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Required when the volume type is @io1@ . (Not used with @standard@ , @gp2@ , @st1@ , or @sc1@ volumes.)
-- * 'snapshotId' - The snapshot ID of the volume to use.
--
-- You must specify either a @VolumeSize@ or a @SnapshotId@ .
-- * 'volumeSize' - The volume size, in Gibibytes (GiB).
--
-- This can be a number from 1-1,024 for @standard@ , 4-16,384 for @io1@ , 1-16,384 for @gp2@ , and 500-16,384 for @st1@ and @sc1@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
-- Default: If you create a volume from a snapshot and you don't specify a volume size, the default is the snapshot size.
-- You must specify either a @VolumeSize@ or a @SnapshotId@ . If you specify both @SnapshotId@ and @VolumeSize@ , the volume size must be equal or greater than the size of the snapshot.
-- * 'volumeType' - The volume type, which can be @standard@ for Magnetic, @io1@ for Provisioned IOPS SSD, @gp2@ for General Purpose SSD, @st1@ for Throughput Optimized HDD, or @sc1@ for Cold HDD. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Valid Values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@
mkEBS ::
  EBS
mkEBS =
  EBS'
    { deleteOnTermination = Lude.Nothing,
      volumeSize = Lude.Nothing,
      iops = Lude.Nothing,
      encrypted = Lude.Nothing,
      volumeType = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | Indicates whether the volume is deleted on instance termination. For Amazon EC2 Auto Scaling, the default value is @true@ .
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsDeleteOnTermination :: Lens.Lens' EBS (Lude.Maybe Lude.Bool)
ebsDeleteOnTermination = Lens.lens (deleteOnTermination :: EBS -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: EBS)
{-# DEPRECATED ebsDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The volume size, in Gibibytes (GiB).
--
-- This can be a number from 1-1,024 for @standard@ , 4-16,384 for @io1@ , 1-16,384 for @gp2@ , and 500-16,384 for @st1@ and @sc1@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
-- Default: If you create a volume from a snapshot and you don't specify a volume size, the default is the snapshot size.
-- You must specify either a @VolumeSize@ or a @SnapshotId@ . If you specify both @SnapshotId@ and @VolumeSize@ , the volume size must be equal or greater than the size of the snapshot.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsVolumeSize :: Lens.Lens' EBS (Lude.Maybe Lude.Natural)
ebsVolumeSize = Lens.lens (volumeSize :: EBS -> Lude.Maybe Lude.Natural) (\s a -> s {volumeSize = a} :: EBS)
{-# DEPRECATED ebsVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

-- | The number of I/O operations per second (IOPS) to provision for the volume. The maximum ratio of IOPS to volume size (in GiB) is 50:1. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Required when the volume type is @io1@ . (Not used with @standard@ , @gp2@ , @st1@ , or @sc1@ volumes.)
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsIOPS :: Lens.Lens' EBS (Lude.Maybe Lude.Natural)
ebsIOPS = Lens.lens (iops :: EBS -> Lude.Maybe Lude.Natural) (\s a -> s {iops = a} :: EBS)
{-# DEPRECATED ebsIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Specifies whether the volume should be encrypted. Encrypted EBS volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported Instance Types> . If your AMI uses encrypted volumes, you can also only launch it on supported instance types.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIEncryption.html Using Encryption with EBS-Backed AMIs> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/key-policy-requirements-EBS-encryption.html Required CMK key policy for use with encrypted volumes> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsEncrypted :: Lens.Lens' EBS (Lude.Maybe Lude.Bool)
ebsEncrypted = Lens.lens (encrypted :: EBS -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: EBS)
{-# DEPRECATED ebsEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The volume type, which can be @standard@ for Magnetic, @io1@ for Provisioned IOPS SSD, @gp2@ for General Purpose SSD, @st1@ for Throughput Optimized HDD, or @sc1@ for Cold HDD. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Valid Values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsVolumeType :: Lens.Lens' EBS (Lude.Maybe Lude.Text)
ebsVolumeType = Lens.lens (volumeType :: EBS -> Lude.Maybe Lude.Text) (\s a -> s {volumeType = a} :: EBS)
{-# DEPRECATED ebsVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The snapshot ID of the volume to use.
--
-- You must specify either a @VolumeSize@ or a @SnapshotId@ .
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsSnapshotId :: Lens.Lens' EBS (Lude.Maybe Lude.Text)
ebsSnapshotId = Lens.lens (snapshotId :: EBS -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: EBS)
{-# DEPRECATED ebsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromXML EBS where
  parseXML x =
    EBS'
      Lude.<$> (x Lude..@? "DeleteOnTermination")
      Lude.<*> (x Lude..@? "VolumeSize")
      Lude.<*> (x Lude..@? "Iops")
      Lude.<*> (x Lude..@? "Encrypted")
      Lude.<*> (x Lude..@? "VolumeType")
      Lude.<*> (x Lude..@? "SnapshotId")

instance Lude.ToQuery EBS where
  toQuery EBS' {..} =
    Lude.mconcat
      [ "DeleteOnTermination" Lude.=: deleteOnTermination,
        "VolumeSize" Lude.=: volumeSize,
        "Iops" Lude.=: iops,
        "Encrypted" Lude.=: encrypted,
        "VolumeType" Lude.=: volumeType,
        "SnapshotId" Lude.=: snapshotId
      ]
