{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Ebs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.Ebs
  ( Ebs (..)
  -- * Smart constructor
  , mkEbs
  -- * Lenses
  , eDeleteOnTermination
  , eEncrypted
  , eIops
  , eSnapshotId
  , eVolumeSize
  , eVolumeType
  ) where

import qualified Network.AWS.AutoScaling.Types.VolumeType as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes information used to set up an Amazon EBS volume specified in a block device mapping.
--
-- /See:/ 'mkEbs' smart constructor.
data Ebs = Ebs'
  { deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ Indicates whether the volume is deleted on instance termination. For Amazon EC2 Auto Scaling, the default value is @true@ .
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Specifies whether the volume should be encrypted. Encrypted EBS volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported Instance Types> . If your AMI uses encrypted volumes, you can also only launch it on supported instance types.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIEncryption.html Using Encryption with EBS-Backed AMIs> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/key-policy-requirements-EBS-encryption.html Required CMK key policy for use with encrypted volumes> in the /Amazon EC2 Auto Scaling User Guide/ .
  , iops :: Core.Maybe Core.Natural
    -- ^ The number of I/O operations per second (IOPS) to provision for the volume. The maximum ratio of IOPS to volume size (in GiB) is 50:1. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Required when the volume type is @io1@ . (Not used with @standard@ , @gp2@ , @st1@ , or @sc1@ volumes.) 
  , snapshotId :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The snapshot ID of the volume to use.
--
-- You must specify either a @VolumeSize@ or a @SnapshotId@ .
  , volumeSize :: Core.Maybe Core.Natural
    -- ^ The volume size, in Gibibytes (GiB).
--
-- This can be a number from 1-1,024 for @standard@ , 4-16,384 for @io1@ , 1-16,384 for @gp2@ , and 500-16,384 for @st1@ and @sc1@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
-- Default: If you create a volume from a snapshot and you don't specify a volume size, the default is the snapshot size.
-- You must specify either a @VolumeSize@ or a @SnapshotId@ . If you specify both @SnapshotId@ and @VolumeSize@ , the volume size must be equal or greater than the size of the snapshot.
  , volumeType :: Core.Maybe Types.VolumeType
    -- ^ The volume type, which can be @standard@ for Magnetic, @io1@ for Provisioned IOPS SSD, @gp2@ for General Purpose SSD, @st1@ for Throughput Optimized HDD, or @sc1@ for Cold HDD. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Valid Values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Ebs' value with any optional fields omitted.
mkEbs
    :: Ebs
mkEbs
  = Ebs'{deleteOnTermination = Core.Nothing,
         encrypted = Core.Nothing, iops = Core.Nothing,
         snapshotId = Core.Nothing, volumeSize = Core.Nothing,
         volumeType = Core.Nothing}

-- | Indicates whether the volume is deleted on instance termination. For Amazon EC2 Auto Scaling, the default value is @true@ .
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDeleteOnTermination :: Lens.Lens' Ebs (Core.Maybe Core.Bool)
eDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE eDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | Specifies whether the volume should be encrypted. Encrypted EBS volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported Instance Types> . If your AMI uses encrypted volumes, you can also only launch it on supported instance types.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIEncryption.html Using Encryption with EBS-Backed AMIs> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/key-policy-requirements-EBS-encryption.html Required CMK key policy for use with encrypted volumes> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEncrypted :: Lens.Lens' Ebs (Core.Maybe Core.Bool)
eEncrypted = Lens.field @"encrypted"
{-# INLINEABLE eEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The number of I/O operations per second (IOPS) to provision for the volume. The maximum ratio of IOPS to volume size (in GiB) is 50:1. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Required when the volume type is @io1@ . (Not used with @standard@ , @gp2@ , @st1@ , or @sc1@ volumes.) 
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eIops :: Lens.Lens' Ebs (Core.Maybe Core.Natural)
eIops = Lens.field @"iops"
{-# INLINEABLE eIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | The snapshot ID of the volume to use.
--
-- You must specify either a @VolumeSize@ or a @SnapshotId@ .
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSnapshotId :: Lens.Lens' Ebs (Core.Maybe Types.XmlStringMaxLen255)
eSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE eSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The volume size, in Gibibytes (GiB).
--
-- This can be a number from 1-1,024 for @standard@ , 4-16,384 for @io1@ , 1-16,384 for @gp2@ , and 500-16,384 for @st1@ and @sc1@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
-- Default: If you create a volume from a snapshot and you don't specify a volume size, the default is the snapshot size.
-- You must specify either a @VolumeSize@ or a @SnapshotId@ . If you specify both @SnapshotId@ and @VolumeSize@ , the volume size must be equal or greater than the size of the snapshot.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eVolumeSize :: Lens.Lens' Ebs (Core.Maybe Core.Natural)
eVolumeSize = Lens.field @"volumeSize"
{-# INLINEABLE eVolumeSize #-}
{-# DEPRECATED volumeSize "Use generic-lens or generic-optics with 'volumeSize' instead"  #-}

-- | The volume type, which can be @standard@ for Magnetic, @io1@ for Provisioned IOPS SSD, @gp2@ for General Purpose SSD, @st1@ for Throughput Optimized HDD, or @sc1@ for Cold HDD. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Valid Values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@ 
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eVolumeType :: Lens.Lens' Ebs (Core.Maybe Types.VolumeType)
eVolumeType = Lens.field @"volumeType"
{-# INLINEABLE eVolumeType #-}
{-# DEPRECATED volumeType "Use generic-lens or generic-optics with 'volumeType' instead"  #-}

instance Core.ToQuery Ebs where
        toQuery Ebs{..}
          = Core.maybe Core.mempty (Core.toQueryPair "DeleteOnTermination")
              deleteOnTermination
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Encrypted") encrypted
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Iops") iops
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotId") snapshotId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VolumeSize") volumeSize
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VolumeType") volumeType

instance Core.FromXML Ebs where
        parseXML x
          = Ebs' Core.<$>
              (x Core..@? "DeleteOnTermination") Core.<*> x Core..@? "Encrypted"
                Core.<*> x Core..@? "Iops"
                Core.<*> x Core..@? "SnapshotId"
                Core.<*> x Core..@? "VolumeSize"
                Core.<*> x Core..@? "VolumeType"
