{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesEbs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ScheduledInstancesEbs
  ( ScheduledInstancesEbs (..)
  -- * Smart constructor
  , mkScheduledInstancesEbs
  -- * Lenses
  , sieDeleteOnTermination
  , sieEncrypted
  , sieIops
  , sieSnapshotId
  , sieVolumeSize
  , sieVolumeType
  ) where

import qualified Network.AWS.EC2.Types.SnapshotId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an EBS volume for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesEbs' smart constructor.
data ScheduledInstancesEbs = ScheduledInstancesEbs'
  { deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ Indicates whether the volume is deleted on instance termination.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the volume is encrypted. You can attached encrypted volumes only to instances that support them.
  , iops :: Core.Maybe Core.Int
    -- ^ The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
  , snapshotId :: Core.Maybe Types.SnapshotId
    -- ^ The ID of the snapshot.
  , volumeSize :: Core.Maybe Core.Int
    -- ^ The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
  , volumeType :: Core.Maybe Core.Text
    -- ^ The volume type. @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, Throughput Optimized HDD for @st1@ , Cold HDD for @sc1@ , or @standard@ for Magnetic.
--
-- Default: @gp2@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledInstancesEbs' value with any optional fields omitted.
mkScheduledInstancesEbs
    :: ScheduledInstancesEbs
mkScheduledInstancesEbs
  = ScheduledInstancesEbs'{deleteOnTermination = Core.Nothing,
                           encrypted = Core.Nothing, iops = Core.Nothing,
                           snapshotId = Core.Nothing, volumeSize = Core.Nothing,
                           volumeType = Core.Nothing}

-- | Indicates whether the volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieDeleteOnTermination :: Lens.Lens' ScheduledInstancesEbs (Core.Maybe Core.Bool)
sieDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE sieDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | Indicates whether the volume is encrypted. You can attached encrypted volumes only to instances that support them.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieEncrypted :: Lens.Lens' ScheduledInstancesEbs (Core.Maybe Core.Bool)
sieEncrypted = Lens.field @"encrypted"
{-# INLINEABLE sieEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieIops :: Lens.Lens' ScheduledInstancesEbs (Core.Maybe Core.Int)
sieIops = Lens.field @"iops"
{-# INLINEABLE sieIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieSnapshotId :: Lens.Lens' ScheduledInstancesEbs (Core.Maybe Types.SnapshotId)
sieSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE sieSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieVolumeSize :: Lens.Lens' ScheduledInstancesEbs (Core.Maybe Core.Int)
sieVolumeSize = Lens.field @"volumeSize"
{-# INLINEABLE sieVolumeSize #-}
{-# DEPRECATED volumeSize "Use generic-lens or generic-optics with 'volumeSize' instead"  #-}

-- | The volume type. @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, Throughput Optimized HDD for @st1@ , Cold HDD for @sc1@ , or @standard@ for Magnetic.
--
-- Default: @gp2@ 
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sieVolumeType :: Lens.Lens' ScheduledInstancesEbs (Core.Maybe Core.Text)
sieVolumeType = Lens.field @"volumeType"
{-# INLINEABLE sieVolumeType #-}
{-# DEPRECATED volumeType "Use generic-lens or generic-optics with 'volumeType' instead"  #-}

instance Core.ToQuery ScheduledInstancesEbs where
        toQuery ScheduledInstancesEbs{..}
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
