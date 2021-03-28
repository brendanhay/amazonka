{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EbsBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EbsBlockDevice
  ( EbsBlockDevice (..)
  -- * Smart constructor
  , mkEbsBlockDevice
  -- * Lenses
  , ebdDeleteOnTermination
  , ebdEncrypted
  , ebdIops
  , ebdKmsKeyId
  , ebdSnapshotId
  , ebdVolumeSize
  , ebdVolumeType
  ) where

import qualified Network.AWS.EC2.Types.VolumeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a block device for an EBS volume.
--
-- /See:/ 'mkEbsBlockDevice' smart constructor.
data EbsBlockDevice = EbsBlockDevice'
  { deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ Indicates whether the EBS volume is deleted on instance termination. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/terminating-instances.html#preserving-volumes-on-termination Preserving Amazon EBS volumes on instance termination> in the Amazon Elastic Compute Cloud User Guide.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the encryption state of an EBS volume is changed while being restored from a backing snapshot. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-parameters Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- In no case can you remove encryption from an encrypted volume.
-- Encrypted volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
-- This parameter is not returned by .
  , iops :: Core.Maybe Core.Int
    -- ^ The number of I/O operations per second (IOPS) that the volume supports. For @io1@ and @io2@ volumes, this represents the number of IOPS that are provisioned for the volume. For @gp2@ volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Constraints: Range is 100-16,000 IOPS for @gp2@ volumes and 100 to 64,000 IOPS for @io1@ and @io2@ volumes in most Regions. Maximum @io1@ and @io2@ IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- Condition: This parameter is required for requests to create @io1@ and @io2@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ Identifier (key ID, key alias, ID ARN, or alias ARN) for a customer managed CMK under which the EBS volume is encrypted.
--
-- This parameter is only supported on @BlockDeviceMapping@ objects called by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> , <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html RequestSpotFleet> , and <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances> .
  , snapshotId :: Core.Maybe Core.Text
    -- ^ The ID of the snapshot.
  , volumeSize :: Core.Maybe Core.Int
    -- ^ The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
-- Constraints: 1-16384 for General Purpose SSD (@gp2@ ), 4-16384 for Provisioned IOPS SSD (@io1@ and @io2@ ), 500-16384 for Throughput Optimized HDD (@st1@ ), 500-16384 for Cold HDD (@sc1@ ), and 1-1024 for Magnetic (@standard@ ) volumes. If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
  , volumeType :: Core.Maybe Types.VolumeType
    -- ^ The volume type. If you set the type to @io1@ or @io2@ , you must also specify the __Iops__ parameter. If you set the type to @gp2@ , @st1@ , @sc1@ , or @standard@ , you must omit the __Iops__ parameter.
--
-- Default: @gp2@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EbsBlockDevice' value with any optional fields omitted.
mkEbsBlockDevice
    :: EbsBlockDevice
mkEbsBlockDevice
  = EbsBlockDevice'{deleteOnTermination = Core.Nothing,
                    encrypted = Core.Nothing, iops = Core.Nothing,
                    kmsKeyId = Core.Nothing, snapshotId = Core.Nothing,
                    volumeSize = Core.Nothing, volumeType = Core.Nothing}

-- | Indicates whether the EBS volume is deleted on instance termination. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/terminating-instances.html#preserving-volumes-on-termination Preserving Amazon EBS volumes on instance termination> in the Amazon Elastic Compute Cloud User Guide.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdDeleteOnTermination :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Bool)
ebdDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE ebdDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | Indicates whether the encryption state of an EBS volume is changed while being restored from a backing snapshot. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-parameters Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- In no case can you remove encryption from an encrypted volume.
-- Encrypted volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
-- This parameter is not returned by .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdEncrypted :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Bool)
ebdEncrypted = Lens.field @"encrypted"
{-# INLINEABLE ebdEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The number of I/O operations per second (IOPS) that the volume supports. For @io1@ and @io2@ volumes, this represents the number of IOPS that are provisioned for the volume. For @gp2@ volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Constraints: Range is 100-16,000 IOPS for @gp2@ volumes and 100 to 64,000 IOPS for @io1@ and @io2@ volumes in most Regions. Maximum @io1@ and @io2@ IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- Condition: This parameter is required for requests to create @io1@ and @io2@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdIops :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Int)
ebdIops = Lens.field @"iops"
{-# INLINEABLE ebdIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | Identifier (key ID, key alias, ID ARN, or alias ARN) for a customer managed CMK under which the EBS volume is encrypted.
--
-- This parameter is only supported on @BlockDeviceMapping@ objects called by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> , <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html RequestSpotFleet> , and <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances> .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdKmsKeyId :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Text)
ebdKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE ebdKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdSnapshotId :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Text)
ebdSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE ebdSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
-- Constraints: 1-16384 for General Purpose SSD (@gp2@ ), 4-16384 for Provisioned IOPS SSD (@io1@ and @io2@ ), 500-16384 for Throughput Optimized HDD (@st1@ ), 500-16384 for Cold HDD (@sc1@ ), and 1-1024 for Magnetic (@standard@ ) volumes. If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdVolumeSize :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Int)
ebdVolumeSize = Lens.field @"volumeSize"
{-# INLINEABLE ebdVolumeSize #-}
{-# DEPRECATED volumeSize "Use generic-lens or generic-optics with 'volumeSize' instead"  #-}

-- | The volume type. If you set the type to @io1@ or @io2@ , you must also specify the __Iops__ parameter. If you set the type to @gp2@ , @st1@ , @sc1@ , or @standard@ , you must omit the __Iops__ parameter.
--
-- Default: @gp2@ 
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdVolumeType :: Lens.Lens' EbsBlockDevice (Core.Maybe Types.VolumeType)
ebdVolumeType = Lens.field @"volumeType"
{-# INLINEABLE ebdVolumeType #-}
{-# DEPRECATED volumeType "Use generic-lens or generic-optics with 'volumeType' instead"  #-}

instance Core.ToQuery EbsBlockDevice where
        toQuery EbsBlockDevice{..}
          = Core.maybe Core.mempty (Core.toQueryPair "DeleteOnTermination")
              deleteOnTermination
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Encrypted") encrypted
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Iops") iops
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotId") snapshotId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VolumeSize") volumeSize
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VolumeType") volumeType

instance Core.FromXML EbsBlockDevice where
        parseXML x
          = EbsBlockDevice' Core.<$>
              (x Core..@? "deleteOnTermination") Core.<*> x Core..@? "encrypted"
                Core.<*> x Core..@? "iops"
                Core.<*> x Core..@? "KmsKeyId"
                Core.<*> x Core..@? "snapshotId"
                Core.<*> x Core..@? "volumeSize"
                Core.<*> x Core..@? "volumeType"
