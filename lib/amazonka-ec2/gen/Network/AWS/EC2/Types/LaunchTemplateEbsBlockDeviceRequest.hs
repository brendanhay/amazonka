{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateEbsBlockDeviceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateEbsBlockDeviceRequest
  ( LaunchTemplateEbsBlockDeviceRequest (..)
  -- * Smart constructor
  , mkLaunchTemplateEbsBlockDeviceRequest
  -- * Lenses
  , ltebdrDeleteOnTermination
  , ltebdrEncrypted
  , ltebdrIops
  , ltebdrKmsKeyId
  , ltebdrSnapshotId
  , ltebdrVolumeSize
  , ltebdrVolumeType
  ) where

import qualified Network.AWS.EC2.Types.KmsKeyId as Types
import qualified Network.AWS.EC2.Types.SnapshotId as Types
import qualified Network.AWS.EC2.Types.VolumeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The parameters for a block device for an EBS volume.
--
-- /See:/ 'mkLaunchTemplateEbsBlockDeviceRequest' smart constructor.
data LaunchTemplateEbsBlockDeviceRequest = LaunchTemplateEbsBlockDeviceRequest'
  { deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ Indicates whether the EBS volume is deleted on instance termination.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the EBS volume is encrypted. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. If you are creating a volume from a snapshot, you can't specify an encryption value.
  , iops :: Core.Maybe Core.Int
    -- ^ The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The ARN of the symmetric AWS Key Management Service (AWS KMS) CMK used for encryption.
  , snapshotId :: Core.Maybe Types.SnapshotId
    -- ^ The ID of the snapshot.
  , volumeSize :: Core.Maybe Core.Int
    -- ^ The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
  , volumeType :: Core.Maybe Types.VolumeType
    -- ^ The volume type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateEbsBlockDeviceRequest' value with any optional fields omitted.
mkLaunchTemplateEbsBlockDeviceRequest
    :: LaunchTemplateEbsBlockDeviceRequest
mkLaunchTemplateEbsBlockDeviceRequest
  = LaunchTemplateEbsBlockDeviceRequest'{deleteOnTermination =
                                           Core.Nothing,
                                         encrypted = Core.Nothing, iops = Core.Nothing,
                                         kmsKeyId = Core.Nothing, snapshotId = Core.Nothing,
                                         volumeSize = Core.Nothing, volumeType = Core.Nothing}

-- | Indicates whether the EBS volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrDeleteOnTermination :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Core.Maybe Core.Bool)
ltebdrDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE ltebdrDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | Indicates whether the EBS volume is encrypted. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. If you are creating a volume from a snapshot, you can't specify an encryption value.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrEncrypted :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Core.Maybe Core.Bool)
ltebdrEncrypted = Lens.field @"encrypted"
{-# INLINEABLE ltebdrEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrIops :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Core.Maybe Core.Int)
ltebdrIops = Lens.field @"iops"
{-# INLINEABLE ltebdrIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | The ARN of the symmetric AWS Key Management Service (AWS KMS) CMK used for encryption.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrKmsKeyId :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Core.Maybe Types.KmsKeyId)
ltebdrKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE ltebdrKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrSnapshotId :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Core.Maybe Types.SnapshotId)
ltebdrSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE ltebdrSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrVolumeSize :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Core.Maybe Core.Int)
ltebdrVolumeSize = Lens.field @"volumeSize"
{-# INLINEABLE ltebdrVolumeSize #-}
{-# DEPRECATED volumeSize "Use generic-lens or generic-optics with 'volumeSize' instead"  #-}

-- | The volume type.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrVolumeType :: Lens.Lens' LaunchTemplateEbsBlockDeviceRequest (Core.Maybe Types.VolumeType)
ltebdrVolumeType = Lens.field @"volumeType"
{-# INLINEABLE ltebdrVolumeType #-}
{-# DEPRECATED volumeType "Use generic-lens or generic-optics with 'volumeType' instead"  #-}

instance Core.ToQuery LaunchTemplateEbsBlockDeviceRequest where
        toQuery LaunchTemplateEbsBlockDeviceRequest{..}
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
