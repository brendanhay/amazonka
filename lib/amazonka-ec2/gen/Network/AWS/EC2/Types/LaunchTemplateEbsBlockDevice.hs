{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateEbsBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateEbsBlockDevice
  ( LaunchTemplateEbsBlockDevice (..)
  -- * Smart constructor
  , mkLaunchTemplateEbsBlockDevice
  -- * Lenses
  , ltebdDeleteOnTermination
  , ltebdEncrypted
  , ltebdIops
  , ltebdKmsKeyId
  , ltebdSnapshotId
  , ltebdVolumeSize
  , ltebdVolumeType
  ) where

import qualified Network.AWS.EC2.Types.KmsKeyId as Types
import qualified Network.AWS.EC2.Types.SnapshotId as Types
import qualified Network.AWS.EC2.Types.VolumeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a block device for an EBS volume.
--
-- /See:/ 'mkLaunchTemplateEbsBlockDevice' smart constructor.
data LaunchTemplateEbsBlockDevice = LaunchTemplateEbsBlockDevice'
  { deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ Indicates whether the EBS volume is deleted on instance termination.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the EBS volume is encrypted.
  , iops :: Core.Maybe Core.Int
    -- ^ The number of I/O operations per second (IOPS) that the volume supports. 
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The ARN of the AWS Key Management Service (AWS KMS) CMK used for encryption.
  , snapshotId :: Core.Maybe Types.SnapshotId
    -- ^ The ID of the snapshot.
  , volumeSize :: Core.Maybe Core.Int
    -- ^ The size of the volume, in GiB.
  , volumeType :: Core.Maybe Types.VolumeType
    -- ^ The volume type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateEbsBlockDevice' value with any optional fields omitted.
mkLaunchTemplateEbsBlockDevice
    :: LaunchTemplateEbsBlockDevice
mkLaunchTemplateEbsBlockDevice
  = LaunchTemplateEbsBlockDevice'{deleteOnTermination = Core.Nothing,
                                  encrypted = Core.Nothing, iops = Core.Nothing,
                                  kmsKeyId = Core.Nothing, snapshotId = Core.Nothing,
                                  volumeSize = Core.Nothing, volumeType = Core.Nothing}

-- | Indicates whether the EBS volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdDeleteOnTermination :: Lens.Lens' LaunchTemplateEbsBlockDevice (Core.Maybe Core.Bool)
ltebdDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE ltebdDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | Indicates whether the EBS volume is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdEncrypted :: Lens.Lens' LaunchTemplateEbsBlockDevice (Core.Maybe Core.Bool)
ltebdEncrypted = Lens.field @"encrypted"
{-# INLINEABLE ltebdEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The number of I/O operations per second (IOPS) that the volume supports. 
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdIops :: Lens.Lens' LaunchTemplateEbsBlockDevice (Core.Maybe Core.Int)
ltebdIops = Lens.field @"iops"
{-# INLINEABLE ltebdIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | The ARN of the AWS Key Management Service (AWS KMS) CMK used for encryption.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdKmsKeyId :: Lens.Lens' LaunchTemplateEbsBlockDevice (Core.Maybe Types.KmsKeyId)
ltebdKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE ltebdKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdSnapshotId :: Lens.Lens' LaunchTemplateEbsBlockDevice (Core.Maybe Types.SnapshotId)
ltebdSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE ltebdSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The size of the volume, in GiB.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdVolumeSize :: Lens.Lens' LaunchTemplateEbsBlockDevice (Core.Maybe Core.Int)
ltebdVolumeSize = Lens.field @"volumeSize"
{-# INLINEABLE ltebdVolumeSize #-}
{-# DEPRECATED volumeSize "Use generic-lens or generic-optics with 'volumeSize' instead"  #-}

-- | The volume type.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdVolumeType :: Lens.Lens' LaunchTemplateEbsBlockDevice (Core.Maybe Types.VolumeType)
ltebdVolumeType = Lens.field @"volumeType"
{-# INLINEABLE ltebdVolumeType #-}
{-# DEPRECATED volumeType "Use generic-lens or generic-optics with 'volumeType' instead"  #-}

instance Core.FromXML LaunchTemplateEbsBlockDevice where
        parseXML x
          = LaunchTemplateEbsBlockDevice' Core.<$>
              (x Core..@? "deleteOnTermination") Core.<*> x Core..@? "encrypted"
                Core.<*> x Core..@? "iops"
                Core.<*> x Core..@? "kmsKeyId"
                Core.<*> x Core..@? "snapshotId"
                Core.<*> x Core..@? "volumeSize"
                Core.<*> x Core..@? "volumeType"
