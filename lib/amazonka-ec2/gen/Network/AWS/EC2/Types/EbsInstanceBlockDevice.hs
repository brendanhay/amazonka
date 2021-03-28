{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EbsInstanceBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EbsInstanceBlockDevice
  ( EbsInstanceBlockDevice (..)
  -- * Smart constructor
  , mkEbsInstanceBlockDevice
  -- * Lenses
  , eibdAttachTime
  , eibdDeleteOnTermination
  , eibdStatus
  , eibdVolumeId
  ) where

import qualified Network.AWS.EC2.Types.AttachmentStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a parameter used to set up an EBS volume in a block device mapping.
--
-- /See:/ 'mkEbsInstanceBlockDevice' smart constructor.
data EbsInstanceBlockDevice = EbsInstanceBlockDevice'
  { attachTime :: Core.Maybe Core.UTCTime
    -- ^ The time stamp when the attachment initiated.
  , deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ Indicates whether the volume is deleted on instance termination.
  , status :: Core.Maybe Types.AttachmentStatus
    -- ^ The attachment state.
  , volumeId :: Core.Maybe Core.Text
    -- ^ The ID of the EBS volume.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EbsInstanceBlockDevice' value with any optional fields omitted.
mkEbsInstanceBlockDevice
    :: EbsInstanceBlockDevice
mkEbsInstanceBlockDevice
  = EbsInstanceBlockDevice'{attachTime = Core.Nothing,
                            deleteOnTermination = Core.Nothing, status = Core.Nothing,
                            volumeId = Core.Nothing}

-- | The time stamp when the attachment initiated.
--
-- /Note:/ Consider using 'attachTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdAttachTime :: Lens.Lens' EbsInstanceBlockDevice (Core.Maybe Core.UTCTime)
eibdAttachTime = Lens.field @"attachTime"
{-# INLINEABLE eibdAttachTime #-}
{-# DEPRECATED attachTime "Use generic-lens or generic-optics with 'attachTime' instead"  #-}

-- | Indicates whether the volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdDeleteOnTermination :: Lens.Lens' EbsInstanceBlockDevice (Core.Maybe Core.Bool)
eibdDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE eibdDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | The attachment state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdStatus :: Lens.Lens' EbsInstanceBlockDevice (Core.Maybe Types.AttachmentStatus)
eibdStatus = Lens.field @"status"
{-# INLINEABLE eibdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The ID of the EBS volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eibdVolumeId :: Lens.Lens' EbsInstanceBlockDevice (Core.Maybe Core.Text)
eibdVolumeId = Lens.field @"volumeId"
{-# INLINEABLE eibdVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

instance Core.FromXML EbsInstanceBlockDevice where
        parseXML x
          = EbsInstanceBlockDevice' Core.<$>
              (x Core..@? "attachTime") Core.<*> x Core..@? "deleteOnTermination"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "volumeId"
