{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
  ( InstanceNetworkInterfaceAttachment (..)
  -- * Smart constructor
  , mkInstanceNetworkInterfaceAttachment
  -- * Lenses
  , iniaAttachTime
  , iniaAttachmentId
  , iniaDeleteOnTermination
  , iniaDeviceIndex
  , iniaNetworkCardIndex
  , iniaStatus
  ) where

import qualified Network.AWS.EC2.Types.AttachmentStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a network interface attachment.
--
-- /See:/ 'mkInstanceNetworkInterfaceAttachment' smart constructor.
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment'
  { attachTime :: Core.Maybe Core.UTCTime
    -- ^ The time stamp when the attachment initiated.
  , attachmentId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface attachment.
  , deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ Indicates whether the network interface is deleted when the instance is terminated.
  , deviceIndex :: Core.Maybe Core.Int
    -- ^ The index of the device on the instance for the network interface attachment.
  , networkCardIndex :: Core.Maybe Core.Int
    -- ^ The index of the network card.
  , status :: Core.Maybe Types.AttachmentStatus
    -- ^ The attachment state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceNetworkInterfaceAttachment' value with any optional fields omitted.
mkInstanceNetworkInterfaceAttachment
    :: InstanceNetworkInterfaceAttachment
mkInstanceNetworkInterfaceAttachment
  = InstanceNetworkInterfaceAttachment'{attachTime = Core.Nothing,
                                        attachmentId = Core.Nothing,
                                        deleteOnTermination = Core.Nothing,
                                        deviceIndex = Core.Nothing, networkCardIndex = Core.Nothing,
                                        status = Core.Nothing}

-- | The time stamp when the attachment initiated.
--
-- /Note:/ Consider using 'attachTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaAttachTime :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe Core.UTCTime)
iniaAttachTime = Lens.field @"attachTime"
{-# INLINEABLE iniaAttachTime #-}
{-# DEPRECATED attachTime "Use generic-lens or generic-optics with 'attachTime' instead"  #-}

-- | The ID of the network interface attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaAttachmentId :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe Core.Text)
iniaAttachmentId = Lens.field @"attachmentId"
{-# INLINEABLE iniaAttachmentId #-}
{-# DEPRECATED attachmentId "Use generic-lens or generic-optics with 'attachmentId' instead"  #-}

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaDeleteOnTermination :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe Core.Bool)
iniaDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE iniaDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | The index of the device on the instance for the network interface attachment.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaDeviceIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe Core.Int)
iniaDeviceIndex = Lens.field @"deviceIndex"
{-# INLINEABLE iniaDeviceIndex #-}
{-# DEPRECATED deviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead"  #-}

-- | The index of the network card.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaNetworkCardIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe Core.Int)
iniaNetworkCardIndex = Lens.field @"networkCardIndex"
{-# INLINEABLE iniaNetworkCardIndex #-}
{-# DEPRECATED networkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead"  #-}

-- | The attachment state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaStatus :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe Types.AttachmentStatus)
iniaStatus = Lens.field @"status"
{-# INLINEABLE iniaStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML InstanceNetworkInterfaceAttachment where
        parseXML x
          = InstanceNetworkInterfaceAttachment' Core.<$>
              (x Core..@? "attachTime") Core.<*> x Core..@? "attachmentId"
                Core.<*> x Core..@? "deleteOnTermination"
                Core.<*> x Core..@? "deviceIndex"
                Core.<*> x Core..@? "networkCardIndex"
                Core.<*> x Core..@? "status"
