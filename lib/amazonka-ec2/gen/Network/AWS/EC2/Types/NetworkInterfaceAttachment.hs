{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfaceAttachment
  ( NetworkInterfaceAttachment (..)
  -- * Smart constructor
  , mkNetworkInterfaceAttachment
  -- * Lenses
  , niaAttachTime
  , niaAttachmentId
  , niaDeleteOnTermination
  , niaDeviceIndex
  , niaInstanceId
  , niaInstanceOwnerId
  , niaNetworkCardIndex
  , niaStatus
  ) where

import qualified Network.AWS.EC2.Types.AttachmentStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a network interface attachment.
--
-- /See:/ 'mkNetworkInterfaceAttachment' smart constructor.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment'
  { attachTime :: Core.Maybe Core.UTCTime
    -- ^ The timestamp indicating when the attachment initiated.
  , attachmentId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface attachment.
  , deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ Indicates whether the network interface is deleted when the instance is terminated.
  , deviceIndex :: Core.Maybe Core.Int
    -- ^ The device index of the network interface attachment on the instance.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance.
  , instanceOwnerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID of the owner of the instance.
  , networkCardIndex :: Core.Maybe Core.Int
    -- ^ The index of the network card.
  , status :: Core.Maybe Types.AttachmentStatus
    -- ^ The attachment state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'NetworkInterfaceAttachment' value with any optional fields omitted.
mkNetworkInterfaceAttachment
    :: NetworkInterfaceAttachment
mkNetworkInterfaceAttachment
  = NetworkInterfaceAttachment'{attachTime = Core.Nothing,
                                attachmentId = Core.Nothing, deleteOnTermination = Core.Nothing,
                                deviceIndex = Core.Nothing, instanceId = Core.Nothing,
                                instanceOwnerId = Core.Nothing, networkCardIndex = Core.Nothing,
                                status = Core.Nothing}

-- | The timestamp indicating when the attachment initiated.
--
-- /Note:/ Consider using 'attachTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaAttachTime :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.UTCTime)
niaAttachTime = Lens.field @"attachTime"
{-# INLINEABLE niaAttachTime #-}
{-# DEPRECATED attachTime "Use generic-lens or generic-optics with 'attachTime' instead"  #-}

-- | The ID of the network interface attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaAttachmentId :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Text)
niaAttachmentId = Lens.field @"attachmentId"
{-# INLINEABLE niaAttachmentId #-}
{-# DEPRECATED attachmentId "Use generic-lens or generic-optics with 'attachmentId' instead"  #-}

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaDeleteOnTermination :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Bool)
niaDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE niaDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | The device index of the network interface attachment on the instance.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaDeviceIndex :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Int)
niaDeviceIndex = Lens.field @"deviceIndex"
{-# INLINEABLE niaDeviceIndex #-}
{-# DEPRECATED deviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaInstanceId :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Text)
niaInstanceId = Lens.field @"instanceId"
{-# INLINEABLE niaInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The AWS account ID of the owner of the instance.
--
-- /Note:/ Consider using 'instanceOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaInstanceOwnerId :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Text)
niaInstanceOwnerId = Lens.field @"instanceOwnerId"
{-# INLINEABLE niaInstanceOwnerId #-}
{-# DEPRECATED instanceOwnerId "Use generic-lens or generic-optics with 'instanceOwnerId' instead"  #-}

-- | The index of the network card.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaNetworkCardIndex :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Int)
niaNetworkCardIndex = Lens.field @"networkCardIndex"
{-# INLINEABLE niaNetworkCardIndex #-}
{-# DEPRECATED networkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead"  #-}

-- | The attachment state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaStatus :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Types.AttachmentStatus)
niaStatus = Lens.field @"status"
{-# INLINEABLE niaStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML NetworkInterfaceAttachment where
        parseXML x
          = NetworkInterfaceAttachment' Core.<$>
              (x Core..@? "attachTime") Core.<*> x Core..@? "attachmentId"
                Core.<*> x Core..@? "deleteOnTermination"
                Core.<*> x Core..@? "deviceIndex"
                Core.<*> x Core..@? "instanceId"
                Core.<*> x Core..@? "instanceOwnerId"
                Core.<*> x Core..@? "networkCardIndex"
                Core.<*> x Core..@? "status"
