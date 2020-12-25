{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAttachment
  ( NetworkInterfaceAttachment (..),

    -- * Smart constructor
    mkNetworkInterfaceAttachment,

    -- * Lenses
    niaAttachTime,
    niaAttachmentId,
    niaDeleteOnTermination,
    niaDeviceIndex,
    niaInstanceId,
    niaInstanceOwnerId,
    niaNetworkCardIndex,
    niaStatus,
  )
where

import qualified Network.AWS.EC2.Types.AttachmentId as Types
import qualified Network.AWS.EC2.Types.AttachmentStatus as Types
import qualified Network.AWS.EC2.Types.InstanceId as Types
import qualified Network.AWS.EC2.Types.InstanceOwnerId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a network interface attachment.
--
-- /See:/ 'mkNetworkInterfaceAttachment' smart constructor.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment'
  { -- | The timestamp indicating when the attachment initiated.
    attachTime :: Core.Maybe Core.UTCTime,
    -- | The ID of the network interface attachment.
    attachmentId :: Core.Maybe Types.AttachmentId,
    -- | Indicates whether the network interface is deleted when the instance is terminated.
    deleteOnTermination :: Core.Maybe Core.Bool,
    -- | The device index of the network interface attachment on the instance.
    deviceIndex :: Core.Maybe Core.Int,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The AWS account ID of the owner of the instance.
    instanceOwnerId :: Core.Maybe Types.InstanceOwnerId,
    -- | The index of the network card.
    networkCardIndex :: Core.Maybe Core.Int,
    -- | The attachment state.
    status :: Core.Maybe Types.AttachmentStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'NetworkInterfaceAttachment' value with any optional fields omitted.
mkNetworkInterfaceAttachment ::
  NetworkInterfaceAttachment
mkNetworkInterfaceAttachment =
  NetworkInterfaceAttachment'
    { attachTime = Core.Nothing,
      attachmentId = Core.Nothing,
      deleteOnTermination = Core.Nothing,
      deviceIndex = Core.Nothing,
      instanceId = Core.Nothing,
      instanceOwnerId = Core.Nothing,
      networkCardIndex = Core.Nothing,
      status = Core.Nothing
    }

-- | The timestamp indicating when the attachment initiated.
--
-- /Note:/ Consider using 'attachTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaAttachTime :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.UTCTime)
niaAttachTime = Lens.field @"attachTime"
{-# DEPRECATED niaAttachTime "Use generic-lens or generic-optics with 'attachTime' instead." #-}

-- | The ID of the network interface attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaAttachmentId :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Types.AttachmentId)
niaAttachmentId = Lens.field @"attachmentId"
{-# DEPRECATED niaAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaDeleteOnTermination :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Bool)
niaDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# DEPRECATED niaDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The device index of the network interface attachment on the instance.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaDeviceIndex :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Int)
niaDeviceIndex = Lens.field @"deviceIndex"
{-# DEPRECATED niaDeviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaInstanceId :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Types.InstanceId)
niaInstanceId = Lens.field @"instanceId"
{-# DEPRECATED niaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The AWS account ID of the owner of the instance.
--
-- /Note:/ Consider using 'instanceOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaInstanceOwnerId :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Types.InstanceOwnerId)
niaInstanceOwnerId = Lens.field @"instanceOwnerId"
{-# DEPRECATED niaInstanceOwnerId "Use generic-lens or generic-optics with 'instanceOwnerId' instead." #-}

-- | The index of the network card.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaNetworkCardIndex :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Int)
niaNetworkCardIndex = Lens.field @"networkCardIndex"
{-# DEPRECATED niaNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

-- | The attachment state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaStatus :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Types.AttachmentStatus)
niaStatus = Lens.field @"status"
{-# DEPRECATED niaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML NetworkInterfaceAttachment where
  parseXML x =
    NetworkInterfaceAttachment'
      Core.<$> (x Core..@? "attachTime")
      Core.<*> (x Core..@? "attachmentId")
      Core.<*> (x Core..@? "deleteOnTermination")
      Core.<*> (x Core..@? "deviceIndex")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "instanceOwnerId")
      Core.<*> (x Core..@? "networkCardIndex")
      Core.<*> (x Core..@? "status")
