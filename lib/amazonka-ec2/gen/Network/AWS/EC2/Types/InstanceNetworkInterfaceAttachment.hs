{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
  ( InstanceNetworkInterfaceAttachment (..),

    -- * Smart constructor
    mkInstanceNetworkInterfaceAttachment,

    -- * Lenses
    iniaStatus,
    iniaDeleteOnTermination,
    iniaAttachmentId,
    iniaNetworkCardIndex,
    iniaAttachTime,
    iniaDeviceIndex,
  )
where

import Network.AWS.EC2.Types.AttachmentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a network interface attachment.
--
-- /See:/ 'mkInstanceNetworkInterfaceAttachment' smart constructor.
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment'
  { -- | The attachment state.
    status :: Lude.Maybe AttachmentStatus,
    -- | Indicates whether the network interface is deleted when the instance is terminated.
    deleteOnTermination :: Lude.Maybe Lude.Bool,
    -- | The ID of the network interface attachment.
    attachmentId :: Lude.Maybe Lude.Text,
    -- | The index of the network card.
    networkCardIndex :: Lude.Maybe Lude.Int,
    -- | The time stamp when the attachment initiated.
    attachTime :: Lude.Maybe Lude.DateTime,
    -- | The index of the device on the instance for the network interface attachment.
    deviceIndex :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceNetworkInterfaceAttachment' with the minimum fields required to make a request.
--
-- * 'status' - The attachment state.
-- * 'deleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
-- * 'attachmentId' - The ID of the network interface attachment.
-- * 'networkCardIndex' - The index of the network card.
-- * 'attachTime' - The time stamp when the attachment initiated.
-- * 'deviceIndex' - The index of the device on the instance for the network interface attachment.
mkInstanceNetworkInterfaceAttachment ::
  InstanceNetworkInterfaceAttachment
mkInstanceNetworkInterfaceAttachment =
  InstanceNetworkInterfaceAttachment'
    { status = Lude.Nothing,
      deleteOnTermination = Lude.Nothing,
      attachmentId = Lude.Nothing,
      networkCardIndex = Lude.Nothing,
      attachTime = Lude.Nothing,
      deviceIndex = Lude.Nothing
    }

-- | The attachment state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaStatus :: Lens.Lens' InstanceNetworkInterfaceAttachment (Lude.Maybe AttachmentStatus)
iniaStatus = Lens.lens (status :: InstanceNetworkInterfaceAttachment -> Lude.Maybe AttachmentStatus) (\s a -> s {status = a} :: InstanceNetworkInterfaceAttachment)
{-# DEPRECATED iniaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaDeleteOnTermination :: Lens.Lens' InstanceNetworkInterfaceAttachment (Lude.Maybe Lude.Bool)
iniaDeleteOnTermination = Lens.lens (deleteOnTermination :: InstanceNetworkInterfaceAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: InstanceNetworkInterfaceAttachment)
{-# DEPRECATED iniaDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The ID of the network interface attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaAttachmentId :: Lens.Lens' InstanceNetworkInterfaceAttachment (Lude.Maybe Lude.Text)
iniaAttachmentId = Lens.lens (attachmentId :: InstanceNetworkInterfaceAttachment -> Lude.Maybe Lude.Text) (\s a -> s {attachmentId = a} :: InstanceNetworkInterfaceAttachment)
{-# DEPRECATED iniaAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

-- | The index of the network card.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaNetworkCardIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Lude.Maybe Lude.Int)
iniaNetworkCardIndex = Lens.lens (networkCardIndex :: InstanceNetworkInterfaceAttachment -> Lude.Maybe Lude.Int) (\s a -> s {networkCardIndex = a} :: InstanceNetworkInterfaceAttachment)
{-# DEPRECATED iniaNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

-- | The time stamp when the attachment initiated.
--
-- /Note:/ Consider using 'attachTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaAttachTime :: Lens.Lens' InstanceNetworkInterfaceAttachment (Lude.Maybe Lude.DateTime)
iniaAttachTime = Lens.lens (attachTime :: InstanceNetworkInterfaceAttachment -> Lude.Maybe Lude.DateTime) (\s a -> s {attachTime = a} :: InstanceNetworkInterfaceAttachment)
{-# DEPRECATED iniaAttachTime "Use generic-lens or generic-optics with 'attachTime' instead." #-}

-- | The index of the device on the instance for the network interface attachment.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaDeviceIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Lude.Maybe Lude.Int)
iniaDeviceIndex = Lens.lens (deviceIndex :: InstanceNetworkInterfaceAttachment -> Lude.Maybe Lude.Int) (\s a -> s {deviceIndex = a} :: InstanceNetworkInterfaceAttachment)
{-# DEPRECATED iniaDeviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead." #-}

instance Lude.FromXML InstanceNetworkInterfaceAttachment where
  parseXML x =
    InstanceNetworkInterfaceAttachment'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "deleteOnTermination")
      Lude.<*> (x Lude..@? "attachmentId")
      Lude.<*> (x Lude..@? "networkCardIndex")
      Lude.<*> (x Lude..@? "attachTime")
      Lude.<*> (x Lude..@? "deviceIndex")
