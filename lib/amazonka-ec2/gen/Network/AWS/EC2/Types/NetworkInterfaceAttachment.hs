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
    niaInstanceId,
    niaStatus,
    niaDeleteOnTermination,
    niaAttachmentId,
    niaNetworkCardIndex,
    niaInstanceOwnerId,
    niaAttachTime,
    niaDeviceIndex,
  )
where

import Network.AWS.EC2.Types.AttachmentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a network interface attachment.
--
-- /See:/ 'mkNetworkInterfaceAttachment' smart constructor.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment'
  { instanceId ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe AttachmentStatus,
    deleteOnTermination ::
      Lude.Maybe Lude.Bool,
    attachmentId :: Lude.Maybe Lude.Text,
    networkCardIndex ::
      Lude.Maybe Lude.Int,
    instanceOwnerId ::
      Lude.Maybe Lude.Text,
    attachTime ::
      Lude.Maybe Lude.DateTime,
    deviceIndex :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterfaceAttachment' with the minimum fields required to make a request.
--
-- * 'attachTime' - The timestamp indicating when the attachment initiated.
-- * 'attachmentId' - The ID of the network interface attachment.
-- * 'deleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
-- * 'deviceIndex' - The device index of the network interface attachment on the instance.
-- * 'instanceId' - The ID of the instance.
-- * 'instanceOwnerId' - The AWS account ID of the owner of the instance.
-- * 'networkCardIndex' - The index of the network card.
-- * 'status' - The attachment state.
mkNetworkInterfaceAttachment ::
  NetworkInterfaceAttachment
mkNetworkInterfaceAttachment =
  NetworkInterfaceAttachment'
    { instanceId = Lude.Nothing,
      status = Lude.Nothing,
      deleteOnTermination = Lude.Nothing,
      attachmentId = Lude.Nothing,
      networkCardIndex = Lude.Nothing,
      instanceOwnerId = Lude.Nothing,
      attachTime = Lude.Nothing,
      deviceIndex = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaInstanceId :: Lens.Lens' NetworkInterfaceAttachment (Lude.Maybe Lude.Text)
niaInstanceId = Lens.lens (instanceId :: NetworkInterfaceAttachment -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: NetworkInterfaceAttachment)
{-# DEPRECATED niaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The attachment state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaStatus :: Lens.Lens' NetworkInterfaceAttachment (Lude.Maybe AttachmentStatus)
niaStatus = Lens.lens (status :: NetworkInterfaceAttachment -> Lude.Maybe AttachmentStatus) (\s a -> s {status = a} :: NetworkInterfaceAttachment)
{-# DEPRECATED niaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaDeleteOnTermination :: Lens.Lens' NetworkInterfaceAttachment (Lude.Maybe Lude.Bool)
niaDeleteOnTermination = Lens.lens (deleteOnTermination :: NetworkInterfaceAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: NetworkInterfaceAttachment)
{-# DEPRECATED niaDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The ID of the network interface attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaAttachmentId :: Lens.Lens' NetworkInterfaceAttachment (Lude.Maybe Lude.Text)
niaAttachmentId = Lens.lens (attachmentId :: NetworkInterfaceAttachment -> Lude.Maybe Lude.Text) (\s a -> s {attachmentId = a} :: NetworkInterfaceAttachment)
{-# DEPRECATED niaAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

-- | The index of the network card.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaNetworkCardIndex :: Lens.Lens' NetworkInterfaceAttachment (Lude.Maybe Lude.Int)
niaNetworkCardIndex = Lens.lens (networkCardIndex :: NetworkInterfaceAttachment -> Lude.Maybe Lude.Int) (\s a -> s {networkCardIndex = a} :: NetworkInterfaceAttachment)
{-# DEPRECATED niaNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

-- | The AWS account ID of the owner of the instance.
--
-- /Note:/ Consider using 'instanceOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaInstanceOwnerId :: Lens.Lens' NetworkInterfaceAttachment (Lude.Maybe Lude.Text)
niaInstanceOwnerId = Lens.lens (instanceOwnerId :: NetworkInterfaceAttachment -> Lude.Maybe Lude.Text) (\s a -> s {instanceOwnerId = a} :: NetworkInterfaceAttachment)
{-# DEPRECATED niaInstanceOwnerId "Use generic-lens or generic-optics with 'instanceOwnerId' instead." #-}

-- | The timestamp indicating when the attachment initiated.
--
-- /Note:/ Consider using 'attachTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaAttachTime :: Lens.Lens' NetworkInterfaceAttachment (Lude.Maybe Lude.DateTime)
niaAttachTime = Lens.lens (attachTime :: NetworkInterfaceAttachment -> Lude.Maybe Lude.DateTime) (\s a -> s {attachTime = a} :: NetworkInterfaceAttachment)
{-# DEPRECATED niaAttachTime "Use generic-lens or generic-optics with 'attachTime' instead." #-}

-- | The device index of the network interface attachment on the instance.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaDeviceIndex :: Lens.Lens' NetworkInterfaceAttachment (Lude.Maybe Lude.Int)
niaDeviceIndex = Lens.lens (deviceIndex :: NetworkInterfaceAttachment -> Lude.Maybe Lude.Int) (\s a -> s {deviceIndex = a} :: NetworkInterfaceAttachment)
{-# DEPRECATED niaDeviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead." #-}

instance Lude.FromXML NetworkInterfaceAttachment where
  parseXML x =
    NetworkInterfaceAttachment'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "deleteOnTermination")
      Lude.<*> (x Lude..@? "attachmentId")
      Lude.<*> (x Lude..@? "networkCardIndex")
      Lude.<*> (x Lude..@? "instanceOwnerId")
      Lude.<*> (x Lude..@? "attachTime")
      Lude.<*> (x Lude..@? "deviceIndex")
