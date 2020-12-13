{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a network interface to an instance.
module Network.AWS.EC2.AttachNetworkInterface
  ( -- * Creating a request
    AttachNetworkInterface (..),
    mkAttachNetworkInterface,

    -- ** Request lenses
    aniInstanceId,
    aniNetworkInterfaceId,
    aniNetworkCardIndex,
    aniDryRun,
    aniDeviceIndex,

    -- * Destructuring the response
    AttachNetworkInterfaceResponse (..),
    mkAttachNetworkInterfaceResponse,

    -- ** Response lenses
    anirsAttachmentId,
    anirsNetworkCardIndex,
    anirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for AttachNetworkInterface.
--
-- /See:/ 'mkAttachNetworkInterface' smart constructor.
data AttachNetworkInterface = AttachNetworkInterface'
  { -- | The ID of the instance.
    instanceId :: Lude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Lude.Text,
    -- | The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
    networkCardIndex :: Lude.Maybe Lude.Int,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The index of the device for the network interface attachment.
    deviceIndex :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachNetworkInterface' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'networkCardIndex' - The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'deviceIndex' - The index of the device for the network interface attachment.
mkAttachNetworkInterface ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'networkInterfaceId'
  Lude.Text ->
  -- | 'deviceIndex'
  Lude.Int ->
  AttachNetworkInterface
mkAttachNetworkInterface
  pInstanceId_
  pNetworkInterfaceId_
  pDeviceIndex_ =
    AttachNetworkInterface'
      { instanceId = pInstanceId_,
        networkInterfaceId = pNetworkInterfaceId_,
        networkCardIndex = Lude.Nothing,
        dryRun = Lude.Nothing,
        deviceIndex = pDeviceIndex_
      }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aniInstanceId :: Lens.Lens' AttachNetworkInterface Lude.Text
aniInstanceId = Lens.lens (instanceId :: AttachNetworkInterface -> Lude.Text) (\s a -> s {instanceId = a} :: AttachNetworkInterface)
{-# DEPRECATED aniInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aniNetworkInterfaceId :: Lens.Lens' AttachNetworkInterface Lude.Text
aniNetworkInterfaceId = Lens.lens (networkInterfaceId :: AttachNetworkInterface -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: AttachNetworkInterface)
{-# DEPRECATED aniNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aniNetworkCardIndex :: Lens.Lens' AttachNetworkInterface (Lude.Maybe Lude.Int)
aniNetworkCardIndex = Lens.lens (networkCardIndex :: AttachNetworkInterface -> Lude.Maybe Lude.Int) (\s a -> s {networkCardIndex = a} :: AttachNetworkInterface)
{-# DEPRECATED aniNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aniDryRun :: Lens.Lens' AttachNetworkInterface (Lude.Maybe Lude.Bool)
aniDryRun = Lens.lens (dryRun :: AttachNetworkInterface -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AttachNetworkInterface)
{-# DEPRECATED aniDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The index of the device for the network interface attachment.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aniDeviceIndex :: Lens.Lens' AttachNetworkInterface Lude.Int
aniDeviceIndex = Lens.lens (deviceIndex :: AttachNetworkInterface -> Lude.Int) (\s a -> s {deviceIndex = a} :: AttachNetworkInterface)
{-# DEPRECATED aniDeviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead." #-}

instance Lude.AWSRequest AttachNetworkInterface where
  type Rs AttachNetworkInterface = AttachNetworkInterfaceResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AttachNetworkInterfaceResponse'
            Lude.<$> (x Lude..@? "attachmentId")
            Lude.<*> (x Lude..@? "networkCardIndex")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachNetworkInterface where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachNetworkInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachNetworkInterface where
  toQuery AttachNetworkInterface' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AttachNetworkInterface" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        "NetworkCardIndex" Lude.=: networkCardIndex,
        "DryRun" Lude.=: dryRun,
        "DeviceIndex" Lude.=: deviceIndex
      ]

-- | Contains the output of AttachNetworkInterface.
--
-- /See:/ 'mkAttachNetworkInterfaceResponse' smart constructor.
data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse'
  { -- | The ID of the network interface attachment.
    attachmentId :: Lude.Maybe Lude.Text,
    -- | The index of the network card.
    networkCardIndex :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachNetworkInterfaceResponse' with the minimum fields required to make a request.
--
-- * 'attachmentId' - The ID of the network interface attachment.
-- * 'networkCardIndex' - The index of the network card.
-- * 'responseStatus' - The response status code.
mkAttachNetworkInterfaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachNetworkInterfaceResponse
mkAttachNetworkInterfaceResponse pResponseStatus_ =
  AttachNetworkInterfaceResponse'
    { attachmentId = Lude.Nothing,
      networkCardIndex = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the network interface attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anirsAttachmentId :: Lens.Lens' AttachNetworkInterfaceResponse (Lude.Maybe Lude.Text)
anirsAttachmentId = Lens.lens (attachmentId :: AttachNetworkInterfaceResponse -> Lude.Maybe Lude.Text) (\s a -> s {attachmentId = a} :: AttachNetworkInterfaceResponse)
{-# DEPRECATED anirsAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

-- | The index of the network card.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anirsNetworkCardIndex :: Lens.Lens' AttachNetworkInterfaceResponse (Lude.Maybe Lude.Int)
anirsNetworkCardIndex = Lens.lens (networkCardIndex :: AttachNetworkInterfaceResponse -> Lude.Maybe Lude.Int) (\s a -> s {networkCardIndex = a} :: AttachNetworkInterfaceResponse)
{-# DEPRECATED anirsNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anirsResponseStatus :: Lens.Lens' AttachNetworkInterfaceResponse Lude.Int
anirsResponseStatus = Lens.lens (responseStatus :: AttachNetworkInterfaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachNetworkInterfaceResponse)
{-# DEPRECATED anirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
