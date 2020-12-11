{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AttachVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Connects a volume to an iSCSI connection and then attaches the volume to the specified gateway. Detaching and attaching a volume enables you to recover your data from one gateway to a different gateway without creating a snapshot. It also makes it easier to move your volumes from an on-premises gateway to a gateway hosted on an Amazon EC2 instance.
module Network.AWS.StorageGateway.AttachVolume
  ( -- * Creating a request
    AttachVolume (..),
    mkAttachVolume,

    -- ** Request lenses
    avDiskId,
    avTargetName,
    avGatewayARN,
    avVolumeARN,
    avNetworkInterfaceId,

    -- * Destructuring the response
    AttachVolumeResponse (..),
    mkAttachVolumeResponse,

    -- ** Response lenses
    avrsTargetARN,
    avrsVolumeARN,
    avrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | AttachVolumeInput
--
-- /See:/ 'mkAttachVolume' smart constructor.
data AttachVolume = AttachVolume'
  { diskId :: Lude.Maybe Lude.Text,
    targetName :: Lude.Maybe Lude.Text,
    gatewayARN :: Lude.Text,
    volumeARN :: Lude.Text,
    networkInterfaceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachVolume' with the minimum fields required to make a request.
--
-- * 'diskId' - The unique device ID or other distinguishing data that identifies the local disk used to create the volume. This value is only required when you are attaching a stored volume.
-- * 'gatewayARN' - The Amazon Resource Name (ARN) of the gateway that you want to attach the volume to.
-- * 'networkInterfaceId' - The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
-- * 'targetName' - The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume to attach to the specified gateway.
mkAttachVolume ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'volumeARN'
  Lude.Text ->
  -- | 'networkInterfaceId'
  Lude.Text ->
  AttachVolume
mkAttachVolume pGatewayARN_ pVolumeARN_ pNetworkInterfaceId_ =
  AttachVolume'
    { diskId = Lude.Nothing,
      targetName = Lude.Nothing,
      gatewayARN = pGatewayARN_,
      volumeARN = pVolumeARN_,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | The unique device ID or other distinguishing data that identifies the local disk used to create the volume. This value is only required when you are attaching a stored volume.
--
-- /Note:/ Consider using 'diskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avDiskId :: Lens.Lens' AttachVolume (Lude.Maybe Lude.Text)
avDiskId = Lens.lens (diskId :: AttachVolume -> Lude.Maybe Lude.Text) (\s a -> s {diskId = a} :: AttachVolume)
{-# DEPRECATED avDiskId "Use generic-lens or generic-optics with 'diskId' instead." #-}

-- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avTargetName :: Lens.Lens' AttachVolume (Lude.Maybe Lude.Text)
avTargetName = Lens.lens (targetName :: AttachVolume -> Lude.Maybe Lude.Text) (\s a -> s {targetName = a} :: AttachVolume)
{-# DEPRECATED avTargetName "Use generic-lens or generic-optics with 'targetName' instead." #-}

-- | The Amazon Resource Name (ARN) of the gateway that you want to attach the volume to.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avGatewayARN :: Lens.Lens' AttachVolume Lude.Text
avGatewayARN = Lens.lens (gatewayARN :: AttachVolume -> Lude.Text) (\s a -> s {gatewayARN = a} :: AttachVolume)
{-# DEPRECATED avGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume to attach to the specified gateway.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avVolumeARN :: Lens.Lens' AttachVolume Lude.Text
avVolumeARN = Lens.lens (volumeARN :: AttachVolume -> Lude.Text) (\s a -> s {volumeARN = a} :: AttachVolume)
{-# DEPRECATED avVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avNetworkInterfaceId :: Lens.Lens' AttachVolume Lude.Text
avNetworkInterfaceId = Lens.lens (networkInterfaceId :: AttachVolume -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: AttachVolume)
{-# DEPRECATED avNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

instance Lude.AWSRequest AttachVolume where
  type Rs AttachVolume = AttachVolumeResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          AttachVolumeResponse'
            Lude.<$> (x Lude..?> "TargetARN")
            Lude.<*> (x Lude..?> "VolumeARN")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachVolume where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.AttachVolume" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AttachVolume where
  toJSON AttachVolume' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DiskId" Lude..=) Lude.<$> diskId,
            ("TargetName" Lude..=) Lude.<$> targetName,
            Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("VolumeARN" Lude..= volumeARN),
            Lude.Just ("NetworkInterfaceId" Lude..= networkInterfaceId)
          ]
      )

instance Lude.ToPath AttachVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachVolume where
  toQuery = Lude.const Lude.mempty

-- | AttachVolumeOutput
--
-- /See:/ 'mkAttachVolumeResponse' smart constructor.
data AttachVolumeResponse = AttachVolumeResponse'
  { targetARN ::
      Lude.Maybe Lude.Text,
    volumeARN :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachVolumeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'targetARN' - The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name for the initiator that was used to connect to the target.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume that was attached to the gateway.
mkAttachVolumeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachVolumeResponse
mkAttachVolumeResponse pResponseStatus_ =
  AttachVolumeResponse'
    { targetARN = Lude.Nothing,
      volumeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name for the initiator that was used to connect to the target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avrsTargetARN :: Lens.Lens' AttachVolumeResponse (Lude.Maybe Lude.Text)
avrsTargetARN = Lens.lens (targetARN :: AttachVolumeResponse -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: AttachVolumeResponse)
{-# DEPRECATED avrsTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume that was attached to the gateway.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avrsVolumeARN :: Lens.Lens' AttachVolumeResponse (Lude.Maybe Lude.Text)
avrsVolumeARN = Lens.lens (volumeARN :: AttachVolumeResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: AttachVolumeResponse)
{-# DEPRECATED avrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avrsResponseStatus :: Lens.Lens' AttachVolumeResponse Lude.Int
avrsResponseStatus = Lens.lens (responseStatus :: AttachVolumeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachVolumeResponse)
{-# DEPRECATED avrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
