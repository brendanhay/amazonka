{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DetachVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects a volume from an iSCSI connection and then detaches the volume from the specified gateway. Detaching and attaching a volume enables you to recover your data from one gateway to a different gateway without creating a snapshot. It also makes it easier to move your volumes from an on-premises gateway to a gateway hosted on an Amazon EC2 instance. This operation is only supported in the volume gateway type.
module Network.AWS.StorageGateway.DetachVolume
  ( -- * Creating a request
    DetachVolume (..),
    mkDetachVolume,

    -- ** Request lenses
    dvForceDetach,
    dvVolumeARN,

    -- * Destructuring the response
    DetachVolumeResponse (..),
    mkDetachVolumeResponse,

    -- ** Response lenses
    dvrsVolumeARN,
    dvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | AttachVolumeInput
--
-- /See:/ 'mkDetachVolume' smart constructor.
data DetachVolume = DetachVolume'
  { forceDetach ::
      Lude.Maybe Lude.Bool,
    volumeARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachVolume' with the minimum fields required to make a request.
--
-- * 'forceDetach' - Set to @true@ to forcibly remove the iSCSI connection of the target volume and detach the volume. The default is @false@ . If this value is set to @false@ , you must manually disconnect the iSCSI connection from the target volume.
--
-- Valid Values: @true@ | @false@
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume to detach from the gateway.
mkDetachVolume ::
  -- | 'volumeARN'
  Lude.Text ->
  DetachVolume
mkDetachVolume pVolumeARN_ =
  DetachVolume'
    { forceDetach = Lude.Nothing,
      volumeARN = pVolumeARN_
    }

-- | Set to @true@ to forcibly remove the iSCSI connection of the target volume and detach the volume. The default is @false@ . If this value is set to @false@ , you must manually disconnect the iSCSI connection from the target volume.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'forceDetach' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvForceDetach :: Lens.Lens' DetachVolume (Lude.Maybe Lude.Bool)
dvForceDetach = Lens.lens (forceDetach :: DetachVolume -> Lude.Maybe Lude.Bool) (\s a -> s {forceDetach = a} :: DetachVolume)
{-# DEPRECATED dvForceDetach "Use generic-lens or generic-optics with 'forceDetach' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume to detach from the gateway.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVolumeARN :: Lens.Lens' DetachVolume Lude.Text
dvVolumeARN = Lens.lens (volumeARN :: DetachVolume -> Lude.Text) (\s a -> s {volumeARN = a} :: DetachVolume)
{-# DEPRECATED dvVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

instance Lude.AWSRequest DetachVolume where
  type Rs DetachVolume = DetachVolumeResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetachVolumeResponse'
            Lude.<$> (x Lude..?> "VolumeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachVolume where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.DetachVolume" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetachVolume where
  toJSON DetachVolume' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ForceDetach" Lude..=) Lude.<$> forceDetach,
            Lude.Just ("VolumeARN" Lude..= volumeARN)
          ]
      )

instance Lude.ToPath DetachVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachVolume where
  toQuery = Lude.const Lude.mempty

-- | AttachVolumeOutput
--
-- /See:/ 'mkDetachVolumeResponse' smart constructor.
data DetachVolumeResponse = DetachVolumeResponse'
  { volumeARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DetachVolumeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume that was detached.
mkDetachVolumeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachVolumeResponse
mkDetachVolumeResponse pResponseStatus_ =
  DetachVolumeResponse'
    { volumeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the volume that was detached.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrsVolumeARN :: Lens.Lens' DetachVolumeResponse (Lude.Maybe Lude.Text)
dvrsVolumeARN = Lens.lens (volumeARN :: DetachVolumeResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: DetachVolumeResponse)
{-# DEPRECATED dvrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrsResponseStatus :: Lens.Lens' DetachVolumeResponse Lude.Int
dvrsResponseStatus = Lens.lens (responseStatus :: DetachVolumeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachVolumeResponse)
{-# DEPRECATED dvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
