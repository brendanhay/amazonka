{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateVTLDeviceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the type of medium changer in a tape gateway. When you activate a tape gateway, you select a medium changer type for the tape gateway. This operation enables you to select a different type of medium changer after a tape gateway is activated. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.UpdateVTLDeviceType
  ( -- * Creating a request
    UpdateVTLDeviceType (..),
    mkUpdateVTLDeviceType,

    -- ** Request lenses
    uvtldtVTLDeviceARN,
    uvtldtDeviceType,

    -- * Destructuring the response
    UpdateVTLDeviceTypeResponse (..),
    mkUpdateVTLDeviceTypeResponse,

    -- ** Response lenses
    uvtldtrsVTLDeviceARN,
    uvtldtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkUpdateVTLDeviceType' smart constructor.
data UpdateVTLDeviceType = UpdateVTLDeviceType'
  { vTLDeviceARN ::
      Lude.Text,
    deviceType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVTLDeviceType' with the minimum fields required to make a request.
--
-- * 'deviceType' - The type of medium changer you want to select.
--
-- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
-- * 'vTLDeviceARN' - The Amazon Resource Name (ARN) of the medium changer you want to select.
mkUpdateVTLDeviceType ::
  -- | 'vTLDeviceARN'
  Lude.Text ->
  -- | 'deviceType'
  Lude.Text ->
  UpdateVTLDeviceType
mkUpdateVTLDeviceType pVTLDeviceARN_ pDeviceType_ =
  UpdateVTLDeviceType'
    { vTLDeviceARN = pVTLDeviceARN_,
      deviceType = pDeviceType_
    }

-- | The Amazon Resource Name (ARN) of the medium changer you want to select.
--
-- /Note:/ Consider using 'vTLDeviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtldtVTLDeviceARN :: Lens.Lens' UpdateVTLDeviceType Lude.Text
uvtldtVTLDeviceARN = Lens.lens (vTLDeviceARN :: UpdateVTLDeviceType -> Lude.Text) (\s a -> s {vTLDeviceARN = a} :: UpdateVTLDeviceType)
{-# DEPRECATED uvtldtVTLDeviceARN "Use generic-lens or generic-optics with 'vTLDeviceARN' instead." #-}

-- | The type of medium changer you want to select.
--
-- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
--
-- /Note:/ Consider using 'deviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtldtDeviceType :: Lens.Lens' UpdateVTLDeviceType Lude.Text
uvtldtDeviceType = Lens.lens (deviceType :: UpdateVTLDeviceType -> Lude.Text) (\s a -> s {deviceType = a} :: UpdateVTLDeviceType)
{-# DEPRECATED uvtldtDeviceType "Use generic-lens or generic-optics with 'deviceType' instead." #-}

instance Lude.AWSRequest UpdateVTLDeviceType where
  type Rs UpdateVTLDeviceType = UpdateVTLDeviceTypeResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateVTLDeviceTypeResponse'
            Lude.<$> (x Lude..?> "VTLDeviceARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateVTLDeviceType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.UpdateVTLDeviceType" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateVTLDeviceType where
  toJSON UpdateVTLDeviceType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("VTLDeviceARN" Lude..= vTLDeviceARN),
            Lude.Just ("DeviceType" Lude..= deviceType)
          ]
      )

instance Lude.ToPath UpdateVTLDeviceType where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateVTLDeviceType where
  toQuery = Lude.const Lude.mempty

-- | UpdateVTLDeviceTypeOutput
--
-- /See:/ 'mkUpdateVTLDeviceTypeResponse' smart constructor.
data UpdateVTLDeviceTypeResponse = UpdateVTLDeviceTypeResponse'
  { vTLDeviceARN ::
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

-- | Creates a value of 'UpdateVTLDeviceTypeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vTLDeviceARN' - The Amazon Resource Name (ARN) of the medium changer you have selected.
mkUpdateVTLDeviceTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateVTLDeviceTypeResponse
mkUpdateVTLDeviceTypeResponse pResponseStatus_ =
  UpdateVTLDeviceTypeResponse'
    { vTLDeviceARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the medium changer you have selected.
--
-- /Note:/ Consider using 'vTLDeviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtldtrsVTLDeviceARN :: Lens.Lens' UpdateVTLDeviceTypeResponse (Lude.Maybe Lude.Text)
uvtldtrsVTLDeviceARN = Lens.lens (vTLDeviceARN :: UpdateVTLDeviceTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {vTLDeviceARN = a} :: UpdateVTLDeviceTypeResponse)
{-# DEPRECATED uvtldtrsVTLDeviceARN "Use generic-lens or generic-optics with 'vTLDeviceARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtldtrsResponseStatus :: Lens.Lens' UpdateVTLDeviceTypeResponse Lude.Int
uvtldtrsResponseStatus = Lens.lens (responseStatus :: UpdateVTLDeviceTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateVTLDeviceTypeResponse)
{-# DEPRECATED uvtldtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
