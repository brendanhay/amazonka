{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a device from Alexa For Business.
module Network.AWS.AlexaBusiness.DeleteDevice
  ( -- * Creating a request
    DeleteDevice (..),
    mkDeleteDevice,

    -- ** Request lenses
    ddfDeviceARN,

    -- * Destructuring the response
    DeleteDeviceResponse (..),
    mkDeleteDeviceResponse,

    -- ** Response lenses
    ddrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDevice' smart constructor.
newtype DeleteDevice = DeleteDevice'
  { -- | The ARN of the device for which to request details.
    deviceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDevice' with the minimum fields required to make a request.
--
-- * 'deviceARN' - The ARN of the device for which to request details.
mkDeleteDevice ::
  -- | 'deviceARN'
  Lude.Text ->
  DeleteDevice
mkDeleteDevice pDeviceARN_ = DeleteDevice' {deviceARN = pDeviceARN_}

-- | The ARN of the device for which to request details.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDeviceARN :: Lens.Lens' DeleteDevice Lude.Text
ddfDeviceARN = Lens.lens (deviceARN :: DeleteDevice -> Lude.Text) (\s a -> s {deviceARN = a} :: DeleteDevice)
{-# DEPRECATED ddfDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

instance Lude.AWSRequest DeleteDevice where
  type Rs DeleteDevice = DeleteDeviceResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDeviceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteDevice" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDevice where
  toJSON DeleteDevice' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DeviceArn" Lude..= deviceARN)])

instance Lude.ToPath DeleteDevice where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDevice where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDeviceResponse' smart constructor.
newtype DeleteDeviceResponse = DeleteDeviceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeviceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDeviceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDeviceResponse
mkDeleteDeviceResponse pResponseStatus_ =
  DeleteDeviceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DeleteDeviceResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DeleteDeviceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDeviceResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
