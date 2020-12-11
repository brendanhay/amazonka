{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetDeviceInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a device instance that belongs to a private device fleet.
module Network.AWS.DeviceFarm.GetDeviceInstance
  ( -- * Creating a request
    GetDeviceInstance (..),
    mkGetDeviceInstance,

    -- ** Request lenses
    gdiArn,

    -- * Destructuring the response
    GetDeviceInstanceResponse (..),
    mkGetDeviceInstanceResponse,

    -- ** Response lenses
    gdirsDeviceInstance,
    gdirsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDeviceInstance' smart constructor.
newtype GetDeviceInstance = GetDeviceInstance' {arn :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeviceInstance' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the instance you're requesting information about.
mkGetDeviceInstance ::
  -- | 'arn'
  Lude.Text ->
  GetDeviceInstance
mkGetDeviceInstance pArn_ = GetDeviceInstance' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the instance you're requesting information about.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdiArn :: Lens.Lens' GetDeviceInstance Lude.Text
gdiArn = Lens.lens (arn :: GetDeviceInstance -> Lude.Text) (\s a -> s {arn = a} :: GetDeviceInstance)
{-# DEPRECATED gdiArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetDeviceInstance where
  type Rs GetDeviceInstance = GetDeviceInstanceResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeviceInstanceResponse'
            Lude.<$> (x Lude..?> "deviceInstance")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDeviceInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetDeviceInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDeviceInstance where
  toJSON GetDeviceInstance' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetDeviceInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDeviceInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDeviceInstanceResponse' smart constructor.
data GetDeviceInstanceResponse = GetDeviceInstanceResponse'
  { deviceInstance ::
      Lude.Maybe DeviceInstance,
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

-- | Creates a value of 'GetDeviceInstanceResponse' with the minimum fields required to make a request.
--
-- * 'deviceInstance' - An object that contains information about your device instance.
-- * 'responseStatus' - The response status code.
mkGetDeviceInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDeviceInstanceResponse
mkGetDeviceInstanceResponse pResponseStatus_ =
  GetDeviceInstanceResponse'
    { deviceInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about your device instance.
--
-- /Note:/ Consider using 'deviceInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdirsDeviceInstance :: Lens.Lens' GetDeviceInstanceResponse (Lude.Maybe DeviceInstance)
gdirsDeviceInstance = Lens.lens (deviceInstance :: GetDeviceInstanceResponse -> Lude.Maybe DeviceInstance) (\s a -> s {deviceInstance = a} :: GetDeviceInstanceResponse)
{-# DEPRECATED gdirsDeviceInstance "Use generic-lens or generic-optics with 'deviceInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdirsResponseStatus :: Lens.Lens' GetDeviceInstanceResponse Lude.Int
gdirsResponseStatus = Lens.lens (responseStatus :: GetDeviceInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeviceInstanceResponse)
{-# DEPRECATED gdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
