{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateDeviceInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about a private device instance.
module Network.AWS.DeviceFarm.UpdateDeviceInstance
  ( -- * Creating a request
    UpdateDeviceInstance (..),
    mkUpdateDeviceInstance,

    -- ** Request lenses
    udiArn,
    udiProfileARN,
    udiLabels,

    -- * Destructuring the response
    UpdateDeviceInstanceResponse (..),
    mkUpdateDeviceInstanceResponse,

    -- ** Response lenses
    udirsDeviceInstance,
    udirsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDeviceInstance' smart constructor.
data UpdateDeviceInstance = UpdateDeviceInstance'
  { -- | The Amazon Resource Name (ARN) of the device instance.
    arn :: Lude.Text,
    -- | The ARN of the profile that you want to associate with the device instance.
    profileARN :: Lude.Maybe Lude.Text,
    -- | An array of strings that you want to associate with the device instance.
    labels :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDeviceInstance' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the device instance.
-- * 'profileARN' - The ARN of the profile that you want to associate with the device instance.
-- * 'labels' - An array of strings that you want to associate with the device instance.
mkUpdateDeviceInstance ::
  -- | 'arn'
  Lude.Text ->
  UpdateDeviceInstance
mkUpdateDeviceInstance pArn_ =
  UpdateDeviceInstance'
    { arn = pArn_,
      profileARN = Lude.Nothing,
      labels = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the device instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiArn :: Lens.Lens' UpdateDeviceInstance Lude.Text
udiArn = Lens.lens (arn :: UpdateDeviceInstance -> Lude.Text) (\s a -> s {arn = a} :: UpdateDeviceInstance)
{-# DEPRECATED udiArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ARN of the profile that you want to associate with the device instance.
--
-- /Note:/ Consider using 'profileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiProfileARN :: Lens.Lens' UpdateDeviceInstance (Lude.Maybe Lude.Text)
udiProfileARN = Lens.lens (profileARN :: UpdateDeviceInstance -> Lude.Maybe Lude.Text) (\s a -> s {profileARN = a} :: UpdateDeviceInstance)
{-# DEPRECATED udiProfileARN "Use generic-lens or generic-optics with 'profileARN' instead." #-}

-- | An array of strings that you want to associate with the device instance.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiLabels :: Lens.Lens' UpdateDeviceInstance (Lude.Maybe [Lude.Text])
udiLabels = Lens.lens (labels :: UpdateDeviceInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {labels = a} :: UpdateDeviceInstance)
{-# DEPRECATED udiLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

instance Lude.AWSRequest UpdateDeviceInstance where
  type Rs UpdateDeviceInstance = UpdateDeviceInstanceResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDeviceInstanceResponse'
            Lude.<$> (x Lude..?> "deviceInstance")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDeviceInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.UpdateDeviceInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDeviceInstance where
  toJSON UpdateDeviceInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("arn" Lude..= arn),
            ("profileArn" Lude..=) Lude.<$> profileARN,
            ("labels" Lude..=) Lude.<$> labels
          ]
      )

instance Lude.ToPath UpdateDeviceInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDeviceInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDeviceInstanceResponse' smart constructor.
data UpdateDeviceInstanceResponse = UpdateDeviceInstanceResponse'
  { -- | An object that contains information about your device instance.
    deviceInstance :: Lude.Maybe DeviceInstance,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDeviceInstanceResponse' with the minimum fields required to make a request.
--
-- * 'deviceInstance' - An object that contains information about your device instance.
-- * 'responseStatus' - The response status code.
mkUpdateDeviceInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDeviceInstanceResponse
mkUpdateDeviceInstanceResponse pResponseStatus_ =
  UpdateDeviceInstanceResponse'
    { deviceInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about your device instance.
--
-- /Note:/ Consider using 'deviceInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udirsDeviceInstance :: Lens.Lens' UpdateDeviceInstanceResponse (Lude.Maybe DeviceInstance)
udirsDeviceInstance = Lens.lens (deviceInstance :: UpdateDeviceInstanceResponse -> Lude.Maybe DeviceInstance) (\s a -> s {deviceInstance = a} :: UpdateDeviceInstanceResponse)
{-# DEPRECATED udirsDeviceInstance "Use generic-lens or generic-optics with 'deviceInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udirsResponseStatus :: Lens.Lens' UpdateDeviceInstanceResponse Lude.Int
udirsResponseStatus = Lens.lens (responseStatus :: UpdateDeviceInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDeviceInstanceResponse)
{-# DEPRECATED udirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
