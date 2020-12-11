{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateDevicePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the name, description, and rules in a device pool given the attributes and the pool ARN. Rule updates are all-or-nothing, meaning they can only be updated as a whole (or not at all).
module Network.AWS.DeviceFarm.UpdateDevicePool
  ( -- * Creating a request
    UpdateDevicePool (..),
    mkUpdateDevicePool,

    -- ** Request lenses
    udpRules,
    udpClearMaxDevices,
    udpName,
    udpMaxDevices,
    udpDescription,
    udpArn,

    -- * Destructuring the response
    UpdateDevicePoolResponse (..),
    mkUpdateDevicePoolResponse,

    -- ** Response lenses
    udprsDevicePool,
    udprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the update device pool operation.
--
-- /See:/ 'mkUpdateDevicePool' smart constructor.
data UpdateDevicePool = UpdateDevicePool'
  { rules ::
      Lude.Maybe [Rule],
    clearMaxDevices :: Lude.Maybe Lude.Bool,
    name :: Lude.Maybe Lude.Text,
    maxDevices :: Lude.Maybe Lude.Int,
    description :: Lude.Maybe Lude.Text,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDevicePool' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the Device Farm device pool to update.
-- * 'clearMaxDevices' - Sets whether the @maxDevices@ parameter applies to your device pool. If you set this parameter to @true@ , the @maxDevices@ parameter does not apply, and Device Farm does not limit the number of devices that it adds to your device pool. In this case, Device Farm adds all available devices that meet the criteria specified in the @rules@ parameter.
--
-- If you use this parameter in your request, you cannot use the @maxDevices@ parameter in the same request.
-- * 'description' - A description of the device pool to update.
-- * 'maxDevices' - The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and that meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
-- If you use this parameter in your request, you cannot use the @clearMaxDevices@ parameter in the same request.
-- * 'name' - A string that represents the name of the device pool to update.
-- * 'rules' - Represents the rules to modify for the device pool. Updating rules is optional. If you update rules for your request, the update replaces the existing rules.
mkUpdateDevicePool ::
  -- | 'arn'
  Lude.Text ->
  UpdateDevicePool
mkUpdateDevicePool pArn_ =
  UpdateDevicePool'
    { rules = Lude.Nothing,
      clearMaxDevices = Lude.Nothing,
      name = Lude.Nothing,
      maxDevices = Lude.Nothing,
      description = Lude.Nothing,
      arn = pArn_
    }

-- | Represents the rules to modify for the device pool. Updating rules is optional. If you update rules for your request, the update replaces the existing rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpRules :: Lens.Lens' UpdateDevicePool (Lude.Maybe [Rule])
udpRules = Lens.lens (rules :: UpdateDevicePool -> Lude.Maybe [Rule]) (\s a -> s {rules = a} :: UpdateDevicePool)
{-# DEPRECATED udpRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | Sets whether the @maxDevices@ parameter applies to your device pool. If you set this parameter to @true@ , the @maxDevices@ parameter does not apply, and Device Farm does not limit the number of devices that it adds to your device pool. In this case, Device Farm adds all available devices that meet the criteria specified in the @rules@ parameter.
--
-- If you use this parameter in your request, you cannot use the @maxDevices@ parameter in the same request.
--
-- /Note:/ Consider using 'clearMaxDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpClearMaxDevices :: Lens.Lens' UpdateDevicePool (Lude.Maybe Lude.Bool)
udpClearMaxDevices = Lens.lens (clearMaxDevices :: UpdateDevicePool -> Lude.Maybe Lude.Bool) (\s a -> s {clearMaxDevices = a} :: UpdateDevicePool)
{-# DEPRECATED udpClearMaxDevices "Use generic-lens or generic-optics with 'clearMaxDevices' instead." #-}

-- | A string that represents the name of the device pool to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpName :: Lens.Lens' UpdateDevicePool (Lude.Maybe Lude.Text)
udpName = Lens.lens (name :: UpdateDevicePool -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateDevicePool)
{-# DEPRECATED udpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and that meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
-- If you use this parameter in your request, you cannot use the @clearMaxDevices@ parameter in the same request.
--
-- /Note:/ Consider using 'maxDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpMaxDevices :: Lens.Lens' UpdateDevicePool (Lude.Maybe Lude.Int)
udpMaxDevices = Lens.lens (maxDevices :: UpdateDevicePool -> Lude.Maybe Lude.Int) (\s a -> s {maxDevices = a} :: UpdateDevicePool)
{-# DEPRECATED udpMaxDevices "Use generic-lens or generic-optics with 'maxDevices' instead." #-}

-- | A description of the device pool to update.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpDescription :: Lens.Lens' UpdateDevicePool (Lude.Maybe Lude.Text)
udpDescription = Lens.lens (description :: UpdateDevicePool -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateDevicePool)
{-# DEPRECATED udpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of the Device Farm device pool to update.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpArn :: Lens.Lens' UpdateDevicePool Lude.Text
udpArn = Lens.lens (arn :: UpdateDevicePool -> Lude.Text) (\s a -> s {arn = a} :: UpdateDevicePool)
{-# DEPRECATED udpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest UpdateDevicePool where
  type Rs UpdateDevicePool = UpdateDevicePoolResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDevicePoolResponse'
            Lude.<$> (x Lude..?> "devicePool") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDevicePool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.UpdateDevicePool" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDevicePool where
  toJSON UpdateDevicePool' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("rules" Lude..=) Lude.<$> rules,
            ("clearMaxDevices" Lude..=) Lude.<$> clearMaxDevices,
            ("name" Lude..=) Lude.<$> name,
            ("maxDevices" Lude..=) Lude.<$> maxDevices,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("arn" Lude..= arn)
          ]
      )

instance Lude.ToPath UpdateDevicePool where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDevicePool where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of an update device pool request.
--
-- /See:/ 'mkUpdateDevicePoolResponse' smart constructor.
data UpdateDevicePoolResponse = UpdateDevicePoolResponse'
  { devicePool ::
      Lude.Maybe DevicePool,
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

-- | Creates a value of 'UpdateDevicePoolResponse' with the minimum fields required to make a request.
--
-- * 'devicePool' - The device pool you just updated.
-- * 'responseStatus' - The response status code.
mkUpdateDevicePoolResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDevicePoolResponse
mkUpdateDevicePoolResponse pResponseStatus_ =
  UpdateDevicePoolResponse'
    { devicePool = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The device pool you just updated.
--
-- /Note:/ Consider using 'devicePool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udprsDevicePool :: Lens.Lens' UpdateDevicePoolResponse (Lude.Maybe DevicePool)
udprsDevicePool = Lens.lens (devicePool :: UpdateDevicePoolResponse -> Lude.Maybe DevicePool) (\s a -> s {devicePool = a} :: UpdateDevicePoolResponse)
{-# DEPRECATED udprsDevicePool "Use generic-lens or generic-optics with 'devicePool' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udprsResponseStatus :: Lens.Lens' UpdateDevicePoolResponse Lude.Int
udprsResponseStatus = Lens.lens (responseStatus :: UpdateDevicePoolResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDevicePoolResponse)
{-# DEPRECATED udprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
