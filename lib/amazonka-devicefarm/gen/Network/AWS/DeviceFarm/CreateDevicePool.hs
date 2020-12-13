{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateDevicePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device pool.
module Network.AWS.DeviceFarm.CreateDevicePool
  ( -- * Creating a request
    CreateDevicePool (..),
    mkCreateDevicePool,

    -- ** Request lenses
    cdpRules,
    cdpName,
    cdpMaxDevices,
    cdpProjectARN,
    cdpDescription,

    -- * Destructuring the response
    CreateDevicePoolResponse (..),
    mkCreateDevicePoolResponse,

    -- ** Response lenses
    cdprsDevicePool,
    cdprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the create device pool operation.
--
-- /See:/ 'mkCreateDevicePool' smart constructor.
data CreateDevicePool = CreateDevicePool'
  { -- | The device pool's rules.
    rules :: [Rule],
    -- | The device pool's name.
    name :: Lude.Text,
    -- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
    --
    -- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
    maxDevices :: Lude.Maybe Lude.Int,
    -- | The ARN of the project for the device pool.
    projectARN :: Lude.Text,
    -- | The device pool's description.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDevicePool' with the minimum fields required to make a request.
--
-- * 'rules' - The device pool's rules.
-- * 'name' - The device pool's name.
-- * 'maxDevices' - The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
-- * 'projectARN' - The ARN of the project for the device pool.
-- * 'description' - The device pool's description.
mkCreateDevicePool ::
  -- | 'name'
  Lude.Text ->
  -- | 'projectARN'
  Lude.Text ->
  CreateDevicePool
mkCreateDevicePool pName_ pProjectARN_ =
  CreateDevicePool'
    { rules = Lude.mempty,
      name = pName_,
      maxDevices = Lude.Nothing,
      projectARN = pProjectARN_,
      description = Lude.Nothing
    }

-- | The device pool's rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpRules :: Lens.Lens' CreateDevicePool [Rule]
cdpRules = Lens.lens (rules :: CreateDevicePool -> [Rule]) (\s a -> s {rules = a} :: CreateDevicePool)
{-# DEPRECATED cdpRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The device pool's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpName :: Lens.Lens' CreateDevicePool Lude.Text
cdpName = Lens.lens (name :: CreateDevicePool -> Lude.Text) (\s a -> s {name = a} :: CreateDevicePool)
{-# DEPRECATED cdpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
--
-- /Note:/ Consider using 'maxDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpMaxDevices :: Lens.Lens' CreateDevicePool (Lude.Maybe Lude.Int)
cdpMaxDevices = Lens.lens (maxDevices :: CreateDevicePool -> Lude.Maybe Lude.Int) (\s a -> s {maxDevices = a} :: CreateDevicePool)
{-# DEPRECATED cdpMaxDevices "Use generic-lens or generic-optics with 'maxDevices' instead." #-}

-- | The ARN of the project for the device pool.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpProjectARN :: Lens.Lens' CreateDevicePool Lude.Text
cdpProjectARN = Lens.lens (projectARN :: CreateDevicePool -> Lude.Text) (\s a -> s {projectARN = a} :: CreateDevicePool)
{-# DEPRECATED cdpProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

-- | The device pool's description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpDescription :: Lens.Lens' CreateDevicePool (Lude.Maybe Lude.Text)
cdpDescription = Lens.lens (description :: CreateDevicePool -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateDevicePool)
{-# DEPRECATED cdpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateDevicePool where
  type Rs CreateDevicePool = CreateDevicePoolResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDevicePoolResponse'
            Lude.<$> (x Lude..?> "devicePool") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDevicePool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.CreateDevicePool" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDevicePool where
  toJSON CreateDevicePool' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("rules" Lude..= rules),
            Lude.Just ("name" Lude..= name),
            ("maxDevices" Lude..=) Lude.<$> maxDevices,
            Lude.Just ("projectArn" Lude..= projectARN),
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateDevicePool where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDevicePool where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a create device pool request.
--
-- /See:/ 'mkCreateDevicePoolResponse' smart constructor.
data CreateDevicePoolResponse = CreateDevicePoolResponse'
  { -- | The newly created device pool.
    devicePool :: Lude.Maybe DevicePool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDevicePoolResponse' with the minimum fields required to make a request.
--
-- * 'devicePool' - The newly created device pool.
-- * 'responseStatus' - The response status code.
mkCreateDevicePoolResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDevicePoolResponse
mkCreateDevicePoolResponse pResponseStatus_ =
  CreateDevicePoolResponse'
    { devicePool = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly created device pool.
--
-- /Note:/ Consider using 'devicePool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdprsDevicePool :: Lens.Lens' CreateDevicePoolResponse (Lude.Maybe DevicePool)
cdprsDevicePool = Lens.lens (devicePool :: CreateDevicePoolResponse -> Lude.Maybe DevicePool) (\s a -> s {devicePool = a} :: CreateDevicePoolResponse)
{-# DEPRECATED cdprsDevicePool "Use generic-lens or generic-optics with 'devicePool' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdprsResponseStatus :: Lens.Lens' CreateDevicePoolResponse Lude.Int
cdprsResponseStatus = Lens.lens (responseStatus :: CreateDevicePoolResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDevicePoolResponse)
{-# DEPRECATED cdprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
