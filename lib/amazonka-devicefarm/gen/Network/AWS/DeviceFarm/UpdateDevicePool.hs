{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    udpArn,
    udpClearMaxDevices,
    udpDescription,
    udpMaxDevices,
    udpName,
    udpRules,

    -- * Destructuring the response
    UpdateDevicePoolResponse (..),
    mkUpdateDevicePoolResponse,

    -- ** Response lenses
    udprrsDevicePool,
    udprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the update device pool operation.
--
-- /See:/ 'mkUpdateDevicePool' smart constructor.
data UpdateDevicePool = UpdateDevicePool'
  { -- | The Amazon Resource Name (ARN) of the Device Farm device pool to update.
    arn :: Types.Arn,
    -- | Sets whether the @maxDevices@ parameter applies to your device pool. If you set this parameter to @true@ , the @maxDevices@ parameter does not apply, and Device Farm does not limit the number of devices that it adds to your device pool. In this case, Device Farm adds all available devices that meet the criteria specified in the @rules@ parameter.
    --
    -- If you use this parameter in your request, you cannot use the @maxDevices@ parameter in the same request.
    clearMaxDevices :: Core.Maybe Core.Bool,
    -- | A description of the device pool to update.
    description :: Core.Maybe Types.Description,
    -- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and that meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
    --
    -- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
    -- If you use this parameter in your request, you cannot use the @clearMaxDevices@ parameter in the same request.
    maxDevices :: Core.Maybe Core.Int,
    -- | A string that represents the name of the device pool to update.
    name :: Core.Maybe Types.Name,
    -- | Represents the rules to modify for the device pool. Updating rules is optional. If you update rules for your request, the update replaces the existing rules.
    rules :: Core.Maybe [Types.Rule]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDevicePool' value with any optional fields omitted.
mkUpdateDevicePool ::
  -- | 'arn'
  Types.Arn ->
  UpdateDevicePool
mkUpdateDevicePool arn =
  UpdateDevicePool'
    { arn,
      clearMaxDevices = Core.Nothing,
      description = Core.Nothing,
      maxDevices = Core.Nothing,
      name = Core.Nothing,
      rules = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Device Farm device pool to update.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpArn :: Lens.Lens' UpdateDevicePool Types.Arn
udpArn = Lens.field @"arn"
{-# DEPRECATED udpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Sets whether the @maxDevices@ parameter applies to your device pool. If you set this parameter to @true@ , the @maxDevices@ parameter does not apply, and Device Farm does not limit the number of devices that it adds to your device pool. In this case, Device Farm adds all available devices that meet the criteria specified in the @rules@ parameter.
--
-- If you use this parameter in your request, you cannot use the @maxDevices@ parameter in the same request.
--
-- /Note:/ Consider using 'clearMaxDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpClearMaxDevices :: Lens.Lens' UpdateDevicePool (Core.Maybe Core.Bool)
udpClearMaxDevices = Lens.field @"clearMaxDevices"
{-# DEPRECATED udpClearMaxDevices "Use generic-lens or generic-optics with 'clearMaxDevices' instead." #-}

-- | A description of the device pool to update.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpDescription :: Lens.Lens' UpdateDevicePool (Core.Maybe Types.Description)
udpDescription = Lens.field @"description"
{-# DEPRECATED udpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and that meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
-- If you use this parameter in your request, you cannot use the @clearMaxDevices@ parameter in the same request.
--
-- /Note:/ Consider using 'maxDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpMaxDevices :: Lens.Lens' UpdateDevicePool (Core.Maybe Core.Int)
udpMaxDevices = Lens.field @"maxDevices"
{-# DEPRECATED udpMaxDevices "Use generic-lens or generic-optics with 'maxDevices' instead." #-}

-- | A string that represents the name of the device pool to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpName :: Lens.Lens' UpdateDevicePool (Core.Maybe Types.Name)
udpName = Lens.field @"name"
{-# DEPRECATED udpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Represents the rules to modify for the device pool. Updating rules is optional. If you update rules for your request, the update replaces the existing rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpRules :: Lens.Lens' UpdateDevicePool (Core.Maybe [Types.Rule])
udpRules = Lens.field @"rules"
{-# DEPRECATED udpRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Core.FromJSON UpdateDevicePool where
  toJSON UpdateDevicePool {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("arn" Core..= arn),
            ("clearMaxDevices" Core..=) Core.<$> clearMaxDevices,
            ("description" Core..=) Core.<$> description,
            ("maxDevices" Core..=) Core.<$> maxDevices,
            ("name" Core..=) Core.<$> name,
            ("rules" Core..=) Core.<$> rules
          ]
      )

instance Core.AWSRequest UpdateDevicePool where
  type Rs UpdateDevicePool = UpdateDevicePoolResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.UpdateDevicePool")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDevicePoolResponse'
            Core.<$> (x Core..:? "devicePool") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of an update device pool request.
--
-- /See:/ 'mkUpdateDevicePoolResponse' smart constructor.
data UpdateDevicePoolResponse = UpdateDevicePoolResponse'
  { -- | The device pool you just updated.
    devicePool :: Core.Maybe Types.DevicePool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDevicePoolResponse' value with any optional fields omitted.
mkUpdateDevicePoolResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDevicePoolResponse
mkUpdateDevicePoolResponse responseStatus =
  UpdateDevicePoolResponse'
    { devicePool = Core.Nothing,
      responseStatus
    }

-- | The device pool you just updated.
--
-- /Note:/ Consider using 'devicePool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udprrsDevicePool :: Lens.Lens' UpdateDevicePoolResponse (Core.Maybe Types.DevicePool)
udprrsDevicePool = Lens.field @"devicePool"
{-# DEPRECATED udprrsDevicePool "Use generic-lens or generic-optics with 'devicePool' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udprrsResponseStatus :: Lens.Lens' UpdateDevicePoolResponse Core.Int
udprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
