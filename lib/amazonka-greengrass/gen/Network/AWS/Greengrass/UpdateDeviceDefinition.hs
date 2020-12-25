{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateDeviceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a device definition.
module Network.AWS.Greengrass.UpdateDeviceDefinition
  ( -- * Creating a request
    UpdateDeviceDefinition (..),
    mkUpdateDeviceDefinition,

    -- ** Request lenses
    uddDeviceDefinitionId,
    uddName,

    -- * Destructuring the response
    UpdateDeviceDefinitionResponse (..),
    mkUpdateDeviceDefinitionResponse,

    -- ** Response lenses
    uddrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDeviceDefinition' smart constructor.
data UpdateDeviceDefinition = UpdateDeviceDefinition'
  { -- | The ID of the device definition.
    deviceDefinitionId :: Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDeviceDefinition' value with any optional fields omitted.
mkUpdateDeviceDefinition ::
  -- | 'deviceDefinitionId'
  Core.Text ->
  UpdateDeviceDefinition
mkUpdateDeviceDefinition deviceDefinitionId =
  UpdateDeviceDefinition' {deviceDefinitionId, name = Core.Nothing}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddDeviceDefinitionId :: Lens.Lens' UpdateDeviceDefinition Core.Text
uddDeviceDefinitionId = Lens.field @"deviceDefinitionId"
{-# DEPRECATED uddDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddName :: Lens.Lens' UpdateDeviceDefinition (Core.Maybe Core.Text)
uddName = Lens.field @"name"
{-# DEPRECATED uddName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateDeviceDefinition where
  toJSON UpdateDeviceDefinition {..} =
    Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateDeviceDefinition where
  type Rs UpdateDeviceDefinition = UpdateDeviceDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/devices/"
                Core.<> (Core.toText deviceDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDeviceDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDeviceDefinitionResponse' smart constructor.
newtype UpdateDeviceDefinitionResponse = UpdateDeviceDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDeviceDefinitionResponse' value with any optional fields omitted.
mkUpdateDeviceDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDeviceDefinitionResponse
mkUpdateDeviceDefinitionResponse responseStatus =
  UpdateDeviceDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddrrsResponseStatus :: Lens.Lens' UpdateDeviceDefinitionResponse Core.Int
uddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
