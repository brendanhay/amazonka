{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteDeviceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a device definition.
module Network.AWS.Greengrass.DeleteDeviceDefinition
  ( -- * Creating a request
    DeleteDeviceDefinition (..),
    mkDeleteDeviceDefinition,

    -- ** Request lenses
    dddDeviceDefinitionId,

    -- * Destructuring the response
    DeleteDeviceDefinitionResponse (..),
    mkDeleteDeviceDefinitionResponse,

    -- ** Response lenses
    dddrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDeviceDefinition' smart constructor.
newtype DeleteDeviceDefinition = DeleteDeviceDefinition'
  { -- | The ID of the device definition.
    deviceDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeviceDefinition' value with any optional fields omitted.
mkDeleteDeviceDefinition ::
  -- | 'deviceDefinitionId'
  Core.Text ->
  DeleteDeviceDefinition
mkDeleteDeviceDefinition deviceDefinitionId =
  DeleteDeviceDefinition' {deviceDefinitionId}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddDeviceDefinitionId :: Lens.Lens' DeleteDeviceDefinition Core.Text
dddDeviceDefinitionId = Lens.field @"deviceDefinitionId"
{-# DEPRECATED dddDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

instance Core.AWSRequest DeleteDeviceDefinition where
  type Rs DeleteDeviceDefinition = DeleteDeviceDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/devices/"
                Core.<> (Core.toText deviceDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDeviceDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDeviceDefinitionResponse' smart constructor.
newtype DeleteDeviceDefinitionResponse = DeleteDeviceDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeviceDefinitionResponse' value with any optional fields omitted.
mkDeleteDeviceDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDeviceDefinitionResponse
mkDeleteDeviceDefinitionResponse responseStatus =
  DeleteDeviceDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrrsResponseStatus :: Lens.Lens' DeleteDeviceDefinitionResponse Core.Int
dddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
