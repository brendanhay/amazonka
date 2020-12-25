{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateResourceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a resource definition.
module Network.AWS.Greengrass.UpdateResourceDefinition
  ( -- * Creating a request
    UpdateResourceDefinition (..),
    mkUpdateResourceDefinition,

    -- ** Request lenses
    urdResourceDefinitionId,
    urdName,

    -- * Destructuring the response
    UpdateResourceDefinitionResponse (..),
    mkUpdateResourceDefinitionResponse,

    -- ** Response lenses
    urdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateResourceDefinition' smart constructor.
data UpdateResourceDefinition = UpdateResourceDefinition'
  { -- | The ID of the resource definition.
    resourceDefinitionId :: Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResourceDefinition' value with any optional fields omitted.
mkUpdateResourceDefinition ::
  -- | 'resourceDefinitionId'
  Core.Text ->
  UpdateResourceDefinition
mkUpdateResourceDefinition resourceDefinitionId =
  UpdateResourceDefinition'
    { resourceDefinitionId,
      name = Core.Nothing
    }

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdResourceDefinitionId :: Lens.Lens' UpdateResourceDefinition Core.Text
urdResourceDefinitionId = Lens.field @"resourceDefinitionId"
{-# DEPRECATED urdResourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdName :: Lens.Lens' UpdateResourceDefinition (Core.Maybe Core.Text)
urdName = Lens.field @"name"
{-# DEPRECATED urdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateResourceDefinition where
  toJSON UpdateResourceDefinition {..} =
    Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateResourceDefinition where
  type Rs UpdateResourceDefinition = UpdateResourceDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/resources/"
                Core.<> (Core.toText resourceDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResourceDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateResourceDefinitionResponse' smart constructor.
newtype UpdateResourceDefinitionResponse = UpdateResourceDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResourceDefinitionResponse' value with any optional fields omitted.
mkUpdateResourceDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateResourceDefinitionResponse
mkUpdateResourceDefinitionResponse responseStatus =
  UpdateResourceDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdrrsResponseStatus :: Lens.Lens' UpdateResourceDefinitionResponse Core.Int
urdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
