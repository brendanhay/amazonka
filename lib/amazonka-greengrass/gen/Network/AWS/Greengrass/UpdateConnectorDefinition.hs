{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateConnectorDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a connector definition.
module Network.AWS.Greengrass.UpdateConnectorDefinition
  ( -- * Creating a request
    UpdateConnectorDefinition (..),
    mkUpdateConnectorDefinition,

    -- ** Request lenses
    uConnectorDefinitionId,
    uName,

    -- * Destructuring the response
    UpdateConnectorDefinitionResponse (..),
    mkUpdateConnectorDefinitionResponse,

    -- ** Response lenses
    ucdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateConnectorDefinition' smart constructor.
data UpdateConnectorDefinition = UpdateConnectorDefinition'
  { -- | The ID of the connector definition.
    connectorDefinitionId :: Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConnectorDefinition' value with any optional fields omitted.
mkUpdateConnectorDefinition ::
  -- | 'connectorDefinitionId'
  Core.Text ->
  UpdateConnectorDefinition
mkUpdateConnectorDefinition connectorDefinitionId =
  UpdateConnectorDefinition'
    { connectorDefinitionId,
      name = Core.Nothing
    }

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uConnectorDefinitionId :: Lens.Lens' UpdateConnectorDefinition Core.Text
uConnectorDefinitionId = Lens.field @"connectorDefinitionId"
{-# DEPRECATED uConnectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' UpdateConnectorDefinition (Core.Maybe Core.Text)
uName = Lens.field @"name"
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateConnectorDefinition where
  toJSON UpdateConnectorDefinition {..} =
    Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateConnectorDefinition where
  type
    Rs UpdateConnectorDefinition =
      UpdateConnectorDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/connectors/"
                Core.<> (Core.toText connectorDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConnectorDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateConnectorDefinitionResponse' smart constructor.
newtype UpdateConnectorDefinitionResponse = UpdateConnectorDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConnectorDefinitionResponse' value with any optional fields omitted.
mkUpdateConnectorDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateConnectorDefinitionResponse
mkUpdateConnectorDefinitionResponse responseStatus =
  UpdateConnectorDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucdrrsResponseStatus :: Lens.Lens' UpdateConnectorDefinitionResponse Core.Int
ucdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
