{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteConnectorDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connector definition.
module Network.AWS.Greengrass.DeleteConnectorDefinition
  ( -- * Creating a request
    DeleteConnectorDefinition (..),
    mkDeleteConnectorDefinition,

    -- ** Request lenses
    dcdConnectorDefinitionId,

    -- * Destructuring the response
    DeleteConnectorDefinitionResponse (..),
    mkDeleteConnectorDefinitionResponse,

    -- ** Response lenses
    dcdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteConnectorDefinition' smart constructor.
newtype DeleteConnectorDefinition = DeleteConnectorDefinition'
  { -- | The ID of the connector definition.
    connectorDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnectorDefinition' value with any optional fields omitted.
mkDeleteConnectorDefinition ::
  -- | 'connectorDefinitionId'
  Core.Text ->
  DeleteConnectorDefinition
mkDeleteConnectorDefinition connectorDefinitionId =
  DeleteConnectorDefinition' {connectorDefinitionId}

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdConnectorDefinitionId :: Lens.Lens' DeleteConnectorDefinition Core.Text
dcdConnectorDefinitionId = Lens.field @"connectorDefinitionId"
{-# DEPRECATED dcdConnectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead." #-}

instance Core.AWSRequest DeleteConnectorDefinition where
  type
    Rs DeleteConnectorDefinition =
      DeleteConnectorDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/connectors/"
                Core.<> (Core.toText connectorDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConnectorDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteConnectorDefinitionResponse' smart constructor.
newtype DeleteConnectorDefinitionResponse = DeleteConnectorDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnectorDefinitionResponse' value with any optional fields omitted.
mkDeleteConnectorDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteConnectorDefinitionResponse
mkDeleteConnectorDefinitionResponse responseStatus =
  DeleteConnectorDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrrsResponseStatus :: Lens.Lens' DeleteConnectorDefinitionResponse Core.Int
dcdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
