{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateLoggerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a logger definition.
module Network.AWS.Greengrass.UpdateLoggerDefinition
  ( -- * Creating a request
    UpdateLoggerDefinition (..),
    mkUpdateLoggerDefinition,

    -- ** Request lenses
    uldLoggerDefinitionId,
    uldName,

    -- * Destructuring the response
    UpdateLoggerDefinitionResponse (..),
    mkUpdateLoggerDefinitionResponse,

    -- ** Response lenses
    uldrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateLoggerDefinition' smart constructor.
data UpdateLoggerDefinition = UpdateLoggerDefinition'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateLoggerDefinition' value with any optional fields omitted.
mkUpdateLoggerDefinition ::
  -- | 'loggerDefinitionId'
  Core.Text ->
  UpdateLoggerDefinition
mkUpdateLoggerDefinition loggerDefinitionId =
  UpdateLoggerDefinition' {loggerDefinitionId, name = Core.Nothing}

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uldLoggerDefinitionId :: Lens.Lens' UpdateLoggerDefinition Core.Text
uldLoggerDefinitionId = Lens.field @"loggerDefinitionId"
{-# DEPRECATED uldLoggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uldName :: Lens.Lens' UpdateLoggerDefinition (Core.Maybe Core.Text)
uldName = Lens.field @"name"
{-# DEPRECATED uldName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateLoggerDefinition where
  toJSON UpdateLoggerDefinition {..} =
    Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateLoggerDefinition where
  type Rs UpdateLoggerDefinition = UpdateLoggerDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/loggers/"
                Core.<> (Core.toText loggerDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLoggerDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateLoggerDefinitionResponse' smart constructor.
newtype UpdateLoggerDefinitionResponse = UpdateLoggerDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateLoggerDefinitionResponse' value with any optional fields omitted.
mkUpdateLoggerDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateLoggerDefinitionResponse
mkUpdateLoggerDefinitionResponse responseStatus =
  UpdateLoggerDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uldrrsResponseStatus :: Lens.Lens' UpdateLoggerDefinitionResponse Core.Int
uldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
