{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteLoggerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a logger definition.
module Network.AWS.Greengrass.DeleteLoggerDefinition
  ( -- * Creating a request
    DeleteLoggerDefinition (..),
    mkDeleteLoggerDefinition,

    -- ** Request lenses
    dldLoggerDefinitionId,

    -- * Destructuring the response
    DeleteLoggerDefinitionResponse (..),
    mkDeleteLoggerDefinitionResponse,

    -- ** Response lenses
    dldrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLoggerDefinition' smart constructor.
newtype DeleteLoggerDefinition = DeleteLoggerDefinition'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoggerDefinition' value with any optional fields omitted.
mkDeleteLoggerDefinition ::
  -- | 'loggerDefinitionId'
  Core.Text ->
  DeleteLoggerDefinition
mkDeleteLoggerDefinition loggerDefinitionId =
  DeleteLoggerDefinition' {loggerDefinitionId}

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldLoggerDefinitionId :: Lens.Lens' DeleteLoggerDefinition Core.Text
dldLoggerDefinitionId = Lens.field @"loggerDefinitionId"
{-# DEPRECATED dldLoggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead." #-}

instance Core.AWSRequest DeleteLoggerDefinition where
  type Rs DeleteLoggerDefinition = DeleteLoggerDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/loggers/"
                Core.<> (Core.toText loggerDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLoggerDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteLoggerDefinitionResponse' smart constructor.
newtype DeleteLoggerDefinitionResponse = DeleteLoggerDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoggerDefinitionResponse' value with any optional fields omitted.
mkDeleteLoggerDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteLoggerDefinitionResponse
mkDeleteLoggerDefinitionResponse responseStatus =
  DeleteLoggerDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldrrsResponseStatus :: Lens.Lens' DeleteLoggerDefinitionResponse Core.Int
dldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
