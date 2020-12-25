{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteQueryLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration for DNS query logging. If you delete a configuration, Amazon Route 53 stops sending query logs to CloudWatch Logs. Route 53 doesn't delete any logs that are already in CloudWatch Logs.
--
-- For more information about DNS query logs, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateQueryLoggingConfig.html CreateQueryLoggingConfig> .
module Network.AWS.Route53.DeleteQueryLoggingConfig
  ( -- * Creating a request
    DeleteQueryLoggingConfig (..),
    mkDeleteQueryLoggingConfig,

    -- ** Request lenses
    dqlcId,

    -- * Destructuring the response
    DeleteQueryLoggingConfigResponse (..),
    mkDeleteQueryLoggingConfigResponse,

    -- ** Response lenses
    dqlcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | /See:/ 'mkDeleteQueryLoggingConfig' smart constructor.
newtype DeleteQueryLoggingConfig = DeleteQueryLoggingConfig'
  { -- | The ID of the configuration that you want to delete.
    id :: Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueryLoggingConfig' value with any optional fields omitted.
mkDeleteQueryLoggingConfig ::
  -- | 'id'
  Types.Id ->
  DeleteQueryLoggingConfig
mkDeleteQueryLoggingConfig id = DeleteQueryLoggingConfig' {id}

-- | The ID of the configuration that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqlcId :: Lens.Lens' DeleteQueryLoggingConfig Types.Id
dqlcId = Lens.field @"id"
{-# DEPRECATED dqlcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest DeleteQueryLoggingConfig where
  type Rs DeleteQueryLoggingConfig = DeleteQueryLoggingConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/2013-04-01/queryloggingconfig/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteQueryLoggingConfigResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteQueryLoggingConfigResponse' smart constructor.
newtype DeleteQueryLoggingConfigResponse = DeleteQueryLoggingConfigResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueryLoggingConfigResponse' value with any optional fields omitted.
mkDeleteQueryLoggingConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteQueryLoggingConfigResponse
mkDeleteQueryLoggingConfigResponse responseStatus =
  DeleteQueryLoggingConfigResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqlcrrsResponseStatus :: Lens.Lens' DeleteQueryLoggingConfigResponse Core.Int
dqlcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dqlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
