{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetQueryExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a single execution of a query if you have access to the workgroup in which the query ran. Each time a query executes, information about the query execution is saved with a unique ID.
module Network.AWS.Athena.GetQueryExecution
  ( -- * Creating a request
    GetQueryExecution (..),
    mkGetQueryExecution,

    -- ** Request lenses
    gqeQueryExecutionId,

    -- * Destructuring the response
    GetQueryExecutionResponse (..),
    mkGetQueryExecutionResponse,

    -- ** Response lenses
    gqerrsQueryExecution,
    gqerrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetQueryExecution' smart constructor.
newtype GetQueryExecution = GetQueryExecution'
  { -- | The unique ID of the query execution.
    queryExecutionId :: Types.QueryExecutionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetQueryExecution' value with any optional fields omitted.
mkGetQueryExecution ::
  -- | 'queryExecutionId'
  Types.QueryExecutionId ->
  GetQueryExecution
mkGetQueryExecution queryExecutionId =
  GetQueryExecution' {queryExecutionId}

-- | The unique ID of the query execution.
--
-- /Note:/ Consider using 'queryExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqeQueryExecutionId :: Lens.Lens' GetQueryExecution Types.QueryExecutionId
gqeQueryExecutionId = Lens.field @"queryExecutionId"
{-# DEPRECATED gqeQueryExecutionId "Use generic-lens or generic-optics with 'queryExecutionId' instead." #-}

instance Core.FromJSON GetQueryExecution where
  toJSON GetQueryExecution {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("QueryExecutionId" Core..= queryExecutionId)]
      )

instance Core.AWSRequest GetQueryExecution where
  type Rs GetQueryExecution = GetQueryExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.GetQueryExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQueryExecutionResponse'
            Core.<$> (x Core..:? "QueryExecution")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetQueryExecutionResponse' smart constructor.
data GetQueryExecutionResponse = GetQueryExecutionResponse'
  { -- | Information about the query execution.
    queryExecution :: Core.Maybe Types.QueryExecution,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetQueryExecutionResponse' value with any optional fields omitted.
mkGetQueryExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetQueryExecutionResponse
mkGetQueryExecutionResponse responseStatus =
  GetQueryExecutionResponse'
    { queryExecution = Core.Nothing,
      responseStatus
    }

-- | Information about the query execution.
--
-- /Note:/ Consider using 'queryExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqerrsQueryExecution :: Lens.Lens' GetQueryExecutionResponse (Core.Maybe Types.QueryExecution)
gqerrsQueryExecution = Lens.field @"queryExecution"
{-# DEPRECATED gqerrsQueryExecution "Use generic-lens or generic-optics with 'queryExecution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqerrsResponseStatus :: Lens.Lens' GetQueryExecutionResponse Core.Int
gqerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gqerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
