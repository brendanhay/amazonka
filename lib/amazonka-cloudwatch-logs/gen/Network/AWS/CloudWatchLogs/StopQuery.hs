{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.StopQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a CloudWatch Logs Insights query that is in progress. If the query has already ended, the operation returns an error indicating that the specified query is not running.
module Network.AWS.CloudWatchLogs.StopQuery
  ( -- * Creating a request
    StopQuery (..),
    mkStopQuery,

    -- ** Request lenses
    sqQueryId,

    -- * Destructuring the response
    StopQueryResponse (..),
    mkStopQueryResponse,

    -- ** Response lenses
    sqrrsSuccess,
    sqrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopQuery' smart constructor.
newtype StopQuery = StopQuery'
  { -- | The ID number of the query to stop. To find this ID number, use @DescribeQueries@ .
    queryId :: Types.QueryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopQuery' value with any optional fields omitted.
mkStopQuery ::
  -- | 'queryId'
  Types.QueryId ->
  StopQuery
mkStopQuery queryId = StopQuery' {queryId}

-- | The ID number of the query to stop. To find this ID number, use @DescribeQueries@ .
--
-- /Note:/ Consider using 'queryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqQueryId :: Lens.Lens' StopQuery Types.QueryId
sqQueryId = Lens.field @"queryId"
{-# DEPRECATED sqQueryId "Use generic-lens or generic-optics with 'queryId' instead." #-}

instance Core.FromJSON StopQuery where
  toJSON StopQuery {..} =
    Core.object
      (Core.catMaybes [Core.Just ("queryId" Core..= queryId)])

instance Core.AWSRequest StopQuery where
  type Rs StopQuery = StopQueryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.StopQuery")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopQueryResponse'
            Core.<$> (x Core..:? "success") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopQueryResponse' smart constructor.
data StopQueryResponse = StopQueryResponse'
  { -- | This is true if the query was stopped by the @StopQuery@ operation.
    success :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopQueryResponse' value with any optional fields omitted.
mkStopQueryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopQueryResponse
mkStopQueryResponse responseStatus =
  StopQueryResponse' {success = Core.Nothing, responseStatus}

-- | This is true if the query was stopped by the @StopQuery@ operation.
--
-- /Note:/ Consider using 'success' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqrrsSuccess :: Lens.Lens' StopQueryResponse (Core.Maybe Core.Bool)
sqrrsSuccess = Lens.field @"success"
{-# DEPRECATED sqrrsSuccess "Use generic-lens or generic-optics with 'success' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqrrsResponseStatus :: Lens.Lens' StopQueryResponse Core.Int
sqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
