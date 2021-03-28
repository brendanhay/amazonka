{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.GetQueryResults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the results from the specified query.
--
-- Only the fields requested in the query are returned, along with a @@ptr@ field, which is the identifier for the log record. You can use the value of @@ptr@ in a <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_GetLogRecord.html GetLogRecord> operation to get the full log record.
-- @GetQueryResults@ does not start a query execution. To run a query, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_StartQuery.html StartQuery> .
-- If the value of the @Status@ field in the output is @Running@ , this operation returns only partial results. If you see a value of @Scheduled@ or @Running@ for the status, you can retry the operation later to see the final results. 
module Network.AWS.CloudWatchLogs.GetQueryResults
    (
    -- * Creating a request
      GetQueryResults (..)
    , mkGetQueryResults
    -- ** Request lenses
    , gqrQueryId

    -- * Destructuring the response
    , GetQueryResultsResponse (..)
    , mkGetQueryResultsResponse
    -- ** Response lenses
    , gqrrrsResults
    , gqrrrsStatistics
    , gqrrrsStatus
    , gqrrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetQueryResults' smart constructor.
newtype GetQueryResults = GetQueryResults'
  { queryId :: Types.QueryId
    -- ^ The ID number of the query.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetQueryResults' value with any optional fields omitted.
mkGetQueryResults
    :: Types.QueryId -- ^ 'queryId'
    -> GetQueryResults
mkGetQueryResults queryId = GetQueryResults'{queryId}

-- | The ID number of the query.
--
-- /Note:/ Consider using 'queryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrQueryId :: Lens.Lens' GetQueryResults Types.QueryId
gqrQueryId = Lens.field @"queryId"
{-# INLINEABLE gqrQueryId #-}
{-# DEPRECATED queryId "Use generic-lens or generic-optics with 'queryId' instead"  #-}

instance Core.ToQuery GetQueryResults where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetQueryResults where
        toHeaders GetQueryResults{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.GetQueryResults")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetQueryResults where
        toJSON GetQueryResults{..}
          = Core.object
              (Core.catMaybes [Core.Just ("queryId" Core..= queryId)])

instance Core.AWSRequest GetQueryResults where
        type Rs GetQueryResults = GetQueryResultsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetQueryResultsResponse' Core.<$>
                   (x Core..:? "results") Core.<*> x Core..:? "statistics" Core.<*>
                     x Core..:? "status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetQueryResultsResponse' smart constructor.
data GetQueryResultsResponse = GetQueryResultsResponse'
  { results :: Core.Maybe [[Types.ResultField]]
    -- ^ The log events that matched the query criteria during the most recent time it ran.
--
-- The @results@ value is an array of arrays. Each log event is one object in the top-level array. Each of these log event objects is an array of @field@ /@value@ pairs.
  , statistics :: Core.Maybe Types.QueryStatistics
    -- ^ Includes the number of log events scanned by the query, the number of log events that matched the query criteria, and the total number of bytes in the log events that were scanned. These values reflect the full raw results of the query.
  , status :: Core.Maybe Types.QueryStatus
    -- ^ The status of the most recent running of the query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , @Timeout@ , and @Unknown@ .
--
-- Queries time out after 15 minutes of execution. To avoid having your queries time out, reduce the time range being searched or partition your query into a number of queries.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetQueryResultsResponse' value with any optional fields omitted.
mkGetQueryResultsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetQueryResultsResponse
mkGetQueryResultsResponse responseStatus
  = GetQueryResultsResponse'{results = Core.Nothing,
                             statistics = Core.Nothing, status = Core.Nothing, responseStatus}

-- | The log events that matched the query criteria during the most recent time it ran.
--
-- The @results@ value is an array of arrays. Each log event is one object in the top-level array. Each of these log event objects is an array of @field@ /@value@ pairs.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrrrsResults :: Lens.Lens' GetQueryResultsResponse (Core.Maybe [[Types.ResultField]])
gqrrrsResults = Lens.field @"results"
{-# INLINEABLE gqrrrsResults #-}
{-# DEPRECATED results "Use generic-lens or generic-optics with 'results' instead"  #-}

-- | Includes the number of log events scanned by the query, the number of log events that matched the query criteria, and the total number of bytes in the log events that were scanned. These values reflect the full raw results of the query.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrrrsStatistics :: Lens.Lens' GetQueryResultsResponse (Core.Maybe Types.QueryStatistics)
gqrrrsStatistics = Lens.field @"statistics"
{-# INLINEABLE gqrrrsStatistics #-}
{-# DEPRECATED statistics "Use generic-lens or generic-optics with 'statistics' instead"  #-}

-- | The status of the most recent running of the query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , @Timeout@ , and @Unknown@ .
--
-- Queries time out after 15 minutes of execution. To avoid having your queries time out, reduce the time range being searched or partition your query into a number of queries.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrrrsStatus :: Lens.Lens' GetQueryResultsResponse (Core.Maybe Types.QueryStatus)
gqrrrsStatus = Lens.field @"status"
{-# INLINEABLE gqrrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrrrsResponseStatus :: Lens.Lens' GetQueryResultsResponse Core.Int
gqrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gqrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
