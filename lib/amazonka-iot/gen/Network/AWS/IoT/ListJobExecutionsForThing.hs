{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListJobExecutionsForThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for the specified thing.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListJobExecutionsForThing
    (
    -- * Creating a request
      ListJobExecutionsForThing (..)
    , mkListJobExecutionsForThing
    -- ** Request lenses
    , ljeftThingName
    , ljeftMaxResults
    , ljeftNamespaceId
    , ljeftNextToken
    , ljeftStatus

    -- * Destructuring the response
    , ListJobExecutionsForThingResponse (..)
    , mkListJobExecutionsForThingResponse
    -- ** Response lenses
    , ljeftrrsExecutionSummaries
    , ljeftrrsNextToken
    , ljeftrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListJobExecutionsForThing' smart constructor.
data ListJobExecutionsForThing = ListJobExecutionsForThing'
  { thingName :: Types.ThingName
    -- ^ The thing name.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to be returned per request.
  , namespaceId :: Core.Maybe Types.NamespaceId
    -- ^ The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@ 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next set of results.
  , status :: Core.Maybe Types.JobExecutionStatus
    -- ^ An optional filter that lets you search for jobs that have the specified status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobExecutionsForThing' value with any optional fields omitted.
mkListJobExecutionsForThing
    :: Types.ThingName -- ^ 'thingName'
    -> ListJobExecutionsForThing
mkListJobExecutionsForThing thingName
  = ListJobExecutionsForThing'{thingName, maxResults = Core.Nothing,
                               namespaceId = Core.Nothing, nextToken = Core.Nothing,
                               status = Core.Nothing}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftThingName :: Lens.Lens' ListJobExecutionsForThing Types.ThingName
ljeftThingName = Lens.field @"thingName"
{-# INLINEABLE ljeftThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftMaxResults :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe Core.Natural)
ljeftMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ljeftMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@ 
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftNamespaceId :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe Types.NamespaceId)
ljeftNamespaceId = Lens.field @"namespaceId"
{-# INLINEABLE ljeftNamespaceId #-}
{-# DEPRECATED namespaceId "Use generic-lens or generic-optics with 'namespaceId' instead"  #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftNextToken :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe Types.NextToken)
ljeftNextToken = Lens.field @"nextToken"
{-# INLINEABLE ljeftNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An optional filter that lets you search for jobs that have the specified status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftStatus :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe Types.JobExecutionStatus)
ljeftStatus = Lens.field @"status"
{-# INLINEABLE ljeftStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery ListJobExecutionsForThing where
        toQuery ListJobExecutionsForThing{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "namespaceId") namespaceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "status") status

instance Core.ToHeaders ListJobExecutionsForThing where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListJobExecutionsForThing where
        type Rs ListJobExecutionsForThing =
             ListJobExecutionsForThingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/things/" Core.<> Core.toText thingName Core.<> "/jobs",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListJobExecutionsForThingResponse' Core.<$>
                   (x Core..:? "executionSummaries") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListJobExecutionsForThing where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"executionSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListJobExecutionsForThingResponse' smart constructor.
data ListJobExecutionsForThingResponse = ListJobExecutionsForThingResponse'
  { executionSummaries :: Core.Maybe [Types.JobExecutionSummaryForThing]
    -- ^ A list of job execution summaries.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results, or __null__ if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListJobExecutionsForThingResponse' value with any optional fields omitted.
mkListJobExecutionsForThingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListJobExecutionsForThingResponse
mkListJobExecutionsForThingResponse responseStatus
  = ListJobExecutionsForThingResponse'{executionSummaries =
                                         Core.Nothing,
                                       nextToken = Core.Nothing, responseStatus}

-- | A list of job execution summaries.
--
-- /Note:/ Consider using 'executionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftrrsExecutionSummaries :: Lens.Lens' ListJobExecutionsForThingResponse (Core.Maybe [Types.JobExecutionSummaryForThing])
ljeftrrsExecutionSummaries = Lens.field @"executionSummaries"
{-# INLINEABLE ljeftrrsExecutionSummaries #-}
{-# DEPRECATED executionSummaries "Use generic-lens or generic-optics with 'executionSummaries' instead"  #-}

-- | The token for the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftrrsNextToken :: Lens.Lens' ListJobExecutionsForThingResponse (Core.Maybe Types.NextToken)
ljeftrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ljeftrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljeftrrsResponseStatus :: Lens.Lens' ListJobExecutionsForThingResponse Core.Int
ljeftrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ljeftrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
