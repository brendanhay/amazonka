{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.GetWorkflowExecutionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the history of the specified workflow execution. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the @nextPageToken@ returned by the initial call.
--
-- __Access Control__ 
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SWF.GetWorkflowExecutionHistory
    (
    -- * Creating a request
      GetWorkflowExecutionHistory (..)
    , mkGetWorkflowExecutionHistory
    -- ** Request lenses
    , gwehDomain
    , gwehExecution
    , gwehMaximumPageSize
    , gwehNextPageToken
    , gwehReverseOrder

    -- * Destructuring the response
    , GetWorkflowExecutionHistoryResponse (..)
    , mkGetWorkflowExecutionHistoryResponse
    -- ** Response lenses
    , gwehrrsEvents
    , gwehrrsNextPageToken
    , gwehrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkGetWorkflowExecutionHistory' smart constructor.
data GetWorkflowExecutionHistory = GetWorkflowExecutionHistory'
  { domain :: Types.DomainName
    -- ^ The name of the domain containing the workflow execution.
  , execution :: Types.WorkflowExecution
    -- ^ Specifies the workflow execution for which to return the history.
  , maximumPageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results. 
  , nextPageToken :: Core.Maybe Types.PageToken
    -- ^ If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ". 
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call. 
  , reverseOrder :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , returns the events in reverse order. By default the results are returned in ascending order of the @eventTimeStamp@ of the events.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetWorkflowExecutionHistory' value with any optional fields omitted.
mkGetWorkflowExecutionHistory
    :: Types.DomainName -- ^ 'domain'
    -> Types.WorkflowExecution -- ^ 'execution'
    -> GetWorkflowExecutionHistory
mkGetWorkflowExecutionHistory domain execution
  = GetWorkflowExecutionHistory'{domain, execution,
                                 maximumPageSize = Core.Nothing, nextPageToken = Core.Nothing,
                                 reverseOrder = Core.Nothing}

-- | The name of the domain containing the workflow execution.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehDomain :: Lens.Lens' GetWorkflowExecutionHistory Types.DomainName
gwehDomain = Lens.field @"domain"
{-# INLINEABLE gwehDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | Specifies the workflow execution for which to return the history.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehExecution :: Lens.Lens' GetWorkflowExecutionHistory Types.WorkflowExecution
gwehExecution = Lens.field @"execution"
{-# INLINEABLE gwehExecution #-}
{-# DEPRECATED execution "Use generic-lens or generic-optics with 'execution' instead"  #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results. 
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehMaximumPageSize :: Lens.Lens' GetWorkflowExecutionHistory (Core.Maybe Core.Natural)
gwehMaximumPageSize = Lens.field @"maximumPageSize"
{-# INLINEABLE gwehMaximumPageSize #-}
{-# DEPRECATED maximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead"  #-}

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ". 
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call. 
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehNextPageToken :: Lens.Lens' GetWorkflowExecutionHistory (Core.Maybe Types.PageToken)
gwehNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gwehNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | When set to @true@ , returns the events in reverse order. By default the results are returned in ascending order of the @eventTimeStamp@ of the events.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehReverseOrder :: Lens.Lens' GetWorkflowExecutionHistory (Core.Maybe Core.Bool)
gwehReverseOrder = Lens.field @"reverseOrder"
{-# INLINEABLE gwehReverseOrder #-}
{-# DEPRECATED reverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead"  #-}

instance Core.ToQuery GetWorkflowExecutionHistory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetWorkflowExecutionHistory where
        toHeaders GetWorkflowExecutionHistory{..}
          = Core.pure
              ("X-Amz-Target",
               "SimpleWorkflowService.GetWorkflowExecutionHistory")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON GetWorkflowExecutionHistory where
        toJSON GetWorkflowExecutionHistory{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domain" Core..= domain),
                  Core.Just ("execution" Core..= execution),
                  ("maximumPageSize" Core..=) Core.<$> maximumPageSize,
                  ("nextPageToken" Core..=) Core.<$> nextPageToken,
                  ("reverseOrder" Core..=) Core.<$> reverseOrder])

instance Core.AWSRequest GetWorkflowExecutionHistory where
        type Rs GetWorkflowExecutionHistory =
             GetWorkflowExecutionHistoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetWorkflowExecutionHistoryResponse' Core.<$>
                   (x Core..:? "events" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetWorkflowExecutionHistory where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"events") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextPageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | Paginated representation of a workflow history for a workflow execution. This is the up to date, complete and authoritative record of the events related to all tasks and events in the life of the workflow execution.
--
-- /See:/ 'mkGetWorkflowExecutionHistoryResponse' smart constructor.
data GetWorkflowExecutionHistoryResponse = GetWorkflowExecutionHistoryResponse'
  { events :: [Types.HistoryEvent]
    -- ^ The list of history events.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetWorkflowExecutionHistoryResponse' value with any optional fields omitted.
mkGetWorkflowExecutionHistoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetWorkflowExecutionHistoryResponse
mkGetWorkflowExecutionHistoryResponse responseStatus
  = GetWorkflowExecutionHistoryResponse'{events = Core.mempty,
                                         nextPageToken = Core.Nothing, responseStatus}

-- | The list of history events.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehrrsEvents :: Lens.Lens' GetWorkflowExecutionHistoryResponse [Types.HistoryEvent]
gwehrrsEvents = Lens.field @"events"
{-# INLINEABLE gwehrrsEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehrrsNextPageToken :: Lens.Lens' GetWorkflowExecutionHistoryResponse (Core.Maybe Types.NextPageToken)
gwehrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gwehrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehrrsResponseStatus :: Lens.Lens' GetWorkflowExecutionHistoryResponse Core.Int
gwehrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gwehrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
