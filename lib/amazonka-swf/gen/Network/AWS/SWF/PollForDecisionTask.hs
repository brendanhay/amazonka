{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.PollForDecisionTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by deciders to get a 'DecisionTask' from the specified decision @taskList@ . A decision task may be returned for any open workflow execution that is using the specified task list. The task includes a paginated view of the history of the workflow execution. The decider should use the workflow type and the history to determine how to properly handle the task.
--
-- This action initiates a long poll, where the service holds the HTTP connection open and responds as soon a task becomes available. If no decision task is available in the specified task list before the timeout of 60 seconds expires, an empty result is returned. An empty result, in this context, means that a DecisionTask is returned, but that the value of taskToken is an empty string.
-- /Important:/ Deciders should set their client side socket timeout to at least 70 seconds (10 seconds higher than the timeout).
-- /Important:/ Because the number of workflow history events for a single workflow execution might be very large, the result returned might be split up across a number of pages. To retrieve subsequent pages, make additional calls to @PollForDecisionTask@ using the @nextPageToken@ returned by the initial call. Note that you do /not/ call @GetWorkflowExecutionHistory@ with this @nextPageToken@ . Instead, call @PollForDecisionTask@ again.
-- __Access Control__ 
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the @taskList.name@ parameter by using a @Condition@ element with the @swf:taskList.name@ key to allow the action to access only certain task lists.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SWF.PollForDecisionTask
    (
    -- * Creating a request
      PollForDecisionTask (..)
    , mkPollForDecisionTask
    -- ** Request lenses
    , pfdtDomain
    , pfdtTaskList
    , pfdtIdentity
    , pfdtMaximumPageSize
    , pfdtNextPageToken
    , pfdtReverseOrder

    -- * Destructuring the response
    , PollForDecisionTaskResponse (..)
    , mkPollForDecisionTaskResponse
    -- ** Response lenses
    , pfdtrrsTaskToken
    , pfdtrrsStartedEventId
    , pfdtrrsWorkflowExecution
    , pfdtrrsWorkflowType
    , pfdtrrsEvents
    , pfdtrrsNextPageToken
    , pfdtrrsPreviousStartedEventId
    , pfdtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkPollForDecisionTask' smart constructor.
data PollForDecisionTask = PollForDecisionTask'
  { domain :: Types.Domain
    -- ^ The name of the domain containing the task lists to poll.
  , taskList :: Types.TaskList
    -- ^ Specifies the task list to poll for decision tasks.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
  , identity :: Core.Maybe Types.Identity
    -- ^ Identity of the decider making the request, which is recorded in the DecisionTaskStarted event in the workflow history. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
  , maximumPageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results. 
--
-- This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ". 
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call. 
  , reverseOrder :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , returns the events in reverse order. By default the results are returned in ascending order of the @eventTimestamp@ of the events.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PollForDecisionTask' value with any optional fields omitted.
mkPollForDecisionTask
    :: Types.Domain -- ^ 'domain'
    -> Types.TaskList -- ^ 'taskList'
    -> PollForDecisionTask
mkPollForDecisionTask domain taskList
  = PollForDecisionTask'{domain, taskList, identity = Core.Nothing,
                         maximumPageSize = Core.Nothing, nextPageToken = Core.Nothing,
                         reverseOrder = Core.Nothing}

-- | The name of the domain containing the task lists to poll.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtDomain :: Lens.Lens' PollForDecisionTask Types.Domain
pfdtDomain = Lens.field @"domain"
{-# INLINEABLE pfdtDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | Specifies the task list to poll for decision tasks.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtTaskList :: Lens.Lens' PollForDecisionTask Types.TaskList
pfdtTaskList = Lens.field @"taskList"
{-# INLINEABLE pfdtTaskList #-}
{-# DEPRECATED taskList "Use generic-lens or generic-optics with 'taskList' instead"  #-}

-- | Identity of the decider making the request, which is recorded in the DecisionTaskStarted event in the workflow history. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtIdentity :: Lens.Lens' PollForDecisionTask (Core.Maybe Types.Identity)
pfdtIdentity = Lens.field @"identity"
{-# INLINEABLE pfdtIdentity #-}
{-# DEPRECATED identity "Use generic-lens or generic-optics with 'identity' instead"  #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results. 
--
-- This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtMaximumPageSize :: Lens.Lens' PollForDecisionTask (Core.Maybe Core.Natural)
pfdtMaximumPageSize = Lens.field @"maximumPageSize"
{-# INLINEABLE pfdtMaximumPageSize #-}
{-# DEPRECATED maximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead"  #-}

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ". 
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call. 
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtNextPageToken :: Lens.Lens' PollForDecisionTask (Core.Maybe Types.NextPageToken)
pfdtNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE pfdtNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | When set to @true@ , returns the events in reverse order. By default the results are returned in ascending order of the @eventTimestamp@ of the events.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtReverseOrder :: Lens.Lens' PollForDecisionTask (Core.Maybe Core.Bool)
pfdtReverseOrder = Lens.field @"reverseOrder"
{-# INLINEABLE pfdtReverseOrder #-}
{-# DEPRECATED reverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead"  #-}

instance Core.ToQuery PollForDecisionTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PollForDecisionTask where
        toHeaders PollForDecisionTask{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.PollForDecisionTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON PollForDecisionTask where
        toJSON PollForDecisionTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domain" Core..= domain),
                  Core.Just ("taskList" Core..= taskList),
                  ("identity" Core..=) Core.<$> identity,
                  ("maximumPageSize" Core..=) Core.<$> maximumPageSize,
                  ("nextPageToken" Core..=) Core.<$> nextPageToken,
                  ("reverseOrder" Core..=) Core.<$> reverseOrder])

instance Core.AWSRequest PollForDecisionTask where
        type Rs PollForDecisionTask = PollForDecisionTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PollForDecisionTaskResponse' Core.<$>
                   (x Core..:? "taskToken") Core.<*> x Core..: "startedEventId"
                     Core.<*> x Core..:? "workflowExecution"
                     Core.<*> x Core..:? "workflowType"
                     Core.<*> x Core..:? "events"
                     Core.<*> x Core..:? "nextPageToken"
                     Core.<*> x Core..:? "previousStartedEventId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager PollForDecisionTask where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"events" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextPageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | A structure that represents a decision task. Decision tasks are sent to deciders in order for them to make decisions.
--
-- /See:/ 'mkPollForDecisionTaskResponse' smart constructor.
data PollForDecisionTaskResponse = PollForDecisionTaskResponse'
  { taskToken :: Core.Maybe Types.TaskToken
    -- ^ The opaque string used as a handle on the task. This token is used by workers to communicate progress and response information back to the system about the task.
  , startedEventId :: Core.Integer
    -- ^ The ID of the @DecisionTaskStarted@ event recorded in the history.
  , workflowExecution :: Core.Maybe Types.WorkflowExecution
    -- ^ The workflow execution for which this decision task was created.
  , workflowType :: Core.Maybe Types.WorkflowType
    -- ^ The type of the workflow execution for which this decision task was created.
  , events :: Core.Maybe [Types.HistoryEvent]
    -- ^ A paginated list of history events of the workflow execution. The decider uses this during the processing of the decision task.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
  , previousStartedEventId :: Core.Maybe Core.Integer
    -- ^ The ID of the DecisionTaskStarted event of the previous decision task of this workflow execution that was processed by the decider. This can be used to determine the events in the history new since the last decision task received by the decider.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PollForDecisionTaskResponse' value with any optional fields omitted.
mkPollForDecisionTaskResponse
    :: Core.Integer -- ^ 'startedEventId'
    -> Core.Int -- ^ 'responseStatus'
    -> PollForDecisionTaskResponse
mkPollForDecisionTaskResponse startedEventId responseStatus
  = PollForDecisionTaskResponse'{taskToken = Core.Nothing,
                                 startedEventId, workflowExecution = Core.Nothing,
                                 workflowType = Core.Nothing, events = Core.Nothing,
                                 nextPageToken = Core.Nothing,
                                 previousStartedEventId = Core.Nothing, responseStatus}

-- | The opaque string used as a handle on the task. This token is used by workers to communicate progress and response information back to the system about the task.
--
-- /Note:/ Consider using 'taskToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrrsTaskToken :: Lens.Lens' PollForDecisionTaskResponse (Core.Maybe Types.TaskToken)
pfdtrrsTaskToken = Lens.field @"taskToken"
{-# INLINEABLE pfdtrrsTaskToken #-}
{-# DEPRECATED taskToken "Use generic-lens or generic-optics with 'taskToken' instead"  #-}

-- | The ID of the @DecisionTaskStarted@ event recorded in the history.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrrsStartedEventId :: Lens.Lens' PollForDecisionTaskResponse Core.Integer
pfdtrrsStartedEventId = Lens.field @"startedEventId"
{-# INLINEABLE pfdtrrsStartedEventId #-}
{-# DEPRECATED startedEventId "Use generic-lens or generic-optics with 'startedEventId' instead"  #-}

-- | The workflow execution for which this decision task was created.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrrsWorkflowExecution :: Lens.Lens' PollForDecisionTaskResponse (Core.Maybe Types.WorkflowExecution)
pfdtrrsWorkflowExecution = Lens.field @"workflowExecution"
{-# INLINEABLE pfdtrrsWorkflowExecution #-}
{-# DEPRECATED workflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead"  #-}

-- | The type of the workflow execution for which this decision task was created.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrrsWorkflowType :: Lens.Lens' PollForDecisionTaskResponse (Core.Maybe Types.WorkflowType)
pfdtrrsWorkflowType = Lens.field @"workflowType"
{-# INLINEABLE pfdtrrsWorkflowType #-}
{-# DEPRECATED workflowType "Use generic-lens or generic-optics with 'workflowType' instead"  #-}

-- | A paginated list of history events of the workflow execution. The decider uses this during the processing of the decision task.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrrsEvents :: Lens.Lens' PollForDecisionTaskResponse (Core.Maybe [Types.HistoryEvent])
pfdtrrsEvents = Lens.field @"events"
{-# INLINEABLE pfdtrrsEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrrsNextPageToken :: Lens.Lens' PollForDecisionTaskResponse (Core.Maybe Types.NextPageToken)
pfdtrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE pfdtrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The ID of the DecisionTaskStarted event of the previous decision task of this workflow execution that was processed by the decider. This can be used to determine the events in the history new since the last decision task received by the decider.
--
-- /Note:/ Consider using 'previousStartedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrrsPreviousStartedEventId :: Lens.Lens' PollForDecisionTaskResponse (Core.Maybe Core.Integer)
pfdtrrsPreviousStartedEventId = Lens.field @"previousStartedEventId"
{-# INLINEABLE pfdtrrsPreviousStartedEventId #-}
{-# DEPRECATED previousStartedEventId "Use generic-lens or generic-optics with 'previousStartedEventId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrrsResponseStatus :: Lens.Lens' PollForDecisionTaskResponse Core.Int
pfdtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pfdtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
