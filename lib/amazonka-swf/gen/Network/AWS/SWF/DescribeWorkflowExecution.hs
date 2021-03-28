{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DescribeWorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified workflow execution including its type and some statistics.
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
module Network.AWS.SWF.DescribeWorkflowExecution
    (
    -- * Creating a request
      DescribeWorkflowExecution (..)
    , mkDescribeWorkflowExecution
    -- ** Request lenses
    , dweDomain
    , dweExecution

    -- * Destructuring the response
    , DescribeWorkflowExecutionResponse (..)
    , mkDescribeWorkflowExecutionResponse
    -- ** Response lenses
    , dwerrsExecutionInfo
    , dwerrsExecutionConfiguration
    , dwerrsOpenCounts
    , dwerrsLatestActivityTaskTimestamp
    , dwerrsLatestExecutionContext
    , dwerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkDescribeWorkflowExecution' smart constructor.
data DescribeWorkflowExecution = DescribeWorkflowExecution'
  { domain :: Types.Domain
    -- ^ The name of the domain containing the workflow execution.
  , execution :: Types.WorkflowExecution
    -- ^ The workflow execution to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkflowExecution' value with any optional fields omitted.
mkDescribeWorkflowExecution
    :: Types.Domain -- ^ 'domain'
    -> Types.WorkflowExecution -- ^ 'execution'
    -> DescribeWorkflowExecution
mkDescribeWorkflowExecution domain execution
  = DescribeWorkflowExecution'{domain, execution}

-- | The name of the domain containing the workflow execution.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dweDomain :: Lens.Lens' DescribeWorkflowExecution Types.Domain
dweDomain = Lens.field @"domain"
{-# INLINEABLE dweDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The workflow execution to describe.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dweExecution :: Lens.Lens' DescribeWorkflowExecution Types.WorkflowExecution
dweExecution = Lens.field @"execution"
{-# INLINEABLE dweExecution #-}
{-# DEPRECATED execution "Use generic-lens or generic-optics with 'execution' instead"  #-}

instance Core.ToQuery DescribeWorkflowExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeWorkflowExecution where
        toHeaders DescribeWorkflowExecution{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.DescribeWorkflowExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DescribeWorkflowExecution where
        toJSON DescribeWorkflowExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domain" Core..= domain),
                  Core.Just ("execution" Core..= execution)])

instance Core.AWSRequest DescribeWorkflowExecution where
        type Rs DescribeWorkflowExecution =
             DescribeWorkflowExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeWorkflowExecutionResponse' Core.<$>
                   (x Core..: "executionInfo") Core.<*>
                     x Core..: "executionConfiguration"
                     Core.<*> x Core..: "openCounts"
                     Core.<*> x Core..:? "latestActivityTaskTimestamp"
                     Core.<*> x Core..:? "latestExecutionContext"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains details about a workflow execution.
--
-- /See:/ 'mkDescribeWorkflowExecutionResponse' smart constructor.
data DescribeWorkflowExecutionResponse = DescribeWorkflowExecutionResponse'
  { executionInfo :: Types.WorkflowExecutionInfo
    -- ^ Information about the workflow execution.
  , executionConfiguration :: Types.WorkflowExecutionConfiguration
    -- ^ The configuration settings for this workflow execution including timeout values, tasklist etc.
  , openCounts :: Types.WorkflowExecutionOpenCounts
    -- ^ The number of tasks for this workflow execution. This includes open and closed tasks of all types.
  , latestActivityTaskTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the last activity task was scheduled for this workflow execution. You can use this information to determine if the workflow has not made progress for an unusually long period of time and might require a corrective action.
  , latestExecutionContext :: Core.Maybe Types.Data
    -- ^ The latest executionContext provided by the decider for this workflow execution. A decider can provide an executionContext (a free-form string) when closing a decision task using 'RespondDecisionTaskCompleted' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeWorkflowExecutionResponse' value with any optional fields omitted.
mkDescribeWorkflowExecutionResponse
    :: Types.WorkflowExecutionInfo -- ^ 'executionInfo'
    -> Types.WorkflowExecutionConfiguration -- ^ 'executionConfiguration'
    -> Types.WorkflowExecutionOpenCounts -- ^ 'openCounts'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeWorkflowExecutionResponse
mkDescribeWorkflowExecutionResponse executionInfo
  executionConfiguration openCounts responseStatus
  = DescribeWorkflowExecutionResponse'{executionInfo,
                                       executionConfiguration, openCounts,
                                       latestActivityTaskTimestamp = Core.Nothing,
                                       latestExecutionContext = Core.Nothing, responseStatus}

-- | Information about the workflow execution.
--
-- /Note:/ Consider using 'executionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwerrsExecutionInfo :: Lens.Lens' DescribeWorkflowExecutionResponse Types.WorkflowExecutionInfo
dwerrsExecutionInfo = Lens.field @"executionInfo"
{-# INLINEABLE dwerrsExecutionInfo #-}
{-# DEPRECATED executionInfo "Use generic-lens or generic-optics with 'executionInfo' instead"  #-}

-- | The configuration settings for this workflow execution including timeout values, tasklist etc.
--
-- /Note:/ Consider using 'executionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwerrsExecutionConfiguration :: Lens.Lens' DescribeWorkflowExecutionResponse Types.WorkflowExecutionConfiguration
dwerrsExecutionConfiguration = Lens.field @"executionConfiguration"
{-# INLINEABLE dwerrsExecutionConfiguration #-}
{-# DEPRECATED executionConfiguration "Use generic-lens or generic-optics with 'executionConfiguration' instead"  #-}

-- | The number of tasks for this workflow execution. This includes open and closed tasks of all types.
--
-- /Note:/ Consider using 'openCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwerrsOpenCounts :: Lens.Lens' DescribeWorkflowExecutionResponse Types.WorkflowExecutionOpenCounts
dwerrsOpenCounts = Lens.field @"openCounts"
{-# INLINEABLE dwerrsOpenCounts #-}
{-# DEPRECATED openCounts "Use generic-lens or generic-optics with 'openCounts' instead"  #-}

-- | The time when the last activity task was scheduled for this workflow execution. You can use this information to determine if the workflow has not made progress for an unusually long period of time and might require a corrective action.
--
-- /Note:/ Consider using 'latestActivityTaskTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwerrsLatestActivityTaskTimestamp :: Lens.Lens' DescribeWorkflowExecutionResponse (Core.Maybe Core.NominalDiffTime)
dwerrsLatestActivityTaskTimestamp = Lens.field @"latestActivityTaskTimestamp"
{-# INLINEABLE dwerrsLatestActivityTaskTimestamp #-}
{-# DEPRECATED latestActivityTaskTimestamp "Use generic-lens or generic-optics with 'latestActivityTaskTimestamp' instead"  #-}

-- | The latest executionContext provided by the decider for this workflow execution. A decider can provide an executionContext (a free-form string) when closing a decision task using 'RespondDecisionTaskCompleted' .
--
-- /Note:/ Consider using 'latestExecutionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwerrsLatestExecutionContext :: Lens.Lens' DescribeWorkflowExecutionResponse (Core.Maybe Types.Data)
dwerrsLatestExecutionContext = Lens.field @"latestExecutionContext"
{-# INLINEABLE dwerrsLatestExecutionContext #-}
{-# DEPRECATED latestExecutionContext "Use generic-lens or generic-optics with 'latestExecutionContext' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwerrsResponseStatus :: Lens.Lens' DescribeWorkflowExecutionResponse Core.Int
dwerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
