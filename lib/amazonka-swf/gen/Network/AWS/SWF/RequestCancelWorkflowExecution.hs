{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RequestCancelWorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionCancelRequested@ event in the currently running workflow execution identified by the given domain, workflowId, and runId. This logically requests the cancellation of the workflow execution as a whole. It is up to the decider to take appropriate actions when it receives an execution history with this event.
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
module Network.AWS.SWF.RequestCancelWorkflowExecution
    (
    -- * Creating a request
      RequestCancelWorkflowExecution (..)
    , mkRequestCancelWorkflowExecution
    -- ** Request lenses
    , rcweDomain
    , rcweWorkflowId
    , rcweRunId

    -- * Destructuring the response
    , RequestCancelWorkflowExecutionResponse (..)
    , mkRequestCancelWorkflowExecutionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkRequestCancelWorkflowExecution' smart constructor.
data RequestCancelWorkflowExecution = RequestCancelWorkflowExecution'
  { domain :: Types.Domain
    -- ^ The name of the domain containing the workflow execution to cancel.
  , workflowId :: Types.WorkflowId
    -- ^ The workflowId of the workflow execution to cancel.
  , runId :: Core.Maybe Types.RunId
    -- ^ The runId of the workflow execution to cancel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestCancelWorkflowExecution' value with any optional fields omitted.
mkRequestCancelWorkflowExecution
    :: Types.Domain -- ^ 'domain'
    -> Types.WorkflowId -- ^ 'workflowId'
    -> RequestCancelWorkflowExecution
mkRequestCancelWorkflowExecution domain workflowId
  = RequestCancelWorkflowExecution'{domain, workflowId,
                                    runId = Core.Nothing}

-- | The name of the domain containing the workflow execution to cancel.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcweDomain :: Lens.Lens' RequestCancelWorkflowExecution Types.Domain
rcweDomain = Lens.field @"domain"
{-# INLINEABLE rcweDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The workflowId of the workflow execution to cancel.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcweWorkflowId :: Lens.Lens' RequestCancelWorkflowExecution Types.WorkflowId
rcweWorkflowId = Lens.field @"workflowId"
{-# INLINEABLE rcweWorkflowId #-}
{-# DEPRECATED workflowId "Use generic-lens or generic-optics with 'workflowId' instead"  #-}

-- | The runId of the workflow execution to cancel.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcweRunId :: Lens.Lens' RequestCancelWorkflowExecution (Core.Maybe Types.RunId)
rcweRunId = Lens.field @"runId"
{-# INLINEABLE rcweRunId #-}
{-# DEPRECATED runId "Use generic-lens or generic-optics with 'runId' instead"  #-}

instance Core.ToQuery RequestCancelWorkflowExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RequestCancelWorkflowExecution where
        toHeaders RequestCancelWorkflowExecution{..}
          = Core.pure
              ("X-Amz-Target",
               "SimpleWorkflowService.RequestCancelWorkflowExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON RequestCancelWorkflowExecution where
        toJSON RequestCancelWorkflowExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domain" Core..= domain),
                  Core.Just ("workflowId" Core..= workflowId),
                  ("runId" Core..=) Core.<$> runId])

instance Core.AWSRequest RequestCancelWorkflowExecution where
        type Rs RequestCancelWorkflowExecution =
             RequestCancelWorkflowExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull RequestCancelWorkflowExecutionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRequestCancelWorkflowExecutionResponse' smart constructor.
data RequestCancelWorkflowExecutionResponse = RequestCancelWorkflowExecutionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestCancelWorkflowExecutionResponse' value with any optional fields omitted.
mkRequestCancelWorkflowExecutionResponse
    :: RequestCancelWorkflowExecutionResponse
mkRequestCancelWorkflowExecutionResponse
  = RequestCancelWorkflowExecutionResponse'
