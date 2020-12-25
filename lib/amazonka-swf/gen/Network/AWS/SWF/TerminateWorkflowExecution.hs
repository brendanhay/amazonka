{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.TerminateWorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionTerminated@ event and forces closure of the workflow execution identified by the given domain, runId, and workflowId. The child policy, registered with the workflow type or specified when starting this execution, is applied to any open child workflow executions of this workflow execution.
--
-- /Important:/ If the identified workflow execution was in progress, it is terminated immediately.
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
module Network.AWS.SWF.TerminateWorkflowExecution
  ( -- * Creating a request
    TerminateWorkflowExecution (..),
    mkTerminateWorkflowExecution,

    -- ** Request lenses
    tweDomain,
    tweWorkflowId,
    tweChildPolicy,
    tweDetails,
    tweReason,
    tweRunId,

    -- * Destructuring the response
    TerminateWorkflowExecutionResponse (..),
    mkTerminateWorkflowExecutionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkTerminateWorkflowExecution' smart constructor.
data TerminateWorkflowExecution = TerminateWorkflowExecution'
  { -- | The domain of the workflow execution to terminate.
    domain :: Types.DomainName,
    -- | The workflowId of the workflow execution to terminate.
    workflowId :: Types.WorkflowId,
    -- | If set, specifies the policy to use for the child workflow executions of the workflow execution being terminated. This policy overrides the child policy specified for the workflow execution at registration time or when starting the execution.
    --
    -- The supported child policies are:
    --
    --     * @TERMINATE@ – The child executions are terminated.
    --
    --
    --     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.
    --
    --
    --     * @ABANDON@ – No action is taken. The child executions continue to run.
    childPolicy :: Core.Maybe Types.ChildPolicy,
    -- | Details for terminating the workflow execution.
    details :: Core.Maybe Types.Data,
    -- | A descriptive reason for terminating the workflow execution.
    reason :: Core.Maybe Types.TerminateReason,
    -- | The runId of the workflow execution to terminate.
    runId :: Core.Maybe Types.RunId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateWorkflowExecution' value with any optional fields omitted.
mkTerminateWorkflowExecution ::
  -- | 'domain'
  Types.DomainName ->
  -- | 'workflowId'
  Types.WorkflowId ->
  TerminateWorkflowExecution
mkTerminateWorkflowExecution domain workflowId =
  TerminateWorkflowExecution'
    { domain,
      workflowId,
      childPolicy = Core.Nothing,
      details = Core.Nothing,
      reason = Core.Nothing,
      runId = Core.Nothing
    }

-- | The domain of the workflow execution to terminate.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweDomain :: Lens.Lens' TerminateWorkflowExecution Types.DomainName
tweDomain = Lens.field @"domain"
{-# DEPRECATED tweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The workflowId of the workflow execution to terminate.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweWorkflowId :: Lens.Lens' TerminateWorkflowExecution Types.WorkflowId
tweWorkflowId = Lens.field @"workflowId"
{-# DEPRECATED tweWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

-- | If set, specifies the policy to use for the child workflow executions of the workflow execution being terminated. This policy overrides the child policy specified for the workflow execution at registration time or when starting the execution.
--
-- The supported child policies are:
--
--     * @TERMINATE@ – The child executions are terminated.
--
--
--     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.
--
--
--     * @ABANDON@ – No action is taken. The child executions continue to run.
--
--
--
-- /Note:/ Consider using 'childPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweChildPolicy :: Lens.Lens' TerminateWorkflowExecution (Core.Maybe Types.ChildPolicy)
tweChildPolicy = Lens.field @"childPolicy"
{-# DEPRECATED tweChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

-- | Details for terminating the workflow execution.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweDetails :: Lens.Lens' TerminateWorkflowExecution (Core.Maybe Types.Data)
tweDetails = Lens.field @"details"
{-# DEPRECATED tweDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | A descriptive reason for terminating the workflow execution.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweReason :: Lens.Lens' TerminateWorkflowExecution (Core.Maybe Types.TerminateReason)
tweReason = Lens.field @"reason"
{-# DEPRECATED tweReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The runId of the workflow execution to terminate.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweRunId :: Lens.Lens' TerminateWorkflowExecution (Core.Maybe Types.RunId)
tweRunId = Lens.field @"runId"
{-# DEPRECATED tweRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Core.FromJSON TerminateWorkflowExecution where
  toJSON TerminateWorkflowExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("workflowId" Core..= workflowId),
            ("childPolicy" Core..=) Core.<$> childPolicy,
            ("details" Core..=) Core.<$> details,
            ("reason" Core..=) Core.<$> reason,
            ("runId" Core..=) Core.<$> runId
          ]
      )

instance Core.AWSRequest TerminateWorkflowExecution where
  type
    Rs TerminateWorkflowExecution =
      TerminateWorkflowExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "SimpleWorkflowService.TerminateWorkflowExecution"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull TerminateWorkflowExecutionResponse'

-- | /See:/ 'mkTerminateWorkflowExecutionResponse' smart constructor.
data TerminateWorkflowExecutionResponse = TerminateWorkflowExecutionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateWorkflowExecutionResponse' value with any optional fields omitted.
mkTerminateWorkflowExecutionResponse ::
  TerminateWorkflowExecutionResponse
mkTerminateWorkflowExecutionResponse =
  TerminateWorkflowExecutionResponse'
