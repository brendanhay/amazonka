{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.SignalWorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionSignaled@ event in the workflow execution history and creates a decision task for the workflow execution identified by the given domain, workflowId and runId. The event is recorded with the specified user defined signalName and input (if provided).
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
module Network.AWS.SWF.SignalWorkflowExecution
    (
    -- * Creating a request
      SignalWorkflowExecution (..)
    , mkSignalWorkflowExecution
    -- ** Request lenses
    , sweDomain
    , sweWorkflowId
    , sweSignalName
    , sweInput
    , sweRunId

    -- * Destructuring the response
    , SignalWorkflowExecutionResponse (..)
    , mkSignalWorkflowExecutionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkSignalWorkflowExecution' smart constructor.
data SignalWorkflowExecution = SignalWorkflowExecution'
  { domain :: Types.DomainName
    -- ^ The name of the domain containing the workflow execution to signal.
  , workflowId :: Types.WorkflowId
    -- ^ The workflowId of the workflow execution to signal.
  , signalName :: Types.SignalName
    -- ^ The name of the signal. This name must be meaningful to the target workflow.
  , input :: Core.Maybe Types.Data
    -- ^ Data to attach to the @WorkflowExecutionSignaled@ event in the target workflow execution's history.
  , runId :: Core.Maybe Types.RunId
    -- ^ The runId of the workflow execution to signal.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SignalWorkflowExecution' value with any optional fields omitted.
mkSignalWorkflowExecution
    :: Types.DomainName -- ^ 'domain'
    -> Types.WorkflowId -- ^ 'workflowId'
    -> Types.SignalName -- ^ 'signalName'
    -> SignalWorkflowExecution
mkSignalWorkflowExecution domain workflowId signalName
  = SignalWorkflowExecution'{domain, workflowId, signalName,
                             input = Core.Nothing, runId = Core.Nothing}

-- | The name of the domain containing the workflow execution to signal.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweDomain :: Lens.Lens' SignalWorkflowExecution Types.DomainName
sweDomain = Lens.field @"domain"
{-# INLINEABLE sweDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The workflowId of the workflow execution to signal.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweWorkflowId :: Lens.Lens' SignalWorkflowExecution Types.WorkflowId
sweWorkflowId = Lens.field @"workflowId"
{-# INLINEABLE sweWorkflowId #-}
{-# DEPRECATED workflowId "Use generic-lens or generic-optics with 'workflowId' instead"  #-}

-- | The name of the signal. This name must be meaningful to the target workflow.
--
-- /Note:/ Consider using 'signalName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweSignalName :: Lens.Lens' SignalWorkflowExecution Types.SignalName
sweSignalName = Lens.field @"signalName"
{-# INLINEABLE sweSignalName #-}
{-# DEPRECATED signalName "Use generic-lens or generic-optics with 'signalName' instead"  #-}

-- | Data to attach to the @WorkflowExecutionSignaled@ event in the target workflow execution's history.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweInput :: Lens.Lens' SignalWorkflowExecution (Core.Maybe Types.Data)
sweInput = Lens.field @"input"
{-# INLINEABLE sweInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | The runId of the workflow execution to signal.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweRunId :: Lens.Lens' SignalWorkflowExecution (Core.Maybe Types.RunId)
sweRunId = Lens.field @"runId"
{-# INLINEABLE sweRunId #-}
{-# DEPRECATED runId "Use generic-lens or generic-optics with 'runId' instead"  #-}

instance Core.ToQuery SignalWorkflowExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SignalWorkflowExecution where
        toHeaders SignalWorkflowExecution{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.SignalWorkflowExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON SignalWorkflowExecution where
        toJSON SignalWorkflowExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domain" Core..= domain),
                  Core.Just ("workflowId" Core..= workflowId),
                  Core.Just ("signalName" Core..= signalName),
                  ("input" Core..=) Core.<$> input,
                  ("runId" Core..=) Core.<$> runId])

instance Core.AWSRequest SignalWorkflowExecution where
        type Rs SignalWorkflowExecution = SignalWorkflowExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull SignalWorkflowExecutionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSignalWorkflowExecutionResponse' smart constructor.
data SignalWorkflowExecutionResponse = SignalWorkflowExecutionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SignalWorkflowExecutionResponse' value with any optional fields omitted.
mkSignalWorkflowExecutionResponse
    :: SignalWorkflowExecutionResponse
mkSignalWorkflowExecutionResponse
  = SignalWorkflowExecutionResponse'
