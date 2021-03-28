{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes
  ( SignalExternalWorkflowExecutionDecisionAttributes (..)
  -- * Smart constructor
  , mkSignalExternalWorkflowExecutionDecisionAttributes
  -- * Lenses
  , sewedaWorkflowId
  , sewedaSignalName
  , sewedaControl
  , sewedaInput
  , sewedaRunId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Control as Types
import qualified Network.AWS.SWF.Types.Input as Types
import qualified Network.AWS.SWF.Types.RunId as Types
import qualified Network.AWS.SWF.Types.SignalName as Types
import qualified Network.AWS.SWF.Types.WorkflowId as Types

-- | Provides the details of the @SignalExternalWorkflowExecution@ decision.
--
-- __Access Control__ 
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
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
-- /See:/ 'mkSignalExternalWorkflowExecutionDecisionAttributes' smart constructor.
data SignalExternalWorkflowExecutionDecisionAttributes = SignalExternalWorkflowExecutionDecisionAttributes'
  { workflowId :: Types.WorkflowId
    -- ^ The @workflowId@ of the workflow execution to be signaled.
  , signalName :: Types.SignalName
    -- ^ The name of the signal.The target workflow execution uses the signal name and input to process the signal.
  , control :: Core.Maybe Types.Control
    -- ^ The data attached to the event that can be used by the decider in subsequent decision tasks.
  , input :: Core.Maybe Types.Input
    -- ^ The input data to be provided with the signal. The target workflow execution uses the signal name and input data to process the signal.
  , runId :: Core.Maybe Types.RunId
    -- ^ The @runId@ of the workflow execution to be signaled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SignalExternalWorkflowExecutionDecisionAttributes' value with any optional fields omitted.
mkSignalExternalWorkflowExecutionDecisionAttributes
    :: Types.WorkflowId -- ^ 'workflowId'
    -> Types.SignalName -- ^ 'signalName'
    -> SignalExternalWorkflowExecutionDecisionAttributes
mkSignalExternalWorkflowExecutionDecisionAttributes workflowId
  signalName
  = SignalExternalWorkflowExecutionDecisionAttributes'{workflowId,
                                                       signalName, control = Core.Nothing,
                                                       input = Core.Nothing, runId = Core.Nothing}

-- | The @workflowId@ of the workflow execution to be signaled.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sewedaWorkflowId :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes Types.WorkflowId
sewedaWorkflowId = Lens.field @"workflowId"
{-# INLINEABLE sewedaWorkflowId #-}
{-# DEPRECATED workflowId "Use generic-lens or generic-optics with 'workflowId' instead"  #-}

-- | The name of the signal.The target workflow execution uses the signal name and input to process the signal.
--
-- /Note:/ Consider using 'signalName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sewedaSignalName :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes Types.SignalName
sewedaSignalName = Lens.field @"signalName"
{-# INLINEABLE sewedaSignalName #-}
{-# DEPRECATED signalName "Use generic-lens or generic-optics with 'signalName' instead"  #-}

-- | The data attached to the event that can be used by the decider in subsequent decision tasks.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sewedaControl :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Core.Maybe Types.Control)
sewedaControl = Lens.field @"control"
{-# INLINEABLE sewedaControl #-}
{-# DEPRECATED control "Use generic-lens or generic-optics with 'control' instead"  #-}

-- | The input data to be provided with the signal. The target workflow execution uses the signal name and input data to process the signal.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sewedaInput :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Core.Maybe Types.Input)
sewedaInput = Lens.field @"input"
{-# INLINEABLE sewedaInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | The @runId@ of the workflow execution to be signaled.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sewedaRunId :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Core.Maybe Types.RunId)
sewedaRunId = Lens.field @"runId"
{-# INLINEABLE sewedaRunId #-}
{-# DEPRECATED runId "Use generic-lens or generic-optics with 'runId' instead"  #-}

instance Core.FromJSON
           SignalExternalWorkflowExecutionDecisionAttributes
         where
        toJSON SignalExternalWorkflowExecutionDecisionAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("workflowId" Core..= workflowId),
                  Core.Just ("signalName" Core..= signalName),
                  ("control" Core..=) Core.<$> control,
                  ("input" Core..=) Core.<$> input,
                  ("runId" Core..=) Core.<$> runId])
