{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes
  ( RequestCancelExternalWorkflowExecutionDecisionAttributes (..)
  -- * Smart constructor
  , mkRequestCancelExternalWorkflowExecutionDecisionAttributes
  -- * Lenses
  , rcewedaWorkflowId
  , rcewedaControl
  , rcewedaRunId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Control as Types
import qualified Network.AWS.SWF.Types.RunId as Types
import qualified Network.AWS.SWF.Types.WorkflowId as Types

-- | Provides the details of the @RequestCancelExternalWorkflowExecution@ decision.
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
-- /See:/ 'mkRequestCancelExternalWorkflowExecutionDecisionAttributes' smart constructor.
data RequestCancelExternalWorkflowExecutionDecisionAttributes = RequestCancelExternalWorkflowExecutionDecisionAttributes'
  { workflowId :: Types.WorkflowId
    -- ^ The @workflowId@ of the external workflow execution to cancel.
  , control :: Core.Maybe Types.Control
    -- ^ The data attached to the event that can be used by the decider in subsequent workflow tasks.
  , runId :: Core.Maybe Types.RunId
    -- ^ The @runId@ of the external workflow execution to cancel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestCancelExternalWorkflowExecutionDecisionAttributes' value with any optional fields omitted.
mkRequestCancelExternalWorkflowExecutionDecisionAttributes
    :: Types.WorkflowId -- ^ 'workflowId'
    -> RequestCancelExternalWorkflowExecutionDecisionAttributes
mkRequestCancelExternalWorkflowExecutionDecisionAttributes
  workflowId
  = RequestCancelExternalWorkflowExecutionDecisionAttributes'{workflowId,
                                                              control = Core.Nothing,
                                                              runId = Core.Nothing}

-- | The @workflowId@ of the external workflow execution to cancel.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcewedaWorkflowId :: Lens.Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes Types.WorkflowId
rcewedaWorkflowId = Lens.field @"workflowId"
{-# INLINEABLE rcewedaWorkflowId #-}
{-# DEPRECATED workflowId "Use generic-lens or generic-optics with 'workflowId' instead"  #-}

-- | The data attached to the event that can be used by the decider in subsequent workflow tasks.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcewedaControl :: Lens.Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Core.Maybe Types.Control)
rcewedaControl = Lens.field @"control"
{-# INLINEABLE rcewedaControl #-}
{-# DEPRECATED control "Use generic-lens or generic-optics with 'control' instead"  #-}

-- | The @runId@ of the external workflow execution to cancel.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcewedaRunId :: Lens.Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Core.Maybe Types.RunId)
rcewedaRunId = Lens.field @"runId"
{-# INLINEABLE rcewedaRunId #-}
{-# DEPRECATED runId "Use generic-lens or generic-optics with 'runId' instead"  #-}

instance Core.FromJSON
           RequestCancelExternalWorkflowExecutionDecisionAttributes
         where
        toJSON RequestCancelExternalWorkflowExecutionDecisionAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("workflowId" Core..= workflowId),
                  ("control" Core..=) Core.<$> control,
                  ("runId" Core..=) Core.<$> runId])
