-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes
  ( RequestCancelExternalWorkflowExecutionDecisionAttributes (..),

    -- * Smart constructor
    mkRequestCancelExternalWorkflowExecutionDecisionAttributes,

    -- * Lenses
    rcewedaControl,
    rcewedaRunId,
    rcewedaWorkflowId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { control ::
      Lude.Maybe
        Lude.Text,
    runId ::
      Lude.Maybe
        Lude.Text,
    workflowId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'RequestCancelExternalWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- * 'control' - The data attached to the event that can be used by the decider in subsequent workflow tasks.
-- * 'runId' - The @runId@ of the external workflow execution to cancel.
-- * 'workflowId' - The @workflowId@ of the external workflow execution to cancel.
mkRequestCancelExternalWorkflowExecutionDecisionAttributes ::
  -- | 'workflowId'
  Lude.Text ->
  RequestCancelExternalWorkflowExecutionDecisionAttributes
mkRequestCancelExternalWorkflowExecutionDecisionAttributes
  pWorkflowId_ =
    RequestCancelExternalWorkflowExecutionDecisionAttributes'
      { control =
          Lude.Nothing,
        runId = Lude.Nothing,
        workflowId = pWorkflowId_
      }

-- | The data attached to the event that can be used by the decider in subsequent workflow tasks.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcewedaControl :: Lens.Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
rcewedaControl = Lens.lens (control :: RequestCancelExternalWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: RequestCancelExternalWorkflowExecutionDecisionAttributes)
{-# DEPRECATED rcewedaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The @runId@ of the external workflow execution to cancel.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcewedaRunId :: Lens.Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
rcewedaRunId = Lens.lens (runId :: RequestCancelExternalWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: RequestCancelExternalWorkflowExecutionDecisionAttributes)
{-# DEPRECATED rcewedaRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The @workflowId@ of the external workflow execution to cancel.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcewedaWorkflowId :: Lens.Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes Lude.Text
rcewedaWorkflowId = Lens.lens (workflowId :: RequestCancelExternalWorkflowExecutionDecisionAttributes -> Lude.Text) (\s a -> s {workflowId = a} :: RequestCancelExternalWorkflowExecutionDecisionAttributes)
{-# DEPRECATED rcewedaWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

instance
  Lude.ToJSON
    RequestCancelExternalWorkflowExecutionDecisionAttributes
  where
  toJSON
    RequestCancelExternalWorkflowExecutionDecisionAttributes' {..} =
      Lude.object
        ( Lude.catMaybes
            [ ("control" Lude..=) Lude.<$> control,
              ("runId" Lude..=) Lude.<$> runId,
              Lude.Just ("workflowId" Lude..= workflowId)
            ]
        )
