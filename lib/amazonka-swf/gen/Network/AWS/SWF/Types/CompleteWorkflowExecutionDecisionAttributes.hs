{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CompleteWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CompleteWorkflowExecutionDecisionAttributes
  ( CompleteWorkflowExecutionDecisionAttributes (..),

    -- * Smart constructor
    mkCompleteWorkflowExecutionDecisionAttributes,

    -- * Lenses
    cwedaResult,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @CompleteWorkflowExecution@ decision.
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
-- /See:/ 'mkCompleteWorkflowExecutionDecisionAttributes' smart constructor.
newtype CompleteWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes'
  { -- | The result of the workflow execution. The form of the result is implementation defined.
    result :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompleteWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- * 'result' - The result of the workflow execution. The form of the result is implementation defined.
mkCompleteWorkflowExecutionDecisionAttributes ::
  CompleteWorkflowExecutionDecisionAttributes
mkCompleteWorkflowExecutionDecisionAttributes =
  CompleteWorkflowExecutionDecisionAttributes'
    { result =
        Lude.Nothing
    }

-- | The result of the workflow execution. The form of the result is implementation defined.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwedaResult :: Lens.Lens' CompleteWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
cwedaResult = Lens.lens (result :: CompleteWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {result = a} :: CompleteWorkflowExecutionDecisionAttributes)
{-# DEPRECATED cwedaResult "Use generic-lens or generic-optics with 'result' instead." #-}

instance Lude.ToJSON CompleteWorkflowExecutionDecisionAttributes where
  toJSON CompleteWorkflowExecutionDecisionAttributes' {..} =
    Lude.object (Lude.catMaybes [("result" Lude..=) Lude.<$> result])
