-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CancelWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CancelWorkflowExecutionDecisionAttributes
  ( CancelWorkflowExecutionDecisionAttributes (..),

    -- * Smart constructor
    mkCancelWorkflowExecutionDecisionAttributes,

    -- * Lenses
    cwedaDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @CancelWorkflowExecution@ decision.
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
-- /See:/ 'mkCancelWorkflowExecutionDecisionAttributes' smart constructor.
newtype CancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes'
  { details ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- * 'details' - Details of the cancellation.
mkCancelWorkflowExecutionDecisionAttributes ::
  CancelWorkflowExecutionDecisionAttributes
mkCancelWorkflowExecutionDecisionAttributes =
  CancelWorkflowExecutionDecisionAttributes'
    { details =
        Lude.Nothing
    }

-- | Details of the cancellation.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwedaDetails :: Lens.Lens' CancelWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
cwedaDetails = Lens.lens (details :: CancelWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: CancelWorkflowExecutionDecisionAttributes)
{-# DEPRECATED cwedaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Lude.ToJSON CancelWorkflowExecutionDecisionAttributes where
  toJSON CancelWorkflowExecutionDecisionAttributes' {..} =
    Lude.object
      (Lude.catMaybes [("details" Lude..=) Lude.<$> details])
