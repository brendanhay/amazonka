{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.FailWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.FailWorkflowExecutionDecisionAttributes
  ( FailWorkflowExecutionDecisionAttributes (..),

    -- * Smart constructor
    mkFailWorkflowExecutionDecisionAttributes,

    -- * Lenses
    fwedaReason,
    fwedaDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @FailWorkflowExecution@ decision.
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
-- /See:/ 'mkFailWorkflowExecutionDecisionAttributes' smart constructor.
data FailWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes'
  { -- | A descriptive reason for the failure that may help in diagnostics.
    reason :: Lude.Maybe Lude.Text,
    -- | Details of the failure.
    details :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- * 'reason' - A descriptive reason for the failure that may help in diagnostics.
-- * 'details' - Details of the failure.
mkFailWorkflowExecutionDecisionAttributes ::
  FailWorkflowExecutionDecisionAttributes
mkFailWorkflowExecutionDecisionAttributes =
  FailWorkflowExecutionDecisionAttributes'
    { reason = Lude.Nothing,
      details = Lude.Nothing
    }

-- | A descriptive reason for the failure that may help in diagnostics.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fwedaReason :: Lens.Lens' FailWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
fwedaReason = Lens.lens (reason :: FailWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: FailWorkflowExecutionDecisionAttributes)
{-# DEPRECATED fwedaReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | Details of the failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fwedaDetails :: Lens.Lens' FailWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
fwedaDetails = Lens.lens (details :: FailWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: FailWorkflowExecutionDecisionAttributes)
{-# DEPRECATED fwedaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Lude.ToJSON FailWorkflowExecutionDecisionAttributes where
  toJSON FailWorkflowExecutionDecisionAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("reason" Lude..=) Lude.<$> reason,
            ("details" Lude..=) Lude.<$> details
          ]
      )
