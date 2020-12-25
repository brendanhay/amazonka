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
    fwedaDetails,
    fwedaReason,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.FailureReason as Types

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
  { -- | Details of the failure.
    details :: Core.Maybe Types.Data,
    -- | A descriptive reason for the failure that may help in diagnostics.
    reason :: Core.Maybe Types.FailureReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailWorkflowExecutionDecisionAttributes' value with any optional fields omitted.
mkFailWorkflowExecutionDecisionAttributes ::
  FailWorkflowExecutionDecisionAttributes
mkFailWorkflowExecutionDecisionAttributes =
  FailWorkflowExecutionDecisionAttributes'
    { details = Core.Nothing,
      reason = Core.Nothing
    }

-- | Details of the failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fwedaDetails :: Lens.Lens' FailWorkflowExecutionDecisionAttributes (Core.Maybe Types.Data)
fwedaDetails = Lens.field @"details"
{-# DEPRECATED fwedaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | A descriptive reason for the failure that may help in diagnostics.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fwedaReason :: Lens.Lens' FailWorkflowExecutionDecisionAttributes (Core.Maybe Types.FailureReason)
fwedaReason = Lens.field @"reason"
{-# DEPRECATED fwedaReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON FailWorkflowExecutionDecisionAttributes where
  toJSON FailWorkflowExecutionDecisionAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ ("details" Core..=) Core.<$> details,
            ("reason" Core..=) Core.<$> reason
          ]
      )
