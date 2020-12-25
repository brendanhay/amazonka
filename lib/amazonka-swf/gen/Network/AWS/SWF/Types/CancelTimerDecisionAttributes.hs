{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CancelTimerDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CancelTimerDecisionAttributes
  ( CancelTimerDecisionAttributes (..),

    -- * Smart constructor
    mkCancelTimerDecisionAttributes,

    -- * Lenses
    ctdaTimerId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.TimerId as Types

-- | Provides the details of the @CancelTimer@ decision.
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
-- /See:/ 'mkCancelTimerDecisionAttributes' smart constructor.
newtype CancelTimerDecisionAttributes = CancelTimerDecisionAttributes'
  { -- | The unique ID of the timer to cancel.
    timerId :: Types.TimerId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelTimerDecisionAttributes' value with any optional fields omitted.
mkCancelTimerDecisionAttributes ::
  -- | 'timerId'
  Types.TimerId ->
  CancelTimerDecisionAttributes
mkCancelTimerDecisionAttributes timerId =
  CancelTimerDecisionAttributes' {timerId}

-- | The unique ID of the timer to cancel.
--
-- /Note:/ Consider using 'timerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctdaTimerId :: Lens.Lens' CancelTimerDecisionAttributes Types.TimerId
ctdaTimerId = Lens.field @"timerId"
{-# DEPRECATED ctdaTimerId "Use generic-lens or generic-optics with 'timerId' instead." #-}

instance Core.FromJSON CancelTimerDecisionAttributes where
  toJSON CancelTimerDecisionAttributes {..} =
    Core.object
      (Core.catMaybes [Core.Just ("timerId" Core..= timerId)])
