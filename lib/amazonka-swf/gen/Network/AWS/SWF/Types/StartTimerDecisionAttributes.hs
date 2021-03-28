{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartTimerDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.StartTimerDecisionAttributes
  ( StartTimerDecisionAttributes (..)
  -- * Smart constructor
  , mkStartTimerDecisionAttributes
  -- * Lenses
  , stdaTimerId
  , stdaStartToFireTimeout
  , stdaControl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.StartToFireTimeout as Types
import qualified Network.AWS.SWF.Types.TimerId as Types

-- | Provides the details of the @StartTimer@ decision.
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
-- /See:/ 'mkStartTimerDecisionAttributes' smart constructor.
data StartTimerDecisionAttributes = StartTimerDecisionAttributes'
  { timerId :: Types.TimerId
    -- ^ The unique ID of the timer.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
  , startToFireTimeout :: Types.StartToFireTimeout
    -- ^ The duration to wait before firing the timer.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ .
  , control :: Core.Maybe Types.Data
    -- ^ The data attached to the event that can be used by the decider in subsequent workflow tasks.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartTimerDecisionAttributes' value with any optional fields omitted.
mkStartTimerDecisionAttributes
    :: Types.TimerId -- ^ 'timerId'
    -> Types.StartToFireTimeout -- ^ 'startToFireTimeout'
    -> StartTimerDecisionAttributes
mkStartTimerDecisionAttributes timerId startToFireTimeout
  = StartTimerDecisionAttributes'{timerId, startToFireTimeout,
                                  control = Core.Nothing}

-- | The unique ID of the timer.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- /Note:/ Consider using 'timerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdaTimerId :: Lens.Lens' StartTimerDecisionAttributes Types.TimerId
stdaTimerId = Lens.field @"timerId"
{-# INLINEABLE stdaTimerId #-}
{-# DEPRECATED timerId "Use generic-lens or generic-optics with 'timerId' instead"  #-}

-- | The duration to wait before firing the timer.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ .
--
-- /Note:/ Consider using 'startToFireTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdaStartToFireTimeout :: Lens.Lens' StartTimerDecisionAttributes Types.StartToFireTimeout
stdaStartToFireTimeout = Lens.field @"startToFireTimeout"
{-# INLINEABLE stdaStartToFireTimeout #-}
{-# DEPRECATED startToFireTimeout "Use generic-lens or generic-optics with 'startToFireTimeout' instead"  #-}

-- | The data attached to the event that can be used by the decider in subsequent workflow tasks.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdaControl :: Lens.Lens' StartTimerDecisionAttributes (Core.Maybe Types.Data)
stdaControl = Lens.field @"control"
{-# INLINEABLE stdaControl #-}
{-# DEPRECATED control "Use generic-lens or generic-optics with 'control' instead"  #-}

instance Core.FromJSON StartTimerDecisionAttributes where
        toJSON StartTimerDecisionAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("timerId" Core..= timerId),
                  Core.Just ("startToFireTimeout" Core..= startToFireTimeout),
                  ("control" Core..=) Core.<$> control])
