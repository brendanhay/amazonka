{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartTimerDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartTimerDecisionAttributes
  ( StartTimerDecisionAttributes (..),

    -- * Smart constructor
    mkStartTimerDecisionAttributes,

    -- * Lenses
    stdaControl,
    stdaTimerId,
    stdaStartToFireTimeout,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { -- | The data attached to the event that can be used by the decider in subsequent workflow tasks.
    control :: Lude.Maybe Lude.Text,
    -- | The unique ID of the timer.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
    timerId :: Lude.Text,
    -- | The duration to wait before firing the timer.
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ .
    startToFireTimeout :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTimerDecisionAttributes' with the minimum fields required to make a request.
--
-- * 'control' - The data attached to the event that can be used by the decider in subsequent workflow tasks.
-- * 'timerId' - The unique ID of the timer.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
-- * 'startToFireTimeout' - The duration to wait before firing the timer.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ .
mkStartTimerDecisionAttributes ::
  -- | 'timerId'
  Lude.Text ->
  -- | 'startToFireTimeout'
  Lude.Text ->
  StartTimerDecisionAttributes
mkStartTimerDecisionAttributes pTimerId_ pStartToFireTimeout_ =
  StartTimerDecisionAttributes'
    { control = Lude.Nothing,
      timerId = pTimerId_,
      startToFireTimeout = pStartToFireTimeout_
    }

-- | The data attached to the event that can be used by the decider in subsequent workflow tasks.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdaControl :: Lens.Lens' StartTimerDecisionAttributes (Lude.Maybe Lude.Text)
stdaControl = Lens.lens (control :: StartTimerDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: StartTimerDecisionAttributes)
{-# DEPRECATED stdaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The unique ID of the timer.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- /Note:/ Consider using 'timerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdaTimerId :: Lens.Lens' StartTimerDecisionAttributes Lude.Text
stdaTimerId = Lens.lens (timerId :: StartTimerDecisionAttributes -> Lude.Text) (\s a -> s {timerId = a} :: StartTimerDecisionAttributes)
{-# DEPRECATED stdaTimerId "Use generic-lens or generic-optics with 'timerId' instead." #-}

-- | The duration to wait before firing the timer.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ .
--
-- /Note:/ Consider using 'startToFireTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdaStartToFireTimeout :: Lens.Lens' StartTimerDecisionAttributes Lude.Text
stdaStartToFireTimeout = Lens.lens (startToFireTimeout :: StartTimerDecisionAttributes -> Lude.Text) (\s a -> s {startToFireTimeout = a} :: StartTimerDecisionAttributes)
{-# DEPRECATED stdaStartToFireTimeout "Use generic-lens or generic-optics with 'startToFireTimeout' instead." #-}

instance Lude.ToJSON StartTimerDecisionAttributes where
  toJSON StartTimerDecisionAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("control" Lude..=) Lude.<$> control,
            Lude.Just ("timerId" Lude..= timerId),
            Lude.Just ("startToFireTimeout" Lude..= startToFireTimeout)
          ]
      )
