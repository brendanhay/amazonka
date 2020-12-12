{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes
  ( LambdaFunctionCompletedEventAttributes (..),

    -- * Smart constructor
    mkLambdaFunctionCompletedEventAttributes,

    -- * Lenses
    lfceaResult,
    lfceaScheduledEventId,
    lfceaStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @LambdaFunctionCompleted@ event. It isn't set for other event types.
--
-- /See:/ 'mkLambdaFunctionCompletedEventAttributes' smart constructor.
data LambdaFunctionCompletedEventAttributes = LambdaFunctionCompletedEventAttributes'
  { result ::
      Lude.Maybe
        Lude.Text,
    scheduledEventId ::
      Lude.Integer,
    startedEventId ::
      Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaFunctionCompletedEventAttributes' with the minimum fields required to make a request.
--
-- * 'result' - The results of the Lambda task.
-- * 'scheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when this Lambda task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
-- * 'startedEventId' - The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
mkLambdaFunctionCompletedEventAttributes ::
  -- | 'scheduledEventId'
  Lude.Integer ->
  -- | 'startedEventId'
  Lude.Integer ->
  LambdaFunctionCompletedEventAttributes
mkLambdaFunctionCompletedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    LambdaFunctionCompletedEventAttributes'
      { result = Lude.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The results of the Lambda task.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfceaResult :: Lens.Lens' LambdaFunctionCompletedEventAttributes (Lude.Maybe Lude.Text)
lfceaResult = Lens.lens (result :: LambdaFunctionCompletedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {result = a} :: LambdaFunctionCompletedEventAttributes)
{-# DEPRECATED lfceaResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this Lambda task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfceaScheduledEventId :: Lens.Lens' LambdaFunctionCompletedEventAttributes Lude.Integer
lfceaScheduledEventId = Lens.lens (scheduledEventId :: LambdaFunctionCompletedEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: LambdaFunctionCompletedEventAttributes)
{-# DEPRECATED lfceaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfceaStartedEventId :: Lens.Lens' LambdaFunctionCompletedEventAttributes Lude.Integer
lfceaStartedEventId = Lens.lens (startedEventId :: LambdaFunctionCompletedEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: LambdaFunctionCompletedEventAttributes)
{-# DEPRECATED lfceaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance Lude.FromJSON LambdaFunctionCompletedEventAttributes where
  parseJSON =
    Lude.withObject
      "LambdaFunctionCompletedEventAttributes"
      ( \x ->
          LambdaFunctionCompletedEventAttributes'
            Lude.<$> (x Lude..:? "result")
            Lude.<*> (x Lude..: "scheduledEventId")
            Lude.<*> (x Lude..: "startedEventId")
      )
