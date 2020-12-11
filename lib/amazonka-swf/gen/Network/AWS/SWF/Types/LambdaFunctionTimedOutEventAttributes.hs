-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionTimedOutEventAttributes
  ( LambdaFunctionTimedOutEventAttributes (..),

    -- * Smart constructor
    mkLambdaFunctionTimedOutEventAttributes,

    -- * Lenses
    lftoeaTimeoutType,
    lftoeaScheduledEventId,
    lftoeaStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.LambdaFunctionTimeoutType

-- | Provides details of the @LambdaFunctionTimedOut@ event.
--
-- /See:/ 'mkLambdaFunctionTimedOutEventAttributes' smart constructor.
data LambdaFunctionTimedOutEventAttributes = LambdaFunctionTimedOutEventAttributes'
  { timeoutType ::
      Lude.Maybe
        LambdaFunctionTimeoutType,
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

-- | Creates a value of 'LambdaFunctionTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- * 'scheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
-- * 'startedEventId' - The ID of the @ActivityTaskStarted@ event that was recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
-- * 'timeoutType' - The type of the timeout that caused this event.
mkLambdaFunctionTimedOutEventAttributes ::
  -- | 'scheduledEventId'
  Lude.Integer ->
  -- | 'startedEventId'
  Lude.Integer ->
  LambdaFunctionTimedOutEventAttributes
mkLambdaFunctionTimedOutEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    LambdaFunctionTimedOutEventAttributes'
      { timeoutType =
          Lude.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The type of the timeout that caused this event.
--
-- /Note:/ Consider using 'timeoutType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lftoeaTimeoutType :: Lens.Lens' LambdaFunctionTimedOutEventAttributes (Lude.Maybe LambdaFunctionTimeoutType)
lftoeaTimeoutType = Lens.lens (timeoutType :: LambdaFunctionTimedOutEventAttributes -> Lude.Maybe LambdaFunctionTimeoutType) (\s a -> s {timeoutType = a} :: LambdaFunctionTimedOutEventAttributes)
{-# DEPRECATED lftoeaTimeoutType "Use generic-lens or generic-optics with 'timeoutType' instead." #-}

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lftoeaScheduledEventId :: Lens.Lens' LambdaFunctionTimedOutEventAttributes Lude.Integer
lftoeaScheduledEventId = Lens.lens (scheduledEventId :: LambdaFunctionTimedOutEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: LambdaFunctionTimedOutEventAttributes)
{-# DEPRECATED lftoeaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The ID of the @ActivityTaskStarted@ event that was recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lftoeaStartedEventId :: Lens.Lens' LambdaFunctionTimedOutEventAttributes Lude.Integer
lftoeaStartedEventId = Lens.lens (startedEventId :: LambdaFunctionTimedOutEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: LambdaFunctionTimedOutEventAttributes)
{-# DEPRECATED lftoeaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance Lude.FromJSON LambdaFunctionTimedOutEventAttributes where
  parseJSON =
    Lude.withObject
      "LambdaFunctionTimedOutEventAttributes"
      ( \x ->
          LambdaFunctionTimedOutEventAttributes'
            Lude.<$> (x Lude..:? "timeoutType")
            Lude.<*> (x Lude..: "scheduledEventId")
            Lude.<*> (x Lude..: "startedEventId")
      )
