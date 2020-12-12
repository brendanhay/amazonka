{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes
  ( LambdaFunctionFailedEventAttributes (..),

    -- * Smart constructor
    mkLambdaFunctionFailedEventAttributes,

    -- * Lenses
    lffeaReason,
    lffeaDetails,
    lffeaScheduledEventId,
    lffeaStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @LambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /See:/ 'mkLambdaFunctionFailedEventAttributes' smart constructor.
data LambdaFunctionFailedEventAttributes = LambdaFunctionFailedEventAttributes'
  { reason ::
      Lude.Maybe
        Lude.Text,
    details ::
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

-- | Creates a value of 'LambdaFunctionFailedEventAttributes' with the minimum fields required to make a request.
--
-- * 'details' - The details of the failure.
-- * 'reason' - The reason provided for the failure.
-- * 'scheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
-- * 'startedEventId' - The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
mkLambdaFunctionFailedEventAttributes ::
  -- | 'scheduledEventId'
  Lude.Integer ->
  -- | 'startedEventId'
  Lude.Integer ->
  LambdaFunctionFailedEventAttributes
mkLambdaFunctionFailedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    LambdaFunctionFailedEventAttributes'
      { reason = Lude.Nothing,
        details = Lude.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The reason provided for the failure.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lffeaReason :: Lens.Lens' LambdaFunctionFailedEventAttributes (Lude.Maybe Lude.Text)
lffeaReason = Lens.lens (reason :: LambdaFunctionFailedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: LambdaFunctionFailedEventAttributes)
{-# DEPRECATED lffeaReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The details of the failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lffeaDetails :: Lens.Lens' LambdaFunctionFailedEventAttributes (Lude.Maybe Lude.Text)
lffeaDetails = Lens.lens (details :: LambdaFunctionFailedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: LambdaFunctionFailedEventAttributes)
{-# DEPRECATED lffeaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lffeaScheduledEventId :: Lens.Lens' LambdaFunctionFailedEventAttributes Lude.Integer
lffeaScheduledEventId = Lens.lens (scheduledEventId :: LambdaFunctionFailedEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: LambdaFunctionFailedEventAttributes)
{-# DEPRECATED lffeaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lffeaStartedEventId :: Lens.Lens' LambdaFunctionFailedEventAttributes Lude.Integer
lffeaStartedEventId = Lens.lens (startedEventId :: LambdaFunctionFailedEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: LambdaFunctionFailedEventAttributes)
{-# DEPRECATED lffeaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance Lude.FromJSON LambdaFunctionFailedEventAttributes where
  parseJSON =
    Lude.withObject
      "LambdaFunctionFailedEventAttributes"
      ( \x ->
          LambdaFunctionFailedEventAttributes'
            Lude.<$> (x Lude..:? "reason")
            Lude.<*> (x Lude..:? "details")
            Lude.<*> (x Lude..: "scheduledEventId")
            Lude.<*> (x Lude..: "startedEventId")
      )
