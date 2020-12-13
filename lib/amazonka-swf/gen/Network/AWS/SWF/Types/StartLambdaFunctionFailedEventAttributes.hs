{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartLambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartLambdaFunctionFailedEventAttributes
  ( StartLambdaFunctionFailedEventAttributes (..),

    -- * Smart constructor
    mkStartLambdaFunctionFailedEventAttributes,

    -- * Lenses
    sScheduledEventId,
    sCause,
    sMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.StartLambdaFunctionFailedCause

-- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /See:/ 'mkStartLambdaFunctionFailedEventAttributes' smart constructor.
data StartLambdaFunctionFailedEventAttributes = StartLambdaFunctionFailedEventAttributes'
  { -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    scheduledEventId :: Lude.Maybe Lude.Integer,
    -- | The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    cause :: Lude.Maybe StartLambdaFunctionFailedCause,
    -- | A description that can help diagnose the cause of the fault.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartLambdaFunctionFailedEventAttributes' with the minimum fields required to make a request.
--
-- * 'scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
-- * 'cause' - The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
-- * 'message' - A description that can help diagnose the cause of the fault.
mkStartLambdaFunctionFailedEventAttributes ::
  StartLambdaFunctionFailedEventAttributes
mkStartLambdaFunctionFailedEventAttributes =
  StartLambdaFunctionFailedEventAttributes'
    { scheduledEventId =
        Lude.Nothing,
      cause = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScheduledEventId :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Lude.Maybe Lude.Integer)
sScheduledEventId = Lens.lens (scheduledEventId :: StartLambdaFunctionFailedEventAttributes -> Lude.Maybe Lude.Integer) (\s a -> s {scheduledEventId = a} :: StartLambdaFunctionFailedEventAttributes)
{-# DEPRECATED sScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCause :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Lude.Maybe StartLambdaFunctionFailedCause)
sCause = Lens.lens (cause :: StartLambdaFunctionFailedEventAttributes -> Lude.Maybe StartLambdaFunctionFailedCause) (\s a -> s {cause = a} :: StartLambdaFunctionFailedEventAttributes)
{-# DEPRECATED sCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | A description that can help diagnose the cause of the fault.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessage :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Lude.Maybe Lude.Text)
sMessage = Lens.lens (message :: StartLambdaFunctionFailedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: StartLambdaFunctionFailedEventAttributes)
{-# DEPRECATED sMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON StartLambdaFunctionFailedEventAttributes where
  parseJSON =
    Lude.withObject
      "StartLambdaFunctionFailedEventAttributes"
      ( \x ->
          StartLambdaFunctionFailedEventAttributes'
            Lude.<$> (x Lude..:? "scheduledEventId")
            Lude.<*> (x Lude..:? "cause")
            Lude.<*> (x Lude..:? "message")
      )
