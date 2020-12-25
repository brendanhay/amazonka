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
    sCause,
    sMessage,
    sScheduledEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.CauseMessage as Types
import qualified Network.AWS.SWF.Types.StartLambdaFunctionFailedCause as Types

-- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /See:/ 'mkStartLambdaFunctionFailedEventAttributes' smart constructor.
data StartLambdaFunctionFailedEventAttributes = StartLambdaFunctionFailedEventAttributes'
  { -- | The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    cause :: Core.Maybe Types.StartLambdaFunctionFailedCause,
    -- | A description that can help diagnose the cause of the fault.
    message :: Core.Maybe Types.CauseMessage,
    -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    scheduledEventId :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartLambdaFunctionFailedEventAttributes' value with any optional fields omitted.
mkStartLambdaFunctionFailedEventAttributes ::
  StartLambdaFunctionFailedEventAttributes
mkStartLambdaFunctionFailedEventAttributes =
  StartLambdaFunctionFailedEventAttributes'
    { cause = Core.Nothing,
      message = Core.Nothing,
      scheduledEventId = Core.Nothing
    }

-- | The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCause :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Core.Maybe Types.StartLambdaFunctionFailedCause)
sCause = Lens.field @"cause"
{-# DEPRECATED sCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | A description that can help diagnose the cause of the fault.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessage :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Core.Maybe Types.CauseMessage)
sMessage = Lens.field @"message"
{-# DEPRECATED sMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScheduledEventId :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Core.Maybe Core.Integer)
sScheduledEventId = Lens.field @"scheduledEventId"
{-# DEPRECATED sScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

instance Core.FromJSON StartLambdaFunctionFailedEventAttributes where
  parseJSON =
    Core.withObject "StartLambdaFunctionFailedEventAttributes" Core.$
      \x ->
        StartLambdaFunctionFailedEventAttributes'
          Core.<$> (x Core..:? "cause")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "scheduledEventId")
