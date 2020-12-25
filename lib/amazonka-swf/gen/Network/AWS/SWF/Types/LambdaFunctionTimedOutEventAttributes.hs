{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    lftoeaScheduledEventId,
    lftoeaStartedEventId,
    lftoeaTimeoutType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.LambdaFunctionTimeoutType as Types

-- | Provides details of the @LambdaFunctionTimedOut@ event.
--
-- /See:/ 'mkLambdaFunctionTimedOutEventAttributes' smart constructor.
data LambdaFunctionTimedOutEventAttributes = LambdaFunctionTimedOutEventAttributes'
  { -- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    scheduledEventId :: Core.Integer,
    -- | The ID of the @ActivityTaskStarted@ event that was recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    startedEventId :: Core.Integer,
    -- | The type of the timeout that caused this event.
    timeoutType :: Core.Maybe Types.LambdaFunctionTimeoutType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionTimedOutEventAttributes' value with any optional fields omitted.
mkLambdaFunctionTimedOutEventAttributes ::
  -- | 'scheduledEventId'
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
  LambdaFunctionTimedOutEventAttributes
mkLambdaFunctionTimedOutEventAttributes
  scheduledEventId
  startedEventId =
    LambdaFunctionTimedOutEventAttributes'
      { scheduledEventId,
        startedEventId,
        timeoutType = Core.Nothing
      }

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lftoeaScheduledEventId :: Lens.Lens' LambdaFunctionTimedOutEventAttributes Core.Integer
lftoeaScheduledEventId = Lens.field @"scheduledEventId"
{-# DEPRECATED lftoeaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The ID of the @ActivityTaskStarted@ event that was recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lftoeaStartedEventId :: Lens.Lens' LambdaFunctionTimedOutEventAttributes Core.Integer
lftoeaStartedEventId = Lens.field @"startedEventId"
{-# DEPRECATED lftoeaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | The type of the timeout that caused this event.
--
-- /Note:/ Consider using 'timeoutType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lftoeaTimeoutType :: Lens.Lens' LambdaFunctionTimedOutEventAttributes (Core.Maybe Types.LambdaFunctionTimeoutType)
lftoeaTimeoutType = Lens.field @"timeoutType"
{-# DEPRECATED lftoeaTimeoutType "Use generic-lens or generic-optics with 'timeoutType' instead." #-}

instance Core.FromJSON LambdaFunctionTimedOutEventAttributes where
  parseJSON =
    Core.withObject "LambdaFunctionTimedOutEventAttributes" Core.$
      \x ->
        LambdaFunctionTimedOutEventAttributes'
          Core.<$> (x Core..: "scheduledEventId")
          Core.<*> (x Core..: "startedEventId")
          Core.<*> (x Core..:? "timeoutType")
