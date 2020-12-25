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
    lffeaScheduledEventId,
    lffeaStartedEventId,
    lffeaDetails,
    lffeaReason,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.FailureReason as Types

-- | Provides the details of the @LambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /See:/ 'mkLambdaFunctionFailedEventAttributes' smart constructor.
data LambdaFunctionFailedEventAttributes = LambdaFunctionFailedEventAttributes'
  { -- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    scheduledEventId :: Core.Integer,
    -- | The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    startedEventId :: Core.Integer,
    -- | The details of the failure.
    details :: Core.Maybe Types.Data,
    -- | The reason provided for the failure.
    reason :: Core.Maybe Types.FailureReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionFailedEventAttributes' value with any optional fields omitted.
mkLambdaFunctionFailedEventAttributes ::
  -- | 'scheduledEventId'
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
  LambdaFunctionFailedEventAttributes
mkLambdaFunctionFailedEventAttributes
  scheduledEventId
  startedEventId =
    LambdaFunctionFailedEventAttributes'
      { scheduledEventId,
        startedEventId,
        details = Core.Nothing,
        reason = Core.Nothing
      }

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lffeaScheduledEventId :: Lens.Lens' LambdaFunctionFailedEventAttributes Core.Integer
lffeaScheduledEventId = Lens.field @"scheduledEventId"
{-# DEPRECATED lffeaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lffeaStartedEventId :: Lens.Lens' LambdaFunctionFailedEventAttributes Core.Integer
lffeaStartedEventId = Lens.field @"startedEventId"
{-# DEPRECATED lffeaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | The details of the failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lffeaDetails :: Lens.Lens' LambdaFunctionFailedEventAttributes (Core.Maybe Types.Data)
lffeaDetails = Lens.field @"details"
{-# DEPRECATED lffeaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The reason provided for the failure.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lffeaReason :: Lens.Lens' LambdaFunctionFailedEventAttributes (Core.Maybe Types.FailureReason)
lffeaReason = Lens.field @"reason"
{-# DEPRECATED lffeaReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON LambdaFunctionFailedEventAttributes where
  parseJSON =
    Core.withObject "LambdaFunctionFailedEventAttributes" Core.$
      \x ->
        LambdaFunctionFailedEventAttributes'
          Core.<$> (x Core..: "scheduledEventId")
          Core.<*> (x Core..: "startedEventId")
          Core.<*> (x Core..:? "details")
          Core.<*> (x Core..:? "reason")
