{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskFailedEventAttributes
  ( ActivityTaskFailedEventAttributes (..),

    -- * Smart constructor
    mkActivityTaskFailedEventAttributes,

    -- * Lenses
    atfeaScheduledEventId,
    atfeaStartedEventId,
    atfeaDetails,
    atfeaReason,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.FailureReason as Types

-- | Provides the details of the @ActivityTaskFailed@ event.
--
-- /See:/ 'mkActivityTaskFailedEventAttributes' smart constructor.
data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes'
  { -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    scheduledEventId :: Core.Integer,
    -- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Core.Integer,
    -- | The details of the failure.
    details :: Core.Maybe Types.Data,
    -- | The reason provided for the failure.
    reason :: Core.Maybe Types.FailureReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityTaskFailedEventAttributes' value with any optional fields omitted.
mkActivityTaskFailedEventAttributes ::
  -- | 'scheduledEventId'
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
  ActivityTaskFailedEventAttributes
mkActivityTaskFailedEventAttributes scheduledEventId startedEventId =
  ActivityTaskFailedEventAttributes'
    { scheduledEventId,
      startedEventId,
      details = Core.Nothing,
      reason = Core.Nothing
    }

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfeaScheduledEventId :: Lens.Lens' ActivityTaskFailedEventAttributes Core.Integer
atfeaScheduledEventId = Lens.field @"scheduledEventId"
{-# DEPRECATED atfeaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfeaStartedEventId :: Lens.Lens' ActivityTaskFailedEventAttributes Core.Integer
atfeaStartedEventId = Lens.field @"startedEventId"
{-# DEPRECATED atfeaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | The details of the failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfeaDetails :: Lens.Lens' ActivityTaskFailedEventAttributes (Core.Maybe Types.Data)
atfeaDetails = Lens.field @"details"
{-# DEPRECATED atfeaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The reason provided for the failure.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfeaReason :: Lens.Lens' ActivityTaskFailedEventAttributes (Core.Maybe Types.FailureReason)
atfeaReason = Lens.field @"reason"
{-# DEPRECATED atfeaReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON ActivityTaskFailedEventAttributes where
  parseJSON =
    Core.withObject "ActivityTaskFailedEventAttributes" Core.$
      \x ->
        ActivityTaskFailedEventAttributes'
          Core.<$> (x Core..: "scheduledEventId")
          Core.<*> (x Core..: "startedEventId")
          Core.<*> (x Core..:? "details")
          Core.<*> (x Core..:? "reason")
