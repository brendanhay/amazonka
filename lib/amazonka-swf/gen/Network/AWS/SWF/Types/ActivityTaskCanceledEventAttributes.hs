{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskCanceledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskCanceledEventAttributes
  ( ActivityTaskCanceledEventAttributes (..),

    -- * Smart constructor
    mkActivityTaskCanceledEventAttributes,

    -- * Lenses
    atceaScheduledEventId,
    atceaStartedEventId,
    atceaDetails,
    atceaLatestCancelRequestedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types

-- | Provides the details of the @ActivityTaskCanceled@ event.
--
-- /See:/ 'mkActivityTaskCanceledEventAttributes' smart constructor.
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes'
  { -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    scheduledEventId :: Core.Integer,
    -- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Core.Integer,
    -- | Details of the cancellation.
    details :: Core.Maybe Types.Data,
    -- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event recorded for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    latestCancelRequestedEventId :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityTaskCanceledEventAttributes' value with any optional fields omitted.
mkActivityTaskCanceledEventAttributes ::
  -- | 'scheduledEventId'
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
  ActivityTaskCanceledEventAttributes
mkActivityTaskCanceledEventAttributes
  scheduledEventId
  startedEventId =
    ActivityTaskCanceledEventAttributes'
      { scheduledEventId,
        startedEventId,
        details = Core.Nothing,
        latestCancelRequestedEventId = Core.Nothing
      }

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atceaScheduledEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes Core.Integer
atceaScheduledEventId = Lens.field @"scheduledEventId"
{-# DEPRECATED atceaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atceaStartedEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes Core.Integer
atceaStartedEventId = Lens.field @"startedEventId"
{-# DEPRECATED atceaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | Details of the cancellation.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atceaDetails :: Lens.Lens' ActivityTaskCanceledEventAttributes (Core.Maybe Types.Data)
atceaDetails = Lens.field @"details"
{-# DEPRECATED atceaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event recorded for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'latestCancelRequestedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atceaLatestCancelRequestedEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes (Core.Maybe Core.Integer)
atceaLatestCancelRequestedEventId = Lens.field @"latestCancelRequestedEventId"
{-# DEPRECATED atceaLatestCancelRequestedEventId "Use generic-lens or generic-optics with 'latestCancelRequestedEventId' instead." #-}

instance Core.FromJSON ActivityTaskCanceledEventAttributes where
  parseJSON =
    Core.withObject "ActivityTaskCanceledEventAttributes" Core.$
      \x ->
        ActivityTaskCanceledEventAttributes'
          Core.<$> (x Core..: "scheduledEventId")
          Core.<*> (x Core..: "startedEventId")
          Core.<*> (x Core..:? "details")
          Core.<*> (x Core..:? "latestCancelRequestedEventId")
