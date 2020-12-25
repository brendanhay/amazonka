{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskCancelRequestedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskCancelRequestedEventAttributes
  ( ActivityTaskCancelRequestedEventAttributes (..),

    -- * Smart constructor
    mkActivityTaskCancelRequestedEventAttributes,

    -- * Lenses
    atcreaDecisionTaskCompletedEventId,
    atcreaActivityId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.ActivityId as Types

-- | Provides the details of the @ActivityTaskCancelRequested@ event.
--
-- /See:/ 'mkActivityTaskCancelRequestedEventAttributes' smart constructor.
data ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes'
  { -- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelActivityTask@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer,
    -- | The unique ID of the task.
    activityId :: Types.ActivityId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityTaskCancelRequestedEventAttributes' value with any optional fields omitted.
mkActivityTaskCancelRequestedEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  -- | 'activityId'
  Types.ActivityId ->
  ActivityTaskCancelRequestedEventAttributes
mkActivityTaskCancelRequestedEventAttributes
  decisionTaskCompletedEventId
  activityId =
    ActivityTaskCancelRequestedEventAttributes'
      { decisionTaskCompletedEventId,
        activityId
      }

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelActivityTask@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcreaDecisionTaskCompletedEventId :: Lens.Lens' ActivityTaskCancelRequestedEventAttributes Core.Integer
atcreaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# DEPRECATED atcreaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

-- | The unique ID of the task.
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcreaActivityId :: Lens.Lens' ActivityTaskCancelRequestedEventAttributes Types.ActivityId
atcreaActivityId = Lens.field @"activityId"
{-# DEPRECATED atcreaActivityId "Use generic-lens or generic-optics with 'activityId' instead." #-}

instance Core.FromJSON ActivityTaskCancelRequestedEventAttributes where
  parseJSON =
    Core.withObject "ActivityTaskCancelRequestedEventAttributes" Core.$
      \x ->
        ActivityTaskCancelRequestedEventAttributes'
          Core.<$> (x Core..: "decisionTaskCompletedEventId")
          Core.<*> (x Core..: "activityId")
