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
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @ActivityTaskCancelRequested@ event.
--
-- /See:/ 'mkActivityTaskCancelRequestedEventAttributes' smart constructor.
data ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes'
  { decisionTaskCompletedEventId ::
      Lude.Integer,
    activityId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityTaskCancelRequestedEventAttributes' with the minimum fields required to make a request.
--
-- * 'activityId' - The unique ID of the task.
-- * 'decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelActivityTask@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
mkActivityTaskCancelRequestedEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  -- | 'activityId'
  Lude.Text ->
  ActivityTaskCancelRequestedEventAttributes
mkActivityTaskCancelRequestedEventAttributes
  pDecisionTaskCompletedEventId_
  pActivityId_ =
    ActivityTaskCancelRequestedEventAttributes'
      { decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_,
        activityId = pActivityId_
      }

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelActivityTask@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcreaDecisionTaskCompletedEventId :: Lens.Lens' ActivityTaskCancelRequestedEventAttributes Lude.Integer
atcreaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: ActivityTaskCancelRequestedEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: ActivityTaskCancelRequestedEventAttributes)
{-# DEPRECATED atcreaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

-- | The unique ID of the task.
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcreaActivityId :: Lens.Lens' ActivityTaskCancelRequestedEventAttributes Lude.Text
atcreaActivityId = Lens.lens (activityId :: ActivityTaskCancelRequestedEventAttributes -> Lude.Text) (\s a -> s {activityId = a} :: ActivityTaskCancelRequestedEventAttributes)
{-# DEPRECATED atcreaActivityId "Use generic-lens or generic-optics with 'activityId' instead." #-}

instance Lude.FromJSON ActivityTaskCancelRequestedEventAttributes where
  parseJSON =
    Lude.withObject
      "ActivityTaskCancelRequestedEventAttributes"
      ( \x ->
          ActivityTaskCancelRequestedEventAttributes'
            Lude.<$> (x Lude..: "decisionTaskCompletedEventId")
            Lude.<*> (x Lude..: "activityId")
      )
