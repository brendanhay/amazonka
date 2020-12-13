{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskCompletedEventAttributes
  ( ActivityTaskCompletedEventAttributes (..),

    -- * Smart constructor
    mkActivityTaskCompletedEventAttributes,

    -- * Lenses
    atceaScheduledEventId,
    atceaResult,
    atceaStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @ActivityTaskCompleted@ event.
--
-- /See:/ 'mkActivityTaskCompletedEventAttributes' smart constructor.
data ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes'
  { -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    scheduledEventId :: Lude.Integer,
    -- | The results of the activity task.
    result :: Lude.Maybe Lude.Text,
    -- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityTaskCompletedEventAttributes' with the minimum fields required to make a request.
--
-- * 'scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'result' - The results of the activity task.
-- * 'startedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
mkActivityTaskCompletedEventAttributes ::
  -- | 'scheduledEventId'
  Lude.Integer ->
  -- | 'startedEventId'
  Lude.Integer ->
  ActivityTaskCompletedEventAttributes
mkActivityTaskCompletedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskCompletedEventAttributes'
      { scheduledEventId =
          pScheduledEventId_,
        result = Lude.Nothing,
        startedEventId = pStartedEventId_
      }

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atceaScheduledEventId :: Lens.Lens' ActivityTaskCompletedEventAttributes Lude.Integer
atceaScheduledEventId = Lens.lens (scheduledEventId :: ActivityTaskCompletedEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: ActivityTaskCompletedEventAttributes)
{-# DEPRECATED atceaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The results of the activity task.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atceaResult :: Lens.Lens' ActivityTaskCompletedEventAttributes (Lude.Maybe Lude.Text)
atceaResult = Lens.lens (result :: ActivityTaskCompletedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {result = a} :: ActivityTaskCompletedEventAttributes)
{-# DEPRECATED atceaResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atceaStartedEventId :: Lens.Lens' ActivityTaskCompletedEventAttributes Lude.Integer
atceaStartedEventId = Lens.lens (startedEventId :: ActivityTaskCompletedEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: ActivityTaskCompletedEventAttributes)
{-# DEPRECATED atceaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance Lude.FromJSON ActivityTaskCompletedEventAttributes where
  parseJSON =
    Lude.withObject
      "ActivityTaskCompletedEventAttributes"
      ( \x ->
          ActivityTaskCompletedEventAttributes'
            Lude.<$> (x Lude..: "scheduledEventId")
            Lude.<*> (x Lude..:? "result")
            Lude.<*> (x Lude..: "startedEventId")
      )
