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
    aLatestCancelRequestedEventId,
    aScheduledEventId,
    aDetails,
    aStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @ActivityTaskCanceled@ event.
--
-- /See:/ 'mkActivityTaskCanceledEventAttributes' smart constructor.
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes'
  { -- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event recorded for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    latestCancelRequestedEventId :: Lude.Maybe Lude.Integer,
    -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    scheduledEventId :: Lude.Integer,
    -- | Details of the cancellation.
    details :: Lude.Maybe Lude.Text,
    -- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityTaskCanceledEventAttributes' with the minimum fields required to make a request.
--
-- * 'latestCancelRequestedEventId' - If set, contains the ID of the last @ActivityTaskCancelRequested@ event recorded for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'details' - Details of the cancellation.
-- * 'startedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
mkActivityTaskCanceledEventAttributes ::
  -- | 'scheduledEventId'
  Lude.Integer ->
  -- | 'startedEventId'
  Lude.Integer ->
  ActivityTaskCanceledEventAttributes
mkActivityTaskCanceledEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskCanceledEventAttributes'
      { latestCancelRequestedEventId =
          Lude.Nothing,
        scheduledEventId = pScheduledEventId_,
        details = Lude.Nothing,
        startedEventId = pStartedEventId_
      }

-- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event recorded for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'latestCancelRequestedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLatestCancelRequestedEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes (Lude.Maybe Lude.Integer)
aLatestCancelRequestedEventId = Lens.lens (latestCancelRequestedEventId :: ActivityTaskCanceledEventAttributes -> Lude.Maybe Lude.Integer) (\s a -> s {latestCancelRequestedEventId = a} :: ActivityTaskCanceledEventAttributes)
{-# DEPRECATED aLatestCancelRequestedEventId "Use generic-lens or generic-optics with 'latestCancelRequestedEventId' instead." #-}

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aScheduledEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes Lude.Integer
aScheduledEventId = Lens.lens (scheduledEventId :: ActivityTaskCanceledEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: ActivityTaskCanceledEventAttributes)
{-# DEPRECATED aScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | Details of the cancellation.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDetails :: Lens.Lens' ActivityTaskCanceledEventAttributes (Lude.Maybe Lude.Text)
aDetails = Lens.lens (details :: ActivityTaskCanceledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: ActivityTaskCanceledEventAttributes)
{-# DEPRECATED aDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStartedEventId :: Lens.Lens' ActivityTaskCanceledEventAttributes Lude.Integer
aStartedEventId = Lens.lens (startedEventId :: ActivityTaskCanceledEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: ActivityTaskCanceledEventAttributes)
{-# DEPRECATED aStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance Lude.FromJSON ActivityTaskCanceledEventAttributes where
  parseJSON =
    Lude.withObject
      "ActivityTaskCanceledEventAttributes"
      ( \x ->
          ActivityTaskCanceledEventAttributes'
            Lude.<$> (x Lude..:? "latestCancelRequestedEventId")
            Lude.<*> (x Lude..: "scheduledEventId")
            Lude.<*> (x Lude..:? "details")
            Lude.<*> (x Lude..: "startedEventId")
      )
