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
    atfeaReason,
    atfeaDetails,
    atfeaStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @ActivityTaskFailed@ event.
--
-- /See:/ 'mkActivityTaskFailedEventAttributes' smart constructor.
data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes'
  { -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    scheduledEventId :: Lude.Integer,
    -- | The reason provided for the failure.
    reason :: Lude.Maybe Lude.Text,
    -- | The details of the failure.
    details :: Lude.Maybe Lude.Text,
    -- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityTaskFailedEventAttributes' with the minimum fields required to make a request.
--
-- * 'scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'reason' - The reason provided for the failure.
-- * 'details' - The details of the failure.
-- * 'startedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
mkActivityTaskFailedEventAttributes ::
  -- | 'scheduledEventId'
  Lude.Integer ->
  -- | 'startedEventId'
  Lude.Integer ->
  ActivityTaskFailedEventAttributes
mkActivityTaskFailedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskFailedEventAttributes'
      { scheduledEventId =
          pScheduledEventId_,
        reason = Lude.Nothing,
        details = Lude.Nothing,
        startedEventId = pStartedEventId_
      }

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfeaScheduledEventId :: Lens.Lens' ActivityTaskFailedEventAttributes Lude.Integer
atfeaScheduledEventId = Lens.lens (scheduledEventId :: ActivityTaskFailedEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: ActivityTaskFailedEventAttributes)
{-# DEPRECATED atfeaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The reason provided for the failure.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfeaReason :: Lens.Lens' ActivityTaskFailedEventAttributes (Lude.Maybe Lude.Text)
atfeaReason = Lens.lens (reason :: ActivityTaskFailedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: ActivityTaskFailedEventAttributes)
{-# DEPRECATED atfeaReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The details of the failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfeaDetails :: Lens.Lens' ActivityTaskFailedEventAttributes (Lude.Maybe Lude.Text)
atfeaDetails = Lens.lens (details :: ActivityTaskFailedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: ActivityTaskFailedEventAttributes)
{-# DEPRECATED atfeaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfeaStartedEventId :: Lens.Lens' ActivityTaskFailedEventAttributes Lude.Integer
atfeaStartedEventId = Lens.lens (startedEventId :: ActivityTaskFailedEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: ActivityTaskFailedEventAttributes)
{-# DEPRECATED atfeaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance Lude.FromJSON ActivityTaskFailedEventAttributes where
  parseJSON =
    Lude.withObject
      "ActivityTaskFailedEventAttributes"
      ( \x ->
          ActivityTaskFailedEventAttributes'
            Lude.<$> (x Lude..: "scheduledEventId")
            Lude.<*> (x Lude..:? "reason")
            Lude.<*> (x Lude..:? "details")
            Lude.<*> (x Lude..: "startedEventId")
      )
