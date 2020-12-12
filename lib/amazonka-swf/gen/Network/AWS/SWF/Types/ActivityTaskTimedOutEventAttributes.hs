{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskTimedOutEventAttributes
  ( ActivityTaskTimedOutEventAttributes (..),

    -- * Smart constructor
    mkActivityTaskTimedOutEventAttributes,

    -- * Lenses
    attoeaDetails,
    attoeaTimeoutType,
    attoeaScheduledEventId,
    attoeaStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ActivityTaskTimeoutType

-- | Provides the details of the @ActivityTaskTimedOut@ event.
--
-- /See:/ 'mkActivityTaskTimedOutEventAttributes' smart constructor.
data ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes'
  { details ::
      Lude.Maybe
        Lude.Text,
    timeoutType ::
      ActivityTaskTimeoutType,
    scheduledEventId ::
      Lude.Integer,
    startedEventId ::
      Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityTaskTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- * 'details' - Contains the content of the @details@ parameter for the last call made by the activity to @RecordActivityTaskHeartbeat@ .
-- * 'scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'startedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'timeoutType' - The type of the timeout that caused this event.
mkActivityTaskTimedOutEventAttributes ::
  -- | 'timeoutType'
  ActivityTaskTimeoutType ->
  -- | 'scheduledEventId'
  Lude.Integer ->
  -- | 'startedEventId'
  Lude.Integer ->
  ActivityTaskTimedOutEventAttributes
mkActivityTaskTimedOutEventAttributes
  pTimeoutType_
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskTimedOutEventAttributes'
      { details = Lude.Nothing,
        timeoutType = pTimeoutType_,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | Contains the content of the @details@ parameter for the last call made by the activity to @RecordActivityTaskHeartbeat@ .
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attoeaDetails :: Lens.Lens' ActivityTaskTimedOutEventAttributes (Lude.Maybe Lude.Text)
attoeaDetails = Lens.lens (details :: ActivityTaskTimedOutEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: ActivityTaskTimedOutEventAttributes)
{-# DEPRECATED attoeaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The type of the timeout that caused this event.
--
-- /Note:/ Consider using 'timeoutType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attoeaTimeoutType :: Lens.Lens' ActivityTaskTimedOutEventAttributes ActivityTaskTimeoutType
attoeaTimeoutType = Lens.lens (timeoutType :: ActivityTaskTimedOutEventAttributes -> ActivityTaskTimeoutType) (\s a -> s {timeoutType = a} :: ActivityTaskTimedOutEventAttributes)
{-# DEPRECATED attoeaTimeoutType "Use generic-lens or generic-optics with 'timeoutType' instead." #-}

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attoeaScheduledEventId :: Lens.Lens' ActivityTaskTimedOutEventAttributes Lude.Integer
attoeaScheduledEventId = Lens.lens (scheduledEventId :: ActivityTaskTimedOutEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: ActivityTaskTimedOutEventAttributes)
{-# DEPRECATED attoeaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attoeaStartedEventId :: Lens.Lens' ActivityTaskTimedOutEventAttributes Lude.Integer
attoeaStartedEventId = Lens.lens (startedEventId :: ActivityTaskTimedOutEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: ActivityTaskTimedOutEventAttributes)
{-# DEPRECATED attoeaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance Lude.FromJSON ActivityTaskTimedOutEventAttributes where
  parseJSON =
    Lude.withObject
      "ActivityTaskTimedOutEventAttributes"
      ( \x ->
          ActivityTaskTimedOutEventAttributes'
            Lude.<$> (x Lude..:? "details")
            Lude.<*> (x Lude..: "timeoutType")
            Lude.<*> (x Lude..: "scheduledEventId")
            Lude.<*> (x Lude..: "startedEventId")
      )
