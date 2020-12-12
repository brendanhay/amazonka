{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes
  ( DecisionTaskTimedOutEventAttributes (..),

    -- * Smart constructor
    mkDecisionTaskTimedOutEventAttributes,

    -- * Lenses
    dttoeaTimeoutType,
    dttoeaScheduledEventId,
    dttoeaStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.DecisionTaskTimeoutType

-- | Provides the details of the @DecisionTaskTimedOut@ event.
--
-- /See:/ 'mkDecisionTaskTimedOutEventAttributes' smart constructor.
data DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes'
  { timeoutType ::
      DecisionTaskTimeoutType,
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

-- | Creates a value of 'DecisionTaskTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- * 'scheduledEventId' - The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'startedEventId' - The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'timeoutType' - The type of timeout that expired before the decision task could be completed.
mkDecisionTaskTimedOutEventAttributes ::
  -- | 'timeoutType'
  DecisionTaskTimeoutType ->
  -- | 'scheduledEventId'
  Lude.Integer ->
  -- | 'startedEventId'
  Lude.Integer ->
  DecisionTaskTimedOutEventAttributes
mkDecisionTaskTimedOutEventAttributes
  pTimeoutType_
  pScheduledEventId_
  pStartedEventId_ =
    DecisionTaskTimedOutEventAttributes'
      { timeoutType = pTimeoutType_,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The type of timeout that expired before the decision task could be completed.
--
-- /Note:/ Consider using 'timeoutType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttoeaTimeoutType :: Lens.Lens' DecisionTaskTimedOutEventAttributes DecisionTaskTimeoutType
dttoeaTimeoutType = Lens.lens (timeoutType :: DecisionTaskTimedOutEventAttributes -> DecisionTaskTimeoutType) (\s a -> s {timeoutType = a} :: DecisionTaskTimedOutEventAttributes)
{-# DEPRECATED dttoeaTimeoutType "Use generic-lens or generic-optics with 'timeoutType' instead." #-}

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttoeaScheduledEventId :: Lens.Lens' DecisionTaskTimedOutEventAttributes Lude.Integer
dttoeaScheduledEventId = Lens.lens (scheduledEventId :: DecisionTaskTimedOutEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: DecisionTaskTimedOutEventAttributes)
{-# DEPRECATED dttoeaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttoeaStartedEventId :: Lens.Lens' DecisionTaskTimedOutEventAttributes Lude.Integer
dttoeaStartedEventId = Lens.lens (startedEventId :: DecisionTaskTimedOutEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: DecisionTaskTimedOutEventAttributes)
{-# DEPRECATED dttoeaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance Lude.FromJSON DecisionTaskTimedOutEventAttributes where
  parseJSON =
    Lude.withObject
      "DecisionTaskTimedOutEventAttributes"
      ( \x ->
          DecisionTaskTimedOutEventAttributes'
            Lude.<$> (x Lude..: "timeoutType")
            Lude.<*> (x Lude..: "scheduledEventId")
            Lude.<*> (x Lude..: "startedEventId")
      )
