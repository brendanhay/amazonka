-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskCompletedEventAttributes
  ( DecisionTaskCompletedEventAttributes (..),

    -- * Smart constructor
    mkDecisionTaskCompletedEventAttributes,

    -- * Lenses
    dtceaExecutionContext,
    dtceaScheduledEventId,
    dtceaStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @DecisionTaskCompleted@ event.
--
-- /See:/ 'mkDecisionTaskCompletedEventAttributes' smart constructor.
data DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes'
  { executionContext ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'DecisionTaskCompletedEventAttributes' with the minimum fields required to make a request.
--
-- * 'executionContext' - User defined context for the workflow execution.
-- * 'scheduledEventId' - The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'startedEventId' - The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
mkDecisionTaskCompletedEventAttributes ::
  -- | 'scheduledEventId'
  Lude.Integer ->
  -- | 'startedEventId'
  Lude.Integer ->
  DecisionTaskCompletedEventAttributes
mkDecisionTaskCompletedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    DecisionTaskCompletedEventAttributes'
      { executionContext =
          Lude.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | User defined context for the workflow execution.
--
-- /Note:/ Consider using 'executionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtceaExecutionContext :: Lens.Lens' DecisionTaskCompletedEventAttributes (Lude.Maybe Lude.Text)
dtceaExecutionContext = Lens.lens (executionContext :: DecisionTaskCompletedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {executionContext = a} :: DecisionTaskCompletedEventAttributes)
{-# DEPRECATED dtceaExecutionContext "Use generic-lens or generic-optics with 'executionContext' instead." #-}

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtceaScheduledEventId :: Lens.Lens' DecisionTaskCompletedEventAttributes Lude.Integer
dtceaScheduledEventId = Lens.lens (scheduledEventId :: DecisionTaskCompletedEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: DecisionTaskCompletedEventAttributes)
{-# DEPRECATED dtceaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtceaStartedEventId :: Lens.Lens' DecisionTaskCompletedEventAttributes Lude.Integer
dtceaStartedEventId = Lens.lens (startedEventId :: DecisionTaskCompletedEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: DecisionTaskCompletedEventAttributes)
{-# DEPRECATED dtceaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance Lude.FromJSON DecisionTaskCompletedEventAttributes where
  parseJSON =
    Lude.withObject
      "DecisionTaskCompletedEventAttributes"
      ( \x ->
          DecisionTaskCompletedEventAttributes'
            Lude.<$> (x Lude..:? "executionContext")
            Lude.<*> (x Lude..: "scheduledEventId")
            Lude.<*> (x Lude..: "startedEventId")
      )
