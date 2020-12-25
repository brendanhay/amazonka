{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dtceaScheduledEventId,
    dtceaStartedEventId,
    dtceaExecutionContext,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types

-- | Provides the details of the @DecisionTaskCompleted@ event.
--
-- /See:/ 'mkDecisionTaskCompletedEventAttributes' smart constructor.
data DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes'
  { -- | The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    scheduledEventId :: Core.Integer,
    -- | The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Core.Integer,
    -- | User defined context for the workflow execution.
    executionContext :: Core.Maybe Types.Data
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecisionTaskCompletedEventAttributes' value with any optional fields omitted.
mkDecisionTaskCompletedEventAttributes ::
  -- | 'scheduledEventId'
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
  DecisionTaskCompletedEventAttributes
mkDecisionTaskCompletedEventAttributes
  scheduledEventId
  startedEventId =
    DecisionTaskCompletedEventAttributes'
      { scheduledEventId,
        startedEventId,
        executionContext = Core.Nothing
      }

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtceaScheduledEventId :: Lens.Lens' DecisionTaskCompletedEventAttributes Core.Integer
dtceaScheduledEventId = Lens.field @"scheduledEventId"
{-# DEPRECATED dtceaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtceaStartedEventId :: Lens.Lens' DecisionTaskCompletedEventAttributes Core.Integer
dtceaStartedEventId = Lens.field @"startedEventId"
{-# DEPRECATED dtceaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | User defined context for the workflow execution.
--
-- /Note:/ Consider using 'executionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtceaExecutionContext :: Lens.Lens' DecisionTaskCompletedEventAttributes (Core.Maybe Types.Data)
dtceaExecutionContext = Lens.field @"executionContext"
{-# DEPRECATED dtceaExecutionContext "Use generic-lens or generic-optics with 'executionContext' instead." #-}

instance Core.FromJSON DecisionTaskCompletedEventAttributes where
  parseJSON =
    Core.withObject "DecisionTaskCompletedEventAttributes" Core.$
      \x ->
        DecisionTaskCompletedEventAttributes'
          Core.<$> (x Core..: "scheduledEventId")
          Core.<*> (x Core..: "startedEventId")
          Core.<*> (x Core..:? "executionContext")
