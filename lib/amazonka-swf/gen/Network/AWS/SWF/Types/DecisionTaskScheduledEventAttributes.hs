{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskScheduledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskScheduledEventAttributes
  ( DecisionTaskScheduledEventAttributes (..),

    -- * Smart constructor
    mkDecisionTaskScheduledEventAttributes,

    -- * Lenses
    dtseaTaskList,
    dtseaStartToCloseTimeout,
    dtseaTaskPriority,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.StartToCloseTimeout as Types
import qualified Network.AWS.SWF.Types.TaskList as Types
import qualified Network.AWS.SWF.Types.TaskPriority as Types

-- | Provides details about the @DecisionTaskScheduled@ event.
--
-- /See:/ 'mkDecisionTaskScheduledEventAttributes' smart constructor.
data DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes'
  { -- | The name of the task list in which the decision task was scheduled.
    taskList :: Types.TaskList,
    -- | The maximum duration for this decision task. The task is considered timed out if it doesn't completed within this duration.
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    startToCloseTimeout :: Core.Maybe Types.StartToCloseTimeout,
    -- | A task priority that, if set, specifies the priority for this decision task. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
    taskPriority :: Core.Maybe Types.TaskPriority
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecisionTaskScheduledEventAttributes' value with any optional fields omitted.
mkDecisionTaskScheduledEventAttributes ::
  -- | 'taskList'
  Types.TaskList ->
  DecisionTaskScheduledEventAttributes
mkDecisionTaskScheduledEventAttributes taskList =
  DecisionTaskScheduledEventAttributes'
    { taskList,
      startToCloseTimeout = Core.Nothing,
      taskPriority = Core.Nothing
    }

-- | The name of the task list in which the decision task was scheduled.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtseaTaskList :: Lens.Lens' DecisionTaskScheduledEventAttributes Types.TaskList
dtseaTaskList = Lens.field @"taskList"
{-# DEPRECATED dtseaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | The maximum duration for this decision task. The task is considered timed out if it doesn't completed within this duration.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'startToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtseaStartToCloseTimeout :: Lens.Lens' DecisionTaskScheduledEventAttributes (Core.Maybe Types.StartToCloseTimeout)
dtseaStartToCloseTimeout = Lens.field @"startToCloseTimeout"
{-# DEPRECATED dtseaStartToCloseTimeout "Use generic-lens or generic-optics with 'startToCloseTimeout' instead." #-}

-- | A task priority that, if set, specifies the priority for this decision task. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtseaTaskPriority :: Lens.Lens' DecisionTaskScheduledEventAttributes (Core.Maybe Types.TaskPriority)
dtseaTaskPriority = Lens.field @"taskPriority"
{-# DEPRECATED dtseaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

instance Core.FromJSON DecisionTaskScheduledEventAttributes where
  parseJSON =
    Core.withObject "DecisionTaskScheduledEventAttributes" Core.$
      \x ->
        DecisionTaskScheduledEventAttributes'
          Core.<$> (x Core..: "taskList")
          Core.<*> (x Core..:? "startToCloseTimeout")
          Core.<*> (x Core..:? "taskPriority")
