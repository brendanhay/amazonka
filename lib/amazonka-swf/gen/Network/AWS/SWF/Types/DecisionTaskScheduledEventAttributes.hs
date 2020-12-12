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
    dtseaTaskPriority,
    dtseaStartToCloseTimeout,
    dtseaTaskList,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.TaskList

-- | Provides details about the @DecisionTaskScheduled@ event.
--
-- /See:/ 'mkDecisionTaskScheduledEventAttributes' smart constructor.
data DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes'
  { taskPriority ::
      Lude.Maybe
        Lude.Text,
    startToCloseTimeout ::
      Lude.Maybe
        Lude.Text,
    taskList ::
      TaskList
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DecisionTaskScheduledEventAttributes' with the minimum fields required to make a request.
--
-- * 'startToCloseTimeout' - The maximum duration for this decision task. The task is considered timed out if it doesn't completed within this duration.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'taskList' - The name of the task list in which the decision task was scheduled.
-- * 'taskPriority' - A task priority that, if set, specifies the priority for this decision task. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
mkDecisionTaskScheduledEventAttributes ::
  -- | 'taskList'
  TaskList ->
  DecisionTaskScheduledEventAttributes
mkDecisionTaskScheduledEventAttributes pTaskList_ =
  DecisionTaskScheduledEventAttributes'
    { taskPriority =
        Lude.Nothing,
      startToCloseTimeout = Lude.Nothing,
      taskList = pTaskList_
    }

-- | A task priority that, if set, specifies the priority for this decision task. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtseaTaskPriority :: Lens.Lens' DecisionTaskScheduledEventAttributes (Lude.Maybe Lude.Text)
dtseaTaskPriority = Lens.lens (taskPriority :: DecisionTaskScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {taskPriority = a} :: DecisionTaskScheduledEventAttributes)
{-# DEPRECATED dtseaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | The maximum duration for this decision task. The task is considered timed out if it doesn't completed within this duration.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'startToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtseaStartToCloseTimeout :: Lens.Lens' DecisionTaskScheduledEventAttributes (Lude.Maybe Lude.Text)
dtseaStartToCloseTimeout = Lens.lens (startToCloseTimeout :: DecisionTaskScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {startToCloseTimeout = a} :: DecisionTaskScheduledEventAttributes)
{-# DEPRECATED dtseaStartToCloseTimeout "Use generic-lens or generic-optics with 'startToCloseTimeout' instead." #-}

-- | The name of the task list in which the decision task was scheduled.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtseaTaskList :: Lens.Lens' DecisionTaskScheduledEventAttributes TaskList
dtseaTaskList = Lens.lens (taskList :: DecisionTaskScheduledEventAttributes -> TaskList) (\s a -> s {taskList = a} :: DecisionTaskScheduledEventAttributes)
{-# DEPRECATED dtseaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

instance Lude.FromJSON DecisionTaskScheduledEventAttributes where
  parseJSON =
    Lude.withObject
      "DecisionTaskScheduledEventAttributes"
      ( \x ->
          DecisionTaskScheduledEventAttributes'
            Lude.<$> (x Lude..:? "taskPriority")
            Lude.<*> (x Lude..:? "startToCloseTimeout")
            Lude.<*> (x Lude..: "taskList")
      )
