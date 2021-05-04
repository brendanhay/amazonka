{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskScheduledEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskScheduledEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.TaskList

-- | Provides details about the @DecisionTaskScheduled@ event.
--
-- /See:/ 'newDecisionTaskScheduledEventAttributes' smart constructor.
data DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes'
  { -- | A task priority that, if set, specifies the priority for this decision
    -- task. Valid values are integers that range from Java\'s
    -- @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647).
    -- Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
    -- in the /Amazon SWF Developer Guide/.
    taskPriority :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration for this decision task. The task is considered
    -- timed out if it doesn\'t completed within this duration.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    startToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The name of the task list in which the decision task was scheduled.
    taskList :: TaskList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DecisionTaskScheduledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskPriority', 'decisionTaskScheduledEventAttributes_taskPriority' - A task priority that, if set, specifies the priority for this decision
-- task. Valid values are integers that range from Java\'s
-- @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647).
-- Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
--
-- 'startToCloseTimeout', 'decisionTaskScheduledEventAttributes_startToCloseTimeout' - The maximum duration for this decision task. The task is considered
-- timed out if it doesn\'t completed within this duration.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'taskList', 'decisionTaskScheduledEventAttributes_taskList' - The name of the task list in which the decision task was scheduled.
newDecisionTaskScheduledEventAttributes ::
  -- | 'taskList'
  TaskList ->
  DecisionTaskScheduledEventAttributes
newDecisionTaskScheduledEventAttributes pTaskList_ =
  DecisionTaskScheduledEventAttributes'
    { taskPriority =
        Prelude.Nothing,
      startToCloseTimeout = Prelude.Nothing,
      taskList = pTaskList_
    }

-- | A task priority that, if set, specifies the priority for this decision
-- task. Valid values are integers that range from Java\'s
-- @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647).
-- Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
decisionTaskScheduledEventAttributes_taskPriority :: Lens.Lens' DecisionTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
decisionTaskScheduledEventAttributes_taskPriority = Lens.lens (\DecisionTaskScheduledEventAttributes' {taskPriority} -> taskPriority) (\s@DecisionTaskScheduledEventAttributes' {} a -> s {taskPriority = a} :: DecisionTaskScheduledEventAttributes)

-- | The maximum duration for this decision task. The task is considered
-- timed out if it doesn\'t completed within this duration.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
decisionTaskScheduledEventAttributes_startToCloseTimeout :: Lens.Lens' DecisionTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
decisionTaskScheduledEventAttributes_startToCloseTimeout = Lens.lens (\DecisionTaskScheduledEventAttributes' {startToCloseTimeout} -> startToCloseTimeout) (\s@DecisionTaskScheduledEventAttributes' {} a -> s {startToCloseTimeout = a} :: DecisionTaskScheduledEventAttributes)

-- | The name of the task list in which the decision task was scheduled.
decisionTaskScheduledEventAttributes_taskList :: Lens.Lens' DecisionTaskScheduledEventAttributes TaskList
decisionTaskScheduledEventAttributes_taskList = Lens.lens (\DecisionTaskScheduledEventAttributes' {taskList} -> taskList) (\s@DecisionTaskScheduledEventAttributes' {} a -> s {taskList = a} :: DecisionTaskScheduledEventAttributes)

instance
  Prelude.FromJSON
    DecisionTaskScheduledEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "DecisionTaskScheduledEventAttributes"
      ( \x ->
          DecisionTaskScheduledEventAttributes'
            Prelude.<$> (x Prelude..:? "taskPriority")
            Prelude.<*> (x Prelude..:? "startToCloseTimeout")
            Prelude.<*> (x Prelude..: "taskList")
      )

instance
  Prelude.Hashable
    DecisionTaskScheduledEventAttributes

instance
  Prelude.NFData
    DecisionTaskScheduledEventAttributes
