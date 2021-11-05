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
-- Module      : Amazonka.AppFlow.Types.Task
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.Task where

import Amazonka.AppFlow.Types.ConnectorOperator
import Amazonka.AppFlow.Types.OperatorPropertiesKeys
import Amazonka.AppFlow.Types.TaskType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A class for modeling different type of tasks. Task implementation varies
-- based on the @TaskType@.
--
-- /See:/ 'newTask' smart constructor.
data Task = Task'
  { -- | A map used to store task-related information. The execution service
    -- looks for particular information based on the @TaskType@.
    taskProperties :: Prelude.Maybe (Prelude.HashMap OperatorPropertiesKeys Prelude.Text),
    -- | The operation to be performed on the provided source fields.
    connectorOperator :: Prelude.Maybe ConnectorOperator,
    -- | A field in a destination connector, or a field value against which
    -- Amazon AppFlow validates a source field.
    destinationField :: Prelude.Maybe Prelude.Text,
    -- | The source fields to which a particular task is applied.
    sourceFields :: [Prelude.Text],
    -- | Specifies the particular task implementation that Amazon AppFlow
    -- performs.
    taskType :: TaskType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Task' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskProperties', 'task_taskProperties' - A map used to store task-related information. The execution service
-- looks for particular information based on the @TaskType@.
--
-- 'connectorOperator', 'task_connectorOperator' - The operation to be performed on the provided source fields.
--
-- 'destinationField', 'task_destinationField' - A field in a destination connector, or a field value against which
-- Amazon AppFlow validates a source field.
--
-- 'sourceFields', 'task_sourceFields' - The source fields to which a particular task is applied.
--
-- 'taskType', 'task_taskType' - Specifies the particular task implementation that Amazon AppFlow
-- performs.
newTask ::
  -- | 'taskType'
  TaskType ->
  Task
newTask pTaskType_ =
  Task'
    { taskProperties = Prelude.Nothing,
      connectorOperator = Prelude.Nothing,
      destinationField = Prelude.Nothing,
      sourceFields = Prelude.mempty,
      taskType = pTaskType_
    }

-- | A map used to store task-related information. The execution service
-- looks for particular information based on the @TaskType@.
task_taskProperties :: Lens.Lens' Task (Prelude.Maybe (Prelude.HashMap OperatorPropertiesKeys Prelude.Text))
task_taskProperties = Lens.lens (\Task' {taskProperties} -> taskProperties) (\s@Task' {} a -> s {taskProperties = a} :: Task) Prelude.. Lens.mapping Lens.coerced

-- | The operation to be performed on the provided source fields.
task_connectorOperator :: Lens.Lens' Task (Prelude.Maybe ConnectorOperator)
task_connectorOperator = Lens.lens (\Task' {connectorOperator} -> connectorOperator) (\s@Task' {} a -> s {connectorOperator = a} :: Task)

-- | A field in a destination connector, or a field value against which
-- Amazon AppFlow validates a source field.
task_destinationField :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_destinationField = Lens.lens (\Task' {destinationField} -> destinationField) (\s@Task' {} a -> s {destinationField = a} :: Task)

-- | The source fields to which a particular task is applied.
task_sourceFields :: Lens.Lens' Task [Prelude.Text]
task_sourceFields = Lens.lens (\Task' {sourceFields} -> sourceFields) (\s@Task' {} a -> s {sourceFields = a} :: Task) Prelude.. Lens.coerced

-- | Specifies the particular task implementation that Amazon AppFlow
-- performs.
task_taskType :: Lens.Lens' Task TaskType
task_taskType = Lens.lens (\Task' {taskType} -> taskType) (\s@Task' {} a -> s {taskType = a} :: Task)

instance Core.FromJSON Task where
  parseJSON =
    Core.withObject
      "Task"
      ( \x ->
          Task'
            Prelude.<$> (x Core..:? "taskProperties" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "connectorOperator")
            Prelude.<*> (x Core..:? "destinationField")
            Prelude.<*> (x Core..:? "sourceFields" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "taskType")
      )

instance Prelude.Hashable Task

instance Prelude.NFData Task

instance Core.ToJSON Task where
  toJSON Task' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("taskProperties" Core..=)
              Prelude.<$> taskProperties,
            ("connectorOperator" Core..=)
              Prelude.<$> connectorOperator,
            ("destinationField" Core..=)
              Prelude.<$> destinationField,
            Prelude.Just ("sourceFields" Core..= sourceFields),
            Prelude.Just ("taskType" Core..= taskType)
          ]
      )
