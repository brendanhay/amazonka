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
-- Module      : Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraintType
import qualified Network.AWS.Lens as Lens

-- | An object representing a constraint on task placement in the task
-- definition. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task Placement Constraints>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If you are using the Fargate launch type, task placement constraints are
-- not supported.
--
-- /See:/ 'newTaskDefinitionPlacementConstraint' smart constructor.
data TaskDefinitionPlacementConstraint = TaskDefinitionPlacementConstraint'
  { -- | The type of constraint. The @MemberOf@ constraint restricts selection to
    -- be from a group of valid candidates.
    type' :: Core.Maybe TaskDefinitionPlacementConstraintType,
    -- | A cluster query language expression to apply to the constraint. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    expression :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TaskDefinitionPlacementConstraint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'taskDefinitionPlacementConstraint_type' - The type of constraint. The @MemberOf@ constraint restricts selection to
-- be from a group of valid candidates.
--
-- 'expression', 'taskDefinitionPlacementConstraint_expression' - A cluster query language expression to apply to the constraint. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
-- in the /Amazon Elastic Container Service Developer Guide/.
newTaskDefinitionPlacementConstraint ::
  TaskDefinitionPlacementConstraint
newTaskDefinitionPlacementConstraint =
  TaskDefinitionPlacementConstraint'
    { type' =
        Core.Nothing,
      expression = Core.Nothing
    }

-- | The type of constraint. The @MemberOf@ constraint restricts selection to
-- be from a group of valid candidates.
taskDefinitionPlacementConstraint_type :: Lens.Lens' TaskDefinitionPlacementConstraint (Core.Maybe TaskDefinitionPlacementConstraintType)
taskDefinitionPlacementConstraint_type = Lens.lens (\TaskDefinitionPlacementConstraint' {type'} -> type') (\s@TaskDefinitionPlacementConstraint' {} a -> s {type' = a} :: TaskDefinitionPlacementConstraint)

-- | A cluster query language expression to apply to the constraint. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskDefinitionPlacementConstraint_expression :: Lens.Lens' TaskDefinitionPlacementConstraint (Core.Maybe Core.Text)
taskDefinitionPlacementConstraint_expression = Lens.lens (\TaskDefinitionPlacementConstraint' {expression} -> expression) (\s@TaskDefinitionPlacementConstraint' {} a -> s {expression = a} :: TaskDefinitionPlacementConstraint)

instance
  Core.FromJSON
    TaskDefinitionPlacementConstraint
  where
  parseJSON =
    Core.withObject
      "TaskDefinitionPlacementConstraint"
      ( \x ->
          TaskDefinitionPlacementConstraint'
            Core.<$> (x Core..:? "type")
            Core.<*> (x Core..:? "expression")
      )

instance
  Core.Hashable
    TaskDefinitionPlacementConstraint

instance
  Core.NFData
    TaskDefinitionPlacementConstraint

instance
  Core.ToJSON
    TaskDefinitionPlacementConstraint
  where
  toJSON TaskDefinitionPlacementConstraint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("type" Core..=) Core.<$> type',
            ("expression" Core..=) Core.<$> expression
          ]
      )
