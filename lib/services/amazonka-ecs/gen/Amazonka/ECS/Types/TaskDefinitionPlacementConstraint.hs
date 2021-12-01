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
-- Module      : Amazonka.ECS.Types.TaskDefinitionPlacementConstraint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.TaskDefinitionPlacementConstraint where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types.TaskDefinitionPlacementConstraintType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing a constraint on task placement in the task
-- definition. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task placement constraints>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- Task placement constraints are not supported for tasks run on Fargate.
--
-- /See:/ 'newTaskDefinitionPlacementConstraint' smart constructor.
data TaskDefinitionPlacementConstraint = TaskDefinitionPlacementConstraint'
  { -- | A cluster query language expression to apply to the constraint. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster query language>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    expression :: Prelude.Maybe Prelude.Text,
    -- | The type of constraint. The @MemberOf@ constraint restricts selection to
    -- be from a group of valid candidates.
    type' :: Prelude.Maybe TaskDefinitionPlacementConstraintType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskDefinitionPlacementConstraint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'taskDefinitionPlacementConstraint_expression' - A cluster query language expression to apply to the constraint. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster query language>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'type'', 'taskDefinitionPlacementConstraint_type' - The type of constraint. The @MemberOf@ constraint restricts selection to
-- be from a group of valid candidates.
newTaskDefinitionPlacementConstraint ::
  TaskDefinitionPlacementConstraint
newTaskDefinitionPlacementConstraint =
  TaskDefinitionPlacementConstraint'
    { expression =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A cluster query language expression to apply to the constraint. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster query language>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskDefinitionPlacementConstraint_expression :: Lens.Lens' TaskDefinitionPlacementConstraint (Prelude.Maybe Prelude.Text)
taskDefinitionPlacementConstraint_expression = Lens.lens (\TaskDefinitionPlacementConstraint' {expression} -> expression) (\s@TaskDefinitionPlacementConstraint' {} a -> s {expression = a} :: TaskDefinitionPlacementConstraint)

-- | The type of constraint. The @MemberOf@ constraint restricts selection to
-- be from a group of valid candidates.
taskDefinitionPlacementConstraint_type :: Lens.Lens' TaskDefinitionPlacementConstraint (Prelude.Maybe TaskDefinitionPlacementConstraintType)
taskDefinitionPlacementConstraint_type = Lens.lens (\TaskDefinitionPlacementConstraint' {type'} -> type') (\s@TaskDefinitionPlacementConstraint' {} a -> s {type' = a} :: TaskDefinitionPlacementConstraint)

instance
  Core.FromJSON
    TaskDefinitionPlacementConstraint
  where
  parseJSON =
    Core.withObject
      "TaskDefinitionPlacementConstraint"
      ( \x ->
          TaskDefinitionPlacementConstraint'
            Prelude.<$> (x Core..:? "expression")
            Prelude.<*> (x Core..:? "type")
      )

instance
  Prelude.Hashable
    TaskDefinitionPlacementConstraint
  where
  hashWithSalt
    salt'
    TaskDefinitionPlacementConstraint' {..} =
      salt' `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` expression

instance
  Prelude.NFData
    TaskDefinitionPlacementConstraint
  where
  rnf TaskDefinitionPlacementConstraint' {..} =
    Prelude.rnf expression
      `Prelude.seq` Prelude.rnf type'

instance
  Core.ToJSON
    TaskDefinitionPlacementConstraint
  where
  toJSON TaskDefinitionPlacementConstraint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("expression" Core..=) Prelude.<$> expression,
            ("type" Core..=) Prelude.<$> type'
          ]
      )
