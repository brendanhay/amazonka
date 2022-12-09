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
-- Module      : Amazonka.Scheduler.Types.PlacementConstraint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.PlacementConstraint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Scheduler.Types.PlacementConstraintType

-- | An object representing a constraint on task placement.
--
-- /See:/ 'newPlacementConstraint' smart constructor.
data PlacementConstraint = PlacementConstraint'
  { -- | A cluster query language expression to apply to the constraint. You
    -- cannot specify an expression if the constraint type is
    -- @distinctInstance@. For more information, see
    -- <https://docs.aws.amazon.com/latest/developerguide/cluster-query-language.html Cluster query language>
    -- in the /Amazon ECS Developer Guide/.
    expression :: Prelude.Maybe Prelude.Text,
    -- | The type of constraint. Use @distinctInstance@ to ensure that each task
    -- in a particular group is running on a different container instance. Use
    -- @memberOf@ to restrict the selection to a group of valid candidates.
    type' :: Prelude.Maybe PlacementConstraintType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlacementConstraint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'placementConstraint_expression' - A cluster query language expression to apply to the constraint. You
-- cannot specify an expression if the constraint type is
-- @distinctInstance@. For more information, see
-- <https://docs.aws.amazon.com/latest/developerguide/cluster-query-language.html Cluster query language>
-- in the /Amazon ECS Developer Guide/.
--
-- 'type'', 'placementConstraint_type' - The type of constraint. Use @distinctInstance@ to ensure that each task
-- in a particular group is running on a different container instance. Use
-- @memberOf@ to restrict the selection to a group of valid candidates.
newPlacementConstraint ::
  PlacementConstraint
newPlacementConstraint =
  PlacementConstraint'
    { expression = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A cluster query language expression to apply to the constraint. You
-- cannot specify an expression if the constraint type is
-- @distinctInstance@. For more information, see
-- <https://docs.aws.amazon.com/latest/developerguide/cluster-query-language.html Cluster query language>
-- in the /Amazon ECS Developer Guide/.
placementConstraint_expression :: Lens.Lens' PlacementConstraint (Prelude.Maybe Prelude.Text)
placementConstraint_expression = Lens.lens (\PlacementConstraint' {expression} -> expression) (\s@PlacementConstraint' {} a -> s {expression = a} :: PlacementConstraint)

-- | The type of constraint. Use @distinctInstance@ to ensure that each task
-- in a particular group is running on a different container instance. Use
-- @memberOf@ to restrict the selection to a group of valid candidates.
placementConstraint_type :: Lens.Lens' PlacementConstraint (Prelude.Maybe PlacementConstraintType)
placementConstraint_type = Lens.lens (\PlacementConstraint' {type'} -> type') (\s@PlacementConstraint' {} a -> s {type' = a} :: PlacementConstraint)

instance Data.FromJSON PlacementConstraint where
  parseJSON =
    Data.withObject
      "PlacementConstraint"
      ( \x ->
          PlacementConstraint'
            Prelude.<$> (x Data..:? "expression")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable PlacementConstraint where
  hashWithSalt _salt PlacementConstraint' {..} =
    _salt `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` type'

instance Prelude.NFData PlacementConstraint where
  rnf PlacementConstraint' {..} =
    Prelude.rnf expression
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON PlacementConstraint where
  toJSON PlacementConstraint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("expression" Data..=) Prelude.<$> expression,
            ("type" Data..=) Prelude.<$> type'
          ]
      )
