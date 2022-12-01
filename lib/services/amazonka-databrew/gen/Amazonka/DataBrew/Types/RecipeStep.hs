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
-- Module      : Amazonka.DataBrew.Types.RecipeStep
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.RecipeStep where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataBrew.Types.ConditionExpression
import Amazonka.DataBrew.Types.RecipeAction
import qualified Amazonka.Prelude as Prelude

-- | Represents a single step from a DataBrew recipe to be performed.
--
-- /See:/ 'newRecipeStep' smart constructor.
data RecipeStep = RecipeStep'
  { -- | One or more conditions that must be met for the recipe step to succeed.
    --
    -- All of the conditions in the array must be met. In other words, all of
    -- the conditions must be combined using a logical AND operation.
    conditionExpressions :: Prelude.Maybe [ConditionExpression],
    -- | The particular action to be performed in the recipe step.
    action :: RecipeAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecipeStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionExpressions', 'recipeStep_conditionExpressions' - One or more conditions that must be met for the recipe step to succeed.
--
-- All of the conditions in the array must be met. In other words, all of
-- the conditions must be combined using a logical AND operation.
--
-- 'action', 'recipeStep_action' - The particular action to be performed in the recipe step.
newRecipeStep ::
  -- | 'action'
  RecipeAction ->
  RecipeStep
newRecipeStep pAction_ =
  RecipeStep'
    { conditionExpressions = Prelude.Nothing,
      action = pAction_
    }

-- | One or more conditions that must be met for the recipe step to succeed.
--
-- All of the conditions in the array must be met. In other words, all of
-- the conditions must be combined using a logical AND operation.
recipeStep_conditionExpressions :: Lens.Lens' RecipeStep (Prelude.Maybe [ConditionExpression])
recipeStep_conditionExpressions = Lens.lens (\RecipeStep' {conditionExpressions} -> conditionExpressions) (\s@RecipeStep' {} a -> s {conditionExpressions = a} :: RecipeStep) Prelude.. Lens.mapping Lens.coerced

-- | The particular action to be performed in the recipe step.
recipeStep_action :: Lens.Lens' RecipeStep RecipeAction
recipeStep_action = Lens.lens (\RecipeStep' {action} -> action) (\s@RecipeStep' {} a -> s {action = a} :: RecipeStep)

instance Core.FromJSON RecipeStep where
  parseJSON =
    Core.withObject
      "RecipeStep"
      ( \x ->
          RecipeStep'
            Prelude.<$> ( x Core..:? "ConditionExpressions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "Action")
      )

instance Prelude.Hashable RecipeStep where
  hashWithSalt _salt RecipeStep' {..} =
    _salt `Prelude.hashWithSalt` conditionExpressions
      `Prelude.hashWithSalt` action

instance Prelude.NFData RecipeStep where
  rnf RecipeStep' {..} =
    Prelude.rnf conditionExpressions
      `Prelude.seq` Prelude.rnf action

instance Core.ToJSON RecipeStep where
  toJSON RecipeStep' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConditionExpressions" Core..=)
              Prelude.<$> conditionExpressions,
            Prelude.Just ("Action" Core..= action)
          ]
      )
