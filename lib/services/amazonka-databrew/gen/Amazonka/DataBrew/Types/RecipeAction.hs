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
-- Module      : Amazonka.DataBrew.Types.RecipeAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.RecipeAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a transformation and associated parameters that are used to
-- apply a change to a DataBrew dataset. For more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/recipe-actions-reference.html Recipe actions reference>.
--
-- /See:/ 'newRecipeAction' smart constructor.
data RecipeAction = RecipeAction'
  { -- | Contextual parameters for the transformation.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of a valid DataBrew transformation to be performed on the data.
    operation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecipeAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'recipeAction_parameters' - Contextual parameters for the transformation.
--
-- 'operation', 'recipeAction_operation' - The name of a valid DataBrew transformation to be performed on the data.
newRecipeAction ::
  -- | 'operation'
  Prelude.Text ->
  RecipeAction
newRecipeAction pOperation_ =
  RecipeAction'
    { parameters = Prelude.Nothing,
      operation = pOperation_
    }

-- | Contextual parameters for the transformation.
recipeAction_parameters :: Lens.Lens' RecipeAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recipeAction_parameters = Lens.lens (\RecipeAction' {parameters} -> parameters) (\s@RecipeAction' {} a -> s {parameters = a} :: RecipeAction) Prelude.. Lens.mapping Lens.coerced

-- | The name of a valid DataBrew transformation to be performed on the data.
recipeAction_operation :: Lens.Lens' RecipeAction Prelude.Text
recipeAction_operation = Lens.lens (\RecipeAction' {operation} -> operation) (\s@RecipeAction' {} a -> s {operation = a} :: RecipeAction)

instance Data.FromJSON RecipeAction where
  parseJSON =
    Data.withObject
      "RecipeAction"
      ( \x ->
          RecipeAction'
            Prelude.<$> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Operation")
      )

instance Prelude.Hashable RecipeAction where
  hashWithSalt _salt RecipeAction' {..} =
    _salt `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` operation

instance Prelude.NFData RecipeAction where
  rnf RecipeAction' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf operation

instance Data.ToJSON RecipeAction where
  toJSON RecipeAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Parameters" Data..=) Prelude.<$> parameters,
            Prelude.Just ("Operation" Data..= operation)
          ]
      )
