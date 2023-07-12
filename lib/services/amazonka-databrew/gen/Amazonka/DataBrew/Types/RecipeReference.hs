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
-- Module      : Amazonka.DataBrew.Types.RecipeReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.RecipeReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the name and version of a DataBrew recipe.
--
-- /See:/ 'newRecipeReference' smart constructor.
data RecipeReference = RecipeReference'
  { -- | The identifier for the version for the recipe.
    recipeVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the recipe.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecipeReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recipeVersion', 'recipeReference_recipeVersion' - The identifier for the version for the recipe.
--
-- 'name', 'recipeReference_name' - The name of the recipe.
newRecipeReference ::
  -- | 'name'
  Prelude.Text ->
  RecipeReference
newRecipeReference pName_ =
  RecipeReference'
    { recipeVersion = Prelude.Nothing,
      name = pName_
    }

-- | The identifier for the version for the recipe.
recipeReference_recipeVersion :: Lens.Lens' RecipeReference (Prelude.Maybe Prelude.Text)
recipeReference_recipeVersion = Lens.lens (\RecipeReference' {recipeVersion} -> recipeVersion) (\s@RecipeReference' {} a -> s {recipeVersion = a} :: RecipeReference)

-- | The name of the recipe.
recipeReference_name :: Lens.Lens' RecipeReference Prelude.Text
recipeReference_name = Lens.lens (\RecipeReference' {name} -> name) (\s@RecipeReference' {} a -> s {name = a} :: RecipeReference)

instance Data.FromJSON RecipeReference where
  parseJSON =
    Data.withObject
      "RecipeReference"
      ( \x ->
          RecipeReference'
            Prelude.<$> (x Data..:? "RecipeVersion")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable RecipeReference where
  hashWithSalt _salt RecipeReference' {..} =
    _salt
      `Prelude.hashWithSalt` recipeVersion
      `Prelude.hashWithSalt` name

instance Prelude.NFData RecipeReference where
  rnf RecipeReference' {..} =
    Prelude.rnf recipeVersion
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON RecipeReference where
  toJSON RecipeReference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RecipeVersion" Data..=) Prelude.<$> recipeVersion,
            Prelude.Just ("Name" Data..= name)
          ]
      )
