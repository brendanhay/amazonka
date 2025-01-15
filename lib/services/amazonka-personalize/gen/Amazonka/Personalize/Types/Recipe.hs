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
-- Module      : Amazonka.Personalize.Types.Recipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.Recipe where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a recipe. Each recipe provides an algorithm
-- that Amazon Personalize uses in model training when you use the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSolution.html CreateSolution>
-- operation.
--
-- /See:/ 'newRecipe' smart constructor.
data Recipe = Recipe'
  { -- | The Amazon Resource Name (ARN) of the algorithm that Amazon Personalize
    -- uses to train the model.
    algorithmArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix format) that the recipe was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the recipe.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the FeatureTransformation object.
    featureTransformationArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix format) that the recipe was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the recipe.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the recipe.
    recipeArn :: Prelude.Maybe Prelude.Text,
    -- | One of the following values:
    --
    -- -   PERSONALIZED_RANKING
    --
    -- -   RELATED_ITEMS
    --
    -- -   USER_PERSONALIZATION
    recipeType :: Prelude.Maybe Prelude.Text,
    -- | The status of the recipe.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Recipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmArn', 'recipe_algorithmArn' - The Amazon Resource Name (ARN) of the algorithm that Amazon Personalize
-- uses to train the model.
--
-- 'creationDateTime', 'recipe_creationDateTime' - The date and time (in Unix format) that the recipe was created.
--
-- 'description', 'recipe_description' - The description of the recipe.
--
-- 'featureTransformationArn', 'recipe_featureTransformationArn' - The ARN of the FeatureTransformation object.
--
-- 'lastUpdatedDateTime', 'recipe_lastUpdatedDateTime' - The date and time (in Unix format) that the recipe was last updated.
--
-- 'name', 'recipe_name' - The name of the recipe.
--
-- 'recipeArn', 'recipe_recipeArn' - The Amazon Resource Name (ARN) of the recipe.
--
-- 'recipeType', 'recipe_recipeType' - One of the following values:
--
-- -   PERSONALIZED_RANKING
--
-- -   RELATED_ITEMS
--
-- -   USER_PERSONALIZATION
--
-- 'status', 'recipe_status' - The status of the recipe.
newRecipe ::
  Recipe
newRecipe =
  Recipe'
    { algorithmArn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      featureTransformationArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      recipeArn = Prelude.Nothing,
      recipeType = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the algorithm that Amazon Personalize
-- uses to train the model.
recipe_algorithmArn :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_algorithmArn = Lens.lens (\Recipe' {algorithmArn} -> algorithmArn) (\s@Recipe' {} a -> s {algorithmArn = a} :: Recipe)

-- | The date and time (in Unix format) that the recipe was created.
recipe_creationDateTime :: Lens.Lens' Recipe (Prelude.Maybe Prelude.UTCTime)
recipe_creationDateTime = Lens.lens (\Recipe' {creationDateTime} -> creationDateTime) (\s@Recipe' {} a -> s {creationDateTime = a} :: Recipe) Prelude.. Lens.mapping Data._Time

-- | The description of the recipe.
recipe_description :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_description = Lens.lens (\Recipe' {description} -> description) (\s@Recipe' {} a -> s {description = a} :: Recipe)

-- | The ARN of the FeatureTransformation object.
recipe_featureTransformationArn :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_featureTransformationArn = Lens.lens (\Recipe' {featureTransformationArn} -> featureTransformationArn) (\s@Recipe' {} a -> s {featureTransformationArn = a} :: Recipe)

-- | The date and time (in Unix format) that the recipe was last updated.
recipe_lastUpdatedDateTime :: Lens.Lens' Recipe (Prelude.Maybe Prelude.UTCTime)
recipe_lastUpdatedDateTime = Lens.lens (\Recipe' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@Recipe' {} a -> s {lastUpdatedDateTime = a} :: Recipe) Prelude.. Lens.mapping Data._Time

-- | The name of the recipe.
recipe_name :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_name = Lens.lens (\Recipe' {name} -> name) (\s@Recipe' {} a -> s {name = a} :: Recipe)

-- | The Amazon Resource Name (ARN) of the recipe.
recipe_recipeArn :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_recipeArn = Lens.lens (\Recipe' {recipeArn} -> recipeArn) (\s@Recipe' {} a -> s {recipeArn = a} :: Recipe)

-- | One of the following values:
--
-- -   PERSONALIZED_RANKING
--
-- -   RELATED_ITEMS
--
-- -   USER_PERSONALIZATION
recipe_recipeType :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_recipeType = Lens.lens (\Recipe' {recipeType} -> recipeType) (\s@Recipe' {} a -> s {recipeType = a} :: Recipe)

-- | The status of the recipe.
recipe_status :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_status = Lens.lens (\Recipe' {status} -> status) (\s@Recipe' {} a -> s {status = a} :: Recipe)

instance Data.FromJSON Recipe where
  parseJSON =
    Data.withObject
      "Recipe"
      ( \x ->
          Recipe'
            Prelude.<$> (x Data..:? "algorithmArn")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "featureTransformationArn")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "recipeArn")
            Prelude.<*> (x Data..:? "recipeType")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable Recipe where
  hashWithSalt _salt Recipe' {..} =
    _salt
      `Prelude.hashWithSalt` algorithmArn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` featureTransformationArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recipeArn
      `Prelude.hashWithSalt` recipeType
      `Prelude.hashWithSalt` status

instance Prelude.NFData Recipe where
  rnf Recipe' {..} =
    Prelude.rnf algorithmArn `Prelude.seq`
      Prelude.rnf creationDateTime `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf featureTransformationArn `Prelude.seq`
            Prelude.rnf lastUpdatedDateTime `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf recipeArn `Prelude.seq`
                  Prelude.rnf recipeType `Prelude.seq`
                    Prelude.rnf status
