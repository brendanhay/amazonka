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
-- Module      : Amazonka.DataBrew.Types.Recipe
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.Recipe where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.RecipeStep
import qualified Amazonka.Prelude as Prelude

-- | Represents one or more actions to be performed on a DataBrew dataset.
--
-- /See:/ 'newRecipe' smart constructor.
data Recipe = Recipe'
  { -- | Metadata tags that have been applied to the recipe.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the user who published the recipe.
    publishedBy :: Prelude.Maybe Prelude.Text,
    -- | The last modification date and time of the recipe.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | A list of steps that are defined by the recipe.
    steps :: Prelude.Maybe [RecipeStep],
    -- | The description of the recipe.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the recipe was created.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the recipe was published.
    publishedDate :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the user who last modified the recipe.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the recipe.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the project that the recipe is associated with.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user who created the recipe.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the version for the recipe. Must be one of the
    -- following:
    --
    -- -   Numeric version (@X.Y@) - @X@ and @Y@ stand for major and minor
    --     version numbers. The maximum length of each is 6 digits, and neither
    --     can be negative values. Both @X@ and @Y@ are required, and \"0.0\"
    --     isn\'t a valid version.
    --
    -- -   @LATEST_WORKING@ - the most recent valid version being developed in
    --     a DataBrew project.
    --
    -- -   @LATEST_PUBLISHED@ - the most recent published version.
    recipeVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique name for the recipe.
    name :: Prelude.Text
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
-- 'tags', 'recipe_tags' - Metadata tags that have been applied to the recipe.
--
-- 'publishedBy', 'recipe_publishedBy' - The Amazon Resource Name (ARN) of the user who published the recipe.
--
-- 'lastModifiedDate', 'recipe_lastModifiedDate' - The last modification date and time of the recipe.
--
-- 'steps', 'recipe_steps' - A list of steps that are defined by the recipe.
--
-- 'description', 'recipe_description' - The description of the recipe.
--
-- 'createDate', 'recipe_createDate' - The date and time that the recipe was created.
--
-- 'publishedDate', 'recipe_publishedDate' - The date and time when the recipe was published.
--
-- 'lastModifiedBy', 'recipe_lastModifiedBy' - The Amazon Resource Name (ARN) of the user who last modified the recipe.
--
-- 'resourceArn', 'recipe_resourceArn' - The Amazon Resource Name (ARN) for the recipe.
--
-- 'projectName', 'recipe_projectName' - The name of the project that the recipe is associated with.
--
-- 'createdBy', 'recipe_createdBy' - The Amazon Resource Name (ARN) of the user who created the recipe.
--
-- 'recipeVersion', 'recipe_recipeVersion' - The identifier for the version for the recipe. Must be one of the
-- following:
--
-- -   Numeric version (@X.Y@) - @X@ and @Y@ stand for major and minor
--     version numbers. The maximum length of each is 6 digits, and neither
--     can be negative values. Both @X@ and @Y@ are required, and \"0.0\"
--     isn\'t a valid version.
--
-- -   @LATEST_WORKING@ - the most recent valid version being developed in
--     a DataBrew project.
--
-- -   @LATEST_PUBLISHED@ - the most recent published version.
--
-- 'name', 'recipe_name' - The unique name for the recipe.
newRecipe ::
  -- | 'name'
  Prelude.Text ->
  Recipe
newRecipe pName_ =
  Recipe'
    { tags = Prelude.Nothing,
      publishedBy = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      steps = Prelude.Nothing,
      description = Prelude.Nothing,
      createDate = Prelude.Nothing,
      publishedDate = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      projectName = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      recipeVersion = Prelude.Nothing,
      name = pName_
    }

-- | Metadata tags that have been applied to the recipe.
recipe_tags :: Lens.Lens' Recipe (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recipe_tags = Lens.lens (\Recipe' {tags} -> tags) (\s@Recipe' {} a -> s {tags = a} :: Recipe) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the user who published the recipe.
recipe_publishedBy :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_publishedBy = Lens.lens (\Recipe' {publishedBy} -> publishedBy) (\s@Recipe' {} a -> s {publishedBy = a} :: Recipe)

-- | The last modification date and time of the recipe.
recipe_lastModifiedDate :: Lens.Lens' Recipe (Prelude.Maybe Prelude.UTCTime)
recipe_lastModifiedDate = Lens.lens (\Recipe' {lastModifiedDate} -> lastModifiedDate) (\s@Recipe' {} a -> s {lastModifiedDate = a} :: Recipe) Prelude.. Lens.mapping Data._Time

-- | A list of steps that are defined by the recipe.
recipe_steps :: Lens.Lens' Recipe (Prelude.Maybe [RecipeStep])
recipe_steps = Lens.lens (\Recipe' {steps} -> steps) (\s@Recipe' {} a -> s {steps = a} :: Recipe) Prelude.. Lens.mapping Lens.coerced

-- | The description of the recipe.
recipe_description :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_description = Lens.lens (\Recipe' {description} -> description) (\s@Recipe' {} a -> s {description = a} :: Recipe)

-- | The date and time that the recipe was created.
recipe_createDate :: Lens.Lens' Recipe (Prelude.Maybe Prelude.UTCTime)
recipe_createDate = Lens.lens (\Recipe' {createDate} -> createDate) (\s@Recipe' {} a -> s {createDate = a} :: Recipe) Prelude.. Lens.mapping Data._Time

-- | The date and time when the recipe was published.
recipe_publishedDate :: Lens.Lens' Recipe (Prelude.Maybe Prelude.UTCTime)
recipe_publishedDate = Lens.lens (\Recipe' {publishedDate} -> publishedDate) (\s@Recipe' {} a -> s {publishedDate = a} :: Recipe) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the user who last modified the recipe.
recipe_lastModifiedBy :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_lastModifiedBy = Lens.lens (\Recipe' {lastModifiedBy} -> lastModifiedBy) (\s@Recipe' {} a -> s {lastModifiedBy = a} :: Recipe)

-- | The Amazon Resource Name (ARN) for the recipe.
recipe_resourceArn :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_resourceArn = Lens.lens (\Recipe' {resourceArn} -> resourceArn) (\s@Recipe' {} a -> s {resourceArn = a} :: Recipe)

-- | The name of the project that the recipe is associated with.
recipe_projectName :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_projectName = Lens.lens (\Recipe' {projectName} -> projectName) (\s@Recipe' {} a -> s {projectName = a} :: Recipe)

-- | The Amazon Resource Name (ARN) of the user who created the recipe.
recipe_createdBy :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_createdBy = Lens.lens (\Recipe' {createdBy} -> createdBy) (\s@Recipe' {} a -> s {createdBy = a} :: Recipe)

-- | The identifier for the version for the recipe. Must be one of the
-- following:
--
-- -   Numeric version (@X.Y@) - @X@ and @Y@ stand for major and minor
--     version numbers. The maximum length of each is 6 digits, and neither
--     can be negative values. Both @X@ and @Y@ are required, and \"0.0\"
--     isn\'t a valid version.
--
-- -   @LATEST_WORKING@ - the most recent valid version being developed in
--     a DataBrew project.
--
-- -   @LATEST_PUBLISHED@ - the most recent published version.
recipe_recipeVersion :: Lens.Lens' Recipe (Prelude.Maybe Prelude.Text)
recipe_recipeVersion = Lens.lens (\Recipe' {recipeVersion} -> recipeVersion) (\s@Recipe' {} a -> s {recipeVersion = a} :: Recipe)

-- | The unique name for the recipe.
recipe_name :: Lens.Lens' Recipe Prelude.Text
recipe_name = Lens.lens (\Recipe' {name} -> name) (\s@Recipe' {} a -> s {name = a} :: Recipe)

instance Data.FromJSON Recipe where
  parseJSON =
    Data.withObject
      "Recipe"
      ( \x ->
          Recipe'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PublishedBy")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "Steps" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "PublishedDate")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "ProjectName")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "RecipeVersion")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Recipe where
  hashWithSalt _salt Recipe' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` publishedBy
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` steps
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` publishedDate
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` recipeVersion
      `Prelude.hashWithSalt` name

instance Prelude.NFData Recipe where
  rnf Recipe' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf publishedBy
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf steps
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf publishedDate
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf recipeVersion
      `Prelude.seq` Prelude.rnf name
