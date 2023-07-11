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
-- Module      : Amazonka.Personalize.Types.RecipeSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.RecipeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.Domain
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a recipe. For a complete
-- listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeRecipe.html DescribeRecipe>
-- API.
--
-- /See:/ 'newRecipeSummary' smart constructor.
data RecipeSummary = RecipeSummary'
  { -- | The date and time (in Unix time) that the recipe was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The domain of the recipe (if the recipe is a Domain dataset group use
    -- case).
    domain :: Prelude.Maybe Domain,
    -- | The date and time (in Unix time) that the recipe was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the recipe.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the recipe.
    recipeArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the recipe.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecipeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'recipeSummary_creationDateTime' - The date and time (in Unix time) that the recipe was created.
--
-- 'domain', 'recipeSummary_domain' - The domain of the recipe (if the recipe is a Domain dataset group use
-- case).
--
-- 'lastUpdatedDateTime', 'recipeSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the recipe was last updated.
--
-- 'name', 'recipeSummary_name' - The name of the recipe.
--
-- 'recipeArn', 'recipeSummary_recipeArn' - The Amazon Resource Name (ARN) of the recipe.
--
-- 'status', 'recipeSummary_status' - The status of the recipe.
newRecipeSummary ::
  RecipeSummary
newRecipeSummary =
  RecipeSummary'
    { creationDateTime = Prelude.Nothing,
      domain = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      recipeArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time (in Unix time) that the recipe was created.
recipeSummary_creationDateTime :: Lens.Lens' RecipeSummary (Prelude.Maybe Prelude.UTCTime)
recipeSummary_creationDateTime = Lens.lens (\RecipeSummary' {creationDateTime} -> creationDateTime) (\s@RecipeSummary' {} a -> s {creationDateTime = a} :: RecipeSummary) Prelude.. Lens.mapping Data._Time

-- | The domain of the recipe (if the recipe is a Domain dataset group use
-- case).
recipeSummary_domain :: Lens.Lens' RecipeSummary (Prelude.Maybe Domain)
recipeSummary_domain = Lens.lens (\RecipeSummary' {domain} -> domain) (\s@RecipeSummary' {} a -> s {domain = a} :: RecipeSummary)

-- | The date and time (in Unix time) that the recipe was last updated.
recipeSummary_lastUpdatedDateTime :: Lens.Lens' RecipeSummary (Prelude.Maybe Prelude.UTCTime)
recipeSummary_lastUpdatedDateTime = Lens.lens (\RecipeSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@RecipeSummary' {} a -> s {lastUpdatedDateTime = a} :: RecipeSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the recipe.
recipeSummary_name :: Lens.Lens' RecipeSummary (Prelude.Maybe Prelude.Text)
recipeSummary_name = Lens.lens (\RecipeSummary' {name} -> name) (\s@RecipeSummary' {} a -> s {name = a} :: RecipeSummary)

-- | The Amazon Resource Name (ARN) of the recipe.
recipeSummary_recipeArn :: Lens.Lens' RecipeSummary (Prelude.Maybe Prelude.Text)
recipeSummary_recipeArn = Lens.lens (\RecipeSummary' {recipeArn} -> recipeArn) (\s@RecipeSummary' {} a -> s {recipeArn = a} :: RecipeSummary)

-- | The status of the recipe.
recipeSummary_status :: Lens.Lens' RecipeSummary (Prelude.Maybe Prelude.Text)
recipeSummary_status = Lens.lens (\RecipeSummary' {status} -> status) (\s@RecipeSummary' {} a -> s {status = a} :: RecipeSummary)

instance Data.FromJSON RecipeSummary where
  parseJSON =
    Data.withObject
      "RecipeSummary"
      ( \x ->
          RecipeSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "domain")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "recipeArn")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable RecipeSummary where
  hashWithSalt _salt RecipeSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recipeArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData RecipeSummary where
  rnf RecipeSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recipeArn
      `Prelude.seq` Prelude.rnf status
