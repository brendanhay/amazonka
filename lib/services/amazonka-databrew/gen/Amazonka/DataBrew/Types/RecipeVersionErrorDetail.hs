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
-- Module      : Amazonka.DataBrew.Types.RecipeVersionErrorDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.RecipeVersionErrorDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents any errors encountered when attempting to delete multiple
-- recipe versions.
--
-- /See:/ 'newRecipeVersionErrorDetail' smart constructor.
data RecipeVersionErrorDetail = RecipeVersionErrorDetail'
  { -- | The HTTP status code for the error.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The text of the error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the recipe version associated with this error.
    recipeVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecipeVersionErrorDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'recipeVersionErrorDetail_errorCode' - The HTTP status code for the error.
--
-- 'errorMessage', 'recipeVersionErrorDetail_errorMessage' - The text of the error message.
--
-- 'recipeVersion', 'recipeVersionErrorDetail_recipeVersion' - The identifier for the recipe version associated with this error.
newRecipeVersionErrorDetail ::
  RecipeVersionErrorDetail
newRecipeVersionErrorDetail =
  RecipeVersionErrorDetail'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      recipeVersion = Prelude.Nothing
    }

-- | The HTTP status code for the error.
recipeVersionErrorDetail_errorCode :: Lens.Lens' RecipeVersionErrorDetail (Prelude.Maybe Prelude.Text)
recipeVersionErrorDetail_errorCode = Lens.lens (\RecipeVersionErrorDetail' {errorCode} -> errorCode) (\s@RecipeVersionErrorDetail' {} a -> s {errorCode = a} :: RecipeVersionErrorDetail)

-- | The text of the error message.
recipeVersionErrorDetail_errorMessage :: Lens.Lens' RecipeVersionErrorDetail (Prelude.Maybe Prelude.Text)
recipeVersionErrorDetail_errorMessage = Lens.lens (\RecipeVersionErrorDetail' {errorMessage} -> errorMessage) (\s@RecipeVersionErrorDetail' {} a -> s {errorMessage = a} :: RecipeVersionErrorDetail)

-- | The identifier for the recipe version associated with this error.
recipeVersionErrorDetail_recipeVersion :: Lens.Lens' RecipeVersionErrorDetail (Prelude.Maybe Prelude.Text)
recipeVersionErrorDetail_recipeVersion = Lens.lens (\RecipeVersionErrorDetail' {recipeVersion} -> recipeVersion) (\s@RecipeVersionErrorDetail' {} a -> s {recipeVersion = a} :: RecipeVersionErrorDetail)

instance Data.FromJSON RecipeVersionErrorDetail where
  parseJSON =
    Data.withObject
      "RecipeVersionErrorDetail"
      ( \x ->
          RecipeVersionErrorDetail'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "RecipeVersion")
      )

instance Prelude.Hashable RecipeVersionErrorDetail where
  hashWithSalt _salt RecipeVersionErrorDetail' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` recipeVersion

instance Prelude.NFData RecipeVersionErrorDetail where
  rnf RecipeVersionErrorDetail' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf recipeVersion
