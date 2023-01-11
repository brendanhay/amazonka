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
-- Module      : Amazonka.ConnectContactLens.Types.Categories
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectContactLens.Types.Categories where

import Amazonka.ConnectContactLens.Types.CategoryDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the category rules that are used to automatically categorize
-- contacts based on uttered keywords and phrases.
--
-- /See:/ 'newCategories' smart constructor.
data Categories = Categories'
  { -- | The category rules that have been matched in the analyzed segment.
    matchedCategories :: [Prelude.Text],
    -- | The category rule that was matched and when it occurred in the
    -- transcript.
    matchedDetails :: Prelude.HashMap Prelude.Text CategoryDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Categories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchedCategories', 'categories_matchedCategories' - The category rules that have been matched in the analyzed segment.
--
-- 'matchedDetails', 'categories_matchedDetails' - The category rule that was matched and when it occurred in the
-- transcript.
newCategories ::
  Categories
newCategories =
  Categories'
    { matchedCategories = Prelude.mempty,
      matchedDetails = Prelude.mempty
    }

-- | The category rules that have been matched in the analyzed segment.
categories_matchedCategories :: Lens.Lens' Categories [Prelude.Text]
categories_matchedCategories = Lens.lens (\Categories' {matchedCategories} -> matchedCategories) (\s@Categories' {} a -> s {matchedCategories = a} :: Categories) Prelude.. Lens.coerced

-- | The category rule that was matched and when it occurred in the
-- transcript.
categories_matchedDetails :: Lens.Lens' Categories (Prelude.HashMap Prelude.Text CategoryDetails)
categories_matchedDetails = Lens.lens (\Categories' {matchedDetails} -> matchedDetails) (\s@Categories' {} a -> s {matchedDetails = a} :: Categories) Prelude.. Lens.coerced

instance Data.FromJSON Categories where
  parseJSON =
    Data.withObject
      "Categories"
      ( \x ->
          Categories'
            Prelude.<$> ( x Data..:? "MatchedCategories"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "MatchedDetails"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Categories where
  hashWithSalt _salt Categories' {..} =
    _salt `Prelude.hashWithSalt` matchedCategories
      `Prelude.hashWithSalt` matchedDetails

instance Prelude.NFData Categories where
  rnf Categories' {..} =
    Prelude.rnf matchedCategories
      `Prelude.seq` Prelude.rnf matchedDetails
