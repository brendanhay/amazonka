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
-- Module      : Amazonka.Comprehend.Types.ExtractedCharactersListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.ExtractedCharactersListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Array of the number of characters extracted from each page.
--
-- /See:/ 'newExtractedCharactersListItem' smart constructor.
data ExtractedCharactersListItem = ExtractedCharactersListItem'
  { -- | Number of characters extracted from each page.
    count :: Prelude.Maybe Prelude.Int,
    -- | Page number.
    page :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtractedCharactersListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'extractedCharactersListItem_count' - Number of characters extracted from each page.
--
-- 'page', 'extractedCharactersListItem_page' - Page number.
newExtractedCharactersListItem ::
  ExtractedCharactersListItem
newExtractedCharactersListItem =
  ExtractedCharactersListItem'
    { count =
        Prelude.Nothing,
      page = Prelude.Nothing
    }

-- | Number of characters extracted from each page.
extractedCharactersListItem_count :: Lens.Lens' ExtractedCharactersListItem (Prelude.Maybe Prelude.Int)
extractedCharactersListItem_count = Lens.lens (\ExtractedCharactersListItem' {count} -> count) (\s@ExtractedCharactersListItem' {} a -> s {count = a} :: ExtractedCharactersListItem)

-- | Page number.
extractedCharactersListItem_page :: Lens.Lens' ExtractedCharactersListItem (Prelude.Maybe Prelude.Int)
extractedCharactersListItem_page = Lens.lens (\ExtractedCharactersListItem' {page} -> page) (\s@ExtractedCharactersListItem' {} a -> s {page = a} :: ExtractedCharactersListItem)

instance Data.FromJSON ExtractedCharactersListItem where
  parseJSON =
    Data.withObject
      "ExtractedCharactersListItem"
      ( \x ->
          ExtractedCharactersListItem'
            Prelude.<$> (x Data..:? "Count")
            Prelude.<*> (x Data..:? "Page")
      )

instance Prelude.Hashable ExtractedCharactersListItem where
  hashWithSalt _salt ExtractedCharactersListItem' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` page

instance Prelude.NFData ExtractedCharactersListItem where
  rnf ExtractedCharactersListItem' {..} =
    Prelude.rnf count `Prelude.seq` Prelude.rnf page
