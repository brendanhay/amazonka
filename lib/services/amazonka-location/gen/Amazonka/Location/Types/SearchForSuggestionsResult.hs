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
-- Module      : Amazonka.Location.Types.SearchForSuggestionsResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.SearchForSuggestionsResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a place suggestion resulting from a place suggestion query that
-- is run on a place index resource.
--
-- /See:/ 'newSearchForSuggestionsResult' smart constructor.
data SearchForSuggestionsResult = SearchForSuggestionsResult'
  { -- | The unique identifier of the place. You can use this with the @GetPlace@
    -- operation to find the place again later.
    --
    -- For @SearchPlaceIndexForSuggestions@ operations, the @PlaceId@ is
    -- returned by place indexes that use HERE or Esri as data providers.
    placeId :: Prelude.Maybe Prelude.Text,
    -- | The text of the place suggestion, typically formatted as an address
    -- string.
    text :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchForSuggestionsResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placeId', 'searchForSuggestionsResult_placeId' - The unique identifier of the place. You can use this with the @GetPlace@
-- operation to find the place again later.
--
-- For @SearchPlaceIndexForSuggestions@ operations, the @PlaceId@ is
-- returned by place indexes that use HERE or Esri as data providers.
--
-- 'text', 'searchForSuggestionsResult_text' - The text of the place suggestion, typically formatted as an address
-- string.
newSearchForSuggestionsResult ::
  -- | 'text'
  Prelude.Text ->
  SearchForSuggestionsResult
newSearchForSuggestionsResult pText_ =
  SearchForSuggestionsResult'
    { placeId =
        Prelude.Nothing,
      text = pText_
    }

-- | The unique identifier of the place. You can use this with the @GetPlace@
-- operation to find the place again later.
--
-- For @SearchPlaceIndexForSuggestions@ operations, the @PlaceId@ is
-- returned by place indexes that use HERE or Esri as data providers.
searchForSuggestionsResult_placeId :: Lens.Lens' SearchForSuggestionsResult (Prelude.Maybe Prelude.Text)
searchForSuggestionsResult_placeId = Lens.lens (\SearchForSuggestionsResult' {placeId} -> placeId) (\s@SearchForSuggestionsResult' {} a -> s {placeId = a} :: SearchForSuggestionsResult)

-- | The text of the place suggestion, typically formatted as an address
-- string.
searchForSuggestionsResult_text :: Lens.Lens' SearchForSuggestionsResult Prelude.Text
searchForSuggestionsResult_text = Lens.lens (\SearchForSuggestionsResult' {text} -> text) (\s@SearchForSuggestionsResult' {} a -> s {text = a} :: SearchForSuggestionsResult)

instance Data.FromJSON SearchForSuggestionsResult where
  parseJSON =
    Data.withObject
      "SearchForSuggestionsResult"
      ( \x ->
          SearchForSuggestionsResult'
            Prelude.<$> (x Data..:? "PlaceId")
            Prelude.<*> (x Data..: "Text")
      )

instance Prelude.Hashable SearchForSuggestionsResult where
  hashWithSalt _salt SearchForSuggestionsResult' {..} =
    _salt `Prelude.hashWithSalt` placeId
      `Prelude.hashWithSalt` text

instance Prelude.NFData SearchForSuggestionsResult where
  rnf SearchForSuggestionsResult' {..} =
    Prelude.rnf placeId `Prelude.seq` Prelude.rnf text
