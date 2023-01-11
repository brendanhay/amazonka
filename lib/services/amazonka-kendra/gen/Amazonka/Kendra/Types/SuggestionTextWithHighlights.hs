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
-- Module      : Amazonka.Kendra.Types.SuggestionTextWithHighlights
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SuggestionTextWithHighlights where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.SuggestionHighlight
import qualified Amazonka.Prelude as Prelude

-- | Provides text and information about where to highlight the query
-- suggestion text.
--
-- /See:/ 'newSuggestionTextWithHighlights' smart constructor.
data SuggestionTextWithHighlights = SuggestionTextWithHighlights'
  { -- | The beginning and end of the query suggestion text that should be
    -- highlighted.
    highlights :: Prelude.Maybe [SuggestionHighlight],
    -- | The query suggestion text to display to the user.
    text :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuggestionTextWithHighlights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'highlights', 'suggestionTextWithHighlights_highlights' - The beginning and end of the query suggestion text that should be
-- highlighted.
--
-- 'text', 'suggestionTextWithHighlights_text' - The query suggestion text to display to the user.
newSuggestionTextWithHighlights ::
  SuggestionTextWithHighlights
newSuggestionTextWithHighlights =
  SuggestionTextWithHighlights'
    { highlights =
        Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | The beginning and end of the query suggestion text that should be
-- highlighted.
suggestionTextWithHighlights_highlights :: Lens.Lens' SuggestionTextWithHighlights (Prelude.Maybe [SuggestionHighlight])
suggestionTextWithHighlights_highlights = Lens.lens (\SuggestionTextWithHighlights' {highlights} -> highlights) (\s@SuggestionTextWithHighlights' {} a -> s {highlights = a} :: SuggestionTextWithHighlights) Prelude.. Lens.mapping Lens.coerced

-- | The query suggestion text to display to the user.
suggestionTextWithHighlights_text :: Lens.Lens' SuggestionTextWithHighlights (Prelude.Maybe Prelude.Text)
suggestionTextWithHighlights_text = Lens.lens (\SuggestionTextWithHighlights' {text} -> text) (\s@SuggestionTextWithHighlights' {} a -> s {text = a} :: SuggestionTextWithHighlights)

instance Data.FromJSON SuggestionTextWithHighlights where
  parseJSON =
    Data.withObject
      "SuggestionTextWithHighlights"
      ( \x ->
          SuggestionTextWithHighlights'
            Prelude.<$> (x Data..:? "Highlights" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Text")
      )

instance
  Prelude.Hashable
    SuggestionTextWithHighlights
  where
  hashWithSalt _salt SuggestionTextWithHighlights' {..} =
    _salt `Prelude.hashWithSalt` highlights
      `Prelude.hashWithSalt` text

instance Prelude.NFData SuggestionTextWithHighlights where
  rnf SuggestionTextWithHighlights' {..} =
    Prelude.rnf highlights
      `Prelude.seq` Prelude.rnf text
