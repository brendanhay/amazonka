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
-- Module      : Network.AWS.Kendra.Types.SuggestionTextWithHighlights
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.SuggestionTextWithHighlights where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.SuggestionHighlight
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides text and information about where to highlight the query
-- suggestion text.
--
-- /See:/ 'newSuggestionTextWithHighlights' smart constructor.
data SuggestionTextWithHighlights = SuggestionTextWithHighlights'
  { -- | The query suggestion text to display to the user.
    text :: Prelude.Maybe Prelude.Text,
    -- | The beginning and end of the query suggestion text that should be
    -- highlighted.
    highlights :: Prelude.Maybe [SuggestionHighlight]
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
-- 'text', 'suggestionTextWithHighlights_text' - The query suggestion text to display to the user.
--
-- 'highlights', 'suggestionTextWithHighlights_highlights' - The beginning and end of the query suggestion text that should be
-- highlighted.
newSuggestionTextWithHighlights ::
  SuggestionTextWithHighlights
newSuggestionTextWithHighlights =
  SuggestionTextWithHighlights'
    { text =
        Prelude.Nothing,
      highlights = Prelude.Nothing
    }

-- | The query suggestion text to display to the user.
suggestionTextWithHighlights_text :: Lens.Lens' SuggestionTextWithHighlights (Prelude.Maybe Prelude.Text)
suggestionTextWithHighlights_text = Lens.lens (\SuggestionTextWithHighlights' {text} -> text) (\s@SuggestionTextWithHighlights' {} a -> s {text = a} :: SuggestionTextWithHighlights)

-- | The beginning and end of the query suggestion text that should be
-- highlighted.
suggestionTextWithHighlights_highlights :: Lens.Lens' SuggestionTextWithHighlights (Prelude.Maybe [SuggestionHighlight])
suggestionTextWithHighlights_highlights = Lens.lens (\SuggestionTextWithHighlights' {highlights} -> highlights) (\s@SuggestionTextWithHighlights' {} a -> s {highlights = a} :: SuggestionTextWithHighlights) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SuggestionTextWithHighlights where
  parseJSON =
    Core.withObject
      "SuggestionTextWithHighlights"
      ( \x ->
          SuggestionTextWithHighlights'
            Prelude.<$> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "Highlights" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    SuggestionTextWithHighlights

instance Prelude.NFData SuggestionTextWithHighlights
