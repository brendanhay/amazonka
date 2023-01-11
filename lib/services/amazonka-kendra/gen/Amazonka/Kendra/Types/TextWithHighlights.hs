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
-- Module      : Amazonka.Kendra.Types.TextWithHighlights
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.TextWithHighlights where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.Highlight
import qualified Amazonka.Prelude as Prelude

-- | Provides text and information about where to highlight the text.
--
-- /See:/ 'newTextWithHighlights' smart constructor.
data TextWithHighlights = TextWithHighlights'
  { -- | The beginning and end of the text that should be highlighted.
    highlights :: Prelude.Maybe [Highlight],
    -- | The text to display to the user.
    text :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextWithHighlights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'highlights', 'textWithHighlights_highlights' - The beginning and end of the text that should be highlighted.
--
-- 'text', 'textWithHighlights_text' - The text to display to the user.
newTextWithHighlights ::
  TextWithHighlights
newTextWithHighlights =
  TextWithHighlights'
    { highlights = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | The beginning and end of the text that should be highlighted.
textWithHighlights_highlights :: Lens.Lens' TextWithHighlights (Prelude.Maybe [Highlight])
textWithHighlights_highlights = Lens.lens (\TextWithHighlights' {highlights} -> highlights) (\s@TextWithHighlights' {} a -> s {highlights = a} :: TextWithHighlights) Prelude.. Lens.mapping Lens.coerced

-- | The text to display to the user.
textWithHighlights_text :: Lens.Lens' TextWithHighlights (Prelude.Maybe Prelude.Text)
textWithHighlights_text = Lens.lens (\TextWithHighlights' {text} -> text) (\s@TextWithHighlights' {} a -> s {text = a} :: TextWithHighlights)

instance Data.FromJSON TextWithHighlights where
  parseJSON =
    Data.withObject
      "TextWithHighlights"
      ( \x ->
          TextWithHighlights'
            Prelude.<$> (x Data..:? "Highlights" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Text")
      )

instance Prelude.Hashable TextWithHighlights where
  hashWithSalt _salt TextWithHighlights' {..} =
    _salt `Prelude.hashWithSalt` highlights
      `Prelude.hashWithSalt` text

instance Prelude.NFData TextWithHighlights where
  rnf TextWithHighlights' {..} =
    Prelude.rnf highlights
      `Prelude.seq` Prelude.rnf text
