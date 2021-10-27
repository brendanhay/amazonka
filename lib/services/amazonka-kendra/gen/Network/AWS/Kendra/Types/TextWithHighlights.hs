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
-- Module      : Network.AWS.Kendra.Types.TextWithHighlights
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.TextWithHighlights where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.Highlight
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides text and information about where to highlight the text.
--
-- /See:/ 'newTextWithHighlights' smart constructor.
data TextWithHighlights = TextWithHighlights'
  { -- | The text to display to the user.
    text :: Prelude.Maybe Prelude.Text,
    -- | The beginning and end of the text that should be highlighted.
    highlights :: Prelude.Maybe [Highlight]
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
-- 'text', 'textWithHighlights_text' - The text to display to the user.
--
-- 'highlights', 'textWithHighlights_highlights' - The beginning and end of the text that should be highlighted.
newTextWithHighlights ::
  TextWithHighlights
newTextWithHighlights =
  TextWithHighlights'
    { text = Prelude.Nothing,
      highlights = Prelude.Nothing
    }

-- | The text to display to the user.
textWithHighlights_text :: Lens.Lens' TextWithHighlights (Prelude.Maybe Prelude.Text)
textWithHighlights_text = Lens.lens (\TextWithHighlights' {text} -> text) (\s@TextWithHighlights' {} a -> s {text = a} :: TextWithHighlights)

-- | The beginning and end of the text that should be highlighted.
textWithHighlights_highlights :: Lens.Lens' TextWithHighlights (Prelude.Maybe [Highlight])
textWithHighlights_highlights = Lens.lens (\TextWithHighlights' {highlights} -> highlights) (\s@TextWithHighlights' {} a -> s {highlights = a} :: TextWithHighlights) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TextWithHighlights where
  parseJSON =
    Core.withObject
      "TextWithHighlights"
      ( \x ->
          TextWithHighlights'
            Prelude.<$> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "Highlights" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable TextWithHighlights

instance Prelude.NFData TextWithHighlights
