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
-- Module      : Amazonka.Wisdom.Types.Highlight
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.Highlight where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Offset specification to describe highlighting of document excerpts for
-- rendering search results and recommendations.
--
-- /See:/ 'newHighlight' smart constructor.
data Highlight = Highlight'
  { -- | The offset for the start of the highlight.
    beginOffsetInclusive :: Prelude.Maybe Prelude.Int,
    -- | The offset for the end of the highlight.
    endOffsetExclusive :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Highlight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffsetInclusive', 'highlight_beginOffsetInclusive' - The offset for the start of the highlight.
--
-- 'endOffsetExclusive', 'highlight_endOffsetExclusive' - The offset for the end of the highlight.
newHighlight ::
  Highlight
newHighlight =
  Highlight'
    { beginOffsetInclusive = Prelude.Nothing,
      endOffsetExclusive = Prelude.Nothing
    }

-- | The offset for the start of the highlight.
highlight_beginOffsetInclusive :: Lens.Lens' Highlight (Prelude.Maybe Prelude.Int)
highlight_beginOffsetInclusive = Lens.lens (\Highlight' {beginOffsetInclusive} -> beginOffsetInclusive) (\s@Highlight' {} a -> s {beginOffsetInclusive = a} :: Highlight)

-- | The offset for the end of the highlight.
highlight_endOffsetExclusive :: Lens.Lens' Highlight (Prelude.Maybe Prelude.Int)
highlight_endOffsetExclusive = Lens.lens (\Highlight' {endOffsetExclusive} -> endOffsetExclusive) (\s@Highlight' {} a -> s {endOffsetExclusive = a} :: Highlight)

instance Data.FromJSON Highlight where
  parseJSON =
    Data.withObject
      "Highlight"
      ( \x ->
          Highlight'
            Prelude.<$> (x Data..:? "beginOffsetInclusive")
            Prelude.<*> (x Data..:? "endOffsetExclusive")
      )

instance Prelude.Hashable Highlight where
  hashWithSalt _salt Highlight' {..} =
    _salt `Prelude.hashWithSalt` beginOffsetInclusive
      `Prelude.hashWithSalt` endOffsetExclusive

instance Prelude.NFData Highlight where
  rnf Highlight' {..} =
    Prelude.rnf beginOffsetInclusive
      `Prelude.seq` Prelude.rnf endOffsetExclusive
