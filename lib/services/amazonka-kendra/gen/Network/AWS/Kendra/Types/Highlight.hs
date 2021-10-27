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
-- Module      : Network.AWS.Kendra.Types.Highlight
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.Highlight where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.HighlightType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that you can use to highlight a search result so
-- that your users can quickly identify terms in the response.
--
-- /See:/ 'newHighlight' smart constructor.
data Highlight = Highlight'
  { -- | Indicates whether the response is the best response. True if this is the
    -- best response; otherwise, false.
    topAnswer :: Prelude.Maybe Prelude.Bool,
    -- | The highlight type.
    type' :: Prelude.Maybe HighlightType,
    -- | The zero-based location in the response string where the highlight
    -- starts.
    beginOffset :: Prelude.Int,
    -- | The zero-based location in the response string where the highlight ends.
    endOffset :: Prelude.Int
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
-- 'topAnswer', 'highlight_topAnswer' - Indicates whether the response is the best response. True if this is the
-- best response; otherwise, false.
--
-- 'type'', 'highlight_type' - The highlight type.
--
-- 'beginOffset', 'highlight_beginOffset' - The zero-based location in the response string where the highlight
-- starts.
--
-- 'endOffset', 'highlight_endOffset' - The zero-based location in the response string where the highlight ends.
newHighlight ::
  -- | 'beginOffset'
  Prelude.Int ->
  -- | 'endOffset'
  Prelude.Int ->
  Highlight
newHighlight pBeginOffset_ pEndOffset_ =
  Highlight'
    { topAnswer = Prelude.Nothing,
      type' = Prelude.Nothing,
      beginOffset = pBeginOffset_,
      endOffset = pEndOffset_
    }

-- | Indicates whether the response is the best response. True if this is the
-- best response; otherwise, false.
highlight_topAnswer :: Lens.Lens' Highlight (Prelude.Maybe Prelude.Bool)
highlight_topAnswer = Lens.lens (\Highlight' {topAnswer} -> topAnswer) (\s@Highlight' {} a -> s {topAnswer = a} :: Highlight)

-- | The highlight type.
highlight_type :: Lens.Lens' Highlight (Prelude.Maybe HighlightType)
highlight_type = Lens.lens (\Highlight' {type'} -> type') (\s@Highlight' {} a -> s {type' = a} :: Highlight)

-- | The zero-based location in the response string where the highlight
-- starts.
highlight_beginOffset :: Lens.Lens' Highlight Prelude.Int
highlight_beginOffset = Lens.lens (\Highlight' {beginOffset} -> beginOffset) (\s@Highlight' {} a -> s {beginOffset = a} :: Highlight)

-- | The zero-based location in the response string where the highlight ends.
highlight_endOffset :: Lens.Lens' Highlight Prelude.Int
highlight_endOffset = Lens.lens (\Highlight' {endOffset} -> endOffset) (\s@Highlight' {} a -> s {endOffset = a} :: Highlight)

instance Core.FromJSON Highlight where
  parseJSON =
    Core.withObject
      "Highlight"
      ( \x ->
          Highlight'
            Prelude.<$> (x Core..:? "TopAnswer")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..: "BeginOffset")
            Prelude.<*> (x Core..: "EndOffset")
      )

instance Prelude.Hashable Highlight

instance Prelude.NFData Highlight
