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
-- Module      : Amazonka.Kendra.Types.SuggestionHighlight
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SuggestionHighlight where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The text highlights for a single query suggestion.
--
-- /See:/ 'newSuggestionHighlight' smart constructor.
data SuggestionHighlight = SuggestionHighlight'
  { -- | The zero-based location in the response string where the highlight
    -- starts.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The zero-based location in the response string where the highlight ends.
    endOffset :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuggestionHighlight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffset', 'suggestionHighlight_beginOffset' - The zero-based location in the response string where the highlight
-- starts.
--
-- 'endOffset', 'suggestionHighlight_endOffset' - The zero-based location in the response string where the highlight ends.
newSuggestionHighlight ::
  SuggestionHighlight
newSuggestionHighlight =
  SuggestionHighlight'
    { beginOffset = Prelude.Nothing,
      endOffset = Prelude.Nothing
    }

-- | The zero-based location in the response string where the highlight
-- starts.
suggestionHighlight_beginOffset :: Lens.Lens' SuggestionHighlight (Prelude.Maybe Prelude.Int)
suggestionHighlight_beginOffset = Lens.lens (\SuggestionHighlight' {beginOffset} -> beginOffset) (\s@SuggestionHighlight' {} a -> s {beginOffset = a} :: SuggestionHighlight)

-- | The zero-based location in the response string where the highlight ends.
suggestionHighlight_endOffset :: Lens.Lens' SuggestionHighlight (Prelude.Maybe Prelude.Int)
suggestionHighlight_endOffset = Lens.lens (\SuggestionHighlight' {endOffset} -> endOffset) (\s@SuggestionHighlight' {} a -> s {endOffset = a} :: SuggestionHighlight)

instance Data.FromJSON SuggestionHighlight where
  parseJSON =
    Data.withObject
      "SuggestionHighlight"
      ( \x ->
          SuggestionHighlight'
            Prelude.<$> (x Data..:? "BeginOffset")
            Prelude.<*> (x Data..:? "EndOffset")
      )

instance Prelude.Hashable SuggestionHighlight where
  hashWithSalt _salt SuggestionHighlight' {..} =
    _salt
      `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` endOffset

instance Prelude.NFData SuggestionHighlight where
  rnf SuggestionHighlight' {..} =
    Prelude.rnf beginOffset `Prelude.seq`
      Prelude.rnf endOffset
