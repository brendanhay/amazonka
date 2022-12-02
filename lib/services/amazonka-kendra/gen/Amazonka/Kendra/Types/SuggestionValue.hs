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
-- Module      : Amazonka.Kendra.Types.SuggestionValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SuggestionValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.SuggestionTextWithHighlights
import qualified Amazonka.Prelude as Prelude

-- | The @SuggestionTextWithHighlights@ structure information.
--
-- /See:/ 'newSuggestionValue' smart constructor.
data SuggestionValue = SuggestionValue'
  { -- | The @SuggestionTextWithHighlights@ structure that contains the query
    -- suggestion text and highlights.
    text :: Prelude.Maybe SuggestionTextWithHighlights
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuggestionValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'suggestionValue_text' - The @SuggestionTextWithHighlights@ structure that contains the query
-- suggestion text and highlights.
newSuggestionValue ::
  SuggestionValue
newSuggestionValue =
  SuggestionValue' {text = Prelude.Nothing}

-- | The @SuggestionTextWithHighlights@ structure that contains the query
-- suggestion text and highlights.
suggestionValue_text :: Lens.Lens' SuggestionValue (Prelude.Maybe SuggestionTextWithHighlights)
suggestionValue_text = Lens.lens (\SuggestionValue' {text} -> text) (\s@SuggestionValue' {} a -> s {text = a} :: SuggestionValue)

instance Data.FromJSON SuggestionValue where
  parseJSON =
    Data.withObject
      "SuggestionValue"
      ( \x ->
          SuggestionValue' Prelude.<$> (x Data..:? "Text")
      )

instance Prelude.Hashable SuggestionValue where
  hashWithSalt _salt SuggestionValue' {..} =
    _salt `Prelude.hashWithSalt` text

instance Prelude.NFData SuggestionValue where
  rnf SuggestionValue' {..} = Prelude.rnf text
