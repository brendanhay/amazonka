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
-- Module      : Network.AWS.Kendra.Types.SuggestionValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.SuggestionValue where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.SuggestionTextWithHighlights
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON SuggestionValue where
  parseJSON =
    Core.withObject
      "SuggestionValue"
      ( \x ->
          SuggestionValue' Prelude.<$> (x Core..:? "Text")
      )

instance Prelude.Hashable SuggestionValue

instance Prelude.NFData SuggestionValue
