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
-- Module      : Network.AWS.Kendra.Types.Suggestion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.Suggestion where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.SuggestionValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A single query suggestion.
--
-- /See:/ 'newSuggestion' smart constructor.
data Suggestion = Suggestion'
  { -- | The value for the unique UUID (universally unique identifier) of a
    -- single query suggestion.
    --
    -- The value is the text string of a suggestion.
    value :: Prelude.Maybe SuggestionValue,
    -- | The unique UUID (universally unique identifier) of a single query
    -- suggestion.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Suggestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'suggestion_value' - The value for the unique UUID (universally unique identifier) of a
-- single query suggestion.
--
-- The value is the text string of a suggestion.
--
-- 'id', 'suggestion_id' - The unique UUID (universally unique identifier) of a single query
-- suggestion.
newSuggestion ::
  Suggestion
newSuggestion =
  Suggestion'
    { value = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The value for the unique UUID (universally unique identifier) of a
-- single query suggestion.
--
-- The value is the text string of a suggestion.
suggestion_value :: Lens.Lens' Suggestion (Prelude.Maybe SuggestionValue)
suggestion_value = Lens.lens (\Suggestion' {value} -> value) (\s@Suggestion' {} a -> s {value = a} :: Suggestion)

-- | The unique UUID (universally unique identifier) of a single query
-- suggestion.
suggestion_id :: Lens.Lens' Suggestion (Prelude.Maybe Prelude.Text)
suggestion_id = Lens.lens (\Suggestion' {id} -> id) (\s@Suggestion' {} a -> s {id = a} :: Suggestion)

instance Core.FromJSON Suggestion where
  parseJSON =
    Core.withObject
      "Suggestion"
      ( \x ->
          Suggestion'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable Suggestion

instance Prelude.NFData Suggestion
