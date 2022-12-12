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
-- Module      : Amazonka.CloudSearchDomains.Types.SuggestionMatch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearchDomains.Types.SuggestionMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An autocomplete suggestion that matches the query string specified in a
-- @SuggestRequest@.
--
-- /See:/ 'newSuggestionMatch' smart constructor.
data SuggestionMatch = SuggestionMatch'
  { -- | The document ID of the suggested document.
    id :: Prelude.Maybe Prelude.Text,
    -- | The relevance score of a suggested match.
    score :: Prelude.Maybe Prelude.Integer,
    -- | The string that matches the query string specified in the
    -- @SuggestRequest@.
    suggestion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuggestionMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'suggestionMatch_id' - The document ID of the suggested document.
--
-- 'score', 'suggestionMatch_score' - The relevance score of a suggested match.
--
-- 'suggestion', 'suggestionMatch_suggestion' - The string that matches the query string specified in the
-- @SuggestRequest@.
newSuggestionMatch ::
  SuggestionMatch
newSuggestionMatch =
  SuggestionMatch'
    { id = Prelude.Nothing,
      score = Prelude.Nothing,
      suggestion = Prelude.Nothing
    }

-- | The document ID of the suggested document.
suggestionMatch_id :: Lens.Lens' SuggestionMatch (Prelude.Maybe Prelude.Text)
suggestionMatch_id = Lens.lens (\SuggestionMatch' {id} -> id) (\s@SuggestionMatch' {} a -> s {id = a} :: SuggestionMatch)

-- | The relevance score of a suggested match.
suggestionMatch_score :: Lens.Lens' SuggestionMatch (Prelude.Maybe Prelude.Integer)
suggestionMatch_score = Lens.lens (\SuggestionMatch' {score} -> score) (\s@SuggestionMatch' {} a -> s {score = a} :: SuggestionMatch)

-- | The string that matches the query string specified in the
-- @SuggestRequest@.
suggestionMatch_suggestion :: Lens.Lens' SuggestionMatch (Prelude.Maybe Prelude.Text)
suggestionMatch_suggestion = Lens.lens (\SuggestionMatch' {suggestion} -> suggestion) (\s@SuggestionMatch' {} a -> s {suggestion = a} :: SuggestionMatch)

instance Data.FromJSON SuggestionMatch where
  parseJSON =
    Data.withObject
      "SuggestionMatch"
      ( \x ->
          SuggestionMatch'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "score")
            Prelude.<*> (x Data..:? "suggestion")
      )

instance Prelude.Hashable SuggestionMatch where
  hashWithSalt _salt SuggestionMatch' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` suggestion

instance Prelude.NFData SuggestionMatch where
  rnf SuggestionMatch' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf suggestion
