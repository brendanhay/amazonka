{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestionMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SuggestionMatch where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An autocomplete suggestion that matches the query string specified in a
-- @SuggestRequest@.
--
-- /See:/ 'newSuggestionMatch' smart constructor.
data SuggestionMatch = SuggestionMatch'
  { -- | The string that matches the query string specified in the
    -- @SuggestRequest@.
    suggestion :: Prelude.Maybe Prelude.Text,
    -- | The document ID of the suggested document.
    id :: Prelude.Maybe Prelude.Text,
    -- | The relevance score of a suggested match.
    score :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SuggestionMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suggestion', 'suggestionMatch_suggestion' - The string that matches the query string specified in the
-- @SuggestRequest@.
--
-- 'id', 'suggestionMatch_id' - The document ID of the suggested document.
--
-- 'score', 'suggestionMatch_score' - The relevance score of a suggested match.
newSuggestionMatch ::
  SuggestionMatch
newSuggestionMatch =
  SuggestionMatch'
    { suggestion = Prelude.Nothing,
      id = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The string that matches the query string specified in the
-- @SuggestRequest@.
suggestionMatch_suggestion :: Lens.Lens' SuggestionMatch (Prelude.Maybe Prelude.Text)
suggestionMatch_suggestion = Lens.lens (\SuggestionMatch' {suggestion} -> suggestion) (\s@SuggestionMatch' {} a -> s {suggestion = a} :: SuggestionMatch)

-- | The document ID of the suggested document.
suggestionMatch_id :: Lens.Lens' SuggestionMatch (Prelude.Maybe Prelude.Text)
suggestionMatch_id = Lens.lens (\SuggestionMatch' {id} -> id) (\s@SuggestionMatch' {} a -> s {id = a} :: SuggestionMatch)

-- | The relevance score of a suggested match.
suggestionMatch_score :: Lens.Lens' SuggestionMatch (Prelude.Maybe Prelude.Integer)
suggestionMatch_score = Lens.lens (\SuggestionMatch' {score} -> score) (\s@SuggestionMatch' {} a -> s {score = a} :: SuggestionMatch)

instance Prelude.FromJSON SuggestionMatch where
  parseJSON =
    Prelude.withObject
      "SuggestionMatch"
      ( \x ->
          SuggestionMatch'
            Prelude.<$> (x Prelude..:? "suggestion")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "score")
      )

instance Prelude.Hashable SuggestionMatch

instance Prelude.NFData SuggestionMatch
