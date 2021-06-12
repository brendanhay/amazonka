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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An autocomplete suggestion that matches the query string specified in a
-- @SuggestRequest@.
--
-- /See:/ 'newSuggestionMatch' smart constructor.
data SuggestionMatch = SuggestionMatch'
  { -- | The string that matches the query string specified in the
    -- @SuggestRequest@.
    suggestion :: Core.Maybe Core.Text,
    -- | The document ID of the suggested document.
    id :: Core.Maybe Core.Text,
    -- | The relevance score of a suggested match.
    score :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { suggestion = Core.Nothing,
      id = Core.Nothing,
      score = Core.Nothing
    }

-- | The string that matches the query string specified in the
-- @SuggestRequest@.
suggestionMatch_suggestion :: Lens.Lens' SuggestionMatch (Core.Maybe Core.Text)
suggestionMatch_suggestion = Lens.lens (\SuggestionMatch' {suggestion} -> suggestion) (\s@SuggestionMatch' {} a -> s {suggestion = a} :: SuggestionMatch)

-- | The document ID of the suggested document.
suggestionMatch_id :: Lens.Lens' SuggestionMatch (Core.Maybe Core.Text)
suggestionMatch_id = Lens.lens (\SuggestionMatch' {id} -> id) (\s@SuggestionMatch' {} a -> s {id = a} :: SuggestionMatch)

-- | The relevance score of a suggested match.
suggestionMatch_score :: Lens.Lens' SuggestionMatch (Core.Maybe Core.Integer)
suggestionMatch_score = Lens.lens (\SuggestionMatch' {score} -> score) (\s@SuggestionMatch' {} a -> s {score = a} :: SuggestionMatch)

instance Core.FromJSON SuggestionMatch where
  parseJSON =
    Core.withObject
      "SuggestionMatch"
      ( \x ->
          SuggestionMatch'
            Core.<$> (x Core..:? "suggestion")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "score")
      )

instance Core.Hashable SuggestionMatch

instance Core.NFData SuggestionMatch
