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
-- Module      : Amazonka.Kendra.Types.Suggestion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.Suggestion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.SourceDocument
import Amazonka.Kendra.Types.SuggestionValue
import qualified Amazonka.Prelude as Prelude

-- | A single query suggestion.
--
-- /See:/ 'newSuggestion' smart constructor.
data Suggestion = Suggestion'
  { -- | The UUID (universally unique identifier) of a single query suggestion.
    id :: Prelude.Maybe Prelude.Text,
    -- | The list of document IDs and their fields\/attributes that are used for
    -- a single query suggestion, if document fields set to use for query
    -- suggestions.
    sourceDocuments :: Prelude.Maybe [SourceDocument],
    -- | The value for the UUID (universally unique identifier) of a single query
    -- suggestion.
    --
    -- The value is the text string of a suggestion.
    value :: Prelude.Maybe SuggestionValue
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
-- 'id', 'suggestion_id' - The UUID (universally unique identifier) of a single query suggestion.
--
-- 'sourceDocuments', 'suggestion_sourceDocuments' - The list of document IDs and their fields\/attributes that are used for
-- a single query suggestion, if document fields set to use for query
-- suggestions.
--
-- 'value', 'suggestion_value' - The value for the UUID (universally unique identifier) of a single query
-- suggestion.
--
-- The value is the text string of a suggestion.
newSuggestion ::
  Suggestion
newSuggestion =
  Suggestion'
    { id = Prelude.Nothing,
      sourceDocuments = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The UUID (universally unique identifier) of a single query suggestion.
suggestion_id :: Lens.Lens' Suggestion (Prelude.Maybe Prelude.Text)
suggestion_id = Lens.lens (\Suggestion' {id} -> id) (\s@Suggestion' {} a -> s {id = a} :: Suggestion)

-- | The list of document IDs and their fields\/attributes that are used for
-- a single query suggestion, if document fields set to use for query
-- suggestions.
suggestion_sourceDocuments :: Lens.Lens' Suggestion (Prelude.Maybe [SourceDocument])
suggestion_sourceDocuments = Lens.lens (\Suggestion' {sourceDocuments} -> sourceDocuments) (\s@Suggestion' {} a -> s {sourceDocuments = a} :: Suggestion) Prelude.. Lens.mapping Lens.coerced

-- | The value for the UUID (universally unique identifier) of a single query
-- suggestion.
--
-- The value is the text string of a suggestion.
suggestion_value :: Lens.Lens' Suggestion (Prelude.Maybe SuggestionValue)
suggestion_value = Lens.lens (\Suggestion' {value} -> value) (\s@Suggestion' {} a -> s {value = a} :: Suggestion)

instance Data.FromJSON Suggestion where
  parseJSON =
    Data.withObject
      "Suggestion"
      ( \x ->
          Suggestion'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> ( x
                            Data..:? "SourceDocuments"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable Suggestion where
  hashWithSalt _salt Suggestion' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` sourceDocuments
      `Prelude.hashWithSalt` value

instance Prelude.NFData Suggestion where
  rnf Suggestion' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf sourceDocuments
      `Prelude.seq` Prelude.rnf value
