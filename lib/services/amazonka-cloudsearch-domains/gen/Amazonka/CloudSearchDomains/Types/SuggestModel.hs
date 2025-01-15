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
-- Module      : Amazonka.CloudSearchDomains.Types.SuggestModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearchDomains.Types.SuggestModel where

import Amazonka.CloudSearchDomains.Types.SuggestionMatch
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Container for the suggestion information returned in a
-- @SuggestResponse@.
--
-- /See:/ 'newSuggestModel' smart constructor.
data SuggestModel = SuggestModel'
  { -- | The number of documents that were found to match the query string.
    found :: Prelude.Maybe Prelude.Integer,
    -- | The query string specified in the suggest request.
    query :: Prelude.Maybe Prelude.Text,
    -- | The documents that match the query string.
    suggestions :: Prelude.Maybe [SuggestionMatch]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuggestModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'found', 'suggestModel_found' - The number of documents that were found to match the query string.
--
-- 'query', 'suggestModel_query' - The query string specified in the suggest request.
--
-- 'suggestions', 'suggestModel_suggestions' - The documents that match the query string.
newSuggestModel ::
  SuggestModel
newSuggestModel =
  SuggestModel'
    { found = Prelude.Nothing,
      query = Prelude.Nothing,
      suggestions = Prelude.Nothing
    }

-- | The number of documents that were found to match the query string.
suggestModel_found :: Lens.Lens' SuggestModel (Prelude.Maybe Prelude.Integer)
suggestModel_found = Lens.lens (\SuggestModel' {found} -> found) (\s@SuggestModel' {} a -> s {found = a} :: SuggestModel)

-- | The query string specified in the suggest request.
suggestModel_query :: Lens.Lens' SuggestModel (Prelude.Maybe Prelude.Text)
suggestModel_query = Lens.lens (\SuggestModel' {query} -> query) (\s@SuggestModel' {} a -> s {query = a} :: SuggestModel)

-- | The documents that match the query string.
suggestModel_suggestions :: Lens.Lens' SuggestModel (Prelude.Maybe [SuggestionMatch])
suggestModel_suggestions = Lens.lens (\SuggestModel' {suggestions} -> suggestions) (\s@SuggestModel' {} a -> s {suggestions = a} :: SuggestModel) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SuggestModel where
  parseJSON =
    Data.withObject
      "SuggestModel"
      ( \x ->
          SuggestModel'
            Prelude.<$> (x Data..:? "found")
            Prelude.<*> (x Data..:? "query")
            Prelude.<*> (x Data..:? "suggestions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SuggestModel where
  hashWithSalt _salt SuggestModel' {..} =
    _salt
      `Prelude.hashWithSalt` found
      `Prelude.hashWithSalt` query
      `Prelude.hashWithSalt` suggestions

instance Prelude.NFData SuggestModel where
  rnf SuggestModel' {..} =
    Prelude.rnf found `Prelude.seq`
      Prelude.rnf query `Prelude.seq`
        Prelude.rnf suggestions
