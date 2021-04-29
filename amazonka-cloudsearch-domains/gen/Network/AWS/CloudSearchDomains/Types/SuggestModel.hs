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
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SuggestModel where

import Network.AWS.CloudSearchDomains.Types.SuggestionMatch
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Container for the suggestion information returned in a
-- @SuggestResponse@.
--
-- /See:/ 'newSuggestModel' smart constructor.
data SuggestModel = SuggestModel'
  { -- | The documents that match the query string.
    suggestions :: Prelude.Maybe [SuggestionMatch],
    -- | The number of documents that were found to match the query string.
    found :: Prelude.Maybe Prelude.Integer,
    -- | The query string specified in the suggest request.
    query :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SuggestModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suggestions', 'suggestModel_suggestions' - The documents that match the query string.
--
-- 'found', 'suggestModel_found' - The number of documents that were found to match the query string.
--
-- 'query', 'suggestModel_query' - The query string specified in the suggest request.
newSuggestModel ::
  SuggestModel
newSuggestModel =
  SuggestModel'
    { suggestions = Prelude.Nothing,
      found = Prelude.Nothing,
      query = Prelude.Nothing
    }

-- | The documents that match the query string.
suggestModel_suggestions :: Lens.Lens' SuggestModel (Prelude.Maybe [SuggestionMatch])
suggestModel_suggestions = Lens.lens (\SuggestModel' {suggestions} -> suggestions) (\s@SuggestModel' {} a -> s {suggestions = a} :: SuggestModel) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of documents that were found to match the query string.
suggestModel_found :: Lens.Lens' SuggestModel (Prelude.Maybe Prelude.Integer)
suggestModel_found = Lens.lens (\SuggestModel' {found} -> found) (\s@SuggestModel' {} a -> s {found = a} :: SuggestModel)

-- | The query string specified in the suggest request.
suggestModel_query :: Lens.Lens' SuggestModel (Prelude.Maybe Prelude.Text)
suggestModel_query = Lens.lens (\SuggestModel' {query} -> query) (\s@SuggestModel' {} a -> s {query = a} :: SuggestModel)

instance Prelude.FromJSON SuggestModel where
  parseJSON =
    Prelude.withObject
      "SuggestModel"
      ( \x ->
          SuggestModel'
            Prelude.<$> ( x Prelude..:? "suggestions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "found")
            Prelude.<*> (x Prelude..:? "query")
      )

instance Prelude.Hashable SuggestModel

instance Prelude.NFData SuggestModel
