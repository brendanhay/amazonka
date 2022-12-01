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
-- Module      : Amazonka.Kendra.Types.DocumentAttributeValueCountPair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DocumentAttributeValueCountPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.DocumentAttributeValue
import {-# SOURCE #-} Amazonka.Kendra.Types.FacetResult
import qualified Amazonka.Prelude as Prelude

-- | Provides the count of documents that match a particular attribute when
-- doing a faceted search.
--
-- /See:/ 'newDocumentAttributeValueCountPair' smart constructor.
data DocumentAttributeValueCountPair = DocumentAttributeValueCountPair'
  { -- | Contains the results of a document attribute that is a nested facet. A
    -- @FacetResult@ contains the counts for each facet nested within a facet.
    --
    -- For example, the document attribute or facet \"Department\" includes a
    -- value called \"Engineering\". In addition, the document attribute or
    -- facet \"SubDepartment\" includes the values \"Frontend\" and \"Backend\"
    -- for documents assigned to \"Engineering\". You can display nested facets
    -- in the search results so that documents can be searched not only by
    -- department but also by a sub department within a department. The counts
    -- for documents that belong to \"Frontend\" and \"Backend\" within
    -- \"Engineering\" are returned for a query.
    facetResults :: Prelude.Maybe [FacetResult],
    -- | The number of documents in the response that have the attribute value
    -- for the key.
    count :: Prelude.Maybe Prelude.Int,
    -- | The value of the attribute. For example, \"HR\".
    documentAttributeValue :: Prelude.Maybe DocumentAttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentAttributeValueCountPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'facetResults', 'documentAttributeValueCountPair_facetResults' - Contains the results of a document attribute that is a nested facet. A
-- @FacetResult@ contains the counts for each facet nested within a facet.
--
-- For example, the document attribute or facet \"Department\" includes a
-- value called \"Engineering\". In addition, the document attribute or
-- facet \"SubDepartment\" includes the values \"Frontend\" and \"Backend\"
-- for documents assigned to \"Engineering\". You can display nested facets
-- in the search results so that documents can be searched not only by
-- department but also by a sub department within a department. The counts
-- for documents that belong to \"Frontend\" and \"Backend\" within
-- \"Engineering\" are returned for a query.
--
-- 'count', 'documentAttributeValueCountPair_count' - The number of documents in the response that have the attribute value
-- for the key.
--
-- 'documentAttributeValue', 'documentAttributeValueCountPair_documentAttributeValue' - The value of the attribute. For example, \"HR\".
newDocumentAttributeValueCountPair ::
  DocumentAttributeValueCountPair
newDocumentAttributeValueCountPair =
  DocumentAttributeValueCountPair'
    { facetResults =
        Prelude.Nothing,
      count = Prelude.Nothing,
      documentAttributeValue = Prelude.Nothing
    }

-- | Contains the results of a document attribute that is a nested facet. A
-- @FacetResult@ contains the counts for each facet nested within a facet.
--
-- For example, the document attribute or facet \"Department\" includes a
-- value called \"Engineering\". In addition, the document attribute or
-- facet \"SubDepartment\" includes the values \"Frontend\" and \"Backend\"
-- for documents assigned to \"Engineering\". You can display nested facets
-- in the search results so that documents can be searched not only by
-- department but also by a sub department within a department. The counts
-- for documents that belong to \"Frontend\" and \"Backend\" within
-- \"Engineering\" are returned for a query.
documentAttributeValueCountPair_facetResults :: Lens.Lens' DocumentAttributeValueCountPair (Prelude.Maybe [FacetResult])
documentAttributeValueCountPair_facetResults = Lens.lens (\DocumentAttributeValueCountPair' {facetResults} -> facetResults) (\s@DocumentAttributeValueCountPair' {} a -> s {facetResults = a} :: DocumentAttributeValueCountPair) Prelude.. Lens.mapping Lens.coerced

-- | The number of documents in the response that have the attribute value
-- for the key.
documentAttributeValueCountPair_count :: Lens.Lens' DocumentAttributeValueCountPair (Prelude.Maybe Prelude.Int)
documentAttributeValueCountPair_count = Lens.lens (\DocumentAttributeValueCountPair' {count} -> count) (\s@DocumentAttributeValueCountPair' {} a -> s {count = a} :: DocumentAttributeValueCountPair)

-- | The value of the attribute. For example, \"HR\".
documentAttributeValueCountPair_documentAttributeValue :: Lens.Lens' DocumentAttributeValueCountPair (Prelude.Maybe DocumentAttributeValue)
documentAttributeValueCountPair_documentAttributeValue = Lens.lens (\DocumentAttributeValueCountPair' {documentAttributeValue} -> documentAttributeValue) (\s@DocumentAttributeValueCountPair' {} a -> s {documentAttributeValue = a} :: DocumentAttributeValueCountPair)

instance
  Core.FromJSON
    DocumentAttributeValueCountPair
  where
  parseJSON =
    Core.withObject
      "DocumentAttributeValueCountPair"
      ( \x ->
          DocumentAttributeValueCountPair'
            Prelude.<$> (x Core..:? "FacetResults" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "DocumentAttributeValue")
      )

instance
  Prelude.Hashable
    DocumentAttributeValueCountPair
  where
  hashWithSalt
    _salt
    DocumentAttributeValueCountPair' {..} =
      _salt `Prelude.hashWithSalt` facetResults
        `Prelude.hashWithSalt` count
        `Prelude.hashWithSalt` documentAttributeValue

instance
  Prelude.NFData
    DocumentAttributeValueCountPair
  where
  rnf DocumentAttributeValueCountPair' {..} =
    Prelude.rnf facetResults
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf documentAttributeValue
