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
-- Module      : Amazonka.Kendra.Types.Search
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.Search where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about how a custom index field is used during a
-- search.
--
-- /See:/ 'newSearch' smart constructor.
data Search = Search'
  { -- | Determines whether the field is returned in the query response. The
    -- default is @true@.
    displayable :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the field can be used to sort the results of a query.
    -- If you specify sorting on a field that does not have @Sortable@ set to
    -- @true@, Amazon Kendra returns an exception. The default is @false@.
    sortable :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the field is used in the search. If the @Searchable@
    -- field is @true@, you can use relevance tuning to manually tune how
    -- Amazon Kendra weights the field in the search. The default is @true@ for
    -- string fields and @false@ for number and date fields.
    searchable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates that the field can be used to create search facets, a count of
    -- results for each value in the field. The default is @false@ .
    facetable :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Search' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayable', 'search_displayable' - Determines whether the field is returned in the query response. The
-- default is @true@.
--
-- 'sortable', 'search_sortable' - Determines whether the field can be used to sort the results of a query.
-- If you specify sorting on a field that does not have @Sortable@ set to
-- @true@, Amazon Kendra returns an exception. The default is @false@.
--
-- 'searchable', 'search_searchable' - Determines whether the field is used in the search. If the @Searchable@
-- field is @true@, you can use relevance tuning to manually tune how
-- Amazon Kendra weights the field in the search. The default is @true@ for
-- string fields and @false@ for number and date fields.
--
-- 'facetable', 'search_facetable' - Indicates that the field can be used to create search facets, a count of
-- results for each value in the field. The default is @false@ .
newSearch ::
  Search
newSearch =
  Search'
    { displayable = Prelude.Nothing,
      sortable = Prelude.Nothing,
      searchable = Prelude.Nothing,
      facetable = Prelude.Nothing
    }

-- | Determines whether the field is returned in the query response. The
-- default is @true@.
search_displayable :: Lens.Lens' Search (Prelude.Maybe Prelude.Bool)
search_displayable = Lens.lens (\Search' {displayable} -> displayable) (\s@Search' {} a -> s {displayable = a} :: Search)

-- | Determines whether the field can be used to sort the results of a query.
-- If you specify sorting on a field that does not have @Sortable@ set to
-- @true@, Amazon Kendra returns an exception. The default is @false@.
search_sortable :: Lens.Lens' Search (Prelude.Maybe Prelude.Bool)
search_sortable = Lens.lens (\Search' {sortable} -> sortable) (\s@Search' {} a -> s {sortable = a} :: Search)

-- | Determines whether the field is used in the search. If the @Searchable@
-- field is @true@, you can use relevance tuning to manually tune how
-- Amazon Kendra weights the field in the search. The default is @true@ for
-- string fields and @false@ for number and date fields.
search_searchable :: Lens.Lens' Search (Prelude.Maybe Prelude.Bool)
search_searchable = Lens.lens (\Search' {searchable} -> searchable) (\s@Search' {} a -> s {searchable = a} :: Search)

-- | Indicates that the field can be used to create search facets, a count of
-- results for each value in the field. The default is @false@ .
search_facetable :: Lens.Lens' Search (Prelude.Maybe Prelude.Bool)
search_facetable = Lens.lens (\Search' {facetable} -> facetable) (\s@Search' {} a -> s {facetable = a} :: Search)

instance Data.FromJSON Search where
  parseJSON =
    Data.withObject
      "Search"
      ( \x ->
          Search'
            Prelude.<$> (x Data..:? "Displayable")
            Prelude.<*> (x Data..:? "Sortable")
            Prelude.<*> (x Data..:? "Searchable")
            Prelude.<*> (x Data..:? "Facetable")
      )

instance Prelude.Hashable Search where
  hashWithSalt _salt Search' {..} =
    _salt `Prelude.hashWithSalt` displayable
      `Prelude.hashWithSalt` sortable
      `Prelude.hashWithSalt` searchable
      `Prelude.hashWithSalt` facetable

instance Prelude.NFData Search where
  rnf Search' {..} =
    Prelude.rnf displayable
      `Prelude.seq` Prelude.rnf sortable
      `Prelude.seq` Prelude.rnf searchable
      `Prelude.seq` Prelude.rnf facetable

instance Data.ToJSON Search where
  toJSON Search' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Displayable" Data..=) Prelude.<$> displayable,
            ("Sortable" Data..=) Prelude.<$> sortable,
            ("Searchable" Data..=) Prelude.<$> searchable,
            ("Facetable" Data..=) Prelude.<$> facetable
          ]
      )
