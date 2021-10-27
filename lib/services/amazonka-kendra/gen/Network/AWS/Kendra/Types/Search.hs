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
-- Module      : Network.AWS.Kendra.Types.Search
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.Search where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about how a custom index field is used during a
-- search.
--
-- /See:/ 'newSearch' smart constructor.
data Search = Search'
  { -- | Indicates that the field can be used to create search facets, a count of
    -- results for each value in the field. The default is @false@ .
    facetable :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the field can be used to sort the results of a query.
    -- If you specify sorting on a field that does not have @Sortable@ set to
    -- @true@, Amazon Kendra returns an exception. The default is @false@.
    sortable :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the field is used in the search. If the @Searchable@
    -- field is @true@, you can use relevance tuning to manually tune how
    -- Amazon Kendra weights the field in the search. The default is @true@ for
    -- string fields and @false@ for number and date fields.
    searchable :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the field is returned in the query response. The
    -- default is @true@.
    displayable :: Prelude.Maybe Prelude.Bool
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
-- 'facetable', 'search_facetable' - Indicates that the field can be used to create search facets, a count of
-- results for each value in the field. The default is @false@ .
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
-- 'displayable', 'search_displayable' - Determines whether the field is returned in the query response. The
-- default is @true@.
newSearch ::
  Search
newSearch =
  Search'
    { facetable = Prelude.Nothing,
      sortable = Prelude.Nothing,
      searchable = Prelude.Nothing,
      displayable = Prelude.Nothing
    }

-- | Indicates that the field can be used to create search facets, a count of
-- results for each value in the field. The default is @false@ .
search_facetable :: Lens.Lens' Search (Prelude.Maybe Prelude.Bool)
search_facetable = Lens.lens (\Search' {facetable} -> facetable) (\s@Search' {} a -> s {facetable = a} :: Search)

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

-- | Determines whether the field is returned in the query response. The
-- default is @true@.
search_displayable :: Lens.Lens' Search (Prelude.Maybe Prelude.Bool)
search_displayable = Lens.lens (\Search' {displayable} -> displayable) (\s@Search' {} a -> s {displayable = a} :: Search)

instance Core.FromJSON Search where
  parseJSON =
    Core.withObject
      "Search"
      ( \x ->
          Search'
            Prelude.<$> (x Core..:? "Facetable")
            Prelude.<*> (x Core..:? "Sortable")
            Prelude.<*> (x Core..:? "Searchable")
            Prelude.<*> (x Core..:? "Displayable")
      )

instance Prelude.Hashable Search

instance Prelude.NFData Search

instance Core.ToJSON Search where
  toJSON Search' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Facetable" Core..=) Prelude.<$> facetable,
            ("Sortable" Core..=) Prelude.<$> sortable,
            ("Searchable" Core..=) Prelude.<$> searchable,
            ("Displayable" Core..=) Prelude.<$> displayable
          ]
      )
