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
-- Module      : Amazonka.ResourceExplorer2.Types.SearchFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceExplorer2.Types.SearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A search filter defines which resources can be part of a search query
-- result set.
--
-- /See:/ 'newSearchFilter' smart constructor.
data SearchFilter = SearchFilter'
  { -- | The string that contains the search keywords, prefixes, and operators to
    -- control the results that can be returned by a Search operation. For more
    -- details, see
    -- <https://docs.aws.amazon.com/resource-explorer/latest/APIReference/about-query-syntax.html Search query syntax>.
    filterString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterString', 'searchFilter_filterString' - The string that contains the search keywords, prefixes, and operators to
-- control the results that can be returned by a Search operation. For more
-- details, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/APIReference/about-query-syntax.html Search query syntax>.
newSearchFilter ::
  -- | 'filterString'
  Prelude.Text ->
  SearchFilter
newSearchFilter pFilterString_ =
  SearchFilter' {filterString = pFilterString_}

-- | The string that contains the search keywords, prefixes, and operators to
-- control the results that can be returned by a Search operation. For more
-- details, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/APIReference/about-query-syntax.html Search query syntax>.
searchFilter_filterString :: Lens.Lens' SearchFilter Prelude.Text
searchFilter_filterString = Lens.lens (\SearchFilter' {filterString} -> filterString) (\s@SearchFilter' {} a -> s {filterString = a} :: SearchFilter)

instance Data.FromJSON SearchFilter where
  parseJSON =
    Data.withObject
      "SearchFilter"
      ( \x ->
          SearchFilter' Prelude.<$> (x Data..: "FilterString")
      )

instance Prelude.Hashable SearchFilter where
  hashWithSalt _salt SearchFilter' {..} =
    _salt `Prelude.hashWithSalt` filterString

instance Prelude.NFData SearchFilter where
  rnf SearchFilter' {..} = Prelude.rnf filterString

instance Data.ToJSON SearchFilter where
  toJSON SearchFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("FilterString" Data..= filterString)]
      )
