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
-- Module      : Amazonka.QuickSight.Types.PaginationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PaginationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The pagination configuration for a table visual or boxplot.
--
-- /See:/ 'newPaginationConfiguration' smart constructor.
data PaginationConfiguration = PaginationConfiguration'
  { -- | Indicates how many items render in one page.
    pageSize :: Prelude.Integer,
    -- | Indicates the page number.
    pageNumber :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PaginationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'paginationConfiguration_pageSize' - Indicates how many items render in one page.
--
-- 'pageNumber', 'paginationConfiguration_pageNumber' - Indicates the page number.
newPaginationConfiguration ::
  -- | 'pageSize'
  Prelude.Integer ->
  -- | 'pageNumber'
  Prelude.Natural ->
  PaginationConfiguration
newPaginationConfiguration pPageSize_ pPageNumber_ =
  PaginationConfiguration'
    { pageSize = pPageSize_,
      pageNumber = pPageNumber_
    }

-- | Indicates how many items render in one page.
paginationConfiguration_pageSize :: Lens.Lens' PaginationConfiguration Prelude.Integer
paginationConfiguration_pageSize = Lens.lens (\PaginationConfiguration' {pageSize} -> pageSize) (\s@PaginationConfiguration' {} a -> s {pageSize = a} :: PaginationConfiguration)

-- | Indicates the page number.
paginationConfiguration_pageNumber :: Lens.Lens' PaginationConfiguration Prelude.Natural
paginationConfiguration_pageNumber = Lens.lens (\PaginationConfiguration' {pageNumber} -> pageNumber) (\s@PaginationConfiguration' {} a -> s {pageNumber = a} :: PaginationConfiguration)

instance Data.FromJSON PaginationConfiguration where
  parseJSON =
    Data.withObject
      "PaginationConfiguration"
      ( \x ->
          PaginationConfiguration'
            Prelude.<$> (x Data..: "PageSize")
            Prelude.<*> (x Data..: "PageNumber")
      )

instance Prelude.Hashable PaginationConfiguration where
  hashWithSalt _salt PaginationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageNumber

instance Prelude.NFData PaginationConfiguration where
  rnf PaginationConfiguration' {..} =
    Prelude.rnf pageSize `Prelude.seq`
      Prelude.rnf pageNumber

instance Data.ToJSON PaginationConfiguration where
  toJSON PaginationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("PageSize" Data..= pageSize),
            Prelude.Just ("PageNumber" Data..= pageNumber)
          ]
      )
