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
-- Module      : Amazonka.QuickSight.Types.DataPathSort
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataPathSort where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataPathValue
import Amazonka.QuickSight.Types.SortDirection

-- | Allows data paths to be sorted by a specific data value.
--
-- /See:/ 'newDataPathSort' smart constructor.
data DataPathSort = DataPathSort'
  { -- | Determines the sort direction.
    direction :: SortDirection,
    -- | The list of data paths that need to be sorted.
    sortPaths :: [DataPathValue]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataPathSort' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'direction', 'dataPathSort_direction' - Determines the sort direction.
--
-- 'sortPaths', 'dataPathSort_sortPaths' - The list of data paths that need to be sorted.
newDataPathSort ::
  -- | 'direction'
  SortDirection ->
  DataPathSort
newDataPathSort pDirection_ =
  DataPathSort'
    { direction = pDirection_,
      sortPaths = Prelude.mempty
    }

-- | Determines the sort direction.
dataPathSort_direction :: Lens.Lens' DataPathSort SortDirection
dataPathSort_direction = Lens.lens (\DataPathSort' {direction} -> direction) (\s@DataPathSort' {} a -> s {direction = a} :: DataPathSort)

-- | The list of data paths that need to be sorted.
dataPathSort_sortPaths :: Lens.Lens' DataPathSort [DataPathValue]
dataPathSort_sortPaths = Lens.lens (\DataPathSort' {sortPaths} -> sortPaths) (\s@DataPathSort' {} a -> s {sortPaths = a} :: DataPathSort) Prelude.. Lens.coerced

instance Data.FromJSON DataPathSort where
  parseJSON =
    Data.withObject
      "DataPathSort"
      ( \x ->
          DataPathSort'
            Prelude.<$> (x Data..: "Direction")
            Prelude.<*> (x Data..:? "SortPaths" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DataPathSort where
  hashWithSalt _salt DataPathSort' {..} =
    _salt `Prelude.hashWithSalt` direction
      `Prelude.hashWithSalt` sortPaths

instance Prelude.NFData DataPathSort where
  rnf DataPathSort' {..} =
    Prelude.rnf direction
      `Prelude.seq` Prelude.rnf sortPaths

instance Data.ToJSON DataPathSort where
  toJSON DataPathSort' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Direction" Data..= direction),
            Prelude.Just ("SortPaths" Data..= sortPaths)
          ]
      )
