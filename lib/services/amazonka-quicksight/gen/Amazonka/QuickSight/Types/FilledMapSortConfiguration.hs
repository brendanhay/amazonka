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
-- Module      : Amazonka.QuickSight.Types.FilledMapSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilledMapSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions

-- | The sort configuration of a @FilledMapVisual@.
--
-- /See:/ 'newFilledMapSortConfiguration' smart constructor.
data FilledMapSortConfiguration = FilledMapSortConfiguration'
  { -- | The sort configuration of the location fields.
    categorySort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilledMapSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categorySort', 'filledMapSortConfiguration_categorySort' - The sort configuration of the location fields.
newFilledMapSortConfiguration ::
  FilledMapSortConfiguration
newFilledMapSortConfiguration =
  FilledMapSortConfiguration'
    { categorySort =
        Prelude.Nothing
    }

-- | The sort configuration of the location fields.
filledMapSortConfiguration_categorySort :: Lens.Lens' FilledMapSortConfiguration (Prelude.Maybe [FieldSortOptions])
filledMapSortConfiguration_categorySort = Lens.lens (\FilledMapSortConfiguration' {categorySort} -> categorySort) (\s@FilledMapSortConfiguration' {} a -> s {categorySort = a} :: FilledMapSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FilledMapSortConfiguration where
  parseJSON =
    Data.withObject
      "FilledMapSortConfiguration"
      ( \x ->
          FilledMapSortConfiguration'
            Prelude.<$> (x Data..:? "CategorySort" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FilledMapSortConfiguration where
  hashWithSalt _salt FilledMapSortConfiguration' {..} =
    _salt `Prelude.hashWithSalt` categorySort

instance Prelude.NFData FilledMapSortConfiguration where
  rnf FilledMapSortConfiguration' {..} =
    Prelude.rnf categorySort

instance Data.ToJSON FilledMapSortConfiguration where
  toJSON FilledMapSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("CategorySort" Data..=) Prelude.<$> categorySort]
      )
