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
-- Module      : Amazonka.QuickSight.Types.WordCloudSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WordCloudSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.ItemsLimitConfiguration

-- | The sort configuration of a word cloud visual.
--
-- /See:/ 'newWordCloudSortConfiguration' smart constructor.
data WordCloudSortConfiguration = WordCloudSortConfiguration'
  { -- | The limit on the number of groups that are displayed in a word cloud.
    categoryItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of group by fields.
    categorySort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WordCloudSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryItemsLimit', 'wordCloudSortConfiguration_categoryItemsLimit' - The limit on the number of groups that are displayed in a word cloud.
--
-- 'categorySort', 'wordCloudSortConfiguration_categorySort' - The sort configuration of group by fields.
newWordCloudSortConfiguration ::
  WordCloudSortConfiguration
newWordCloudSortConfiguration =
  WordCloudSortConfiguration'
    { categoryItemsLimit =
        Prelude.Nothing,
      categorySort = Prelude.Nothing
    }

-- | The limit on the number of groups that are displayed in a word cloud.
wordCloudSortConfiguration_categoryItemsLimit :: Lens.Lens' WordCloudSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
wordCloudSortConfiguration_categoryItemsLimit = Lens.lens (\WordCloudSortConfiguration' {categoryItemsLimit} -> categoryItemsLimit) (\s@WordCloudSortConfiguration' {} a -> s {categoryItemsLimit = a} :: WordCloudSortConfiguration)

-- | The sort configuration of group by fields.
wordCloudSortConfiguration_categorySort :: Lens.Lens' WordCloudSortConfiguration (Prelude.Maybe [FieldSortOptions])
wordCloudSortConfiguration_categorySort = Lens.lens (\WordCloudSortConfiguration' {categorySort} -> categorySort) (\s@WordCloudSortConfiguration' {} a -> s {categorySort = a} :: WordCloudSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON WordCloudSortConfiguration where
  parseJSON =
    Data.withObject
      "WordCloudSortConfiguration"
      ( \x ->
          WordCloudSortConfiguration'
            Prelude.<$> (x Data..:? "CategoryItemsLimit")
            Prelude.<*> (x Data..:? "CategorySort" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable WordCloudSortConfiguration where
  hashWithSalt _salt WordCloudSortConfiguration' {..} =
    _salt `Prelude.hashWithSalt` categoryItemsLimit
      `Prelude.hashWithSalt` categorySort

instance Prelude.NFData WordCloudSortConfiguration where
  rnf WordCloudSortConfiguration' {..} =
    Prelude.rnf categoryItemsLimit
      `Prelude.seq` Prelude.rnf categorySort

instance Data.ToJSON WordCloudSortConfiguration where
  toJSON WordCloudSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryItemsLimit" Data..=)
              Prelude.<$> categoryItemsLimit,
            ("CategorySort" Data..=) Prelude.<$> categorySort
          ]
      )
