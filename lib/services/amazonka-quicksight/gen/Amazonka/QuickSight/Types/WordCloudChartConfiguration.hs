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
-- Module      : Amazonka.QuickSight.Types.WordCloudChartConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WordCloudChartConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.WordCloudFieldWells
import Amazonka.QuickSight.Types.WordCloudOptions
import Amazonka.QuickSight.Types.WordCloudSortConfiguration

-- | The configuration of a word cloud visual.
--
-- /See:/ 'newWordCloudChartConfiguration' smart constructor.
data WordCloudChartConfiguration = WordCloudChartConfiguration'
  { -- | The label options (label text, label visibility, and sort icon
    -- visibility) for the word cloud category.
    categoryLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe WordCloudFieldWells,
    -- | The sort configuration of a word cloud visual.
    sortConfiguration :: Prelude.Maybe WordCloudSortConfiguration,
    -- | The options for a word cloud visual.
    wordCloudOptions :: Prelude.Maybe WordCloudOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WordCloudChartConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryLabelOptions', 'wordCloudChartConfiguration_categoryLabelOptions' - The label options (label text, label visibility, and sort icon
-- visibility) for the word cloud category.
--
-- 'fieldWells', 'wordCloudChartConfiguration_fieldWells' - The field wells of the visual.
--
-- 'sortConfiguration', 'wordCloudChartConfiguration_sortConfiguration' - The sort configuration of a word cloud visual.
--
-- 'wordCloudOptions', 'wordCloudChartConfiguration_wordCloudOptions' - The options for a word cloud visual.
newWordCloudChartConfiguration ::
  WordCloudChartConfiguration
newWordCloudChartConfiguration =
  WordCloudChartConfiguration'
    { categoryLabelOptions =
        Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      wordCloudOptions = Prelude.Nothing
    }

-- | The label options (label text, label visibility, and sort icon
-- visibility) for the word cloud category.
wordCloudChartConfiguration_categoryLabelOptions :: Lens.Lens' WordCloudChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
wordCloudChartConfiguration_categoryLabelOptions = Lens.lens (\WordCloudChartConfiguration' {categoryLabelOptions} -> categoryLabelOptions) (\s@WordCloudChartConfiguration' {} a -> s {categoryLabelOptions = a} :: WordCloudChartConfiguration)

-- | The field wells of the visual.
wordCloudChartConfiguration_fieldWells :: Lens.Lens' WordCloudChartConfiguration (Prelude.Maybe WordCloudFieldWells)
wordCloudChartConfiguration_fieldWells = Lens.lens (\WordCloudChartConfiguration' {fieldWells} -> fieldWells) (\s@WordCloudChartConfiguration' {} a -> s {fieldWells = a} :: WordCloudChartConfiguration)

-- | The sort configuration of a word cloud visual.
wordCloudChartConfiguration_sortConfiguration :: Lens.Lens' WordCloudChartConfiguration (Prelude.Maybe WordCloudSortConfiguration)
wordCloudChartConfiguration_sortConfiguration = Lens.lens (\WordCloudChartConfiguration' {sortConfiguration} -> sortConfiguration) (\s@WordCloudChartConfiguration' {} a -> s {sortConfiguration = a} :: WordCloudChartConfiguration)

-- | The options for a word cloud visual.
wordCloudChartConfiguration_wordCloudOptions :: Lens.Lens' WordCloudChartConfiguration (Prelude.Maybe WordCloudOptions)
wordCloudChartConfiguration_wordCloudOptions = Lens.lens (\WordCloudChartConfiguration' {wordCloudOptions} -> wordCloudOptions) (\s@WordCloudChartConfiguration' {} a -> s {wordCloudOptions = a} :: WordCloudChartConfiguration)

instance Data.FromJSON WordCloudChartConfiguration where
  parseJSON =
    Data.withObject
      "WordCloudChartConfiguration"
      ( \x ->
          WordCloudChartConfiguration'
            Prelude.<$> (x Data..:? "CategoryLabelOptions")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "WordCloudOptions")
      )

instance Prelude.Hashable WordCloudChartConfiguration where
  hashWithSalt _salt WordCloudChartConfiguration' {..} =
    _salt `Prelude.hashWithSalt` categoryLabelOptions
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` wordCloudOptions

instance Prelude.NFData WordCloudChartConfiguration where
  rnf WordCloudChartConfiguration' {..} =
    Prelude.rnf categoryLabelOptions
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf wordCloudOptions

instance Data.ToJSON WordCloudChartConfiguration where
  toJSON WordCloudChartConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryLabelOptions" Data..=)
              Prelude.<$> categoryLabelOptions,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("WordCloudOptions" Data..=)
              Prelude.<$> wordCloudOptions
          ]
      )
