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
-- Module      : Amazonka.QuickSight.Types.WordCloudAggregatedFieldWells
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WordCloudAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The aggregated field wells of a word cloud.
--
-- /See:/ 'newWordCloudAggregatedFieldWells' smart constructor.
data WordCloudAggregatedFieldWells = WordCloudAggregatedFieldWells'
  { -- | The group by field well of a word cloud. Values are grouped by group by
    -- fields.
    groupBy :: Prelude.Maybe [DimensionField],
    -- | The size field well of a word cloud. Values are aggregated based on
    -- group by fields.
    size :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WordCloudAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupBy', 'wordCloudAggregatedFieldWells_groupBy' - The group by field well of a word cloud. Values are grouped by group by
-- fields.
--
-- 'size', 'wordCloudAggregatedFieldWells_size' - The size field well of a word cloud. Values are aggregated based on
-- group by fields.
newWordCloudAggregatedFieldWells ::
  WordCloudAggregatedFieldWells
newWordCloudAggregatedFieldWells =
  WordCloudAggregatedFieldWells'
    { groupBy =
        Prelude.Nothing,
      size = Prelude.Nothing
    }

-- | The group by field well of a word cloud. Values are grouped by group by
-- fields.
wordCloudAggregatedFieldWells_groupBy :: Lens.Lens' WordCloudAggregatedFieldWells (Prelude.Maybe [DimensionField])
wordCloudAggregatedFieldWells_groupBy = Lens.lens (\WordCloudAggregatedFieldWells' {groupBy} -> groupBy) (\s@WordCloudAggregatedFieldWells' {} a -> s {groupBy = a} :: WordCloudAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The size field well of a word cloud. Values are aggregated based on
-- group by fields.
wordCloudAggregatedFieldWells_size :: Lens.Lens' WordCloudAggregatedFieldWells (Prelude.Maybe [MeasureField])
wordCloudAggregatedFieldWells_size = Lens.lens (\WordCloudAggregatedFieldWells' {size} -> size) (\s@WordCloudAggregatedFieldWells' {} a -> s {size = a} :: WordCloudAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON WordCloudAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "WordCloudAggregatedFieldWells"
      ( \x ->
          WordCloudAggregatedFieldWells'
            Prelude.<$> (x Data..:? "GroupBy" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Size" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    WordCloudAggregatedFieldWells
  where
  hashWithSalt _salt WordCloudAggregatedFieldWells' {..} =
    _salt `Prelude.hashWithSalt` groupBy
      `Prelude.hashWithSalt` size

instance Prelude.NFData WordCloudAggregatedFieldWells where
  rnf WordCloudAggregatedFieldWells' {..} =
    Prelude.rnf groupBy `Prelude.seq` Prelude.rnf size

instance Data.ToJSON WordCloudAggregatedFieldWells where
  toJSON WordCloudAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupBy" Data..=) Prelude.<$> groupBy,
            ("Size" Data..=) Prelude.<$> size
          ]
      )
