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
-- Module      : Amazonka.QuickSight.Types.WordCloudFieldWells
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WordCloudFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.WordCloudAggregatedFieldWells

-- | The field wells of a word cloud visual.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newWordCloudFieldWells' smart constructor.
data WordCloudFieldWells = WordCloudFieldWells'
  { -- | The aggregated field wells of a word cloud.
    wordCloudAggregatedFieldWells :: Prelude.Maybe WordCloudAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WordCloudFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wordCloudAggregatedFieldWells', 'wordCloudFieldWells_wordCloudAggregatedFieldWells' - The aggregated field wells of a word cloud.
newWordCloudFieldWells ::
  WordCloudFieldWells
newWordCloudFieldWells =
  WordCloudFieldWells'
    { wordCloudAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The aggregated field wells of a word cloud.
wordCloudFieldWells_wordCloudAggregatedFieldWells :: Lens.Lens' WordCloudFieldWells (Prelude.Maybe WordCloudAggregatedFieldWells)
wordCloudFieldWells_wordCloudAggregatedFieldWells = Lens.lens (\WordCloudFieldWells' {wordCloudAggregatedFieldWells} -> wordCloudAggregatedFieldWells) (\s@WordCloudFieldWells' {} a -> s {wordCloudAggregatedFieldWells = a} :: WordCloudFieldWells)

instance Data.FromJSON WordCloudFieldWells where
  parseJSON =
    Data.withObject
      "WordCloudFieldWells"
      ( \x ->
          WordCloudFieldWells'
            Prelude.<$> (x Data..:? "WordCloudAggregatedFieldWells")
      )

instance Prelude.Hashable WordCloudFieldWells where
  hashWithSalt _salt WordCloudFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` wordCloudAggregatedFieldWells

instance Prelude.NFData WordCloudFieldWells where
  rnf WordCloudFieldWells' {..} =
    Prelude.rnf wordCloudAggregatedFieldWells

instance Data.ToJSON WordCloudFieldWells where
  toJSON WordCloudFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WordCloudAggregatedFieldWells" Data..=)
              Prelude.<$> wordCloudAggregatedFieldWells
          ]
      )
