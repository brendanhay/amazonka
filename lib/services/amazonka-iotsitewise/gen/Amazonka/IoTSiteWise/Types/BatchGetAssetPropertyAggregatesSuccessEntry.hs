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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesSuccessEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesSuccessEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.AggregatedValue
import qualified Amazonka.Prelude as Prelude

-- | Contains success information for an entry that is associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyAggregates.html BatchGetAssetPropertyAggregates>
-- API.
--
-- /See:/ 'newBatchGetAssetPropertyAggregatesSuccessEntry' smart constructor.
data BatchGetAssetPropertyAggregatesSuccessEntry = BatchGetAssetPropertyAggregatesSuccessEntry'
  { -- | The ID of the entry.
    entryId :: Prelude.Text,
    -- | The requested aggregated asset property values (for example, average,
    -- minimum, and maximum).
    aggregatedValues :: [AggregatedValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyAggregatesSuccessEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entryId', 'batchGetAssetPropertyAggregatesSuccessEntry_entryId' - The ID of the entry.
--
-- 'aggregatedValues', 'batchGetAssetPropertyAggregatesSuccessEntry_aggregatedValues' - The requested aggregated asset property values (for example, average,
-- minimum, and maximum).
newBatchGetAssetPropertyAggregatesSuccessEntry ::
  -- | 'entryId'
  Prelude.Text ->
  BatchGetAssetPropertyAggregatesSuccessEntry
newBatchGetAssetPropertyAggregatesSuccessEntry
  pEntryId_ =
    BatchGetAssetPropertyAggregatesSuccessEntry'
      { entryId =
          pEntryId_,
        aggregatedValues =
          Prelude.mempty
      }

-- | The ID of the entry.
batchGetAssetPropertyAggregatesSuccessEntry_entryId :: Lens.Lens' BatchGetAssetPropertyAggregatesSuccessEntry Prelude.Text
batchGetAssetPropertyAggregatesSuccessEntry_entryId = Lens.lens (\BatchGetAssetPropertyAggregatesSuccessEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyAggregatesSuccessEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyAggregatesSuccessEntry)

-- | The requested aggregated asset property values (for example, average,
-- minimum, and maximum).
batchGetAssetPropertyAggregatesSuccessEntry_aggregatedValues :: Lens.Lens' BatchGetAssetPropertyAggregatesSuccessEntry [AggregatedValue]
batchGetAssetPropertyAggregatesSuccessEntry_aggregatedValues = Lens.lens (\BatchGetAssetPropertyAggregatesSuccessEntry' {aggregatedValues} -> aggregatedValues) (\s@BatchGetAssetPropertyAggregatesSuccessEntry' {} a -> s {aggregatedValues = a} :: BatchGetAssetPropertyAggregatesSuccessEntry) Prelude.. Lens.coerced

instance
  Data.FromJSON
    BatchGetAssetPropertyAggregatesSuccessEntry
  where
  parseJSON =
    Data.withObject
      "BatchGetAssetPropertyAggregatesSuccessEntry"
      ( \x ->
          BatchGetAssetPropertyAggregatesSuccessEntry'
            Prelude.<$> (x Data..: "entryId")
              Prelude.<*> ( x Data..:? "aggregatedValues"
                              Data..!= Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyAggregatesSuccessEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyAggregatesSuccessEntry' {..} =
      _salt `Prelude.hashWithSalt` entryId
        `Prelude.hashWithSalt` aggregatedValues

instance
  Prelude.NFData
    BatchGetAssetPropertyAggregatesSuccessEntry
  where
  rnf BatchGetAssetPropertyAggregatesSuccessEntry' {..} =
    Prelude.rnf entryId
      `Prelude.seq` Prelude.rnf aggregatedValues
