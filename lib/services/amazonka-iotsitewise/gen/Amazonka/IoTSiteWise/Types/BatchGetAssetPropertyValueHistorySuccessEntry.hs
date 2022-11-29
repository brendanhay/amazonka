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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistorySuccessEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistorySuccessEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types.AssetPropertyValue
import qualified Amazonka.Prelude as Prelude

-- | Contains success information for an entry that is associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValue.html BatchGetAssetPropertyValueHistory>
-- API.
--
-- /See:/ 'newBatchGetAssetPropertyValueHistorySuccessEntry' smart constructor.
data BatchGetAssetPropertyValueHistorySuccessEntry = BatchGetAssetPropertyValueHistorySuccessEntry'
  { -- | The ID of the entry.
    entryId :: Prelude.Text,
    -- | The requested historical values for the specified asset property.
    assetPropertyValueHistory :: [AssetPropertyValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueHistorySuccessEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entryId', 'batchGetAssetPropertyValueHistorySuccessEntry_entryId' - The ID of the entry.
--
-- 'assetPropertyValueHistory', 'batchGetAssetPropertyValueHistorySuccessEntry_assetPropertyValueHistory' - The requested historical values for the specified asset property.
newBatchGetAssetPropertyValueHistorySuccessEntry ::
  -- | 'entryId'
  Prelude.Text ->
  BatchGetAssetPropertyValueHistorySuccessEntry
newBatchGetAssetPropertyValueHistorySuccessEntry
  pEntryId_ =
    BatchGetAssetPropertyValueHistorySuccessEntry'
      { entryId =
          pEntryId_,
        assetPropertyValueHistory =
          Prelude.mempty
      }

-- | The ID of the entry.
batchGetAssetPropertyValueHistorySuccessEntry_entryId :: Lens.Lens' BatchGetAssetPropertyValueHistorySuccessEntry Prelude.Text
batchGetAssetPropertyValueHistorySuccessEntry_entryId = Lens.lens (\BatchGetAssetPropertyValueHistorySuccessEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyValueHistorySuccessEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyValueHistorySuccessEntry)

-- | The requested historical values for the specified asset property.
batchGetAssetPropertyValueHistorySuccessEntry_assetPropertyValueHistory :: Lens.Lens' BatchGetAssetPropertyValueHistorySuccessEntry [AssetPropertyValue]
batchGetAssetPropertyValueHistorySuccessEntry_assetPropertyValueHistory = Lens.lens (\BatchGetAssetPropertyValueHistorySuccessEntry' {assetPropertyValueHistory} -> assetPropertyValueHistory) (\s@BatchGetAssetPropertyValueHistorySuccessEntry' {} a -> s {assetPropertyValueHistory = a} :: BatchGetAssetPropertyValueHistorySuccessEntry) Prelude.. Lens.coerced

instance
  Core.FromJSON
    BatchGetAssetPropertyValueHistorySuccessEntry
  where
  parseJSON =
    Core.withObject
      "BatchGetAssetPropertyValueHistorySuccessEntry"
      ( \x ->
          BatchGetAssetPropertyValueHistorySuccessEntry'
            Prelude.<$> (x Core..: "entryId")
              Prelude.<*> ( x Core..:? "assetPropertyValueHistory"
                              Core..!= Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyValueHistorySuccessEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyValueHistorySuccessEntry' {..} =
      _salt `Prelude.hashWithSalt` entryId
        `Prelude.hashWithSalt` assetPropertyValueHistory

instance
  Prelude.NFData
    BatchGetAssetPropertyValueHistorySuccessEntry
  where
  rnf
    BatchGetAssetPropertyValueHistorySuccessEntry' {..} =
      Prelude.rnf entryId
        `Prelude.seq` Prelude.rnf assetPropertyValueHistory
