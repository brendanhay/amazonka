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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueSuccessEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueSuccessEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.AssetPropertyValue
import qualified Amazonka.Prelude as Prelude

-- | Contains success information for an entry that is associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValue.html BatchGetAssetPropertyValue>
-- API.
--
-- /See:/ 'newBatchGetAssetPropertyValueSuccessEntry' smart constructor.
data BatchGetAssetPropertyValueSuccessEntry = BatchGetAssetPropertyValueSuccessEntry'
  { assetPropertyValue :: Prelude.Maybe AssetPropertyValue,
    -- | The ID of the entry.
    entryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueSuccessEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetPropertyValue', 'batchGetAssetPropertyValueSuccessEntry_assetPropertyValue' - Undocumented member.
--
-- 'entryId', 'batchGetAssetPropertyValueSuccessEntry_entryId' - The ID of the entry.
newBatchGetAssetPropertyValueSuccessEntry ::
  -- | 'entryId'
  Prelude.Text ->
  BatchGetAssetPropertyValueSuccessEntry
newBatchGetAssetPropertyValueSuccessEntry pEntryId_ =
  BatchGetAssetPropertyValueSuccessEntry'
    { assetPropertyValue =
        Prelude.Nothing,
      entryId = pEntryId_
    }

-- | Undocumented member.
batchGetAssetPropertyValueSuccessEntry_assetPropertyValue :: Lens.Lens' BatchGetAssetPropertyValueSuccessEntry (Prelude.Maybe AssetPropertyValue)
batchGetAssetPropertyValueSuccessEntry_assetPropertyValue = Lens.lens (\BatchGetAssetPropertyValueSuccessEntry' {assetPropertyValue} -> assetPropertyValue) (\s@BatchGetAssetPropertyValueSuccessEntry' {} a -> s {assetPropertyValue = a} :: BatchGetAssetPropertyValueSuccessEntry)

-- | The ID of the entry.
batchGetAssetPropertyValueSuccessEntry_entryId :: Lens.Lens' BatchGetAssetPropertyValueSuccessEntry Prelude.Text
batchGetAssetPropertyValueSuccessEntry_entryId = Lens.lens (\BatchGetAssetPropertyValueSuccessEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyValueSuccessEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyValueSuccessEntry)

instance
  Data.FromJSON
    BatchGetAssetPropertyValueSuccessEntry
  where
  parseJSON =
    Data.withObject
      "BatchGetAssetPropertyValueSuccessEntry"
      ( \x ->
          BatchGetAssetPropertyValueSuccessEntry'
            Prelude.<$> (x Data..:? "assetPropertyValue")
            Prelude.<*> (x Data..: "entryId")
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyValueSuccessEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyValueSuccessEntry' {..} =
      _salt
        `Prelude.hashWithSalt` assetPropertyValue
        `Prelude.hashWithSalt` entryId

instance
  Prelude.NFData
    BatchGetAssetPropertyValueSuccessEntry
  where
  rnf BatchGetAssetPropertyValueSuccessEntry' {..} =
    Prelude.rnf assetPropertyValue
      `Prelude.seq` Prelude.rnf entryId
