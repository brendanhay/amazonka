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
-- Module      : Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyErrorEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyError
import qualified Amazonka.Prelude as Prelude

-- | Contains error information for asset property value entries that are
-- associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchPutAssetPropertyValue.html BatchPutAssetPropertyValue>
-- API.
--
-- /See:/ 'newBatchPutAssetPropertyErrorEntry' smart constructor.
data BatchPutAssetPropertyErrorEntry = BatchPutAssetPropertyErrorEntry'
  { -- | The ID of the failed entry.
    entryId :: Prelude.Text,
    -- | The list of update property value errors.
    errors :: [BatchPutAssetPropertyError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutAssetPropertyErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entryId', 'batchPutAssetPropertyErrorEntry_entryId' - The ID of the failed entry.
--
-- 'errors', 'batchPutAssetPropertyErrorEntry_errors' - The list of update property value errors.
newBatchPutAssetPropertyErrorEntry ::
  -- | 'entryId'
  Prelude.Text ->
  BatchPutAssetPropertyErrorEntry
newBatchPutAssetPropertyErrorEntry pEntryId_ =
  BatchPutAssetPropertyErrorEntry'
    { entryId =
        pEntryId_,
      errors = Prelude.mempty
    }

-- | The ID of the failed entry.
batchPutAssetPropertyErrorEntry_entryId :: Lens.Lens' BatchPutAssetPropertyErrorEntry Prelude.Text
batchPutAssetPropertyErrorEntry_entryId = Lens.lens (\BatchPutAssetPropertyErrorEntry' {entryId} -> entryId) (\s@BatchPutAssetPropertyErrorEntry' {} a -> s {entryId = a} :: BatchPutAssetPropertyErrorEntry)

-- | The list of update property value errors.
batchPutAssetPropertyErrorEntry_errors :: Lens.Lens' BatchPutAssetPropertyErrorEntry [BatchPutAssetPropertyError]
batchPutAssetPropertyErrorEntry_errors = Lens.lens (\BatchPutAssetPropertyErrorEntry' {errors} -> errors) (\s@BatchPutAssetPropertyErrorEntry' {} a -> s {errors = a} :: BatchPutAssetPropertyErrorEntry) Prelude.. Lens.coerced

instance
  Data.FromJSON
    BatchPutAssetPropertyErrorEntry
  where
  parseJSON =
    Data.withObject
      "BatchPutAssetPropertyErrorEntry"
      ( \x ->
          BatchPutAssetPropertyErrorEntry'
            Prelude.<$> (x Data..: "entryId")
            Prelude.<*> (x Data..:? "errors" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchPutAssetPropertyErrorEntry
  where
  hashWithSalt
    _salt
    BatchPutAssetPropertyErrorEntry' {..} =
      _salt
        `Prelude.hashWithSalt` entryId
        `Prelude.hashWithSalt` errors

instance
  Prelude.NFData
    BatchPutAssetPropertyErrorEntry
  where
  rnf BatchPutAssetPropertyErrorEntry' {..} =
    Prelude.rnf entryId
      `Prelude.seq` Prelude.rnf errors
