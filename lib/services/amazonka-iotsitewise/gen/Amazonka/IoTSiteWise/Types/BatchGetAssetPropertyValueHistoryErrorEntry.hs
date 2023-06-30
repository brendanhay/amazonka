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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryErrorEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryErrorCode
import qualified Amazonka.Prelude as Prelude

-- | A list of the errors (if any) associated with the batch request. Each
-- error entry contains the @entryId@ of the entry that failed.
--
-- /See:/ 'newBatchGetAssetPropertyValueHistoryErrorEntry' smart constructor.
data BatchGetAssetPropertyValueHistoryErrorEntry = BatchGetAssetPropertyValueHistoryErrorEntry'
  { -- | The error code.
    errorCode :: BatchGetAssetPropertyValueHistoryErrorCode,
    -- | The associated error message.
    errorMessage :: Prelude.Text,
    -- | The ID of the entry.
    entryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueHistoryErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchGetAssetPropertyValueHistoryErrorEntry_errorCode' - The error code.
--
-- 'errorMessage', 'batchGetAssetPropertyValueHistoryErrorEntry_errorMessage' - The associated error message.
--
-- 'entryId', 'batchGetAssetPropertyValueHistoryErrorEntry_entryId' - The ID of the entry.
newBatchGetAssetPropertyValueHistoryErrorEntry ::
  -- | 'errorCode'
  BatchGetAssetPropertyValueHistoryErrorCode ->
  -- | 'errorMessage'
  Prelude.Text ->
  -- | 'entryId'
  Prelude.Text ->
  BatchGetAssetPropertyValueHistoryErrorEntry
newBatchGetAssetPropertyValueHistoryErrorEntry
  pErrorCode_
  pErrorMessage_
  pEntryId_ =
    BatchGetAssetPropertyValueHistoryErrorEntry'
      { errorCode =
          pErrorCode_,
        errorMessage = pErrorMessage_,
        entryId = pEntryId_
      }

-- | The error code.
batchGetAssetPropertyValueHistoryErrorEntry_errorCode :: Lens.Lens' BatchGetAssetPropertyValueHistoryErrorEntry BatchGetAssetPropertyValueHistoryErrorCode
batchGetAssetPropertyValueHistoryErrorEntry_errorCode = Lens.lens (\BatchGetAssetPropertyValueHistoryErrorEntry' {errorCode} -> errorCode) (\s@BatchGetAssetPropertyValueHistoryErrorEntry' {} a -> s {errorCode = a} :: BatchGetAssetPropertyValueHistoryErrorEntry)

-- | The associated error message.
batchGetAssetPropertyValueHistoryErrorEntry_errorMessage :: Lens.Lens' BatchGetAssetPropertyValueHistoryErrorEntry Prelude.Text
batchGetAssetPropertyValueHistoryErrorEntry_errorMessage = Lens.lens (\BatchGetAssetPropertyValueHistoryErrorEntry' {errorMessage} -> errorMessage) (\s@BatchGetAssetPropertyValueHistoryErrorEntry' {} a -> s {errorMessage = a} :: BatchGetAssetPropertyValueHistoryErrorEntry)

-- | The ID of the entry.
batchGetAssetPropertyValueHistoryErrorEntry_entryId :: Lens.Lens' BatchGetAssetPropertyValueHistoryErrorEntry Prelude.Text
batchGetAssetPropertyValueHistoryErrorEntry_entryId = Lens.lens (\BatchGetAssetPropertyValueHistoryErrorEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyValueHistoryErrorEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyValueHistoryErrorEntry)

instance
  Data.FromJSON
    BatchGetAssetPropertyValueHistoryErrorEntry
  where
  parseJSON =
    Data.withObject
      "BatchGetAssetPropertyValueHistoryErrorEntry"
      ( \x ->
          BatchGetAssetPropertyValueHistoryErrorEntry'
            Prelude.<$> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "errorMessage")
            Prelude.<*> (x Data..: "entryId")
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyValueHistoryErrorEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyValueHistoryErrorEntry' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` entryId

instance
  Prelude.NFData
    BatchGetAssetPropertyValueHistoryErrorEntry
  where
  rnf BatchGetAssetPropertyValueHistoryErrorEntry' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf entryId
