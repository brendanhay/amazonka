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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesErrorEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Contains error information for an asset property aggregate entry that is
-- associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyAggregates.html BatchGetAssetPropertyAggregates>
-- API.
--
-- /See:/ 'newBatchGetAssetPropertyAggregatesErrorEntry' smart constructor.
data BatchGetAssetPropertyAggregatesErrorEntry = BatchGetAssetPropertyAggregatesErrorEntry'
  { -- | The error code.
    errorCode :: BatchGetAssetPropertyAggregatesErrorCode,
    -- | The associated error message.
    errorMessage :: Prelude.Text,
    -- | The ID of the entry.
    entryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyAggregatesErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchGetAssetPropertyAggregatesErrorEntry_errorCode' - The error code.
--
-- 'errorMessage', 'batchGetAssetPropertyAggregatesErrorEntry_errorMessage' - The associated error message.
--
-- 'entryId', 'batchGetAssetPropertyAggregatesErrorEntry_entryId' - The ID of the entry.
newBatchGetAssetPropertyAggregatesErrorEntry ::
  -- | 'errorCode'
  BatchGetAssetPropertyAggregatesErrorCode ->
  -- | 'errorMessage'
  Prelude.Text ->
  -- | 'entryId'
  Prelude.Text ->
  BatchGetAssetPropertyAggregatesErrorEntry
newBatchGetAssetPropertyAggregatesErrorEntry
  pErrorCode_
  pErrorMessage_
  pEntryId_ =
    BatchGetAssetPropertyAggregatesErrorEntry'
      { errorCode =
          pErrorCode_,
        errorMessage = pErrorMessage_,
        entryId = pEntryId_
      }

-- | The error code.
batchGetAssetPropertyAggregatesErrorEntry_errorCode :: Lens.Lens' BatchGetAssetPropertyAggregatesErrorEntry BatchGetAssetPropertyAggregatesErrorCode
batchGetAssetPropertyAggregatesErrorEntry_errorCode = Lens.lens (\BatchGetAssetPropertyAggregatesErrorEntry' {errorCode} -> errorCode) (\s@BatchGetAssetPropertyAggregatesErrorEntry' {} a -> s {errorCode = a} :: BatchGetAssetPropertyAggregatesErrorEntry)

-- | The associated error message.
batchGetAssetPropertyAggregatesErrorEntry_errorMessage :: Lens.Lens' BatchGetAssetPropertyAggregatesErrorEntry Prelude.Text
batchGetAssetPropertyAggregatesErrorEntry_errorMessage = Lens.lens (\BatchGetAssetPropertyAggregatesErrorEntry' {errorMessage} -> errorMessage) (\s@BatchGetAssetPropertyAggregatesErrorEntry' {} a -> s {errorMessage = a} :: BatchGetAssetPropertyAggregatesErrorEntry)

-- | The ID of the entry.
batchGetAssetPropertyAggregatesErrorEntry_entryId :: Lens.Lens' BatchGetAssetPropertyAggregatesErrorEntry Prelude.Text
batchGetAssetPropertyAggregatesErrorEntry_entryId = Lens.lens (\BatchGetAssetPropertyAggregatesErrorEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyAggregatesErrorEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyAggregatesErrorEntry)

instance
  Data.FromJSON
    BatchGetAssetPropertyAggregatesErrorEntry
  where
  parseJSON =
    Data.withObject
      "BatchGetAssetPropertyAggregatesErrorEntry"
      ( \x ->
          BatchGetAssetPropertyAggregatesErrorEntry'
            Prelude.<$> (x Data..: "errorCode")
              Prelude.<*> (x Data..: "errorMessage")
              Prelude.<*> (x Data..: "entryId")
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyAggregatesErrorEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyAggregatesErrorEntry' {..} =
      _salt `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` entryId

instance
  Prelude.NFData
    BatchGetAssetPropertyAggregatesErrorEntry
  where
  rnf BatchGetAssetPropertyAggregatesErrorEntry' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf entryId
