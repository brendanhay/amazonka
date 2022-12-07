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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueErrorEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Contains error information for an asset property value entry that is
-- associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValue.html BatchGetAssetPropertyValue>
-- API.
--
-- /See:/ 'newBatchGetAssetPropertyValueErrorEntry' smart constructor.
data BatchGetAssetPropertyValueErrorEntry = BatchGetAssetPropertyValueErrorEntry'
  { -- | The error code.
    errorCode :: BatchGetAssetPropertyValueErrorCode,
    -- | The associated error message.
    errorMessage :: Prelude.Text,
    -- | The ID of the entry.
    entryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchGetAssetPropertyValueErrorEntry_errorCode' - The error code.
--
-- 'errorMessage', 'batchGetAssetPropertyValueErrorEntry_errorMessage' - The associated error message.
--
-- 'entryId', 'batchGetAssetPropertyValueErrorEntry_entryId' - The ID of the entry.
newBatchGetAssetPropertyValueErrorEntry ::
  -- | 'errorCode'
  BatchGetAssetPropertyValueErrorCode ->
  -- | 'errorMessage'
  Prelude.Text ->
  -- | 'entryId'
  Prelude.Text ->
  BatchGetAssetPropertyValueErrorEntry
newBatchGetAssetPropertyValueErrorEntry
  pErrorCode_
  pErrorMessage_
  pEntryId_ =
    BatchGetAssetPropertyValueErrorEntry'
      { errorCode =
          pErrorCode_,
        errorMessage = pErrorMessage_,
        entryId = pEntryId_
      }

-- | The error code.
batchGetAssetPropertyValueErrorEntry_errorCode :: Lens.Lens' BatchGetAssetPropertyValueErrorEntry BatchGetAssetPropertyValueErrorCode
batchGetAssetPropertyValueErrorEntry_errorCode = Lens.lens (\BatchGetAssetPropertyValueErrorEntry' {errorCode} -> errorCode) (\s@BatchGetAssetPropertyValueErrorEntry' {} a -> s {errorCode = a} :: BatchGetAssetPropertyValueErrorEntry)

-- | The associated error message.
batchGetAssetPropertyValueErrorEntry_errorMessage :: Lens.Lens' BatchGetAssetPropertyValueErrorEntry Prelude.Text
batchGetAssetPropertyValueErrorEntry_errorMessage = Lens.lens (\BatchGetAssetPropertyValueErrorEntry' {errorMessage} -> errorMessage) (\s@BatchGetAssetPropertyValueErrorEntry' {} a -> s {errorMessage = a} :: BatchGetAssetPropertyValueErrorEntry)

-- | The ID of the entry.
batchGetAssetPropertyValueErrorEntry_entryId :: Lens.Lens' BatchGetAssetPropertyValueErrorEntry Prelude.Text
batchGetAssetPropertyValueErrorEntry_entryId = Lens.lens (\BatchGetAssetPropertyValueErrorEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyValueErrorEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyValueErrorEntry)

instance
  Data.FromJSON
    BatchGetAssetPropertyValueErrorEntry
  where
  parseJSON =
    Data.withObject
      "BatchGetAssetPropertyValueErrorEntry"
      ( \x ->
          BatchGetAssetPropertyValueErrorEntry'
            Prelude.<$> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "errorMessage")
            Prelude.<*> (x Data..: "entryId")
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyValueErrorEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyValueErrorEntry' {..} =
      _salt `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` entryId

instance
  Prelude.NFData
    BatchGetAssetPropertyValueErrorEntry
  where
  rnf BatchGetAssetPropertyValueErrorEntry' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf entryId
