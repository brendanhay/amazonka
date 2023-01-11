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
-- Module      : Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyValueErrorCode
import Amazonka.IoTSiteWise.Types.TimeInNanos
import qualified Amazonka.Prelude as Prelude

-- | Contains error information from updating a batch of asset property
-- values.
--
-- /See:/ 'newBatchPutAssetPropertyError' smart constructor.
data BatchPutAssetPropertyError = BatchPutAssetPropertyError'
  { -- | The error code.
    errorCode :: BatchPutAssetPropertyValueErrorCode,
    -- | The associated error message.
    errorMessage :: Prelude.Text,
    -- | A list of timestamps for each error, if any.
    timestamps :: [TimeInNanos]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutAssetPropertyError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchPutAssetPropertyError_errorCode' - The error code.
--
-- 'errorMessage', 'batchPutAssetPropertyError_errorMessage' - The associated error message.
--
-- 'timestamps', 'batchPutAssetPropertyError_timestamps' - A list of timestamps for each error, if any.
newBatchPutAssetPropertyError ::
  -- | 'errorCode'
  BatchPutAssetPropertyValueErrorCode ->
  -- | 'errorMessage'
  Prelude.Text ->
  BatchPutAssetPropertyError
newBatchPutAssetPropertyError
  pErrorCode_
  pErrorMessage_ =
    BatchPutAssetPropertyError'
      { errorCode =
          pErrorCode_,
        errorMessage = pErrorMessage_,
        timestamps = Prelude.mempty
      }

-- | The error code.
batchPutAssetPropertyError_errorCode :: Lens.Lens' BatchPutAssetPropertyError BatchPutAssetPropertyValueErrorCode
batchPutAssetPropertyError_errorCode = Lens.lens (\BatchPutAssetPropertyError' {errorCode} -> errorCode) (\s@BatchPutAssetPropertyError' {} a -> s {errorCode = a} :: BatchPutAssetPropertyError)

-- | The associated error message.
batchPutAssetPropertyError_errorMessage :: Lens.Lens' BatchPutAssetPropertyError Prelude.Text
batchPutAssetPropertyError_errorMessage = Lens.lens (\BatchPutAssetPropertyError' {errorMessage} -> errorMessage) (\s@BatchPutAssetPropertyError' {} a -> s {errorMessage = a} :: BatchPutAssetPropertyError)

-- | A list of timestamps for each error, if any.
batchPutAssetPropertyError_timestamps :: Lens.Lens' BatchPutAssetPropertyError [TimeInNanos]
batchPutAssetPropertyError_timestamps = Lens.lens (\BatchPutAssetPropertyError' {timestamps} -> timestamps) (\s@BatchPutAssetPropertyError' {} a -> s {timestamps = a} :: BatchPutAssetPropertyError) Prelude.. Lens.coerced

instance Data.FromJSON BatchPutAssetPropertyError where
  parseJSON =
    Data.withObject
      "BatchPutAssetPropertyError"
      ( \x ->
          BatchPutAssetPropertyError'
            Prelude.<$> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "errorMessage")
            Prelude.<*> (x Data..:? "timestamps" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable BatchPutAssetPropertyError where
  hashWithSalt _salt BatchPutAssetPropertyError' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` timestamps

instance Prelude.NFData BatchPutAssetPropertyError where
  rnf BatchPutAssetPropertyError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf timestamps
