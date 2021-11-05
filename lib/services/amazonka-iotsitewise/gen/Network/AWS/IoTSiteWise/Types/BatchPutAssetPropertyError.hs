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
-- Module      : Network.AWS.IoTSiteWise.Types.BatchPutAssetPropertyError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Types.BatchPutAssetPropertyError where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTSiteWise.Types.BatchPutAssetPropertyValueErrorCode
import Network.AWS.IoTSiteWise.Types.TimeInNanos
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON BatchPutAssetPropertyError where
  parseJSON =
    Core.withObject
      "BatchPutAssetPropertyError"
      ( \x ->
          BatchPutAssetPropertyError'
            Prelude.<$> (x Core..: "errorCode")
            Prelude.<*> (x Core..: "errorMessage")
            Prelude.<*> (x Core..:? "timestamps" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable BatchPutAssetPropertyError

instance Prelude.NFData BatchPutAssetPropertyError
