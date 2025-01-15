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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryErrorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryErrorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryErrorCode
import qualified Amazonka.Prelude as Prelude

-- | The error information, such as the error code and the timestamp.
--
-- /See:/ 'newBatchGetAssetPropertyValueHistoryErrorInfo' smart constructor.
data BatchGetAssetPropertyValueHistoryErrorInfo = BatchGetAssetPropertyValueHistoryErrorInfo'
  { -- | The error code.
    errorCode :: BatchGetAssetPropertyValueHistoryErrorCode,
    -- | The date the error occurred, in Unix epoch time.
    errorTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueHistoryErrorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchGetAssetPropertyValueHistoryErrorInfo_errorCode' - The error code.
--
-- 'errorTimestamp', 'batchGetAssetPropertyValueHistoryErrorInfo_errorTimestamp' - The date the error occurred, in Unix epoch time.
newBatchGetAssetPropertyValueHistoryErrorInfo ::
  -- | 'errorCode'
  BatchGetAssetPropertyValueHistoryErrorCode ->
  -- | 'errorTimestamp'
  Prelude.UTCTime ->
  BatchGetAssetPropertyValueHistoryErrorInfo
newBatchGetAssetPropertyValueHistoryErrorInfo
  pErrorCode_
  pErrorTimestamp_ =
    BatchGetAssetPropertyValueHistoryErrorInfo'
      { errorCode =
          pErrorCode_,
        errorTimestamp =
          Data._Time
            Lens.# pErrorTimestamp_
      }

-- | The error code.
batchGetAssetPropertyValueHistoryErrorInfo_errorCode :: Lens.Lens' BatchGetAssetPropertyValueHistoryErrorInfo BatchGetAssetPropertyValueHistoryErrorCode
batchGetAssetPropertyValueHistoryErrorInfo_errorCode = Lens.lens (\BatchGetAssetPropertyValueHistoryErrorInfo' {errorCode} -> errorCode) (\s@BatchGetAssetPropertyValueHistoryErrorInfo' {} a -> s {errorCode = a} :: BatchGetAssetPropertyValueHistoryErrorInfo)

-- | The date the error occurred, in Unix epoch time.
batchGetAssetPropertyValueHistoryErrorInfo_errorTimestamp :: Lens.Lens' BatchGetAssetPropertyValueHistoryErrorInfo Prelude.UTCTime
batchGetAssetPropertyValueHistoryErrorInfo_errorTimestamp = Lens.lens (\BatchGetAssetPropertyValueHistoryErrorInfo' {errorTimestamp} -> errorTimestamp) (\s@BatchGetAssetPropertyValueHistoryErrorInfo' {} a -> s {errorTimestamp = a} :: BatchGetAssetPropertyValueHistoryErrorInfo) Prelude.. Data._Time

instance
  Data.FromJSON
    BatchGetAssetPropertyValueHistoryErrorInfo
  where
  parseJSON =
    Data.withObject
      "BatchGetAssetPropertyValueHistoryErrorInfo"
      ( \x ->
          BatchGetAssetPropertyValueHistoryErrorInfo'
            Prelude.<$> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "errorTimestamp")
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyValueHistoryErrorInfo
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyValueHistoryErrorInfo' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorTimestamp

instance
  Prelude.NFData
    BatchGetAssetPropertyValueHistoryErrorInfo
  where
  rnf BatchGetAssetPropertyValueHistoryErrorInfo' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf errorTimestamp
