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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueErrorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueErrorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueErrorCode
import qualified Amazonka.Prelude as Prelude

-- | The error information, such as the error code and the timestamp.
--
-- /See:/ 'newBatchGetAssetPropertyValueErrorInfo' smart constructor.
data BatchGetAssetPropertyValueErrorInfo = BatchGetAssetPropertyValueErrorInfo'
  { -- | The error code.
    errorCode :: BatchGetAssetPropertyValueErrorCode,
    -- | The date the error occurred, in Unix epoch time.
    errorTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueErrorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchGetAssetPropertyValueErrorInfo_errorCode' - The error code.
--
-- 'errorTimestamp', 'batchGetAssetPropertyValueErrorInfo_errorTimestamp' - The date the error occurred, in Unix epoch time.
newBatchGetAssetPropertyValueErrorInfo ::
  -- | 'errorCode'
  BatchGetAssetPropertyValueErrorCode ->
  -- | 'errorTimestamp'
  Prelude.UTCTime ->
  BatchGetAssetPropertyValueErrorInfo
newBatchGetAssetPropertyValueErrorInfo
  pErrorCode_
  pErrorTimestamp_ =
    BatchGetAssetPropertyValueErrorInfo'
      { errorCode =
          pErrorCode_,
        errorTimestamp =
          Data._Time Lens.# pErrorTimestamp_
      }

-- | The error code.
batchGetAssetPropertyValueErrorInfo_errorCode :: Lens.Lens' BatchGetAssetPropertyValueErrorInfo BatchGetAssetPropertyValueErrorCode
batchGetAssetPropertyValueErrorInfo_errorCode = Lens.lens (\BatchGetAssetPropertyValueErrorInfo' {errorCode} -> errorCode) (\s@BatchGetAssetPropertyValueErrorInfo' {} a -> s {errorCode = a} :: BatchGetAssetPropertyValueErrorInfo)

-- | The date the error occurred, in Unix epoch time.
batchGetAssetPropertyValueErrorInfo_errorTimestamp :: Lens.Lens' BatchGetAssetPropertyValueErrorInfo Prelude.UTCTime
batchGetAssetPropertyValueErrorInfo_errorTimestamp = Lens.lens (\BatchGetAssetPropertyValueErrorInfo' {errorTimestamp} -> errorTimestamp) (\s@BatchGetAssetPropertyValueErrorInfo' {} a -> s {errorTimestamp = a} :: BatchGetAssetPropertyValueErrorInfo) Prelude.. Data._Time

instance
  Data.FromJSON
    BatchGetAssetPropertyValueErrorInfo
  where
  parseJSON =
    Data.withObject
      "BatchGetAssetPropertyValueErrorInfo"
      ( \x ->
          BatchGetAssetPropertyValueErrorInfo'
            Prelude.<$> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "errorTimestamp")
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyValueErrorInfo
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyValueErrorInfo' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorTimestamp

instance
  Prelude.NFData
    BatchGetAssetPropertyValueErrorInfo
  where
  rnf BatchGetAssetPropertyValueErrorInfo' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf errorTimestamp
