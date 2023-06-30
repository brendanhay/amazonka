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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesErrorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesErrorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Contains the error code and the timestamp for an asset property
-- aggregate entry that is associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyAggregates.html BatchGetAssetPropertyAggregates>
-- API.
--
-- /See:/ 'newBatchGetAssetPropertyAggregatesErrorInfo' smart constructor.
data BatchGetAssetPropertyAggregatesErrorInfo = BatchGetAssetPropertyAggregatesErrorInfo'
  { -- | The error code.
    errorCode :: BatchGetAssetPropertyAggregatesErrorCode,
    -- | The date the error occurred, in Unix epoch time.
    errorTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyAggregatesErrorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchGetAssetPropertyAggregatesErrorInfo_errorCode' - The error code.
--
-- 'errorTimestamp', 'batchGetAssetPropertyAggregatesErrorInfo_errorTimestamp' - The date the error occurred, in Unix epoch time.
newBatchGetAssetPropertyAggregatesErrorInfo ::
  -- | 'errorCode'
  BatchGetAssetPropertyAggregatesErrorCode ->
  -- | 'errorTimestamp'
  Prelude.UTCTime ->
  BatchGetAssetPropertyAggregatesErrorInfo
newBatchGetAssetPropertyAggregatesErrorInfo
  pErrorCode_
  pErrorTimestamp_ =
    BatchGetAssetPropertyAggregatesErrorInfo'
      { errorCode =
          pErrorCode_,
        errorTimestamp =
          Data._Time
            Lens.# pErrorTimestamp_
      }

-- | The error code.
batchGetAssetPropertyAggregatesErrorInfo_errorCode :: Lens.Lens' BatchGetAssetPropertyAggregatesErrorInfo BatchGetAssetPropertyAggregatesErrorCode
batchGetAssetPropertyAggregatesErrorInfo_errorCode = Lens.lens (\BatchGetAssetPropertyAggregatesErrorInfo' {errorCode} -> errorCode) (\s@BatchGetAssetPropertyAggregatesErrorInfo' {} a -> s {errorCode = a} :: BatchGetAssetPropertyAggregatesErrorInfo)

-- | The date the error occurred, in Unix epoch time.
batchGetAssetPropertyAggregatesErrorInfo_errorTimestamp :: Lens.Lens' BatchGetAssetPropertyAggregatesErrorInfo Prelude.UTCTime
batchGetAssetPropertyAggregatesErrorInfo_errorTimestamp = Lens.lens (\BatchGetAssetPropertyAggregatesErrorInfo' {errorTimestamp} -> errorTimestamp) (\s@BatchGetAssetPropertyAggregatesErrorInfo' {} a -> s {errorTimestamp = a} :: BatchGetAssetPropertyAggregatesErrorInfo) Prelude.. Data._Time

instance
  Data.FromJSON
    BatchGetAssetPropertyAggregatesErrorInfo
  where
  parseJSON =
    Data.withObject
      "BatchGetAssetPropertyAggregatesErrorInfo"
      ( \x ->
          BatchGetAssetPropertyAggregatesErrorInfo'
            Prelude.<$> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "errorTimestamp")
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyAggregatesErrorInfo
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyAggregatesErrorInfo' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorTimestamp

instance
  Prelude.NFData
    BatchGetAssetPropertyAggregatesErrorInfo
  where
  rnf BatchGetAssetPropertyAggregatesErrorInfo' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorTimestamp
