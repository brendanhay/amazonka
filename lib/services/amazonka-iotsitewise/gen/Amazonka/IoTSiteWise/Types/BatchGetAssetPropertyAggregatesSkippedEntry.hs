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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesSkippedEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesSkippedEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.BatchEntryCompletionStatus
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesErrorInfo
import qualified Amazonka.Prelude as Prelude

-- | Contains information for an entry that has been processed by the
-- previous
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyAggregates.html BatchGetAssetPropertyAggregates>
-- request.
--
-- /See:/ 'newBatchGetAssetPropertyAggregatesSkippedEntry' smart constructor.
data BatchGetAssetPropertyAggregatesSkippedEntry = BatchGetAssetPropertyAggregatesSkippedEntry'
  { -- | The error information, such as the error code and the timestamp.
    errorInfo :: Prelude.Maybe BatchGetAssetPropertyAggregatesErrorInfo,
    -- | The ID of the entry.
    entryId :: Prelude.Text,
    -- | The completion status of each entry that is associated with the
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyAggregates.html BatchGetAssetPropertyAggregates>
    -- API.
    completionStatus :: BatchEntryCompletionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyAggregatesSkippedEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorInfo', 'batchGetAssetPropertyAggregatesSkippedEntry_errorInfo' - The error information, such as the error code and the timestamp.
--
-- 'entryId', 'batchGetAssetPropertyAggregatesSkippedEntry_entryId' - The ID of the entry.
--
-- 'completionStatus', 'batchGetAssetPropertyAggregatesSkippedEntry_completionStatus' - The completion status of each entry that is associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyAggregates.html BatchGetAssetPropertyAggregates>
-- API.
newBatchGetAssetPropertyAggregatesSkippedEntry ::
  -- | 'entryId'
  Prelude.Text ->
  -- | 'completionStatus'
  BatchEntryCompletionStatus ->
  BatchGetAssetPropertyAggregatesSkippedEntry
newBatchGetAssetPropertyAggregatesSkippedEntry
  pEntryId_
  pCompletionStatus_ =
    BatchGetAssetPropertyAggregatesSkippedEntry'
      { errorInfo =
          Prelude.Nothing,
        entryId = pEntryId_,
        completionStatus =
          pCompletionStatus_
      }

-- | The error information, such as the error code and the timestamp.
batchGetAssetPropertyAggregatesSkippedEntry_errorInfo :: Lens.Lens' BatchGetAssetPropertyAggregatesSkippedEntry (Prelude.Maybe BatchGetAssetPropertyAggregatesErrorInfo)
batchGetAssetPropertyAggregatesSkippedEntry_errorInfo = Lens.lens (\BatchGetAssetPropertyAggregatesSkippedEntry' {errorInfo} -> errorInfo) (\s@BatchGetAssetPropertyAggregatesSkippedEntry' {} a -> s {errorInfo = a} :: BatchGetAssetPropertyAggregatesSkippedEntry)

-- | The ID of the entry.
batchGetAssetPropertyAggregatesSkippedEntry_entryId :: Lens.Lens' BatchGetAssetPropertyAggregatesSkippedEntry Prelude.Text
batchGetAssetPropertyAggregatesSkippedEntry_entryId = Lens.lens (\BatchGetAssetPropertyAggregatesSkippedEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyAggregatesSkippedEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyAggregatesSkippedEntry)

-- | The completion status of each entry that is associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyAggregates.html BatchGetAssetPropertyAggregates>
-- API.
batchGetAssetPropertyAggregatesSkippedEntry_completionStatus :: Lens.Lens' BatchGetAssetPropertyAggregatesSkippedEntry BatchEntryCompletionStatus
batchGetAssetPropertyAggregatesSkippedEntry_completionStatus = Lens.lens (\BatchGetAssetPropertyAggregatesSkippedEntry' {completionStatus} -> completionStatus) (\s@BatchGetAssetPropertyAggregatesSkippedEntry' {} a -> s {completionStatus = a} :: BatchGetAssetPropertyAggregatesSkippedEntry)

instance
  Data.FromJSON
    BatchGetAssetPropertyAggregatesSkippedEntry
  where
  parseJSON =
    Data.withObject
      "BatchGetAssetPropertyAggregatesSkippedEntry"
      ( \x ->
          BatchGetAssetPropertyAggregatesSkippedEntry'
            Prelude.<$> (x Data..:? "errorInfo")
            Prelude.<*> (x Data..: "entryId")
            Prelude.<*> (x Data..: "completionStatus")
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyAggregatesSkippedEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyAggregatesSkippedEntry' {..} =
      _salt
        `Prelude.hashWithSalt` errorInfo
        `Prelude.hashWithSalt` entryId
        `Prelude.hashWithSalt` completionStatus

instance
  Prelude.NFData
    BatchGetAssetPropertyAggregatesSkippedEntry
  where
  rnf BatchGetAssetPropertyAggregatesSkippedEntry' {..} =
    Prelude.rnf errorInfo `Prelude.seq`
      Prelude.rnf entryId `Prelude.seq`
        Prelude.rnf completionStatus
