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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistorySkippedEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistorySkippedEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.BatchEntryCompletionStatus
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryErrorInfo
import qualified Amazonka.Prelude as Prelude

-- | Contains information for an entry that has been processed by the
-- previous
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValue.html BatchGetAssetPropertyValueHistory>
-- request.
--
-- /See:/ 'newBatchGetAssetPropertyValueHistorySkippedEntry' smart constructor.
data BatchGetAssetPropertyValueHistorySkippedEntry = BatchGetAssetPropertyValueHistorySkippedEntry'
  { -- | The error information, such as the error code and the timestamp.
    errorInfo :: Prelude.Maybe BatchGetAssetPropertyValueHistoryErrorInfo,
    -- | The ID of the entry.
    entryId :: Prelude.Text,
    -- | The completion status of each entry that is associated with the
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValueHistory.html BatchGetAssetPropertyValueHistory>
    -- API.
    completionStatus :: BatchEntryCompletionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueHistorySkippedEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorInfo', 'batchGetAssetPropertyValueHistorySkippedEntry_errorInfo' - The error information, such as the error code and the timestamp.
--
-- 'entryId', 'batchGetAssetPropertyValueHistorySkippedEntry_entryId' - The ID of the entry.
--
-- 'completionStatus', 'batchGetAssetPropertyValueHistorySkippedEntry_completionStatus' - The completion status of each entry that is associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValueHistory.html BatchGetAssetPropertyValueHistory>
-- API.
newBatchGetAssetPropertyValueHistorySkippedEntry ::
  -- | 'entryId'
  Prelude.Text ->
  -- | 'completionStatus'
  BatchEntryCompletionStatus ->
  BatchGetAssetPropertyValueHistorySkippedEntry
newBatchGetAssetPropertyValueHistorySkippedEntry
  pEntryId_
  pCompletionStatus_ =
    BatchGetAssetPropertyValueHistorySkippedEntry'
      { errorInfo =
          Prelude.Nothing,
        entryId = pEntryId_,
        completionStatus =
          pCompletionStatus_
      }

-- | The error information, such as the error code and the timestamp.
batchGetAssetPropertyValueHistorySkippedEntry_errorInfo :: Lens.Lens' BatchGetAssetPropertyValueHistorySkippedEntry (Prelude.Maybe BatchGetAssetPropertyValueHistoryErrorInfo)
batchGetAssetPropertyValueHistorySkippedEntry_errorInfo = Lens.lens (\BatchGetAssetPropertyValueHistorySkippedEntry' {errorInfo} -> errorInfo) (\s@BatchGetAssetPropertyValueHistorySkippedEntry' {} a -> s {errorInfo = a} :: BatchGetAssetPropertyValueHistorySkippedEntry)

-- | The ID of the entry.
batchGetAssetPropertyValueHistorySkippedEntry_entryId :: Lens.Lens' BatchGetAssetPropertyValueHistorySkippedEntry Prelude.Text
batchGetAssetPropertyValueHistorySkippedEntry_entryId = Lens.lens (\BatchGetAssetPropertyValueHistorySkippedEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyValueHistorySkippedEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyValueHistorySkippedEntry)

-- | The completion status of each entry that is associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValueHistory.html BatchGetAssetPropertyValueHistory>
-- API.
batchGetAssetPropertyValueHistorySkippedEntry_completionStatus :: Lens.Lens' BatchGetAssetPropertyValueHistorySkippedEntry BatchEntryCompletionStatus
batchGetAssetPropertyValueHistorySkippedEntry_completionStatus = Lens.lens (\BatchGetAssetPropertyValueHistorySkippedEntry' {completionStatus} -> completionStatus) (\s@BatchGetAssetPropertyValueHistorySkippedEntry' {} a -> s {completionStatus = a} :: BatchGetAssetPropertyValueHistorySkippedEntry)

instance
  Data.FromJSON
    BatchGetAssetPropertyValueHistorySkippedEntry
  where
  parseJSON =
    Data.withObject
      "BatchGetAssetPropertyValueHistorySkippedEntry"
      ( \x ->
          BatchGetAssetPropertyValueHistorySkippedEntry'
            Prelude.<$> (x Data..:? "errorInfo")
              Prelude.<*> (x Data..: "entryId")
              Prelude.<*> (x Data..: "completionStatus")
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyValueHistorySkippedEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyValueHistorySkippedEntry' {..} =
      _salt `Prelude.hashWithSalt` errorInfo
        `Prelude.hashWithSalt` entryId
        `Prelude.hashWithSalt` completionStatus

instance
  Prelude.NFData
    BatchGetAssetPropertyValueHistorySkippedEntry
  where
  rnf
    BatchGetAssetPropertyValueHistorySkippedEntry' {..} =
      Prelude.rnf errorInfo
        `Prelude.seq` Prelude.rnf entryId
        `Prelude.seq` Prelude.rnf completionStatus
