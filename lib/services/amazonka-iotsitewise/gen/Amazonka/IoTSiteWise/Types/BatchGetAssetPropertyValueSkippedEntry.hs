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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueSkippedEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueSkippedEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.BatchEntryCompletionStatus
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueErrorInfo
import qualified Amazonka.Prelude as Prelude

-- | Contains information for an entry that has been processed by the
-- previous
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValue.html BatchGetAssetPropertyValue>
-- request.
--
-- /See:/ 'newBatchGetAssetPropertyValueSkippedEntry' smart constructor.
data BatchGetAssetPropertyValueSkippedEntry = BatchGetAssetPropertyValueSkippedEntry'
  { -- | The error information, such as the error code and the timestamp.
    errorInfo :: Prelude.Maybe BatchGetAssetPropertyValueErrorInfo,
    -- | The ID of the entry.
    entryId :: Prelude.Text,
    -- | The completion status of each entry that is associated with the
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValue.html BatchGetAssetPropertyValue>
    -- request.
    completionStatus :: BatchEntryCompletionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueSkippedEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorInfo', 'batchGetAssetPropertyValueSkippedEntry_errorInfo' - The error information, such as the error code and the timestamp.
--
-- 'entryId', 'batchGetAssetPropertyValueSkippedEntry_entryId' - The ID of the entry.
--
-- 'completionStatus', 'batchGetAssetPropertyValueSkippedEntry_completionStatus' - The completion status of each entry that is associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValue.html BatchGetAssetPropertyValue>
-- request.
newBatchGetAssetPropertyValueSkippedEntry ::
  -- | 'entryId'
  Prelude.Text ->
  -- | 'completionStatus'
  BatchEntryCompletionStatus ->
  BatchGetAssetPropertyValueSkippedEntry
newBatchGetAssetPropertyValueSkippedEntry
  pEntryId_
  pCompletionStatus_ =
    BatchGetAssetPropertyValueSkippedEntry'
      { errorInfo =
          Prelude.Nothing,
        entryId = pEntryId_,
        completionStatus =
          pCompletionStatus_
      }

-- | The error information, such as the error code and the timestamp.
batchGetAssetPropertyValueSkippedEntry_errorInfo :: Lens.Lens' BatchGetAssetPropertyValueSkippedEntry (Prelude.Maybe BatchGetAssetPropertyValueErrorInfo)
batchGetAssetPropertyValueSkippedEntry_errorInfo = Lens.lens (\BatchGetAssetPropertyValueSkippedEntry' {errorInfo} -> errorInfo) (\s@BatchGetAssetPropertyValueSkippedEntry' {} a -> s {errorInfo = a} :: BatchGetAssetPropertyValueSkippedEntry)

-- | The ID of the entry.
batchGetAssetPropertyValueSkippedEntry_entryId :: Lens.Lens' BatchGetAssetPropertyValueSkippedEntry Prelude.Text
batchGetAssetPropertyValueSkippedEntry_entryId = Lens.lens (\BatchGetAssetPropertyValueSkippedEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyValueSkippedEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyValueSkippedEntry)

-- | The completion status of each entry that is associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValue.html BatchGetAssetPropertyValue>
-- request.
batchGetAssetPropertyValueSkippedEntry_completionStatus :: Lens.Lens' BatchGetAssetPropertyValueSkippedEntry BatchEntryCompletionStatus
batchGetAssetPropertyValueSkippedEntry_completionStatus = Lens.lens (\BatchGetAssetPropertyValueSkippedEntry' {completionStatus} -> completionStatus) (\s@BatchGetAssetPropertyValueSkippedEntry' {} a -> s {completionStatus = a} :: BatchGetAssetPropertyValueSkippedEntry)

instance
  Data.FromJSON
    BatchGetAssetPropertyValueSkippedEntry
  where
  parseJSON =
    Data.withObject
      "BatchGetAssetPropertyValueSkippedEntry"
      ( \x ->
          BatchGetAssetPropertyValueSkippedEntry'
            Prelude.<$> (x Data..:? "errorInfo")
            Prelude.<*> (x Data..: "entryId")
            Prelude.<*> (x Data..: "completionStatus")
      )

instance
  Prelude.Hashable
    BatchGetAssetPropertyValueSkippedEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyValueSkippedEntry' {..} =
      _salt
        `Prelude.hashWithSalt` errorInfo
        `Prelude.hashWithSalt` entryId
        `Prelude.hashWithSalt` completionStatus

instance
  Prelude.NFData
    BatchGetAssetPropertyValueSkippedEntry
  where
  rnf BatchGetAssetPropertyValueSkippedEntry' {..} =
    Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf entryId
      `Prelude.seq` Prelude.rnf completionStatus
