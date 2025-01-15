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
-- Module      : Amazonka.FSx.Types.DataRepositoryTaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DataRepositoryTaskStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the task status showing a running total of the total number of
-- files to be processed, the number successfully processed, and the number
-- of files the task failed to process.
--
-- /See:/ 'newDataRepositoryTaskStatus' smart constructor.
data DataRepositoryTaskStatus = DataRepositoryTaskStatus'
  { -- | A running total of the number of files that the task failed to process.
    failedCount :: Prelude.Maybe Prelude.Integer,
    -- | The time at which the task status was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The total amount of data, in GiB, released by an Amazon File Cache
    -- AUTO_RELEASE_DATA task that automatically releases files from the cache.
    releasedCapacity :: Prelude.Maybe Prelude.Integer,
    -- | A running total of the number of files that the task has successfully
    -- processed.
    succeededCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of files that the task will process. While a task is
    -- executing, the sum of @SucceededCount@ plus @FailedCount@ may not equal
    -- @TotalCount@. When the task is complete, @TotalCount@ equals the sum of
    -- @SucceededCount@ plus @FailedCount@.
    totalCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataRepositoryTaskStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedCount', 'dataRepositoryTaskStatus_failedCount' - A running total of the number of files that the task failed to process.
--
-- 'lastUpdatedTime', 'dataRepositoryTaskStatus_lastUpdatedTime' - The time at which the task status was last updated.
--
-- 'releasedCapacity', 'dataRepositoryTaskStatus_releasedCapacity' - The total amount of data, in GiB, released by an Amazon File Cache
-- AUTO_RELEASE_DATA task that automatically releases files from the cache.
--
-- 'succeededCount', 'dataRepositoryTaskStatus_succeededCount' - A running total of the number of files that the task has successfully
-- processed.
--
-- 'totalCount', 'dataRepositoryTaskStatus_totalCount' - The total number of files that the task will process. While a task is
-- executing, the sum of @SucceededCount@ plus @FailedCount@ may not equal
-- @TotalCount@. When the task is complete, @TotalCount@ equals the sum of
-- @SucceededCount@ plus @FailedCount@.
newDataRepositoryTaskStatus ::
  DataRepositoryTaskStatus
newDataRepositoryTaskStatus =
  DataRepositoryTaskStatus'
    { failedCount =
        Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      releasedCapacity = Prelude.Nothing,
      succeededCount = Prelude.Nothing,
      totalCount = Prelude.Nothing
    }

-- | A running total of the number of files that the task failed to process.
dataRepositoryTaskStatus_failedCount :: Lens.Lens' DataRepositoryTaskStatus (Prelude.Maybe Prelude.Integer)
dataRepositoryTaskStatus_failedCount = Lens.lens (\DataRepositoryTaskStatus' {failedCount} -> failedCount) (\s@DataRepositoryTaskStatus' {} a -> s {failedCount = a} :: DataRepositoryTaskStatus)

-- | The time at which the task status was last updated.
dataRepositoryTaskStatus_lastUpdatedTime :: Lens.Lens' DataRepositoryTaskStatus (Prelude.Maybe Prelude.UTCTime)
dataRepositoryTaskStatus_lastUpdatedTime = Lens.lens (\DataRepositoryTaskStatus' {lastUpdatedTime} -> lastUpdatedTime) (\s@DataRepositoryTaskStatus' {} a -> s {lastUpdatedTime = a} :: DataRepositoryTaskStatus) Prelude.. Lens.mapping Data._Time

-- | The total amount of data, in GiB, released by an Amazon File Cache
-- AUTO_RELEASE_DATA task that automatically releases files from the cache.
dataRepositoryTaskStatus_releasedCapacity :: Lens.Lens' DataRepositoryTaskStatus (Prelude.Maybe Prelude.Integer)
dataRepositoryTaskStatus_releasedCapacity = Lens.lens (\DataRepositoryTaskStatus' {releasedCapacity} -> releasedCapacity) (\s@DataRepositoryTaskStatus' {} a -> s {releasedCapacity = a} :: DataRepositoryTaskStatus)

-- | A running total of the number of files that the task has successfully
-- processed.
dataRepositoryTaskStatus_succeededCount :: Lens.Lens' DataRepositoryTaskStatus (Prelude.Maybe Prelude.Integer)
dataRepositoryTaskStatus_succeededCount = Lens.lens (\DataRepositoryTaskStatus' {succeededCount} -> succeededCount) (\s@DataRepositoryTaskStatus' {} a -> s {succeededCount = a} :: DataRepositoryTaskStatus)

-- | The total number of files that the task will process. While a task is
-- executing, the sum of @SucceededCount@ plus @FailedCount@ may not equal
-- @TotalCount@. When the task is complete, @TotalCount@ equals the sum of
-- @SucceededCount@ plus @FailedCount@.
dataRepositoryTaskStatus_totalCount :: Lens.Lens' DataRepositoryTaskStatus (Prelude.Maybe Prelude.Integer)
dataRepositoryTaskStatus_totalCount = Lens.lens (\DataRepositoryTaskStatus' {totalCount} -> totalCount) (\s@DataRepositoryTaskStatus' {} a -> s {totalCount = a} :: DataRepositoryTaskStatus)

instance Data.FromJSON DataRepositoryTaskStatus where
  parseJSON =
    Data.withObject
      "DataRepositoryTaskStatus"
      ( \x ->
          DataRepositoryTaskStatus'
            Prelude.<$> (x Data..:? "FailedCount")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "ReleasedCapacity")
            Prelude.<*> (x Data..:? "SucceededCount")
            Prelude.<*> (x Data..:? "TotalCount")
      )

instance Prelude.Hashable DataRepositoryTaskStatus where
  hashWithSalt _salt DataRepositoryTaskStatus' {..} =
    _salt
      `Prelude.hashWithSalt` failedCount
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` releasedCapacity
      `Prelude.hashWithSalt` succeededCount
      `Prelude.hashWithSalt` totalCount

instance Prelude.NFData DataRepositoryTaskStatus where
  rnf DataRepositoryTaskStatus' {..} =
    Prelude.rnf failedCount `Prelude.seq`
      Prelude.rnf lastUpdatedTime `Prelude.seq`
        Prelude.rnf releasedCapacity `Prelude.seq`
          Prelude.rnf succeededCount `Prelude.seq`
            Prelude.rnf totalCount
