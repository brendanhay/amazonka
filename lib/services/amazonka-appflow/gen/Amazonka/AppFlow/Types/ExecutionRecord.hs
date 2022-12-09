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
-- Module      : Amazonka.AppFlow.Types.ExecutionRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ExecutionRecord where

import Amazonka.AppFlow.Types.ExecutionResult
import Amazonka.AppFlow.Types.ExecutionStatus
import Amazonka.AppFlow.Types.MetadataCatalogDetail
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies information about the past flow run instances for a given
-- flow.
--
-- /See:/ 'newExecutionRecord' smart constructor.
data ExecutionRecord = ExecutionRecord'
  { -- | The timestamp that indicates the last new or updated record to be
    -- transferred in the flow run.
    dataPullEndTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp that determines the first new or updated record to be
    -- transferred in the flow run.
    dataPullStartTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies the identifier of the given flow run.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | Describes the result of the given flow run.
    executionResult :: Prelude.Maybe ExecutionResult,
    -- | Specifies the flow run status and whether it is in progress, has
    -- completed successfully, or has failed.
    executionStatus :: Prelude.Maybe ExecutionStatus,
    -- | Specifies the time of the most recent update.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | Describes the metadata catalog, metadata table, and data partitions that
    -- Amazon AppFlow used for the associated flow run.
    metadataCatalogDetails :: Prelude.Maybe [MetadataCatalogDetail],
    -- | Specifies the start time of the flow run.
    startedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataPullEndTime', 'executionRecord_dataPullEndTime' - The timestamp that indicates the last new or updated record to be
-- transferred in the flow run.
--
-- 'dataPullStartTime', 'executionRecord_dataPullStartTime' - The timestamp that determines the first new or updated record to be
-- transferred in the flow run.
--
-- 'executionId', 'executionRecord_executionId' - Specifies the identifier of the given flow run.
--
-- 'executionResult', 'executionRecord_executionResult' - Describes the result of the given flow run.
--
-- 'executionStatus', 'executionRecord_executionStatus' - Specifies the flow run status and whether it is in progress, has
-- completed successfully, or has failed.
--
-- 'lastUpdatedAt', 'executionRecord_lastUpdatedAt' - Specifies the time of the most recent update.
--
-- 'metadataCatalogDetails', 'executionRecord_metadataCatalogDetails' - Describes the metadata catalog, metadata table, and data partitions that
-- Amazon AppFlow used for the associated flow run.
--
-- 'startedAt', 'executionRecord_startedAt' - Specifies the start time of the flow run.
newExecutionRecord ::
  ExecutionRecord
newExecutionRecord =
  ExecutionRecord'
    { dataPullEndTime = Prelude.Nothing,
      dataPullStartTime = Prelude.Nothing,
      executionId = Prelude.Nothing,
      executionResult = Prelude.Nothing,
      executionStatus = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      metadataCatalogDetails = Prelude.Nothing,
      startedAt = Prelude.Nothing
    }

-- | The timestamp that indicates the last new or updated record to be
-- transferred in the flow run.
executionRecord_dataPullEndTime :: Lens.Lens' ExecutionRecord (Prelude.Maybe Prelude.UTCTime)
executionRecord_dataPullEndTime = Lens.lens (\ExecutionRecord' {dataPullEndTime} -> dataPullEndTime) (\s@ExecutionRecord' {} a -> s {dataPullEndTime = a} :: ExecutionRecord) Prelude.. Lens.mapping Data._Time

-- | The timestamp that determines the first new or updated record to be
-- transferred in the flow run.
executionRecord_dataPullStartTime :: Lens.Lens' ExecutionRecord (Prelude.Maybe Prelude.UTCTime)
executionRecord_dataPullStartTime = Lens.lens (\ExecutionRecord' {dataPullStartTime} -> dataPullStartTime) (\s@ExecutionRecord' {} a -> s {dataPullStartTime = a} :: ExecutionRecord) Prelude.. Lens.mapping Data._Time

-- | Specifies the identifier of the given flow run.
executionRecord_executionId :: Lens.Lens' ExecutionRecord (Prelude.Maybe Prelude.Text)
executionRecord_executionId = Lens.lens (\ExecutionRecord' {executionId} -> executionId) (\s@ExecutionRecord' {} a -> s {executionId = a} :: ExecutionRecord)

-- | Describes the result of the given flow run.
executionRecord_executionResult :: Lens.Lens' ExecutionRecord (Prelude.Maybe ExecutionResult)
executionRecord_executionResult = Lens.lens (\ExecutionRecord' {executionResult} -> executionResult) (\s@ExecutionRecord' {} a -> s {executionResult = a} :: ExecutionRecord)

-- | Specifies the flow run status and whether it is in progress, has
-- completed successfully, or has failed.
executionRecord_executionStatus :: Lens.Lens' ExecutionRecord (Prelude.Maybe ExecutionStatus)
executionRecord_executionStatus = Lens.lens (\ExecutionRecord' {executionStatus} -> executionStatus) (\s@ExecutionRecord' {} a -> s {executionStatus = a} :: ExecutionRecord)

-- | Specifies the time of the most recent update.
executionRecord_lastUpdatedAt :: Lens.Lens' ExecutionRecord (Prelude.Maybe Prelude.UTCTime)
executionRecord_lastUpdatedAt = Lens.lens (\ExecutionRecord' {lastUpdatedAt} -> lastUpdatedAt) (\s@ExecutionRecord' {} a -> s {lastUpdatedAt = a} :: ExecutionRecord) Prelude.. Lens.mapping Data._Time

-- | Describes the metadata catalog, metadata table, and data partitions that
-- Amazon AppFlow used for the associated flow run.
executionRecord_metadataCatalogDetails :: Lens.Lens' ExecutionRecord (Prelude.Maybe [MetadataCatalogDetail])
executionRecord_metadataCatalogDetails = Lens.lens (\ExecutionRecord' {metadataCatalogDetails} -> metadataCatalogDetails) (\s@ExecutionRecord' {} a -> s {metadataCatalogDetails = a} :: ExecutionRecord) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the start time of the flow run.
executionRecord_startedAt :: Lens.Lens' ExecutionRecord (Prelude.Maybe Prelude.UTCTime)
executionRecord_startedAt = Lens.lens (\ExecutionRecord' {startedAt} -> startedAt) (\s@ExecutionRecord' {} a -> s {startedAt = a} :: ExecutionRecord) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ExecutionRecord where
  parseJSON =
    Data.withObject
      "ExecutionRecord"
      ( \x ->
          ExecutionRecord'
            Prelude.<$> (x Data..:? "dataPullEndTime")
            Prelude.<*> (x Data..:? "dataPullStartTime")
            Prelude.<*> (x Data..:? "executionId")
            Prelude.<*> (x Data..:? "executionResult")
            Prelude.<*> (x Data..:? "executionStatus")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> ( x Data..:? "metadataCatalogDetails"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "startedAt")
      )

instance Prelude.Hashable ExecutionRecord where
  hashWithSalt _salt ExecutionRecord' {..} =
    _salt `Prelude.hashWithSalt` dataPullEndTime
      `Prelude.hashWithSalt` dataPullStartTime
      `Prelude.hashWithSalt` executionId
      `Prelude.hashWithSalt` executionResult
      `Prelude.hashWithSalt` executionStatus
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` metadataCatalogDetails
      `Prelude.hashWithSalt` startedAt

instance Prelude.NFData ExecutionRecord where
  rnf ExecutionRecord' {..} =
    Prelude.rnf dataPullEndTime
      `Prelude.seq` Prelude.rnf dataPullStartTime
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf executionResult
      `Prelude.seq` Prelude.rnf executionStatus
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf metadataCatalogDetails
      `Prelude.seq` Prelude.rnf startedAt
