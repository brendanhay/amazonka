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
-- Module      : Amazonka.Config.Types.AggregatedSourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregatedSourceStatus where

import Amazonka.Config.Types.AggregatedSourceStatusType
import Amazonka.Config.Types.AggregatedSourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current sync status between the source and the aggregator account.
--
-- /See:/ 'newAggregatedSourceStatus' smart constructor.
data AggregatedSourceStatus = AggregatedSourceStatus'
  { -- | The region authorized to collect aggregated data.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The error code that Config returned when the source account aggregation
    -- last failed.
    lastErrorCode :: Prelude.Maybe Prelude.Text,
    -- | The message indicating that the source account aggregation failed due to
    -- an error.
    lastErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | Filters the last updated status type.
    --
    -- -   Valid value FAILED indicates errors while moving data.
    --
    -- -   Valid value SUCCEEDED indicates the data was successfully moved.
    --
    -- -   Valid value OUTDATED indicates the data is not the most recent.
    lastUpdateStatus :: Prelude.Maybe AggregatedSourceStatusType,
    -- | The time of the last update.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The source account ID or an organization.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The source account or an organization.
    sourceType :: Prelude.Maybe AggregatedSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregatedSourceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsRegion', 'aggregatedSourceStatus_awsRegion' - The region authorized to collect aggregated data.
--
-- 'lastErrorCode', 'aggregatedSourceStatus_lastErrorCode' - The error code that Config returned when the source account aggregation
-- last failed.
--
-- 'lastErrorMessage', 'aggregatedSourceStatus_lastErrorMessage' - The message indicating that the source account aggregation failed due to
-- an error.
--
-- 'lastUpdateStatus', 'aggregatedSourceStatus_lastUpdateStatus' - Filters the last updated status type.
--
-- -   Valid value FAILED indicates errors while moving data.
--
-- -   Valid value SUCCEEDED indicates the data was successfully moved.
--
-- -   Valid value OUTDATED indicates the data is not the most recent.
--
-- 'lastUpdateTime', 'aggregatedSourceStatus_lastUpdateTime' - The time of the last update.
--
-- 'sourceId', 'aggregatedSourceStatus_sourceId' - The source account ID or an organization.
--
-- 'sourceType', 'aggregatedSourceStatus_sourceType' - The source account or an organization.
newAggregatedSourceStatus ::
  AggregatedSourceStatus
newAggregatedSourceStatus =
  AggregatedSourceStatus'
    { awsRegion =
        Prelude.Nothing,
      lastErrorCode = Prelude.Nothing,
      lastErrorMessage = Prelude.Nothing,
      lastUpdateStatus = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The region authorized to collect aggregated data.
aggregatedSourceStatus_awsRegion :: Lens.Lens' AggregatedSourceStatus (Prelude.Maybe Prelude.Text)
aggregatedSourceStatus_awsRegion = Lens.lens (\AggregatedSourceStatus' {awsRegion} -> awsRegion) (\s@AggregatedSourceStatus' {} a -> s {awsRegion = a} :: AggregatedSourceStatus)

-- | The error code that Config returned when the source account aggregation
-- last failed.
aggregatedSourceStatus_lastErrorCode :: Lens.Lens' AggregatedSourceStatus (Prelude.Maybe Prelude.Text)
aggregatedSourceStatus_lastErrorCode = Lens.lens (\AggregatedSourceStatus' {lastErrorCode} -> lastErrorCode) (\s@AggregatedSourceStatus' {} a -> s {lastErrorCode = a} :: AggregatedSourceStatus)

-- | The message indicating that the source account aggregation failed due to
-- an error.
aggregatedSourceStatus_lastErrorMessage :: Lens.Lens' AggregatedSourceStatus (Prelude.Maybe Prelude.Text)
aggregatedSourceStatus_lastErrorMessage = Lens.lens (\AggregatedSourceStatus' {lastErrorMessage} -> lastErrorMessage) (\s@AggregatedSourceStatus' {} a -> s {lastErrorMessage = a} :: AggregatedSourceStatus)

-- | Filters the last updated status type.
--
-- -   Valid value FAILED indicates errors while moving data.
--
-- -   Valid value SUCCEEDED indicates the data was successfully moved.
--
-- -   Valid value OUTDATED indicates the data is not the most recent.
aggregatedSourceStatus_lastUpdateStatus :: Lens.Lens' AggregatedSourceStatus (Prelude.Maybe AggregatedSourceStatusType)
aggregatedSourceStatus_lastUpdateStatus = Lens.lens (\AggregatedSourceStatus' {lastUpdateStatus} -> lastUpdateStatus) (\s@AggregatedSourceStatus' {} a -> s {lastUpdateStatus = a} :: AggregatedSourceStatus)

-- | The time of the last update.
aggregatedSourceStatus_lastUpdateTime :: Lens.Lens' AggregatedSourceStatus (Prelude.Maybe Prelude.UTCTime)
aggregatedSourceStatus_lastUpdateTime = Lens.lens (\AggregatedSourceStatus' {lastUpdateTime} -> lastUpdateTime) (\s@AggregatedSourceStatus' {} a -> s {lastUpdateTime = a} :: AggregatedSourceStatus) Prelude.. Lens.mapping Data._Time

-- | The source account ID or an organization.
aggregatedSourceStatus_sourceId :: Lens.Lens' AggregatedSourceStatus (Prelude.Maybe Prelude.Text)
aggregatedSourceStatus_sourceId = Lens.lens (\AggregatedSourceStatus' {sourceId} -> sourceId) (\s@AggregatedSourceStatus' {} a -> s {sourceId = a} :: AggregatedSourceStatus)

-- | The source account or an organization.
aggregatedSourceStatus_sourceType :: Lens.Lens' AggregatedSourceStatus (Prelude.Maybe AggregatedSourceType)
aggregatedSourceStatus_sourceType = Lens.lens (\AggregatedSourceStatus' {sourceType} -> sourceType) (\s@AggregatedSourceStatus' {} a -> s {sourceType = a} :: AggregatedSourceStatus)

instance Data.FromJSON AggregatedSourceStatus where
  parseJSON =
    Data.withObject
      "AggregatedSourceStatus"
      ( \x ->
          AggregatedSourceStatus'
            Prelude.<$> (x Data..:? "AwsRegion")
            Prelude.<*> (x Data..:? "LastErrorCode")
            Prelude.<*> (x Data..:? "LastErrorMessage")
            Prelude.<*> (x Data..:? "LastUpdateStatus")
            Prelude.<*> (x Data..:? "LastUpdateTime")
            Prelude.<*> (x Data..:? "SourceId")
            Prelude.<*> (x Data..:? "SourceType")
      )

instance Prelude.Hashable AggregatedSourceStatus where
  hashWithSalt _salt AggregatedSourceStatus' {..} =
    _salt
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` lastErrorCode
      `Prelude.hashWithSalt` lastErrorMessage
      `Prelude.hashWithSalt` lastUpdateStatus
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData AggregatedSourceStatus where
  rnf AggregatedSourceStatus' {..} =
    Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf lastErrorCode
      `Prelude.seq` Prelude.rnf lastErrorMessage
      `Prelude.seq` Prelude.rnf lastUpdateStatus
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf sourceType
