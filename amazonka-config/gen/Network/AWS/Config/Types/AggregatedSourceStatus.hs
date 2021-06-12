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
-- Module      : Network.AWS.Config.Types.AggregatedSourceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregatedSourceStatus where

import Network.AWS.Config.Types.AggregatedSourceStatusType
import Network.AWS.Config.Types.AggregatedSourceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The current sync status between the source and the aggregator account.
--
-- /See:/ 'newAggregatedSourceStatus' smart constructor.
data AggregatedSourceStatus = AggregatedSourceStatus'
  { -- | Filters the last updated status type.
    --
    -- -   Valid value FAILED indicates errors while moving data.
    --
    -- -   Valid value SUCCEEDED indicates the data was successfully moved.
    --
    -- -   Valid value OUTDATED indicates the data is not the most recent.
    lastUpdateStatus :: Core.Maybe AggregatedSourceStatusType,
    -- | The message indicating that the source account aggregation failed due to
    -- an error.
    lastErrorMessage :: Core.Maybe Core.Text,
    -- | The time of the last update.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | The source account ID or an organization.
    sourceId :: Core.Maybe Core.Text,
    -- | The error code that AWS Config returned when the source account
    -- aggregation last failed.
    lastErrorCode :: Core.Maybe Core.Text,
    -- | The region authorized to collect aggregated data.
    awsRegion :: Core.Maybe Core.Text,
    -- | The source account or an organization.
    sourceType :: Core.Maybe AggregatedSourceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AggregatedSourceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateStatus', 'aggregatedSourceStatus_lastUpdateStatus' - Filters the last updated status type.
--
-- -   Valid value FAILED indicates errors while moving data.
--
-- -   Valid value SUCCEEDED indicates the data was successfully moved.
--
-- -   Valid value OUTDATED indicates the data is not the most recent.
--
-- 'lastErrorMessage', 'aggregatedSourceStatus_lastErrorMessage' - The message indicating that the source account aggregation failed due to
-- an error.
--
-- 'lastUpdateTime', 'aggregatedSourceStatus_lastUpdateTime' - The time of the last update.
--
-- 'sourceId', 'aggregatedSourceStatus_sourceId' - The source account ID or an organization.
--
-- 'lastErrorCode', 'aggregatedSourceStatus_lastErrorCode' - The error code that AWS Config returned when the source account
-- aggregation last failed.
--
-- 'awsRegion', 'aggregatedSourceStatus_awsRegion' - The region authorized to collect aggregated data.
--
-- 'sourceType', 'aggregatedSourceStatus_sourceType' - The source account or an organization.
newAggregatedSourceStatus ::
  AggregatedSourceStatus
newAggregatedSourceStatus =
  AggregatedSourceStatus'
    { lastUpdateStatus =
        Core.Nothing,
      lastErrorMessage = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      sourceId = Core.Nothing,
      lastErrorCode = Core.Nothing,
      awsRegion = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | Filters the last updated status type.
--
-- -   Valid value FAILED indicates errors while moving data.
--
-- -   Valid value SUCCEEDED indicates the data was successfully moved.
--
-- -   Valid value OUTDATED indicates the data is not the most recent.
aggregatedSourceStatus_lastUpdateStatus :: Lens.Lens' AggregatedSourceStatus (Core.Maybe AggregatedSourceStatusType)
aggregatedSourceStatus_lastUpdateStatus = Lens.lens (\AggregatedSourceStatus' {lastUpdateStatus} -> lastUpdateStatus) (\s@AggregatedSourceStatus' {} a -> s {lastUpdateStatus = a} :: AggregatedSourceStatus)

-- | The message indicating that the source account aggregation failed due to
-- an error.
aggregatedSourceStatus_lastErrorMessage :: Lens.Lens' AggregatedSourceStatus (Core.Maybe Core.Text)
aggregatedSourceStatus_lastErrorMessage = Lens.lens (\AggregatedSourceStatus' {lastErrorMessage} -> lastErrorMessage) (\s@AggregatedSourceStatus' {} a -> s {lastErrorMessage = a} :: AggregatedSourceStatus)

-- | The time of the last update.
aggregatedSourceStatus_lastUpdateTime :: Lens.Lens' AggregatedSourceStatus (Core.Maybe Core.UTCTime)
aggregatedSourceStatus_lastUpdateTime = Lens.lens (\AggregatedSourceStatus' {lastUpdateTime} -> lastUpdateTime) (\s@AggregatedSourceStatus' {} a -> s {lastUpdateTime = a} :: AggregatedSourceStatus) Core.. Lens.mapping Core._Time

-- | The source account ID or an organization.
aggregatedSourceStatus_sourceId :: Lens.Lens' AggregatedSourceStatus (Core.Maybe Core.Text)
aggregatedSourceStatus_sourceId = Lens.lens (\AggregatedSourceStatus' {sourceId} -> sourceId) (\s@AggregatedSourceStatus' {} a -> s {sourceId = a} :: AggregatedSourceStatus)

-- | The error code that AWS Config returned when the source account
-- aggregation last failed.
aggregatedSourceStatus_lastErrorCode :: Lens.Lens' AggregatedSourceStatus (Core.Maybe Core.Text)
aggregatedSourceStatus_lastErrorCode = Lens.lens (\AggregatedSourceStatus' {lastErrorCode} -> lastErrorCode) (\s@AggregatedSourceStatus' {} a -> s {lastErrorCode = a} :: AggregatedSourceStatus)

-- | The region authorized to collect aggregated data.
aggregatedSourceStatus_awsRegion :: Lens.Lens' AggregatedSourceStatus (Core.Maybe Core.Text)
aggregatedSourceStatus_awsRegion = Lens.lens (\AggregatedSourceStatus' {awsRegion} -> awsRegion) (\s@AggregatedSourceStatus' {} a -> s {awsRegion = a} :: AggregatedSourceStatus)

-- | The source account or an organization.
aggregatedSourceStatus_sourceType :: Lens.Lens' AggregatedSourceStatus (Core.Maybe AggregatedSourceType)
aggregatedSourceStatus_sourceType = Lens.lens (\AggregatedSourceStatus' {sourceType} -> sourceType) (\s@AggregatedSourceStatus' {} a -> s {sourceType = a} :: AggregatedSourceStatus)

instance Core.FromJSON AggregatedSourceStatus where
  parseJSON =
    Core.withObject
      "AggregatedSourceStatus"
      ( \x ->
          AggregatedSourceStatus'
            Core.<$> (x Core..:? "LastUpdateStatus")
            Core.<*> (x Core..:? "LastErrorMessage")
            Core.<*> (x Core..:? "LastUpdateTime")
            Core.<*> (x Core..:? "SourceId")
            Core.<*> (x Core..:? "LastErrorCode")
            Core.<*> (x Core..:? "AwsRegion")
            Core.<*> (x Core..:? "SourceType")
      )

instance Core.Hashable AggregatedSourceStatus

instance Core.NFData AggregatedSourceStatus
