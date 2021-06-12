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
-- Module      : Network.AWS.CostExplorer.Types.AnomalyMonitor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalyMonitor where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.Expression
import Network.AWS.CostExplorer.Types.MonitorDimension
import Network.AWS.CostExplorer.Types.MonitorType
import qualified Network.AWS.Lens as Lens

-- | This object continuously inspects your account\'s cost data for
-- anomalies, based on @MonitorType@ and @MonitorSpecification@. The
-- content consists of detailed metadata and the current status of the
-- monitor object.
--
-- /See:/ 'newAnomalyMonitor' smart constructor.
data AnomalyMonitor = AnomalyMonitor'
  { -- | The date when the monitor last evaluated for anomalies.
    lastEvaluatedDate :: Core.Maybe Core.Text,
    monitorSpecification :: Core.Maybe Expression,
    -- | The date when the monitor was last updated.
    lastUpdatedDate :: Core.Maybe Core.Text,
    -- | The date when the monitor was created.
    creationDate :: Core.Maybe Core.Text,
    -- | The value for evaluated dimensions.
    dimensionalValueCount :: Core.Maybe Core.Natural,
    -- | The dimensions to evaluate.
    monitorDimension :: Core.Maybe MonitorDimension,
    -- | The Amazon Resource Name (ARN) value.
    monitorArn :: Core.Maybe Core.Text,
    -- | The name of the monitor.
    monitorName :: Core.Text,
    -- | The possible type values.
    monitorType :: MonitorType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AnomalyMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastEvaluatedDate', 'anomalyMonitor_lastEvaluatedDate' - The date when the monitor last evaluated for anomalies.
--
-- 'monitorSpecification', 'anomalyMonitor_monitorSpecification' - Undocumented member.
--
-- 'lastUpdatedDate', 'anomalyMonitor_lastUpdatedDate' - The date when the monitor was last updated.
--
-- 'creationDate', 'anomalyMonitor_creationDate' - The date when the monitor was created.
--
-- 'dimensionalValueCount', 'anomalyMonitor_dimensionalValueCount' - The value for evaluated dimensions.
--
-- 'monitorDimension', 'anomalyMonitor_monitorDimension' - The dimensions to evaluate.
--
-- 'monitorArn', 'anomalyMonitor_monitorArn' - The Amazon Resource Name (ARN) value.
--
-- 'monitorName', 'anomalyMonitor_monitorName' - The name of the monitor.
--
-- 'monitorType', 'anomalyMonitor_monitorType' - The possible type values.
newAnomalyMonitor ::
  -- | 'monitorName'
  Core.Text ->
  -- | 'monitorType'
  MonitorType ->
  AnomalyMonitor
newAnomalyMonitor pMonitorName_ pMonitorType_ =
  AnomalyMonitor'
    { lastEvaluatedDate = Core.Nothing,
      monitorSpecification = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      creationDate = Core.Nothing,
      dimensionalValueCount = Core.Nothing,
      monitorDimension = Core.Nothing,
      monitorArn = Core.Nothing,
      monitorName = pMonitorName_,
      monitorType = pMonitorType_
    }

-- | The date when the monitor last evaluated for anomalies.
anomalyMonitor_lastEvaluatedDate :: Lens.Lens' AnomalyMonitor (Core.Maybe Core.Text)
anomalyMonitor_lastEvaluatedDate = Lens.lens (\AnomalyMonitor' {lastEvaluatedDate} -> lastEvaluatedDate) (\s@AnomalyMonitor' {} a -> s {lastEvaluatedDate = a} :: AnomalyMonitor)

-- | Undocumented member.
anomalyMonitor_monitorSpecification :: Lens.Lens' AnomalyMonitor (Core.Maybe Expression)
anomalyMonitor_monitorSpecification = Lens.lens (\AnomalyMonitor' {monitorSpecification} -> monitorSpecification) (\s@AnomalyMonitor' {} a -> s {monitorSpecification = a} :: AnomalyMonitor)

-- | The date when the monitor was last updated.
anomalyMonitor_lastUpdatedDate :: Lens.Lens' AnomalyMonitor (Core.Maybe Core.Text)
anomalyMonitor_lastUpdatedDate = Lens.lens (\AnomalyMonitor' {lastUpdatedDate} -> lastUpdatedDate) (\s@AnomalyMonitor' {} a -> s {lastUpdatedDate = a} :: AnomalyMonitor)

-- | The date when the monitor was created.
anomalyMonitor_creationDate :: Lens.Lens' AnomalyMonitor (Core.Maybe Core.Text)
anomalyMonitor_creationDate = Lens.lens (\AnomalyMonitor' {creationDate} -> creationDate) (\s@AnomalyMonitor' {} a -> s {creationDate = a} :: AnomalyMonitor)

-- | The value for evaluated dimensions.
anomalyMonitor_dimensionalValueCount :: Lens.Lens' AnomalyMonitor (Core.Maybe Core.Natural)
anomalyMonitor_dimensionalValueCount = Lens.lens (\AnomalyMonitor' {dimensionalValueCount} -> dimensionalValueCount) (\s@AnomalyMonitor' {} a -> s {dimensionalValueCount = a} :: AnomalyMonitor)

-- | The dimensions to evaluate.
anomalyMonitor_monitorDimension :: Lens.Lens' AnomalyMonitor (Core.Maybe MonitorDimension)
anomalyMonitor_monitorDimension = Lens.lens (\AnomalyMonitor' {monitorDimension} -> monitorDimension) (\s@AnomalyMonitor' {} a -> s {monitorDimension = a} :: AnomalyMonitor)

-- | The Amazon Resource Name (ARN) value.
anomalyMonitor_monitorArn :: Lens.Lens' AnomalyMonitor (Core.Maybe Core.Text)
anomalyMonitor_monitorArn = Lens.lens (\AnomalyMonitor' {monitorArn} -> monitorArn) (\s@AnomalyMonitor' {} a -> s {monitorArn = a} :: AnomalyMonitor)

-- | The name of the monitor.
anomalyMonitor_monitorName :: Lens.Lens' AnomalyMonitor Core.Text
anomalyMonitor_monitorName = Lens.lens (\AnomalyMonitor' {monitorName} -> monitorName) (\s@AnomalyMonitor' {} a -> s {monitorName = a} :: AnomalyMonitor)

-- | The possible type values.
anomalyMonitor_monitorType :: Lens.Lens' AnomalyMonitor MonitorType
anomalyMonitor_monitorType = Lens.lens (\AnomalyMonitor' {monitorType} -> monitorType) (\s@AnomalyMonitor' {} a -> s {monitorType = a} :: AnomalyMonitor)

instance Core.FromJSON AnomalyMonitor where
  parseJSON =
    Core.withObject
      "AnomalyMonitor"
      ( \x ->
          AnomalyMonitor'
            Core.<$> (x Core..:? "LastEvaluatedDate")
            Core.<*> (x Core..:? "MonitorSpecification")
            Core.<*> (x Core..:? "LastUpdatedDate")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "DimensionalValueCount")
            Core.<*> (x Core..:? "MonitorDimension")
            Core.<*> (x Core..:? "MonitorArn")
            Core.<*> (x Core..: "MonitorName")
            Core.<*> (x Core..: "MonitorType")
      )

instance Core.Hashable AnomalyMonitor

instance Core.NFData AnomalyMonitor

instance Core.ToJSON AnomalyMonitor where
  toJSON AnomalyMonitor' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastEvaluatedDate" Core..=)
              Core.<$> lastEvaluatedDate,
            ("MonitorSpecification" Core..=)
              Core.<$> monitorSpecification,
            ("LastUpdatedDate" Core..=) Core.<$> lastUpdatedDate,
            ("CreationDate" Core..=) Core.<$> creationDate,
            ("DimensionalValueCount" Core..=)
              Core.<$> dimensionalValueCount,
            ("MonitorDimension" Core..=)
              Core.<$> monitorDimension,
            ("MonitorArn" Core..=) Core.<$> monitorArn,
            Core.Just ("MonitorName" Core..= monitorName),
            Core.Just ("MonitorType" Core..= monitorType)
          ]
      )
