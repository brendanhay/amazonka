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
-- Module      : Network.AWS.CostExplorer.Types.Anomaly
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Anomaly where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.AnomalyFeedbackType
import Network.AWS.CostExplorer.Types.AnomalyScore
import Network.AWS.CostExplorer.Types.Impact
import Network.AWS.CostExplorer.Types.RootCause
import qualified Network.AWS.Lens as Lens

-- | An unusual cost pattern. This consists of the detailed metadata and the
-- current status of the anomaly object.
--
-- /See:/ 'newAnomaly' smart constructor.
data Anomaly = Anomaly'
  { -- | The dimension for the anomaly. For example, an AWS service in a service
    -- monitor.
    dimensionValue :: Core.Maybe Core.Text,
    -- | The list of identified root causes for the anomaly.
    rootCauses :: Core.Maybe [RootCause],
    -- | The feedback value.
    feedback :: Core.Maybe AnomalyFeedbackType,
    -- | The first day the anomaly is detected.
    anomalyStartDate :: Core.Maybe Core.Text,
    -- | The last day the anomaly is detected.
    anomalyEndDate :: Core.Maybe Core.Text,
    -- | The unique identifier for the anomaly.
    anomalyId :: Core.Text,
    -- | The latest and maximum score for the anomaly.
    anomalyScore :: AnomalyScore,
    -- | The dollar impact for the anomaly.
    impact :: Impact,
    -- | The Amazon Resource Name (ARN) for the cost monitor that generated this
    -- anomaly.
    monitorArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Anomaly' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionValue', 'anomaly_dimensionValue' - The dimension for the anomaly. For example, an AWS service in a service
-- monitor.
--
-- 'rootCauses', 'anomaly_rootCauses' - The list of identified root causes for the anomaly.
--
-- 'feedback', 'anomaly_feedback' - The feedback value.
--
-- 'anomalyStartDate', 'anomaly_anomalyStartDate' - The first day the anomaly is detected.
--
-- 'anomalyEndDate', 'anomaly_anomalyEndDate' - The last day the anomaly is detected.
--
-- 'anomalyId', 'anomaly_anomalyId' - The unique identifier for the anomaly.
--
-- 'anomalyScore', 'anomaly_anomalyScore' - The latest and maximum score for the anomaly.
--
-- 'impact', 'anomaly_impact' - The dollar impact for the anomaly.
--
-- 'monitorArn', 'anomaly_monitorArn' - The Amazon Resource Name (ARN) for the cost monitor that generated this
-- anomaly.
newAnomaly ::
  -- | 'anomalyId'
  Core.Text ->
  -- | 'anomalyScore'
  AnomalyScore ->
  -- | 'impact'
  Impact ->
  -- | 'monitorArn'
  Core.Text ->
  Anomaly
newAnomaly
  pAnomalyId_
  pAnomalyScore_
  pImpact_
  pMonitorArn_ =
    Anomaly'
      { dimensionValue = Core.Nothing,
        rootCauses = Core.Nothing,
        feedback = Core.Nothing,
        anomalyStartDate = Core.Nothing,
        anomalyEndDate = Core.Nothing,
        anomalyId = pAnomalyId_,
        anomalyScore = pAnomalyScore_,
        impact = pImpact_,
        monitorArn = pMonitorArn_
      }

-- | The dimension for the anomaly. For example, an AWS service in a service
-- monitor.
anomaly_dimensionValue :: Lens.Lens' Anomaly (Core.Maybe Core.Text)
anomaly_dimensionValue = Lens.lens (\Anomaly' {dimensionValue} -> dimensionValue) (\s@Anomaly' {} a -> s {dimensionValue = a} :: Anomaly)

-- | The list of identified root causes for the anomaly.
anomaly_rootCauses :: Lens.Lens' Anomaly (Core.Maybe [RootCause])
anomaly_rootCauses = Lens.lens (\Anomaly' {rootCauses} -> rootCauses) (\s@Anomaly' {} a -> s {rootCauses = a} :: Anomaly) Core.. Lens.mapping Lens._Coerce

-- | The feedback value.
anomaly_feedback :: Lens.Lens' Anomaly (Core.Maybe AnomalyFeedbackType)
anomaly_feedback = Lens.lens (\Anomaly' {feedback} -> feedback) (\s@Anomaly' {} a -> s {feedback = a} :: Anomaly)

-- | The first day the anomaly is detected.
anomaly_anomalyStartDate :: Lens.Lens' Anomaly (Core.Maybe Core.Text)
anomaly_anomalyStartDate = Lens.lens (\Anomaly' {anomalyStartDate} -> anomalyStartDate) (\s@Anomaly' {} a -> s {anomalyStartDate = a} :: Anomaly)

-- | The last day the anomaly is detected.
anomaly_anomalyEndDate :: Lens.Lens' Anomaly (Core.Maybe Core.Text)
anomaly_anomalyEndDate = Lens.lens (\Anomaly' {anomalyEndDate} -> anomalyEndDate) (\s@Anomaly' {} a -> s {anomalyEndDate = a} :: Anomaly)

-- | The unique identifier for the anomaly.
anomaly_anomalyId :: Lens.Lens' Anomaly Core.Text
anomaly_anomalyId = Lens.lens (\Anomaly' {anomalyId} -> anomalyId) (\s@Anomaly' {} a -> s {anomalyId = a} :: Anomaly)

-- | The latest and maximum score for the anomaly.
anomaly_anomalyScore :: Lens.Lens' Anomaly AnomalyScore
anomaly_anomalyScore = Lens.lens (\Anomaly' {anomalyScore} -> anomalyScore) (\s@Anomaly' {} a -> s {anomalyScore = a} :: Anomaly)

-- | The dollar impact for the anomaly.
anomaly_impact :: Lens.Lens' Anomaly Impact
anomaly_impact = Lens.lens (\Anomaly' {impact} -> impact) (\s@Anomaly' {} a -> s {impact = a} :: Anomaly)

-- | The Amazon Resource Name (ARN) for the cost monitor that generated this
-- anomaly.
anomaly_monitorArn :: Lens.Lens' Anomaly Core.Text
anomaly_monitorArn = Lens.lens (\Anomaly' {monitorArn} -> monitorArn) (\s@Anomaly' {} a -> s {monitorArn = a} :: Anomaly)

instance Core.FromJSON Anomaly where
  parseJSON =
    Core.withObject
      "Anomaly"
      ( \x ->
          Anomaly'
            Core.<$> (x Core..:? "DimensionValue")
            Core.<*> (x Core..:? "RootCauses" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Feedback")
            Core.<*> (x Core..:? "AnomalyStartDate")
            Core.<*> (x Core..:? "AnomalyEndDate")
            Core.<*> (x Core..: "AnomalyId")
            Core.<*> (x Core..: "AnomalyScore")
            Core.<*> (x Core..: "Impact")
            Core.<*> (x Core..: "MonitorArn")
      )

instance Core.Hashable Anomaly

instance Core.NFData Anomaly
