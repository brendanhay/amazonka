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
-- Module      : Amazonka.CostExplorer.Types.Anomaly
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.Anomaly where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.AnomalyFeedbackType
import Amazonka.CostExplorer.Types.AnomalyScore
import Amazonka.CostExplorer.Types.Impact
import Amazonka.CostExplorer.Types.RootCause
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An unusual cost pattern. This consists of the detailed metadata and the
-- current status of the anomaly object.
--
-- /See:/ 'newAnomaly' smart constructor.
data Anomaly = Anomaly'
  { -- | The last day the anomaly is detected.
    anomalyEndDate :: Prelude.Maybe Prelude.Text,
    -- | The first day the anomaly is detected.
    anomalyStartDate :: Prelude.Maybe Prelude.Text,
    -- | The dimension for the anomaly (for example, an Amazon Web Service in a
    -- service monitor).
    dimensionValue :: Prelude.Maybe Prelude.Text,
    -- | The feedback value.
    feedback :: Prelude.Maybe AnomalyFeedbackType,
    -- | The list of identified root causes for the anomaly.
    rootCauses :: Prelude.Maybe [RootCause],
    -- | The unique identifier for the anomaly.
    anomalyId :: Prelude.Text,
    -- | The latest and maximum score for the anomaly.
    anomalyScore :: AnomalyScore,
    -- | The dollar impact for the anomaly.
    impact :: Impact,
    -- | The Amazon Resource Name (ARN) for the cost monitor that generated this
    -- anomaly.
    monitorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Anomaly' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyEndDate', 'anomaly_anomalyEndDate' - The last day the anomaly is detected.
--
-- 'anomalyStartDate', 'anomaly_anomalyStartDate' - The first day the anomaly is detected.
--
-- 'dimensionValue', 'anomaly_dimensionValue' - The dimension for the anomaly (for example, an Amazon Web Service in a
-- service monitor).
--
-- 'feedback', 'anomaly_feedback' - The feedback value.
--
-- 'rootCauses', 'anomaly_rootCauses' - The list of identified root causes for the anomaly.
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
  Prelude.Text ->
  -- | 'anomalyScore'
  AnomalyScore ->
  -- | 'impact'
  Impact ->
  -- | 'monitorArn'
  Prelude.Text ->
  Anomaly
newAnomaly
  pAnomalyId_
  pAnomalyScore_
  pImpact_
  pMonitorArn_ =
    Anomaly'
      { anomalyEndDate = Prelude.Nothing,
        anomalyStartDate = Prelude.Nothing,
        dimensionValue = Prelude.Nothing,
        feedback = Prelude.Nothing,
        rootCauses = Prelude.Nothing,
        anomalyId = pAnomalyId_,
        anomalyScore = pAnomalyScore_,
        impact = pImpact_,
        monitorArn = pMonitorArn_
      }

-- | The last day the anomaly is detected.
anomaly_anomalyEndDate :: Lens.Lens' Anomaly (Prelude.Maybe Prelude.Text)
anomaly_anomalyEndDate = Lens.lens (\Anomaly' {anomalyEndDate} -> anomalyEndDate) (\s@Anomaly' {} a -> s {anomalyEndDate = a} :: Anomaly)

-- | The first day the anomaly is detected.
anomaly_anomalyStartDate :: Lens.Lens' Anomaly (Prelude.Maybe Prelude.Text)
anomaly_anomalyStartDate = Lens.lens (\Anomaly' {anomalyStartDate} -> anomalyStartDate) (\s@Anomaly' {} a -> s {anomalyStartDate = a} :: Anomaly)

-- | The dimension for the anomaly (for example, an Amazon Web Service in a
-- service monitor).
anomaly_dimensionValue :: Lens.Lens' Anomaly (Prelude.Maybe Prelude.Text)
anomaly_dimensionValue = Lens.lens (\Anomaly' {dimensionValue} -> dimensionValue) (\s@Anomaly' {} a -> s {dimensionValue = a} :: Anomaly)

-- | The feedback value.
anomaly_feedback :: Lens.Lens' Anomaly (Prelude.Maybe AnomalyFeedbackType)
anomaly_feedback = Lens.lens (\Anomaly' {feedback} -> feedback) (\s@Anomaly' {} a -> s {feedback = a} :: Anomaly)

-- | The list of identified root causes for the anomaly.
anomaly_rootCauses :: Lens.Lens' Anomaly (Prelude.Maybe [RootCause])
anomaly_rootCauses = Lens.lens (\Anomaly' {rootCauses} -> rootCauses) (\s@Anomaly' {} a -> s {rootCauses = a} :: Anomaly) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the anomaly.
anomaly_anomalyId :: Lens.Lens' Anomaly Prelude.Text
anomaly_anomalyId = Lens.lens (\Anomaly' {anomalyId} -> anomalyId) (\s@Anomaly' {} a -> s {anomalyId = a} :: Anomaly)

-- | The latest and maximum score for the anomaly.
anomaly_anomalyScore :: Lens.Lens' Anomaly AnomalyScore
anomaly_anomalyScore = Lens.lens (\Anomaly' {anomalyScore} -> anomalyScore) (\s@Anomaly' {} a -> s {anomalyScore = a} :: Anomaly)

-- | The dollar impact for the anomaly.
anomaly_impact :: Lens.Lens' Anomaly Impact
anomaly_impact = Lens.lens (\Anomaly' {impact} -> impact) (\s@Anomaly' {} a -> s {impact = a} :: Anomaly)

-- | The Amazon Resource Name (ARN) for the cost monitor that generated this
-- anomaly.
anomaly_monitorArn :: Lens.Lens' Anomaly Prelude.Text
anomaly_monitorArn = Lens.lens (\Anomaly' {monitorArn} -> monitorArn) (\s@Anomaly' {} a -> s {monitorArn = a} :: Anomaly)

instance Data.FromJSON Anomaly where
  parseJSON =
    Data.withObject
      "Anomaly"
      ( \x ->
          Anomaly'
            Prelude.<$> (x Data..:? "AnomalyEndDate")
            Prelude.<*> (x Data..:? "AnomalyStartDate")
            Prelude.<*> (x Data..:? "DimensionValue")
            Prelude.<*> (x Data..:? "Feedback")
            Prelude.<*> (x Data..:? "RootCauses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "AnomalyId")
            Prelude.<*> (x Data..: "AnomalyScore")
            Prelude.<*> (x Data..: "Impact")
            Prelude.<*> (x Data..: "MonitorArn")
      )

instance Prelude.Hashable Anomaly where
  hashWithSalt _salt Anomaly' {..} =
    _salt
      `Prelude.hashWithSalt` anomalyEndDate
      `Prelude.hashWithSalt` anomalyStartDate
      `Prelude.hashWithSalt` dimensionValue
      `Prelude.hashWithSalt` feedback
      `Prelude.hashWithSalt` rootCauses
      `Prelude.hashWithSalt` anomalyId
      `Prelude.hashWithSalt` anomalyScore
      `Prelude.hashWithSalt` impact
      `Prelude.hashWithSalt` monitorArn

instance Prelude.NFData Anomaly where
  rnf Anomaly' {..} =
    Prelude.rnf anomalyEndDate
      `Prelude.seq` Prelude.rnf anomalyStartDate
      `Prelude.seq` Prelude.rnf dimensionValue
      `Prelude.seq` Prelude.rnf feedback
      `Prelude.seq` Prelude.rnf rootCauses
      `Prelude.seq` Prelude.rnf anomalyId
      `Prelude.seq` Prelude.rnf anomalyScore
      `Prelude.seq` Prelude.rnf impact
      `Prelude.seq` Prelude.rnf monitorArn
