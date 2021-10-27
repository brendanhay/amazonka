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
-- Module      : Network.AWS.DevOpsGuru.Types.ProactiveAnomaly
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.ProactiveAnomaly where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types.AnomalyReportedTimeRange
import Network.AWS.DevOpsGuru.Types.AnomalySeverity
import Network.AWS.DevOpsGuru.Types.AnomalySourceDetails
import Network.AWS.DevOpsGuru.Types.AnomalyStatus
import Network.AWS.DevOpsGuru.Types.AnomalyTimeRange
import Network.AWS.DevOpsGuru.Types.PredictionTimeRange
import Network.AWS.DevOpsGuru.Types.ResourceCollection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an anomaly. This object is returned by
-- @ListAnomalies@.
--
-- /See:/ 'newProactiveAnomaly' smart constructor.
data ProactiveAnomaly = ProactiveAnomaly'
  { -- | A @AnomalyReportedTimeRange@ object that specifies the time range
    -- between when the anomaly is opened and the time when it is closed.
    anomalyReportedTimeRange :: Prelude.Maybe AnomalyReportedTimeRange,
    -- | The status of a proactive anomaly.
    status :: Prelude.Maybe AnomalyStatus,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | The severity of a proactive anomaly.
    severity :: Prelude.Maybe AnomalySeverity,
    -- | The time of the anomaly\'s most recent update.
    updateTime :: Prelude.Maybe Core.POSIX,
    -- | Details about the source of the analyzed operational data that triggered
    -- the anomaly. The one supported source is Amazon CloudWatch metrics.
    sourceDetails :: Prelude.Maybe AnomalySourceDetails,
    predictionTimeRange :: Prelude.Maybe PredictionTimeRange,
    -- | A threshold that was exceeded by behavior in analyzed resources.
    -- Exceeding this threshold is related to the anomalous behavior that
    -- generated this anomaly.
    limit :: Prelude.Maybe Prelude.Double,
    -- | The ID of a proactive anomaly.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the insight that contains this anomaly. An insight is composed
    -- of related anomalies.
    associatedInsightId :: Prelude.Maybe Prelude.Text,
    anomalyTimeRange :: Prelude.Maybe AnomalyTimeRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProactiveAnomaly' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyReportedTimeRange', 'proactiveAnomaly_anomalyReportedTimeRange' - A @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
--
-- 'status', 'proactiveAnomaly_status' - The status of a proactive anomaly.
--
-- 'resourceCollection', 'proactiveAnomaly_resourceCollection' - Undocumented member.
--
-- 'severity', 'proactiveAnomaly_severity' - The severity of a proactive anomaly.
--
-- 'updateTime', 'proactiveAnomaly_updateTime' - The time of the anomaly\'s most recent update.
--
-- 'sourceDetails', 'proactiveAnomaly_sourceDetails' - Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
--
-- 'predictionTimeRange', 'proactiveAnomaly_predictionTimeRange' - Undocumented member.
--
-- 'limit', 'proactiveAnomaly_limit' - A threshold that was exceeded by behavior in analyzed resources.
-- Exceeding this threshold is related to the anomalous behavior that
-- generated this anomaly.
--
-- 'id', 'proactiveAnomaly_id' - The ID of a proactive anomaly.
--
-- 'associatedInsightId', 'proactiveAnomaly_associatedInsightId' - The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
--
-- 'anomalyTimeRange', 'proactiveAnomaly_anomalyTimeRange' - Undocumented member.
newProactiveAnomaly ::
  ProactiveAnomaly
newProactiveAnomaly =
  ProactiveAnomaly'
    { anomalyReportedTimeRange =
        Prelude.Nothing,
      status = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      severity = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      sourceDetails = Prelude.Nothing,
      predictionTimeRange = Prelude.Nothing,
      limit = Prelude.Nothing,
      id = Prelude.Nothing,
      associatedInsightId = Prelude.Nothing,
      anomalyTimeRange = Prelude.Nothing
    }

-- | A @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
proactiveAnomaly_anomalyReportedTimeRange :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe AnomalyReportedTimeRange)
proactiveAnomaly_anomalyReportedTimeRange = Lens.lens (\ProactiveAnomaly' {anomalyReportedTimeRange} -> anomalyReportedTimeRange) (\s@ProactiveAnomaly' {} a -> s {anomalyReportedTimeRange = a} :: ProactiveAnomaly)

-- | The status of a proactive anomaly.
proactiveAnomaly_status :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe AnomalyStatus)
proactiveAnomaly_status = Lens.lens (\ProactiveAnomaly' {status} -> status) (\s@ProactiveAnomaly' {} a -> s {status = a} :: ProactiveAnomaly)

-- | Undocumented member.
proactiveAnomaly_resourceCollection :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe ResourceCollection)
proactiveAnomaly_resourceCollection = Lens.lens (\ProactiveAnomaly' {resourceCollection} -> resourceCollection) (\s@ProactiveAnomaly' {} a -> s {resourceCollection = a} :: ProactiveAnomaly)

-- | The severity of a proactive anomaly.
proactiveAnomaly_severity :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe AnomalySeverity)
proactiveAnomaly_severity = Lens.lens (\ProactiveAnomaly' {severity} -> severity) (\s@ProactiveAnomaly' {} a -> s {severity = a} :: ProactiveAnomaly)

-- | The time of the anomaly\'s most recent update.
proactiveAnomaly_updateTime :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe Prelude.UTCTime)
proactiveAnomaly_updateTime = Lens.lens (\ProactiveAnomaly' {updateTime} -> updateTime) (\s@ProactiveAnomaly' {} a -> s {updateTime = a} :: ProactiveAnomaly) Prelude.. Lens.mapping Core._Time

-- | Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
proactiveAnomaly_sourceDetails :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe AnomalySourceDetails)
proactiveAnomaly_sourceDetails = Lens.lens (\ProactiveAnomaly' {sourceDetails} -> sourceDetails) (\s@ProactiveAnomaly' {} a -> s {sourceDetails = a} :: ProactiveAnomaly)

-- | Undocumented member.
proactiveAnomaly_predictionTimeRange :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe PredictionTimeRange)
proactiveAnomaly_predictionTimeRange = Lens.lens (\ProactiveAnomaly' {predictionTimeRange} -> predictionTimeRange) (\s@ProactiveAnomaly' {} a -> s {predictionTimeRange = a} :: ProactiveAnomaly)

-- | A threshold that was exceeded by behavior in analyzed resources.
-- Exceeding this threshold is related to the anomalous behavior that
-- generated this anomaly.
proactiveAnomaly_limit :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe Prelude.Double)
proactiveAnomaly_limit = Lens.lens (\ProactiveAnomaly' {limit} -> limit) (\s@ProactiveAnomaly' {} a -> s {limit = a} :: ProactiveAnomaly)

-- | The ID of a proactive anomaly.
proactiveAnomaly_id :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe Prelude.Text)
proactiveAnomaly_id = Lens.lens (\ProactiveAnomaly' {id} -> id) (\s@ProactiveAnomaly' {} a -> s {id = a} :: ProactiveAnomaly)

-- | The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
proactiveAnomaly_associatedInsightId :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe Prelude.Text)
proactiveAnomaly_associatedInsightId = Lens.lens (\ProactiveAnomaly' {associatedInsightId} -> associatedInsightId) (\s@ProactiveAnomaly' {} a -> s {associatedInsightId = a} :: ProactiveAnomaly)

-- | Undocumented member.
proactiveAnomaly_anomalyTimeRange :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe AnomalyTimeRange)
proactiveAnomaly_anomalyTimeRange = Lens.lens (\ProactiveAnomaly' {anomalyTimeRange} -> anomalyTimeRange) (\s@ProactiveAnomaly' {} a -> s {anomalyTimeRange = a} :: ProactiveAnomaly)

instance Core.FromJSON ProactiveAnomaly where
  parseJSON =
    Core.withObject
      "ProactiveAnomaly"
      ( \x ->
          ProactiveAnomaly'
            Prelude.<$> (x Core..:? "AnomalyReportedTimeRange")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ResourceCollection")
            Prelude.<*> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "UpdateTime")
            Prelude.<*> (x Core..:? "SourceDetails")
            Prelude.<*> (x Core..:? "PredictionTimeRange")
            Prelude.<*> (x Core..:? "Limit")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "AssociatedInsightId")
            Prelude.<*> (x Core..:? "AnomalyTimeRange")
      )

instance Prelude.Hashable ProactiveAnomaly

instance Prelude.NFData ProactiveAnomaly
