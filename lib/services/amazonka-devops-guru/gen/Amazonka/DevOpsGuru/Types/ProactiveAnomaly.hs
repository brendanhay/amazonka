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
-- Module      : Amazonka.DevOpsGuru.Types.ProactiveAnomaly
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ProactiveAnomaly where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.AnomalyReportedTimeRange
import Amazonka.DevOpsGuru.Types.AnomalyResource
import Amazonka.DevOpsGuru.Types.AnomalySeverity
import Amazonka.DevOpsGuru.Types.AnomalySourceDetails
import Amazonka.DevOpsGuru.Types.AnomalySourceMetadata
import Amazonka.DevOpsGuru.Types.AnomalyStatus
import Amazonka.DevOpsGuru.Types.AnomalyTimeRange
import Amazonka.DevOpsGuru.Types.PredictionTimeRange
import Amazonka.DevOpsGuru.Types.ResourceCollection
import qualified Amazonka.Prelude as Prelude

-- | Information about an anomaly. This object is returned by
-- @ListAnomalies@.
--
-- /See:/ 'newProactiveAnomaly' smart constructor.
data ProactiveAnomaly = ProactiveAnomaly'
  { -- | An @AnomalyReportedTimeRange@ object that specifies the time range
    -- between when the anomaly is opened and the time when it is closed.
    anomalyReportedTimeRange :: Prelude.Maybe AnomalyReportedTimeRange,
    -- | Information about a resource in which DevOps Guru detected anomalous
    -- behavior.
    anomalyResources :: Prelude.Maybe [AnomalyResource],
    anomalyTimeRange :: Prelude.Maybe AnomalyTimeRange,
    -- | The ID of the insight that contains this anomaly. An insight is composed
    -- of related anomalies.
    associatedInsightId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a proactive anomaly.
    id :: Prelude.Maybe Prelude.Text,
    -- | A threshold that was exceeded by behavior in analyzed resources.
    -- Exceeding this threshold is related to the anomalous behavior that
    -- generated this anomaly.
    limit :: Prelude.Maybe Prelude.Double,
    predictionTimeRange :: Prelude.Maybe PredictionTimeRange,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | The severity of the anomaly. The severity of anomalies that generate an
    -- insight determine that insight\'s severity. For more information, see
    -- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
    -- in the /Amazon DevOps Guru User Guide/.
    severity :: Prelude.Maybe AnomalySeverity,
    -- | Details about the source of the analyzed operational data that triggered
    -- the anomaly. The one supported source is Amazon CloudWatch metrics.
    sourceDetails :: Prelude.Maybe AnomalySourceDetails,
    -- | The metadata for the anomaly.
    sourceMetadata :: Prelude.Maybe AnomalySourceMetadata,
    -- | The status of a proactive anomaly.
    status :: Prelude.Maybe AnomalyStatus,
    -- | The time of the anomaly\'s most recent update.
    updateTime :: Prelude.Maybe Data.POSIX
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
-- 'anomalyReportedTimeRange', 'proactiveAnomaly_anomalyReportedTimeRange' - An @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
--
-- 'anomalyResources', 'proactiveAnomaly_anomalyResources' - Information about a resource in which DevOps Guru detected anomalous
-- behavior.
--
-- 'anomalyTimeRange', 'proactiveAnomaly_anomalyTimeRange' - Undocumented member.
--
-- 'associatedInsightId', 'proactiveAnomaly_associatedInsightId' - The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
--
-- 'id', 'proactiveAnomaly_id' - The ID of a proactive anomaly.
--
-- 'limit', 'proactiveAnomaly_limit' - A threshold that was exceeded by behavior in analyzed resources.
-- Exceeding this threshold is related to the anomalous behavior that
-- generated this anomaly.
--
-- 'predictionTimeRange', 'proactiveAnomaly_predictionTimeRange' - Undocumented member.
--
-- 'resourceCollection', 'proactiveAnomaly_resourceCollection' - Undocumented member.
--
-- 'severity', 'proactiveAnomaly_severity' - The severity of the anomaly. The severity of anomalies that generate an
-- insight determine that insight\'s severity. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
--
-- 'sourceDetails', 'proactiveAnomaly_sourceDetails' - Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
--
-- 'sourceMetadata', 'proactiveAnomaly_sourceMetadata' - The metadata for the anomaly.
--
-- 'status', 'proactiveAnomaly_status' - The status of a proactive anomaly.
--
-- 'updateTime', 'proactiveAnomaly_updateTime' - The time of the anomaly\'s most recent update.
newProactiveAnomaly ::
  ProactiveAnomaly
newProactiveAnomaly =
  ProactiveAnomaly'
    { anomalyReportedTimeRange =
        Prelude.Nothing,
      anomalyResources = Prelude.Nothing,
      anomalyTimeRange = Prelude.Nothing,
      associatedInsightId = Prelude.Nothing,
      id = Prelude.Nothing,
      limit = Prelude.Nothing,
      predictionTimeRange = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      severity = Prelude.Nothing,
      sourceDetails = Prelude.Nothing,
      sourceMetadata = Prelude.Nothing,
      status = Prelude.Nothing,
      updateTime = Prelude.Nothing
    }

-- | An @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
proactiveAnomaly_anomalyReportedTimeRange :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe AnomalyReportedTimeRange)
proactiveAnomaly_anomalyReportedTimeRange = Lens.lens (\ProactiveAnomaly' {anomalyReportedTimeRange} -> anomalyReportedTimeRange) (\s@ProactiveAnomaly' {} a -> s {anomalyReportedTimeRange = a} :: ProactiveAnomaly)

-- | Information about a resource in which DevOps Guru detected anomalous
-- behavior.
proactiveAnomaly_anomalyResources :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe [AnomalyResource])
proactiveAnomaly_anomalyResources = Lens.lens (\ProactiveAnomaly' {anomalyResources} -> anomalyResources) (\s@ProactiveAnomaly' {} a -> s {anomalyResources = a} :: ProactiveAnomaly) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
proactiveAnomaly_anomalyTimeRange :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe AnomalyTimeRange)
proactiveAnomaly_anomalyTimeRange = Lens.lens (\ProactiveAnomaly' {anomalyTimeRange} -> anomalyTimeRange) (\s@ProactiveAnomaly' {} a -> s {anomalyTimeRange = a} :: ProactiveAnomaly)

-- | The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
proactiveAnomaly_associatedInsightId :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe Prelude.Text)
proactiveAnomaly_associatedInsightId = Lens.lens (\ProactiveAnomaly' {associatedInsightId} -> associatedInsightId) (\s@ProactiveAnomaly' {} a -> s {associatedInsightId = a} :: ProactiveAnomaly)

-- | The ID of a proactive anomaly.
proactiveAnomaly_id :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe Prelude.Text)
proactiveAnomaly_id = Lens.lens (\ProactiveAnomaly' {id} -> id) (\s@ProactiveAnomaly' {} a -> s {id = a} :: ProactiveAnomaly)

-- | A threshold that was exceeded by behavior in analyzed resources.
-- Exceeding this threshold is related to the anomalous behavior that
-- generated this anomaly.
proactiveAnomaly_limit :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe Prelude.Double)
proactiveAnomaly_limit = Lens.lens (\ProactiveAnomaly' {limit} -> limit) (\s@ProactiveAnomaly' {} a -> s {limit = a} :: ProactiveAnomaly)

-- | Undocumented member.
proactiveAnomaly_predictionTimeRange :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe PredictionTimeRange)
proactiveAnomaly_predictionTimeRange = Lens.lens (\ProactiveAnomaly' {predictionTimeRange} -> predictionTimeRange) (\s@ProactiveAnomaly' {} a -> s {predictionTimeRange = a} :: ProactiveAnomaly)

-- | Undocumented member.
proactiveAnomaly_resourceCollection :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe ResourceCollection)
proactiveAnomaly_resourceCollection = Lens.lens (\ProactiveAnomaly' {resourceCollection} -> resourceCollection) (\s@ProactiveAnomaly' {} a -> s {resourceCollection = a} :: ProactiveAnomaly)

-- | The severity of the anomaly. The severity of anomalies that generate an
-- insight determine that insight\'s severity. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
proactiveAnomaly_severity :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe AnomalySeverity)
proactiveAnomaly_severity = Lens.lens (\ProactiveAnomaly' {severity} -> severity) (\s@ProactiveAnomaly' {} a -> s {severity = a} :: ProactiveAnomaly)

-- | Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
proactiveAnomaly_sourceDetails :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe AnomalySourceDetails)
proactiveAnomaly_sourceDetails = Lens.lens (\ProactiveAnomaly' {sourceDetails} -> sourceDetails) (\s@ProactiveAnomaly' {} a -> s {sourceDetails = a} :: ProactiveAnomaly)

-- | The metadata for the anomaly.
proactiveAnomaly_sourceMetadata :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe AnomalySourceMetadata)
proactiveAnomaly_sourceMetadata = Lens.lens (\ProactiveAnomaly' {sourceMetadata} -> sourceMetadata) (\s@ProactiveAnomaly' {} a -> s {sourceMetadata = a} :: ProactiveAnomaly)

-- | The status of a proactive anomaly.
proactiveAnomaly_status :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe AnomalyStatus)
proactiveAnomaly_status = Lens.lens (\ProactiveAnomaly' {status} -> status) (\s@ProactiveAnomaly' {} a -> s {status = a} :: ProactiveAnomaly)

-- | The time of the anomaly\'s most recent update.
proactiveAnomaly_updateTime :: Lens.Lens' ProactiveAnomaly (Prelude.Maybe Prelude.UTCTime)
proactiveAnomaly_updateTime = Lens.lens (\ProactiveAnomaly' {updateTime} -> updateTime) (\s@ProactiveAnomaly' {} a -> s {updateTime = a} :: ProactiveAnomaly) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ProactiveAnomaly where
  parseJSON =
    Data.withObject
      "ProactiveAnomaly"
      ( \x ->
          ProactiveAnomaly'
            Prelude.<$> (x Data..:? "AnomalyReportedTimeRange")
            Prelude.<*> ( x
                            Data..:? "AnomalyResources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AnomalyTimeRange")
            Prelude.<*> (x Data..:? "AssociatedInsightId")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Limit")
            Prelude.<*> (x Data..:? "PredictionTimeRange")
            Prelude.<*> (x Data..:? "ResourceCollection")
            Prelude.<*> (x Data..:? "Severity")
            Prelude.<*> (x Data..:? "SourceDetails")
            Prelude.<*> (x Data..:? "SourceMetadata")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UpdateTime")
      )

instance Prelude.Hashable ProactiveAnomaly where
  hashWithSalt _salt ProactiveAnomaly' {..} =
    _salt
      `Prelude.hashWithSalt` anomalyReportedTimeRange
      `Prelude.hashWithSalt` anomalyResources
      `Prelude.hashWithSalt` anomalyTimeRange
      `Prelude.hashWithSalt` associatedInsightId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` predictionTimeRange
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` sourceDetails
      `Prelude.hashWithSalt` sourceMetadata
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData ProactiveAnomaly where
  rnf ProactiveAnomaly' {..} =
    Prelude.rnf anomalyReportedTimeRange
      `Prelude.seq` Prelude.rnf anomalyResources
      `Prelude.seq` Prelude.rnf anomalyTimeRange
      `Prelude.seq` Prelude.rnf associatedInsightId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf predictionTimeRange
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf sourceDetails
      `Prelude.seq` Prelude.rnf sourceMetadata
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateTime
