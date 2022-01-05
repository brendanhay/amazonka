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
-- Module      : Amazonka.DevOpsGuru.Types.ReactiveAnomaly
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ReactiveAnomaly where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.AnomalyReportedTimeRange
import Amazonka.DevOpsGuru.Types.AnomalySeverity
import Amazonka.DevOpsGuru.Types.AnomalySourceDetails
import Amazonka.DevOpsGuru.Types.AnomalyStatus
import Amazonka.DevOpsGuru.Types.AnomalyTimeRange
import Amazonka.DevOpsGuru.Types.ResourceCollection
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a reactive anomaly. This object is returned by
-- @ListAnomalies@.
--
-- /See:/ 'newReactiveAnomaly' smart constructor.
data ReactiveAnomaly = ReactiveAnomaly'
  { -- | A @AnomalyReportedTimeRange@ object that specifies the time range
    -- between when the anomaly is opened and the time when it is closed.
    anomalyReportedTimeRange :: Prelude.Maybe AnomalyReportedTimeRange,
    -- | The status of the anomaly.
    status :: Prelude.Maybe AnomalyStatus,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | The severity of the anomaly.
    severity :: Prelude.Maybe AnomalySeverity,
    -- | Details about the source of the analyzed operational data that triggered
    -- the anomaly. The one supported source is Amazon CloudWatch metrics.
    sourceDetails :: Prelude.Maybe AnomalySourceDetails,
    -- | The ID of the reactive anomaly.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the insight that contains this anomaly. An insight is composed
    -- of related anomalies.
    associatedInsightId :: Prelude.Maybe Prelude.Text,
    anomalyTimeRange :: Prelude.Maybe AnomalyTimeRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReactiveAnomaly' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyReportedTimeRange', 'reactiveAnomaly_anomalyReportedTimeRange' - A @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
--
-- 'status', 'reactiveAnomaly_status' - The status of the anomaly.
--
-- 'resourceCollection', 'reactiveAnomaly_resourceCollection' - Undocumented member.
--
-- 'severity', 'reactiveAnomaly_severity' - The severity of the anomaly.
--
-- 'sourceDetails', 'reactiveAnomaly_sourceDetails' - Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
--
-- 'id', 'reactiveAnomaly_id' - The ID of the reactive anomaly.
--
-- 'associatedInsightId', 'reactiveAnomaly_associatedInsightId' - The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
--
-- 'anomalyTimeRange', 'reactiveAnomaly_anomalyTimeRange' - Undocumented member.
newReactiveAnomaly ::
  ReactiveAnomaly
newReactiveAnomaly =
  ReactiveAnomaly'
    { anomalyReportedTimeRange =
        Prelude.Nothing,
      status = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      severity = Prelude.Nothing,
      sourceDetails = Prelude.Nothing,
      id = Prelude.Nothing,
      associatedInsightId = Prelude.Nothing,
      anomalyTimeRange = Prelude.Nothing
    }

-- | A @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
reactiveAnomaly_anomalyReportedTimeRange :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe AnomalyReportedTimeRange)
reactiveAnomaly_anomalyReportedTimeRange = Lens.lens (\ReactiveAnomaly' {anomalyReportedTimeRange} -> anomalyReportedTimeRange) (\s@ReactiveAnomaly' {} a -> s {anomalyReportedTimeRange = a} :: ReactiveAnomaly)

-- | The status of the anomaly.
reactiveAnomaly_status :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe AnomalyStatus)
reactiveAnomaly_status = Lens.lens (\ReactiveAnomaly' {status} -> status) (\s@ReactiveAnomaly' {} a -> s {status = a} :: ReactiveAnomaly)

-- | Undocumented member.
reactiveAnomaly_resourceCollection :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe ResourceCollection)
reactiveAnomaly_resourceCollection = Lens.lens (\ReactiveAnomaly' {resourceCollection} -> resourceCollection) (\s@ReactiveAnomaly' {} a -> s {resourceCollection = a} :: ReactiveAnomaly)

-- | The severity of the anomaly.
reactiveAnomaly_severity :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe AnomalySeverity)
reactiveAnomaly_severity = Lens.lens (\ReactiveAnomaly' {severity} -> severity) (\s@ReactiveAnomaly' {} a -> s {severity = a} :: ReactiveAnomaly)

-- | Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
reactiveAnomaly_sourceDetails :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe AnomalySourceDetails)
reactiveAnomaly_sourceDetails = Lens.lens (\ReactiveAnomaly' {sourceDetails} -> sourceDetails) (\s@ReactiveAnomaly' {} a -> s {sourceDetails = a} :: ReactiveAnomaly)

-- | The ID of the reactive anomaly.
reactiveAnomaly_id :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe Prelude.Text)
reactiveAnomaly_id = Lens.lens (\ReactiveAnomaly' {id} -> id) (\s@ReactiveAnomaly' {} a -> s {id = a} :: ReactiveAnomaly)

-- | The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
reactiveAnomaly_associatedInsightId :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe Prelude.Text)
reactiveAnomaly_associatedInsightId = Lens.lens (\ReactiveAnomaly' {associatedInsightId} -> associatedInsightId) (\s@ReactiveAnomaly' {} a -> s {associatedInsightId = a} :: ReactiveAnomaly)

-- | Undocumented member.
reactiveAnomaly_anomalyTimeRange :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe AnomalyTimeRange)
reactiveAnomaly_anomalyTimeRange = Lens.lens (\ReactiveAnomaly' {anomalyTimeRange} -> anomalyTimeRange) (\s@ReactiveAnomaly' {} a -> s {anomalyTimeRange = a} :: ReactiveAnomaly)

instance Core.FromJSON ReactiveAnomaly where
  parseJSON =
    Core.withObject
      "ReactiveAnomaly"
      ( \x ->
          ReactiveAnomaly'
            Prelude.<$> (x Core..:? "AnomalyReportedTimeRange")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ResourceCollection")
            Prelude.<*> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "SourceDetails")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "AssociatedInsightId")
            Prelude.<*> (x Core..:? "AnomalyTimeRange")
      )

instance Prelude.Hashable ReactiveAnomaly where
  hashWithSalt _salt ReactiveAnomaly' {..} =
    _salt
      `Prelude.hashWithSalt` anomalyReportedTimeRange
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` sourceDetails
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` associatedInsightId
      `Prelude.hashWithSalt` anomalyTimeRange

instance Prelude.NFData ReactiveAnomaly where
  rnf ReactiveAnomaly' {..} =
    Prelude.rnf anomalyReportedTimeRange
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf sourceDetails
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf associatedInsightId
      `Prelude.seq` Prelude.rnf anomalyTimeRange
