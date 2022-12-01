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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ReactiveAnomaly where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.AnomalyReportedTimeRange
import Amazonka.DevOpsGuru.Types.AnomalyResource
import Amazonka.DevOpsGuru.Types.AnomalySeverity
import Amazonka.DevOpsGuru.Types.AnomalySourceDetails
import Amazonka.DevOpsGuru.Types.AnomalyStatus
import Amazonka.DevOpsGuru.Types.AnomalyTimeRange
import Amazonka.DevOpsGuru.Types.AnomalyType
import Amazonka.DevOpsGuru.Types.ResourceCollection
import qualified Amazonka.Prelude as Prelude

-- | Details about a reactive anomaly. This object is returned by
-- @ListAnomalies@.
--
-- /See:/ 'newReactiveAnomaly' smart constructor.
data ReactiveAnomaly = ReactiveAnomaly'
  { anomalyTimeRange :: Prelude.Maybe AnomalyTimeRange,
    -- | The severity of the anomaly. The severity of anomalies that generate an
    -- insight determine that insight\'s severity. For more information, see
    -- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
    -- in the /Amazon DevOps Guru User Guide/.
    severity :: Prelude.Maybe AnomalySeverity,
    -- | The name of the reactive anomaly.
    name :: Prelude.Maybe Prelude.Text,
    -- | An @AnomalyReportedTimeRange@ object that specifies the time range
    -- between when the anomaly is opened and the time when it is closed.
    anomalyReportedTimeRange :: Prelude.Maybe AnomalyReportedTimeRange,
    -- | The type of the reactive anomaly. It can be one of the following types.
    --
    -- -   @CAUSAL@ - the anomaly can cause a new insight.
    --
    -- -   @CONTEXTUAL@ - the anomaly contains additional information about an
    --     insight or its causal anomaly.
    type' :: Prelude.Maybe AnomalyType,
    -- | The ID of the insight that contains this anomaly. An insight is composed
    -- of related anomalies.
    associatedInsightId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services resources in which anomalous behavior was
    -- detected by DevOps Guru.
    anomalyResources :: Prelude.Maybe [AnomalyResource],
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | Details about the source of the analyzed operational data that triggered
    -- the anomaly. The one supported source is Amazon CloudWatch metrics.
    sourceDetails :: Prelude.Maybe AnomalySourceDetails,
    -- | The status of the anomaly.
    status :: Prelude.Maybe AnomalyStatus,
    -- | The ID of the reactive anomaly.
    id :: Prelude.Maybe Prelude.Text,
    -- | A description of the reactive anomaly.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the causal anomaly that is associated with this reactive
    -- anomaly. The ID of a \`CAUSAL\` anomaly is always \`NULL\`.
    causalAnomalyId :: Prelude.Maybe Prelude.Text
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
-- 'anomalyTimeRange', 'reactiveAnomaly_anomalyTimeRange' - Undocumented member.
--
-- 'severity', 'reactiveAnomaly_severity' - The severity of the anomaly. The severity of anomalies that generate an
-- insight determine that insight\'s severity. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
--
-- 'name', 'reactiveAnomaly_name' - The name of the reactive anomaly.
--
-- 'anomalyReportedTimeRange', 'reactiveAnomaly_anomalyReportedTimeRange' - An @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
--
-- 'type'', 'reactiveAnomaly_type' - The type of the reactive anomaly. It can be one of the following types.
--
-- -   @CAUSAL@ - the anomaly can cause a new insight.
--
-- -   @CONTEXTUAL@ - the anomaly contains additional information about an
--     insight or its causal anomaly.
--
-- 'associatedInsightId', 'reactiveAnomaly_associatedInsightId' - The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
--
-- 'anomalyResources', 'reactiveAnomaly_anomalyResources' - The Amazon Web Services resources in which anomalous behavior was
-- detected by DevOps Guru.
--
-- 'resourceCollection', 'reactiveAnomaly_resourceCollection' - Undocumented member.
--
-- 'sourceDetails', 'reactiveAnomaly_sourceDetails' - Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
--
-- 'status', 'reactiveAnomaly_status' - The status of the anomaly.
--
-- 'id', 'reactiveAnomaly_id' - The ID of the reactive anomaly.
--
-- 'description', 'reactiveAnomaly_description' - A description of the reactive anomaly.
--
-- 'causalAnomalyId', 'reactiveAnomaly_causalAnomalyId' - The ID of the causal anomaly that is associated with this reactive
-- anomaly. The ID of a \`CAUSAL\` anomaly is always \`NULL\`.
newReactiveAnomaly ::
  ReactiveAnomaly
newReactiveAnomaly =
  ReactiveAnomaly'
    { anomalyTimeRange =
        Prelude.Nothing,
      severity = Prelude.Nothing,
      name = Prelude.Nothing,
      anomalyReportedTimeRange = Prelude.Nothing,
      type' = Prelude.Nothing,
      associatedInsightId = Prelude.Nothing,
      anomalyResources = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      sourceDetails = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      causalAnomalyId = Prelude.Nothing
    }

-- | Undocumented member.
reactiveAnomaly_anomalyTimeRange :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe AnomalyTimeRange)
reactiveAnomaly_anomalyTimeRange = Lens.lens (\ReactiveAnomaly' {anomalyTimeRange} -> anomalyTimeRange) (\s@ReactiveAnomaly' {} a -> s {anomalyTimeRange = a} :: ReactiveAnomaly)

-- | The severity of the anomaly. The severity of anomalies that generate an
-- insight determine that insight\'s severity. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
reactiveAnomaly_severity :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe AnomalySeverity)
reactiveAnomaly_severity = Lens.lens (\ReactiveAnomaly' {severity} -> severity) (\s@ReactiveAnomaly' {} a -> s {severity = a} :: ReactiveAnomaly)

-- | The name of the reactive anomaly.
reactiveAnomaly_name :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe Prelude.Text)
reactiveAnomaly_name = Lens.lens (\ReactiveAnomaly' {name} -> name) (\s@ReactiveAnomaly' {} a -> s {name = a} :: ReactiveAnomaly)

-- | An @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
reactiveAnomaly_anomalyReportedTimeRange :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe AnomalyReportedTimeRange)
reactiveAnomaly_anomalyReportedTimeRange = Lens.lens (\ReactiveAnomaly' {anomalyReportedTimeRange} -> anomalyReportedTimeRange) (\s@ReactiveAnomaly' {} a -> s {anomalyReportedTimeRange = a} :: ReactiveAnomaly)

-- | The type of the reactive anomaly. It can be one of the following types.
--
-- -   @CAUSAL@ - the anomaly can cause a new insight.
--
-- -   @CONTEXTUAL@ - the anomaly contains additional information about an
--     insight or its causal anomaly.
reactiveAnomaly_type :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe AnomalyType)
reactiveAnomaly_type = Lens.lens (\ReactiveAnomaly' {type'} -> type') (\s@ReactiveAnomaly' {} a -> s {type' = a} :: ReactiveAnomaly)

-- | The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
reactiveAnomaly_associatedInsightId :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe Prelude.Text)
reactiveAnomaly_associatedInsightId = Lens.lens (\ReactiveAnomaly' {associatedInsightId} -> associatedInsightId) (\s@ReactiveAnomaly' {} a -> s {associatedInsightId = a} :: ReactiveAnomaly)

-- | The Amazon Web Services resources in which anomalous behavior was
-- detected by DevOps Guru.
reactiveAnomaly_anomalyResources :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe [AnomalyResource])
reactiveAnomaly_anomalyResources = Lens.lens (\ReactiveAnomaly' {anomalyResources} -> anomalyResources) (\s@ReactiveAnomaly' {} a -> s {anomalyResources = a} :: ReactiveAnomaly) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
reactiveAnomaly_resourceCollection :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe ResourceCollection)
reactiveAnomaly_resourceCollection = Lens.lens (\ReactiveAnomaly' {resourceCollection} -> resourceCollection) (\s@ReactiveAnomaly' {} a -> s {resourceCollection = a} :: ReactiveAnomaly)

-- | Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
reactiveAnomaly_sourceDetails :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe AnomalySourceDetails)
reactiveAnomaly_sourceDetails = Lens.lens (\ReactiveAnomaly' {sourceDetails} -> sourceDetails) (\s@ReactiveAnomaly' {} a -> s {sourceDetails = a} :: ReactiveAnomaly)

-- | The status of the anomaly.
reactiveAnomaly_status :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe AnomalyStatus)
reactiveAnomaly_status = Lens.lens (\ReactiveAnomaly' {status} -> status) (\s@ReactiveAnomaly' {} a -> s {status = a} :: ReactiveAnomaly)

-- | The ID of the reactive anomaly.
reactiveAnomaly_id :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe Prelude.Text)
reactiveAnomaly_id = Lens.lens (\ReactiveAnomaly' {id} -> id) (\s@ReactiveAnomaly' {} a -> s {id = a} :: ReactiveAnomaly)

-- | A description of the reactive anomaly.
reactiveAnomaly_description :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe Prelude.Text)
reactiveAnomaly_description = Lens.lens (\ReactiveAnomaly' {description} -> description) (\s@ReactiveAnomaly' {} a -> s {description = a} :: ReactiveAnomaly)

-- | The ID of the causal anomaly that is associated with this reactive
-- anomaly. The ID of a \`CAUSAL\` anomaly is always \`NULL\`.
reactiveAnomaly_causalAnomalyId :: Lens.Lens' ReactiveAnomaly (Prelude.Maybe Prelude.Text)
reactiveAnomaly_causalAnomalyId = Lens.lens (\ReactiveAnomaly' {causalAnomalyId} -> causalAnomalyId) (\s@ReactiveAnomaly' {} a -> s {causalAnomalyId = a} :: ReactiveAnomaly)

instance Core.FromJSON ReactiveAnomaly where
  parseJSON =
    Core.withObject
      "ReactiveAnomaly"
      ( \x ->
          ReactiveAnomaly'
            Prelude.<$> (x Core..:? "AnomalyTimeRange")
            Prelude.<*> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "AnomalyReportedTimeRange")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "AssociatedInsightId")
            Prelude.<*> ( x Core..:? "AnomalyResources"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ResourceCollection")
            Prelude.<*> (x Core..:? "SourceDetails")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "CausalAnomalyId")
      )

instance Prelude.Hashable ReactiveAnomaly where
  hashWithSalt _salt ReactiveAnomaly' {..} =
    _salt `Prelude.hashWithSalt` anomalyTimeRange
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` anomalyReportedTimeRange
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` associatedInsightId
      `Prelude.hashWithSalt` anomalyResources
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` sourceDetails
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` causalAnomalyId

instance Prelude.NFData ReactiveAnomaly where
  rnf ReactiveAnomaly' {..} =
    Prelude.rnf anomalyTimeRange
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf anomalyReportedTimeRange
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf associatedInsightId
      `Prelude.seq` Prelude.rnf anomalyResources
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf sourceDetails
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf causalAnomalyId
