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
-- Module      : Amazonka.DevOpsGuru.Types.ReactiveAnomalySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ReactiveAnomalySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
-- @DescribeAnomaly.@
--
-- /See:/ 'newReactiveAnomalySummary' smart constructor.
data ReactiveAnomalySummary = ReactiveAnomalySummary'
  { -- | An @AnomalyReportedTimeRange@ object that specifies the time range
    -- between when the anomaly is opened and the time when it is closed.
    anomalyReportedTimeRange :: Prelude.Maybe AnomalyReportedTimeRange,
    -- | The Amazon Web Services resources in which anomalous behavior was
    -- detected by DevOps Guru.
    anomalyResources :: Prelude.Maybe [AnomalyResource],
    anomalyTimeRange :: Prelude.Maybe AnomalyTimeRange,
    -- | The ID of the insight that contains this anomaly. An insight is composed
    -- of related anomalies.
    associatedInsightId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the causal anomaly that is associated with this reactive
    -- anomaly. The ID of a \`CAUSAL\` anomaly is always \`NULL\`.
    causalAnomalyId :: Prelude.Maybe Prelude.Text,
    -- | A description of the reactive anomaly.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the reactive anomaly.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the reactive anomaly.
    name :: Prelude.Maybe Prelude.Text,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | The severity of the anomaly. The severity of anomalies that generate an
    -- insight determine that insight\'s severity. For more information, see
    -- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
    -- in the /Amazon DevOps Guru User Guide/.
    severity :: Prelude.Maybe AnomalySeverity,
    -- | Details about the source of the analyzed operational data that triggered
    -- the anomaly. The one supported source is Amazon CloudWatch metrics.
    sourceDetails :: Prelude.Maybe AnomalySourceDetails,
    -- | The status of the reactive anomaly.
    status :: Prelude.Maybe AnomalyStatus,
    -- | The type of the reactive anomaly. It can be one of the following types.
    --
    -- -   @CAUSAL@ - the anomaly can cause a new insight.
    --
    -- -   @CONTEXTUAL@ - the anomaly contains additional information about an
    --     insight or its causal anomaly.
    type' :: Prelude.Maybe AnomalyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReactiveAnomalySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyReportedTimeRange', 'reactiveAnomalySummary_anomalyReportedTimeRange' - An @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
--
-- 'anomalyResources', 'reactiveAnomalySummary_anomalyResources' - The Amazon Web Services resources in which anomalous behavior was
-- detected by DevOps Guru.
--
-- 'anomalyTimeRange', 'reactiveAnomalySummary_anomalyTimeRange' - Undocumented member.
--
-- 'associatedInsightId', 'reactiveAnomalySummary_associatedInsightId' - The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
--
-- 'causalAnomalyId', 'reactiveAnomalySummary_causalAnomalyId' - The ID of the causal anomaly that is associated with this reactive
-- anomaly. The ID of a \`CAUSAL\` anomaly is always \`NULL\`.
--
-- 'description', 'reactiveAnomalySummary_description' - A description of the reactive anomaly.
--
-- 'id', 'reactiveAnomalySummary_id' - The ID of the reactive anomaly.
--
-- 'name', 'reactiveAnomalySummary_name' - The name of the reactive anomaly.
--
-- 'resourceCollection', 'reactiveAnomalySummary_resourceCollection' - Undocumented member.
--
-- 'severity', 'reactiveAnomalySummary_severity' - The severity of the anomaly. The severity of anomalies that generate an
-- insight determine that insight\'s severity. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
--
-- 'sourceDetails', 'reactiveAnomalySummary_sourceDetails' - Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
--
-- 'status', 'reactiveAnomalySummary_status' - The status of the reactive anomaly.
--
-- 'type'', 'reactiveAnomalySummary_type' - The type of the reactive anomaly. It can be one of the following types.
--
-- -   @CAUSAL@ - the anomaly can cause a new insight.
--
-- -   @CONTEXTUAL@ - the anomaly contains additional information about an
--     insight or its causal anomaly.
newReactiveAnomalySummary ::
  ReactiveAnomalySummary
newReactiveAnomalySummary =
  ReactiveAnomalySummary'
    { anomalyReportedTimeRange =
        Prelude.Nothing,
      anomalyResources = Prelude.Nothing,
      anomalyTimeRange = Prelude.Nothing,
      associatedInsightId = Prelude.Nothing,
      causalAnomalyId = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      severity = Prelude.Nothing,
      sourceDetails = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | An @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
reactiveAnomalySummary_anomalyReportedTimeRange :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe AnomalyReportedTimeRange)
reactiveAnomalySummary_anomalyReportedTimeRange = Lens.lens (\ReactiveAnomalySummary' {anomalyReportedTimeRange} -> anomalyReportedTimeRange) (\s@ReactiveAnomalySummary' {} a -> s {anomalyReportedTimeRange = a} :: ReactiveAnomalySummary)

-- | The Amazon Web Services resources in which anomalous behavior was
-- detected by DevOps Guru.
reactiveAnomalySummary_anomalyResources :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe [AnomalyResource])
reactiveAnomalySummary_anomalyResources = Lens.lens (\ReactiveAnomalySummary' {anomalyResources} -> anomalyResources) (\s@ReactiveAnomalySummary' {} a -> s {anomalyResources = a} :: ReactiveAnomalySummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
reactiveAnomalySummary_anomalyTimeRange :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe AnomalyTimeRange)
reactiveAnomalySummary_anomalyTimeRange = Lens.lens (\ReactiveAnomalySummary' {anomalyTimeRange} -> anomalyTimeRange) (\s@ReactiveAnomalySummary' {} a -> s {anomalyTimeRange = a} :: ReactiveAnomalySummary)

-- | The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
reactiveAnomalySummary_associatedInsightId :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe Prelude.Text)
reactiveAnomalySummary_associatedInsightId = Lens.lens (\ReactiveAnomalySummary' {associatedInsightId} -> associatedInsightId) (\s@ReactiveAnomalySummary' {} a -> s {associatedInsightId = a} :: ReactiveAnomalySummary)

-- | The ID of the causal anomaly that is associated with this reactive
-- anomaly. The ID of a \`CAUSAL\` anomaly is always \`NULL\`.
reactiveAnomalySummary_causalAnomalyId :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe Prelude.Text)
reactiveAnomalySummary_causalAnomalyId = Lens.lens (\ReactiveAnomalySummary' {causalAnomalyId} -> causalAnomalyId) (\s@ReactiveAnomalySummary' {} a -> s {causalAnomalyId = a} :: ReactiveAnomalySummary)

-- | A description of the reactive anomaly.
reactiveAnomalySummary_description :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe Prelude.Text)
reactiveAnomalySummary_description = Lens.lens (\ReactiveAnomalySummary' {description} -> description) (\s@ReactiveAnomalySummary' {} a -> s {description = a} :: ReactiveAnomalySummary)

-- | The ID of the reactive anomaly.
reactiveAnomalySummary_id :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe Prelude.Text)
reactiveAnomalySummary_id = Lens.lens (\ReactiveAnomalySummary' {id} -> id) (\s@ReactiveAnomalySummary' {} a -> s {id = a} :: ReactiveAnomalySummary)

-- | The name of the reactive anomaly.
reactiveAnomalySummary_name :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe Prelude.Text)
reactiveAnomalySummary_name = Lens.lens (\ReactiveAnomalySummary' {name} -> name) (\s@ReactiveAnomalySummary' {} a -> s {name = a} :: ReactiveAnomalySummary)

-- | Undocumented member.
reactiveAnomalySummary_resourceCollection :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe ResourceCollection)
reactiveAnomalySummary_resourceCollection = Lens.lens (\ReactiveAnomalySummary' {resourceCollection} -> resourceCollection) (\s@ReactiveAnomalySummary' {} a -> s {resourceCollection = a} :: ReactiveAnomalySummary)

-- | The severity of the anomaly. The severity of anomalies that generate an
-- insight determine that insight\'s severity. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
reactiveAnomalySummary_severity :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe AnomalySeverity)
reactiveAnomalySummary_severity = Lens.lens (\ReactiveAnomalySummary' {severity} -> severity) (\s@ReactiveAnomalySummary' {} a -> s {severity = a} :: ReactiveAnomalySummary)

-- | Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
reactiveAnomalySummary_sourceDetails :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe AnomalySourceDetails)
reactiveAnomalySummary_sourceDetails = Lens.lens (\ReactiveAnomalySummary' {sourceDetails} -> sourceDetails) (\s@ReactiveAnomalySummary' {} a -> s {sourceDetails = a} :: ReactiveAnomalySummary)

-- | The status of the reactive anomaly.
reactiveAnomalySummary_status :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe AnomalyStatus)
reactiveAnomalySummary_status = Lens.lens (\ReactiveAnomalySummary' {status} -> status) (\s@ReactiveAnomalySummary' {} a -> s {status = a} :: ReactiveAnomalySummary)

-- | The type of the reactive anomaly. It can be one of the following types.
--
-- -   @CAUSAL@ - the anomaly can cause a new insight.
--
-- -   @CONTEXTUAL@ - the anomaly contains additional information about an
--     insight or its causal anomaly.
reactiveAnomalySummary_type :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe AnomalyType)
reactiveAnomalySummary_type = Lens.lens (\ReactiveAnomalySummary' {type'} -> type') (\s@ReactiveAnomalySummary' {} a -> s {type' = a} :: ReactiveAnomalySummary)

instance Data.FromJSON ReactiveAnomalySummary where
  parseJSON =
    Data.withObject
      "ReactiveAnomalySummary"
      ( \x ->
          ReactiveAnomalySummary'
            Prelude.<$> (x Data..:? "AnomalyReportedTimeRange")
            Prelude.<*> ( x Data..:? "AnomalyResources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AnomalyTimeRange")
            Prelude.<*> (x Data..:? "AssociatedInsightId")
            Prelude.<*> (x Data..:? "CausalAnomalyId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ResourceCollection")
            Prelude.<*> (x Data..:? "Severity")
            Prelude.<*> (x Data..:? "SourceDetails")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable ReactiveAnomalySummary where
  hashWithSalt _salt ReactiveAnomalySummary' {..} =
    _salt
      `Prelude.hashWithSalt` anomalyReportedTimeRange
      `Prelude.hashWithSalt` anomalyResources
      `Prelude.hashWithSalt` anomalyTimeRange
      `Prelude.hashWithSalt` associatedInsightId
      `Prelude.hashWithSalt` causalAnomalyId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` sourceDetails
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ReactiveAnomalySummary where
  rnf ReactiveAnomalySummary' {..} =
    Prelude.rnf anomalyReportedTimeRange
      `Prelude.seq` Prelude.rnf anomalyResources
      `Prelude.seq` Prelude.rnf anomalyTimeRange
      `Prelude.seq` Prelude.rnf associatedInsightId
      `Prelude.seq` Prelude.rnf causalAnomalyId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf sourceDetails
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
