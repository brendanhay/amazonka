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
-- Module      : Amazonka.DevOpsGuru.Types.ProactiveInsightSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ProactiveInsightSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.InsightSeverity
import Amazonka.DevOpsGuru.Types.InsightStatus
import Amazonka.DevOpsGuru.Types.InsightTimeRange
import Amazonka.DevOpsGuru.Types.PredictionTimeRange
import Amazonka.DevOpsGuru.Types.ResourceCollection
import Amazonka.DevOpsGuru.Types.ServiceCollection
import qualified Amazonka.Prelude as Prelude

-- | Details about a proactive insight. This object is returned by
-- @DescribeInsight.@
--
-- /See:/ 'newProactiveInsightSummary' smart constructor.
data ProactiveInsightSummary = ProactiveInsightSummary'
  { -- | The severity of the insight. For more information, see
    -- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
    -- in the /Amazon DevOps Guru User Guide/.
    severity :: Prelude.Maybe InsightSeverity,
    -- | The name of the proactive insight.
    name :: Prelude.Maybe Prelude.Text,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | A collection of the names of Amazon Web Services services.
    serviceCollection :: Prelude.Maybe ServiceCollection,
    -- | The Amazon Resource Names (ARNs) of the Amazon Web Services resources
    -- that generated this insight.
    associatedResourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The status of the proactive insight.
    status :: Prelude.Maybe InsightStatus,
    -- | The ID of the proactive insight.
    id :: Prelude.Maybe Prelude.Text,
    predictionTimeRange :: Prelude.Maybe PredictionTimeRange,
    insightTimeRange :: Prelude.Maybe InsightTimeRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProactiveInsightSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severity', 'proactiveInsightSummary_severity' - The severity of the insight. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
--
-- 'name', 'proactiveInsightSummary_name' - The name of the proactive insight.
--
-- 'resourceCollection', 'proactiveInsightSummary_resourceCollection' - Undocumented member.
--
-- 'serviceCollection', 'proactiveInsightSummary_serviceCollection' - A collection of the names of Amazon Web Services services.
--
-- 'associatedResourceArns', 'proactiveInsightSummary_associatedResourceArns' - The Amazon Resource Names (ARNs) of the Amazon Web Services resources
-- that generated this insight.
--
-- 'status', 'proactiveInsightSummary_status' - The status of the proactive insight.
--
-- 'id', 'proactiveInsightSummary_id' - The ID of the proactive insight.
--
-- 'predictionTimeRange', 'proactiveInsightSummary_predictionTimeRange' - Undocumented member.
--
-- 'insightTimeRange', 'proactiveInsightSummary_insightTimeRange' - Undocumented member.
newProactiveInsightSummary ::
  ProactiveInsightSummary
newProactiveInsightSummary =
  ProactiveInsightSummary'
    { severity =
        Prelude.Nothing,
      name = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      serviceCollection = Prelude.Nothing,
      associatedResourceArns = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      predictionTimeRange = Prelude.Nothing,
      insightTimeRange = Prelude.Nothing
    }

-- | The severity of the insight. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
proactiveInsightSummary_severity :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe InsightSeverity)
proactiveInsightSummary_severity = Lens.lens (\ProactiveInsightSummary' {severity} -> severity) (\s@ProactiveInsightSummary' {} a -> s {severity = a} :: ProactiveInsightSummary)

-- | The name of the proactive insight.
proactiveInsightSummary_name :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe Prelude.Text)
proactiveInsightSummary_name = Lens.lens (\ProactiveInsightSummary' {name} -> name) (\s@ProactiveInsightSummary' {} a -> s {name = a} :: ProactiveInsightSummary)

-- | Undocumented member.
proactiveInsightSummary_resourceCollection :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe ResourceCollection)
proactiveInsightSummary_resourceCollection = Lens.lens (\ProactiveInsightSummary' {resourceCollection} -> resourceCollection) (\s@ProactiveInsightSummary' {} a -> s {resourceCollection = a} :: ProactiveInsightSummary)

-- | A collection of the names of Amazon Web Services services.
proactiveInsightSummary_serviceCollection :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe ServiceCollection)
proactiveInsightSummary_serviceCollection = Lens.lens (\ProactiveInsightSummary' {serviceCollection} -> serviceCollection) (\s@ProactiveInsightSummary' {} a -> s {serviceCollection = a} :: ProactiveInsightSummary)

-- | The Amazon Resource Names (ARNs) of the Amazon Web Services resources
-- that generated this insight.
proactiveInsightSummary_associatedResourceArns :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe [Prelude.Text])
proactiveInsightSummary_associatedResourceArns = Lens.lens (\ProactiveInsightSummary' {associatedResourceArns} -> associatedResourceArns) (\s@ProactiveInsightSummary' {} a -> s {associatedResourceArns = a} :: ProactiveInsightSummary) Prelude.. Lens.mapping Lens.coerced

-- | The status of the proactive insight.
proactiveInsightSummary_status :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe InsightStatus)
proactiveInsightSummary_status = Lens.lens (\ProactiveInsightSummary' {status} -> status) (\s@ProactiveInsightSummary' {} a -> s {status = a} :: ProactiveInsightSummary)

-- | The ID of the proactive insight.
proactiveInsightSummary_id :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe Prelude.Text)
proactiveInsightSummary_id = Lens.lens (\ProactiveInsightSummary' {id} -> id) (\s@ProactiveInsightSummary' {} a -> s {id = a} :: ProactiveInsightSummary)

-- | Undocumented member.
proactiveInsightSummary_predictionTimeRange :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe PredictionTimeRange)
proactiveInsightSummary_predictionTimeRange = Lens.lens (\ProactiveInsightSummary' {predictionTimeRange} -> predictionTimeRange) (\s@ProactiveInsightSummary' {} a -> s {predictionTimeRange = a} :: ProactiveInsightSummary)

-- | Undocumented member.
proactiveInsightSummary_insightTimeRange :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe InsightTimeRange)
proactiveInsightSummary_insightTimeRange = Lens.lens (\ProactiveInsightSummary' {insightTimeRange} -> insightTimeRange) (\s@ProactiveInsightSummary' {} a -> s {insightTimeRange = a} :: ProactiveInsightSummary)

instance Data.FromJSON ProactiveInsightSummary where
  parseJSON =
    Data.withObject
      "ProactiveInsightSummary"
      ( \x ->
          ProactiveInsightSummary'
            Prelude.<$> (x Data..:? "Severity")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ResourceCollection")
            Prelude.<*> (x Data..:? "ServiceCollection")
            Prelude.<*> ( x Data..:? "AssociatedResourceArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "PredictionTimeRange")
            Prelude.<*> (x Data..:? "InsightTimeRange")
      )

instance Prelude.Hashable ProactiveInsightSummary where
  hashWithSalt _salt ProactiveInsightSummary' {..} =
    _salt `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` serviceCollection
      `Prelude.hashWithSalt` associatedResourceArns
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` predictionTimeRange
      `Prelude.hashWithSalt` insightTimeRange

instance Prelude.NFData ProactiveInsightSummary where
  rnf ProactiveInsightSummary' {..} =
    Prelude.rnf severity
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf serviceCollection
      `Prelude.seq` Prelude.rnf associatedResourceArns
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf predictionTimeRange
      `Prelude.seq` Prelude.rnf insightTimeRange
