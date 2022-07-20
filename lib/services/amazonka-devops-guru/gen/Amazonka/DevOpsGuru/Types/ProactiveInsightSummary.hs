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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ProactiveInsightSummary where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.InsightSeverity
import Amazonka.DevOpsGuru.Types.InsightStatus
import Amazonka.DevOpsGuru.Types.InsightTimeRange
import Amazonka.DevOpsGuru.Types.PredictionTimeRange
import Amazonka.DevOpsGuru.Types.ResourceCollection
import Amazonka.DevOpsGuru.Types.ServiceCollection
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a proactive insight. This object is returned by
-- @DescribeInsight.@
--
-- /See:/ 'newProactiveInsightSummary' smart constructor.
data ProactiveInsightSummary = ProactiveInsightSummary'
  { -- | The severity of the proactive insight.
    severity :: Prelude.Maybe InsightSeverity,
    -- | The name of the proactive insight.
    name :: Prelude.Maybe Prelude.Text,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | A collection of the names of AWS services.
    serviceCollection :: Prelude.Maybe ServiceCollection,
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
-- 'severity', 'proactiveInsightSummary_severity' - The severity of the proactive insight.
--
-- 'name', 'proactiveInsightSummary_name' - The name of the proactive insight.
--
-- 'resourceCollection', 'proactiveInsightSummary_resourceCollection' - Undocumented member.
--
-- 'serviceCollection', 'proactiveInsightSummary_serviceCollection' - A collection of the names of AWS services.
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
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      predictionTimeRange = Prelude.Nothing,
      insightTimeRange = Prelude.Nothing
    }

-- | The severity of the proactive insight.
proactiveInsightSummary_severity :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe InsightSeverity)
proactiveInsightSummary_severity = Lens.lens (\ProactiveInsightSummary' {severity} -> severity) (\s@ProactiveInsightSummary' {} a -> s {severity = a} :: ProactiveInsightSummary)

-- | The name of the proactive insight.
proactiveInsightSummary_name :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe Prelude.Text)
proactiveInsightSummary_name = Lens.lens (\ProactiveInsightSummary' {name} -> name) (\s@ProactiveInsightSummary' {} a -> s {name = a} :: ProactiveInsightSummary)

-- | Undocumented member.
proactiveInsightSummary_resourceCollection :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe ResourceCollection)
proactiveInsightSummary_resourceCollection = Lens.lens (\ProactiveInsightSummary' {resourceCollection} -> resourceCollection) (\s@ProactiveInsightSummary' {} a -> s {resourceCollection = a} :: ProactiveInsightSummary)

-- | A collection of the names of AWS services.
proactiveInsightSummary_serviceCollection :: Lens.Lens' ProactiveInsightSummary (Prelude.Maybe ServiceCollection)
proactiveInsightSummary_serviceCollection = Lens.lens (\ProactiveInsightSummary' {serviceCollection} -> serviceCollection) (\s@ProactiveInsightSummary' {} a -> s {serviceCollection = a} :: ProactiveInsightSummary)

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

instance Core.FromJSON ProactiveInsightSummary where
  parseJSON =
    Core.withObject
      "ProactiveInsightSummary"
      ( \x ->
          ProactiveInsightSummary'
            Prelude.<$> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ResourceCollection")
            Prelude.<*> (x Core..:? "ServiceCollection")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "PredictionTimeRange")
            Prelude.<*> (x Core..:? "InsightTimeRange")
      )

instance Prelude.Hashable ProactiveInsightSummary where
  hashWithSalt _salt ProactiveInsightSummary' {..} =
    _salt `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` serviceCollection
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
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf predictionTimeRange
      `Prelude.seq` Prelude.rnf insightTimeRange
