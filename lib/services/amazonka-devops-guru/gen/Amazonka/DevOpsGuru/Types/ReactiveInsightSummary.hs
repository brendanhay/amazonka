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
-- Module      : Amazonka.DevOpsGuru.Types.ReactiveInsightSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ReactiveInsightSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.InsightSeverity
import Amazonka.DevOpsGuru.Types.InsightStatus
import Amazonka.DevOpsGuru.Types.InsightTimeRange
import Amazonka.DevOpsGuru.Types.ResourceCollection
import Amazonka.DevOpsGuru.Types.ServiceCollection
import qualified Amazonka.Prelude as Prelude

-- | Information about a reactive insight. This object is returned by
-- @DescribeInsight.@
--
-- /See:/ 'newReactiveInsightSummary' smart constructor.
data ReactiveInsightSummary = ReactiveInsightSummary'
  { -- | The Amazon Resource Names (ARNs) of the Amazon Web Services resources
    -- that generated this insight.
    associatedResourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The ID of a reactive summary.
    id :: Prelude.Maybe Prelude.Text,
    insightTimeRange :: Prelude.Maybe InsightTimeRange,
    -- | The name of a reactive insight.
    name :: Prelude.Maybe Prelude.Text,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | A collection of the names of Amazon Web Services services.
    serviceCollection :: Prelude.Maybe ServiceCollection,
    -- | The severity of the insight. For more information, see
    -- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
    -- in the /Amazon DevOps Guru User Guide/.
    severity :: Prelude.Maybe InsightSeverity,
    -- | The status of a reactive insight.
    status :: Prelude.Maybe InsightStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReactiveInsightSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedResourceArns', 'reactiveInsightSummary_associatedResourceArns' - The Amazon Resource Names (ARNs) of the Amazon Web Services resources
-- that generated this insight.
--
-- 'id', 'reactiveInsightSummary_id' - The ID of a reactive summary.
--
-- 'insightTimeRange', 'reactiveInsightSummary_insightTimeRange' - Undocumented member.
--
-- 'name', 'reactiveInsightSummary_name' - The name of a reactive insight.
--
-- 'resourceCollection', 'reactiveInsightSummary_resourceCollection' - Undocumented member.
--
-- 'serviceCollection', 'reactiveInsightSummary_serviceCollection' - A collection of the names of Amazon Web Services services.
--
-- 'severity', 'reactiveInsightSummary_severity' - The severity of the insight. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
--
-- 'status', 'reactiveInsightSummary_status' - The status of a reactive insight.
newReactiveInsightSummary ::
  ReactiveInsightSummary
newReactiveInsightSummary =
  ReactiveInsightSummary'
    { associatedResourceArns =
        Prelude.Nothing,
      id = Prelude.Nothing,
      insightTimeRange = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      serviceCollection = Prelude.Nothing,
      severity = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Names (ARNs) of the Amazon Web Services resources
-- that generated this insight.
reactiveInsightSummary_associatedResourceArns :: Lens.Lens' ReactiveInsightSummary (Prelude.Maybe [Prelude.Text])
reactiveInsightSummary_associatedResourceArns = Lens.lens (\ReactiveInsightSummary' {associatedResourceArns} -> associatedResourceArns) (\s@ReactiveInsightSummary' {} a -> s {associatedResourceArns = a} :: ReactiveInsightSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ID of a reactive summary.
reactiveInsightSummary_id :: Lens.Lens' ReactiveInsightSummary (Prelude.Maybe Prelude.Text)
reactiveInsightSummary_id = Lens.lens (\ReactiveInsightSummary' {id} -> id) (\s@ReactiveInsightSummary' {} a -> s {id = a} :: ReactiveInsightSummary)

-- | Undocumented member.
reactiveInsightSummary_insightTimeRange :: Lens.Lens' ReactiveInsightSummary (Prelude.Maybe InsightTimeRange)
reactiveInsightSummary_insightTimeRange = Lens.lens (\ReactiveInsightSummary' {insightTimeRange} -> insightTimeRange) (\s@ReactiveInsightSummary' {} a -> s {insightTimeRange = a} :: ReactiveInsightSummary)

-- | The name of a reactive insight.
reactiveInsightSummary_name :: Lens.Lens' ReactiveInsightSummary (Prelude.Maybe Prelude.Text)
reactiveInsightSummary_name = Lens.lens (\ReactiveInsightSummary' {name} -> name) (\s@ReactiveInsightSummary' {} a -> s {name = a} :: ReactiveInsightSummary)

-- | Undocumented member.
reactiveInsightSummary_resourceCollection :: Lens.Lens' ReactiveInsightSummary (Prelude.Maybe ResourceCollection)
reactiveInsightSummary_resourceCollection = Lens.lens (\ReactiveInsightSummary' {resourceCollection} -> resourceCollection) (\s@ReactiveInsightSummary' {} a -> s {resourceCollection = a} :: ReactiveInsightSummary)

-- | A collection of the names of Amazon Web Services services.
reactiveInsightSummary_serviceCollection :: Lens.Lens' ReactiveInsightSummary (Prelude.Maybe ServiceCollection)
reactiveInsightSummary_serviceCollection = Lens.lens (\ReactiveInsightSummary' {serviceCollection} -> serviceCollection) (\s@ReactiveInsightSummary' {} a -> s {serviceCollection = a} :: ReactiveInsightSummary)

-- | The severity of the insight. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
reactiveInsightSummary_severity :: Lens.Lens' ReactiveInsightSummary (Prelude.Maybe InsightSeverity)
reactiveInsightSummary_severity = Lens.lens (\ReactiveInsightSummary' {severity} -> severity) (\s@ReactiveInsightSummary' {} a -> s {severity = a} :: ReactiveInsightSummary)

-- | The status of a reactive insight.
reactiveInsightSummary_status :: Lens.Lens' ReactiveInsightSummary (Prelude.Maybe InsightStatus)
reactiveInsightSummary_status = Lens.lens (\ReactiveInsightSummary' {status} -> status) (\s@ReactiveInsightSummary' {} a -> s {status = a} :: ReactiveInsightSummary)

instance Data.FromJSON ReactiveInsightSummary where
  parseJSON =
    Data.withObject
      "ReactiveInsightSummary"
      ( \x ->
          ReactiveInsightSummary'
            Prelude.<$> ( x
                            Data..:? "AssociatedResourceArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InsightTimeRange")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ResourceCollection")
            Prelude.<*> (x Data..:? "ServiceCollection")
            Prelude.<*> (x Data..:? "Severity")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable ReactiveInsightSummary where
  hashWithSalt _salt ReactiveInsightSummary' {..} =
    _salt
      `Prelude.hashWithSalt` associatedResourceArns
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` insightTimeRange
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` serviceCollection
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` status

instance Prelude.NFData ReactiveInsightSummary where
  rnf ReactiveInsightSummary' {..} =
    Prelude.rnf associatedResourceArns
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf insightTimeRange
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf serviceCollection
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf status
