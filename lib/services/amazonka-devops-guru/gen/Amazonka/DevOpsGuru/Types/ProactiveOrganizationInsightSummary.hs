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
-- Module      : Amazonka.DevOpsGuru.Types.ProactiveOrganizationInsightSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ProactiveOrganizationInsightSummary where

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
-- @DescribeInsight@.
--
-- /See:/ 'newProactiveOrganizationInsightSummary' smart constructor.
data ProactiveOrganizationInsightSummary = ProactiveOrganizationInsightSummary'
  { -- | The ID of the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the insight summary.
    id :: Prelude.Maybe Prelude.Text,
    insightTimeRange :: Prelude.Maybe InsightTimeRange,
    -- | The name of the insight summary.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the organizational unit.
    organizationalUnitId :: Prelude.Maybe Prelude.Text,
    predictionTimeRange :: Prelude.Maybe PredictionTimeRange,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    serviceCollection :: Prelude.Maybe ServiceCollection,
    -- | An array of severity values used to search for insights. For more
    -- information, see
    -- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
    -- in the /Amazon DevOps Guru User Guide/.
    severity :: Prelude.Maybe InsightSeverity,
    -- | An array of status values used to search for insights.
    status :: Prelude.Maybe InsightStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProactiveOrganizationInsightSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'proactiveOrganizationInsightSummary_accountId' - The ID of the Amazon Web Services account.
--
-- 'id', 'proactiveOrganizationInsightSummary_id' - The ID of the insight summary.
--
-- 'insightTimeRange', 'proactiveOrganizationInsightSummary_insightTimeRange' - Undocumented member.
--
-- 'name', 'proactiveOrganizationInsightSummary_name' - The name of the insight summary.
--
-- 'organizationalUnitId', 'proactiveOrganizationInsightSummary_organizationalUnitId' - The ID of the organizational unit.
--
-- 'predictionTimeRange', 'proactiveOrganizationInsightSummary_predictionTimeRange' - Undocumented member.
--
-- 'resourceCollection', 'proactiveOrganizationInsightSummary_resourceCollection' - Undocumented member.
--
-- 'serviceCollection', 'proactiveOrganizationInsightSummary_serviceCollection' - Undocumented member.
--
-- 'severity', 'proactiveOrganizationInsightSummary_severity' - An array of severity values used to search for insights. For more
-- information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
--
-- 'status', 'proactiveOrganizationInsightSummary_status' - An array of status values used to search for insights.
newProactiveOrganizationInsightSummary ::
  ProactiveOrganizationInsightSummary
newProactiveOrganizationInsightSummary =
  ProactiveOrganizationInsightSummary'
    { accountId =
        Prelude.Nothing,
      id = Prelude.Nothing,
      insightTimeRange = Prelude.Nothing,
      name = Prelude.Nothing,
      organizationalUnitId = Prelude.Nothing,
      predictionTimeRange = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      serviceCollection = Prelude.Nothing,
      severity = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services account.
proactiveOrganizationInsightSummary_accountId :: Lens.Lens' ProactiveOrganizationInsightSummary (Prelude.Maybe Prelude.Text)
proactiveOrganizationInsightSummary_accountId = Lens.lens (\ProactiveOrganizationInsightSummary' {accountId} -> accountId) (\s@ProactiveOrganizationInsightSummary' {} a -> s {accountId = a} :: ProactiveOrganizationInsightSummary)

-- | The ID of the insight summary.
proactiveOrganizationInsightSummary_id :: Lens.Lens' ProactiveOrganizationInsightSummary (Prelude.Maybe Prelude.Text)
proactiveOrganizationInsightSummary_id = Lens.lens (\ProactiveOrganizationInsightSummary' {id} -> id) (\s@ProactiveOrganizationInsightSummary' {} a -> s {id = a} :: ProactiveOrganizationInsightSummary)

-- | Undocumented member.
proactiveOrganizationInsightSummary_insightTimeRange :: Lens.Lens' ProactiveOrganizationInsightSummary (Prelude.Maybe InsightTimeRange)
proactiveOrganizationInsightSummary_insightTimeRange = Lens.lens (\ProactiveOrganizationInsightSummary' {insightTimeRange} -> insightTimeRange) (\s@ProactiveOrganizationInsightSummary' {} a -> s {insightTimeRange = a} :: ProactiveOrganizationInsightSummary)

-- | The name of the insight summary.
proactiveOrganizationInsightSummary_name :: Lens.Lens' ProactiveOrganizationInsightSummary (Prelude.Maybe Prelude.Text)
proactiveOrganizationInsightSummary_name = Lens.lens (\ProactiveOrganizationInsightSummary' {name} -> name) (\s@ProactiveOrganizationInsightSummary' {} a -> s {name = a} :: ProactiveOrganizationInsightSummary)

-- | The ID of the organizational unit.
proactiveOrganizationInsightSummary_organizationalUnitId :: Lens.Lens' ProactiveOrganizationInsightSummary (Prelude.Maybe Prelude.Text)
proactiveOrganizationInsightSummary_organizationalUnitId = Lens.lens (\ProactiveOrganizationInsightSummary' {organizationalUnitId} -> organizationalUnitId) (\s@ProactiveOrganizationInsightSummary' {} a -> s {organizationalUnitId = a} :: ProactiveOrganizationInsightSummary)

-- | Undocumented member.
proactiveOrganizationInsightSummary_predictionTimeRange :: Lens.Lens' ProactiveOrganizationInsightSummary (Prelude.Maybe PredictionTimeRange)
proactiveOrganizationInsightSummary_predictionTimeRange = Lens.lens (\ProactiveOrganizationInsightSummary' {predictionTimeRange} -> predictionTimeRange) (\s@ProactiveOrganizationInsightSummary' {} a -> s {predictionTimeRange = a} :: ProactiveOrganizationInsightSummary)

-- | Undocumented member.
proactiveOrganizationInsightSummary_resourceCollection :: Lens.Lens' ProactiveOrganizationInsightSummary (Prelude.Maybe ResourceCollection)
proactiveOrganizationInsightSummary_resourceCollection = Lens.lens (\ProactiveOrganizationInsightSummary' {resourceCollection} -> resourceCollection) (\s@ProactiveOrganizationInsightSummary' {} a -> s {resourceCollection = a} :: ProactiveOrganizationInsightSummary)

-- | Undocumented member.
proactiveOrganizationInsightSummary_serviceCollection :: Lens.Lens' ProactiveOrganizationInsightSummary (Prelude.Maybe ServiceCollection)
proactiveOrganizationInsightSummary_serviceCollection = Lens.lens (\ProactiveOrganizationInsightSummary' {serviceCollection} -> serviceCollection) (\s@ProactiveOrganizationInsightSummary' {} a -> s {serviceCollection = a} :: ProactiveOrganizationInsightSummary)

-- | An array of severity values used to search for insights. For more
-- information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
proactiveOrganizationInsightSummary_severity :: Lens.Lens' ProactiveOrganizationInsightSummary (Prelude.Maybe InsightSeverity)
proactiveOrganizationInsightSummary_severity = Lens.lens (\ProactiveOrganizationInsightSummary' {severity} -> severity) (\s@ProactiveOrganizationInsightSummary' {} a -> s {severity = a} :: ProactiveOrganizationInsightSummary)

-- | An array of status values used to search for insights.
proactiveOrganizationInsightSummary_status :: Lens.Lens' ProactiveOrganizationInsightSummary (Prelude.Maybe InsightStatus)
proactiveOrganizationInsightSummary_status = Lens.lens (\ProactiveOrganizationInsightSummary' {status} -> status) (\s@ProactiveOrganizationInsightSummary' {} a -> s {status = a} :: ProactiveOrganizationInsightSummary)

instance
  Data.FromJSON
    ProactiveOrganizationInsightSummary
  where
  parseJSON =
    Data.withObject
      "ProactiveOrganizationInsightSummary"
      ( \x ->
          ProactiveOrganizationInsightSummary'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InsightTimeRange")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OrganizationalUnitId")
            Prelude.<*> (x Data..:? "PredictionTimeRange")
            Prelude.<*> (x Data..:? "ResourceCollection")
            Prelude.<*> (x Data..:? "ServiceCollection")
            Prelude.<*> (x Data..:? "Severity")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    ProactiveOrganizationInsightSummary
  where
  hashWithSalt
    _salt
    ProactiveOrganizationInsightSummary' {..} =
      _salt `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` insightTimeRange
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` organizationalUnitId
        `Prelude.hashWithSalt` predictionTimeRange
        `Prelude.hashWithSalt` resourceCollection
        `Prelude.hashWithSalt` serviceCollection
        `Prelude.hashWithSalt` severity
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    ProactiveOrganizationInsightSummary
  where
  rnf ProactiveOrganizationInsightSummary' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf insightTimeRange
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf organizationalUnitId
      `Prelude.seq` Prelude.rnf predictionTimeRange
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf serviceCollection
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf status
