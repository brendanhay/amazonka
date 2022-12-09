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
-- Module      : Amazonka.DevOpsGuru.Types.ReactiveOrganizationInsightSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ReactiveOrganizationInsightSummary where

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
-- @DescribeInsight@.
--
-- /See:/ 'newReactiveOrganizationInsightSummary' smart constructor.
data ReactiveOrganizationInsightSummary = ReactiveOrganizationInsightSummary'
  { -- | The ID of the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the insight summary.
    id :: Prelude.Maybe Prelude.Text,
    insightTimeRange :: Prelude.Maybe InsightTimeRange,
    -- | The name of the insight summary.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the organizational unit.
    organizationalUnitId :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'ReactiveOrganizationInsightSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'reactiveOrganizationInsightSummary_accountId' - The ID of the Amazon Web Services account.
--
-- 'id', 'reactiveOrganizationInsightSummary_id' - The ID of the insight summary.
--
-- 'insightTimeRange', 'reactiveOrganizationInsightSummary_insightTimeRange' - Undocumented member.
--
-- 'name', 'reactiveOrganizationInsightSummary_name' - The name of the insight summary.
--
-- 'organizationalUnitId', 'reactiveOrganizationInsightSummary_organizationalUnitId' - The ID of the organizational unit.
--
-- 'resourceCollection', 'reactiveOrganizationInsightSummary_resourceCollection' - Undocumented member.
--
-- 'serviceCollection', 'reactiveOrganizationInsightSummary_serviceCollection' - Undocumented member.
--
-- 'severity', 'reactiveOrganizationInsightSummary_severity' - An array of severity values used to search for insights. For more
-- information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
--
-- 'status', 'reactiveOrganizationInsightSummary_status' - An array of status values used to search for insights.
newReactiveOrganizationInsightSummary ::
  ReactiveOrganizationInsightSummary
newReactiveOrganizationInsightSummary =
  ReactiveOrganizationInsightSummary'
    { accountId =
        Prelude.Nothing,
      id = Prelude.Nothing,
      insightTimeRange = Prelude.Nothing,
      name = Prelude.Nothing,
      organizationalUnitId = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      serviceCollection = Prelude.Nothing,
      severity = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services account.
reactiveOrganizationInsightSummary_accountId :: Lens.Lens' ReactiveOrganizationInsightSummary (Prelude.Maybe Prelude.Text)
reactiveOrganizationInsightSummary_accountId = Lens.lens (\ReactiveOrganizationInsightSummary' {accountId} -> accountId) (\s@ReactiveOrganizationInsightSummary' {} a -> s {accountId = a} :: ReactiveOrganizationInsightSummary)

-- | The ID of the insight summary.
reactiveOrganizationInsightSummary_id :: Lens.Lens' ReactiveOrganizationInsightSummary (Prelude.Maybe Prelude.Text)
reactiveOrganizationInsightSummary_id = Lens.lens (\ReactiveOrganizationInsightSummary' {id} -> id) (\s@ReactiveOrganizationInsightSummary' {} a -> s {id = a} :: ReactiveOrganizationInsightSummary)

-- | Undocumented member.
reactiveOrganizationInsightSummary_insightTimeRange :: Lens.Lens' ReactiveOrganizationInsightSummary (Prelude.Maybe InsightTimeRange)
reactiveOrganizationInsightSummary_insightTimeRange = Lens.lens (\ReactiveOrganizationInsightSummary' {insightTimeRange} -> insightTimeRange) (\s@ReactiveOrganizationInsightSummary' {} a -> s {insightTimeRange = a} :: ReactiveOrganizationInsightSummary)

-- | The name of the insight summary.
reactiveOrganizationInsightSummary_name :: Lens.Lens' ReactiveOrganizationInsightSummary (Prelude.Maybe Prelude.Text)
reactiveOrganizationInsightSummary_name = Lens.lens (\ReactiveOrganizationInsightSummary' {name} -> name) (\s@ReactiveOrganizationInsightSummary' {} a -> s {name = a} :: ReactiveOrganizationInsightSummary)

-- | The ID of the organizational unit.
reactiveOrganizationInsightSummary_organizationalUnitId :: Lens.Lens' ReactiveOrganizationInsightSummary (Prelude.Maybe Prelude.Text)
reactiveOrganizationInsightSummary_organizationalUnitId = Lens.lens (\ReactiveOrganizationInsightSummary' {organizationalUnitId} -> organizationalUnitId) (\s@ReactiveOrganizationInsightSummary' {} a -> s {organizationalUnitId = a} :: ReactiveOrganizationInsightSummary)

-- | Undocumented member.
reactiveOrganizationInsightSummary_resourceCollection :: Lens.Lens' ReactiveOrganizationInsightSummary (Prelude.Maybe ResourceCollection)
reactiveOrganizationInsightSummary_resourceCollection = Lens.lens (\ReactiveOrganizationInsightSummary' {resourceCollection} -> resourceCollection) (\s@ReactiveOrganizationInsightSummary' {} a -> s {resourceCollection = a} :: ReactiveOrganizationInsightSummary)

-- | Undocumented member.
reactiveOrganizationInsightSummary_serviceCollection :: Lens.Lens' ReactiveOrganizationInsightSummary (Prelude.Maybe ServiceCollection)
reactiveOrganizationInsightSummary_serviceCollection = Lens.lens (\ReactiveOrganizationInsightSummary' {serviceCollection} -> serviceCollection) (\s@ReactiveOrganizationInsightSummary' {} a -> s {serviceCollection = a} :: ReactiveOrganizationInsightSummary)

-- | An array of severity values used to search for insights. For more
-- information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>
-- in the /Amazon DevOps Guru User Guide/.
reactiveOrganizationInsightSummary_severity :: Lens.Lens' ReactiveOrganizationInsightSummary (Prelude.Maybe InsightSeverity)
reactiveOrganizationInsightSummary_severity = Lens.lens (\ReactiveOrganizationInsightSummary' {severity} -> severity) (\s@ReactiveOrganizationInsightSummary' {} a -> s {severity = a} :: ReactiveOrganizationInsightSummary)

-- | An array of status values used to search for insights.
reactiveOrganizationInsightSummary_status :: Lens.Lens' ReactiveOrganizationInsightSummary (Prelude.Maybe InsightStatus)
reactiveOrganizationInsightSummary_status = Lens.lens (\ReactiveOrganizationInsightSummary' {status} -> status) (\s@ReactiveOrganizationInsightSummary' {} a -> s {status = a} :: ReactiveOrganizationInsightSummary)

instance
  Data.FromJSON
    ReactiveOrganizationInsightSummary
  where
  parseJSON =
    Data.withObject
      "ReactiveOrganizationInsightSummary"
      ( \x ->
          ReactiveOrganizationInsightSummary'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InsightTimeRange")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OrganizationalUnitId")
            Prelude.<*> (x Data..:? "ResourceCollection")
            Prelude.<*> (x Data..:? "ServiceCollection")
            Prelude.<*> (x Data..:? "Severity")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    ReactiveOrganizationInsightSummary
  where
  hashWithSalt
    _salt
    ReactiveOrganizationInsightSummary' {..} =
      _salt `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` insightTimeRange
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` organizationalUnitId
        `Prelude.hashWithSalt` resourceCollection
        `Prelude.hashWithSalt` serviceCollection
        `Prelude.hashWithSalt` severity
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    ReactiveOrganizationInsightSummary
  where
  rnf ReactiveOrganizationInsightSummary' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf insightTimeRange
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf organizationalUnitId
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf serviceCollection
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf status
