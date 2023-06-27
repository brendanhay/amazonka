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
-- Module      : Amazonka.DevOpsGuru.Types.SearchOrganizationInsightsFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.SearchOrganizationInsightsFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.InsightSeverity
import Amazonka.DevOpsGuru.Types.InsightStatus
import Amazonka.DevOpsGuru.Types.ResourceCollection
import Amazonka.DevOpsGuru.Types.ServiceCollection
import qualified Amazonka.Prelude as Prelude

-- | Filters you can use to specify which events are returned when
-- @ListEvents@ is called.
--
-- /See:/ 'newSearchOrganizationInsightsFilters' smart constructor.
data SearchOrganizationInsightsFilters = SearchOrganizationInsightsFilters'
  { resourceCollection :: Prelude.Maybe ResourceCollection,
    serviceCollection :: Prelude.Maybe ServiceCollection,
    -- | An array of severity values used to search for insights.
    severities :: Prelude.Maybe [InsightSeverity],
    -- | An array of status values used to search for insights.
    statuses :: Prelude.Maybe [InsightStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchOrganizationInsightsFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceCollection', 'searchOrganizationInsightsFilters_resourceCollection' - Undocumented member.
--
-- 'serviceCollection', 'searchOrganizationInsightsFilters_serviceCollection' - Undocumented member.
--
-- 'severities', 'searchOrganizationInsightsFilters_severities' - An array of severity values used to search for insights.
--
-- 'statuses', 'searchOrganizationInsightsFilters_statuses' - An array of status values used to search for insights.
newSearchOrganizationInsightsFilters ::
  SearchOrganizationInsightsFilters
newSearchOrganizationInsightsFilters =
  SearchOrganizationInsightsFilters'
    { resourceCollection =
        Prelude.Nothing,
      serviceCollection = Prelude.Nothing,
      severities = Prelude.Nothing,
      statuses = Prelude.Nothing
    }

-- | Undocumented member.
searchOrganizationInsightsFilters_resourceCollection :: Lens.Lens' SearchOrganizationInsightsFilters (Prelude.Maybe ResourceCollection)
searchOrganizationInsightsFilters_resourceCollection = Lens.lens (\SearchOrganizationInsightsFilters' {resourceCollection} -> resourceCollection) (\s@SearchOrganizationInsightsFilters' {} a -> s {resourceCollection = a} :: SearchOrganizationInsightsFilters)

-- | Undocumented member.
searchOrganizationInsightsFilters_serviceCollection :: Lens.Lens' SearchOrganizationInsightsFilters (Prelude.Maybe ServiceCollection)
searchOrganizationInsightsFilters_serviceCollection = Lens.lens (\SearchOrganizationInsightsFilters' {serviceCollection} -> serviceCollection) (\s@SearchOrganizationInsightsFilters' {} a -> s {serviceCollection = a} :: SearchOrganizationInsightsFilters)

-- | An array of severity values used to search for insights.
searchOrganizationInsightsFilters_severities :: Lens.Lens' SearchOrganizationInsightsFilters (Prelude.Maybe [InsightSeverity])
searchOrganizationInsightsFilters_severities = Lens.lens (\SearchOrganizationInsightsFilters' {severities} -> severities) (\s@SearchOrganizationInsightsFilters' {} a -> s {severities = a} :: SearchOrganizationInsightsFilters) Prelude.. Lens.mapping Lens.coerced

-- | An array of status values used to search for insights.
searchOrganizationInsightsFilters_statuses :: Lens.Lens' SearchOrganizationInsightsFilters (Prelude.Maybe [InsightStatus])
searchOrganizationInsightsFilters_statuses = Lens.lens (\SearchOrganizationInsightsFilters' {statuses} -> statuses) (\s@SearchOrganizationInsightsFilters' {} a -> s {statuses = a} :: SearchOrganizationInsightsFilters) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    SearchOrganizationInsightsFilters
  where
  hashWithSalt
    _salt
    SearchOrganizationInsightsFilters' {..} =
      _salt
        `Prelude.hashWithSalt` resourceCollection
        `Prelude.hashWithSalt` serviceCollection
        `Prelude.hashWithSalt` severities
        `Prelude.hashWithSalt` statuses

instance
  Prelude.NFData
    SearchOrganizationInsightsFilters
  where
  rnf SearchOrganizationInsightsFilters' {..} =
    Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf serviceCollection
      `Prelude.seq` Prelude.rnf severities
      `Prelude.seq` Prelude.rnf statuses

instance
  Data.ToJSON
    SearchOrganizationInsightsFilters
  where
  toJSON SearchOrganizationInsightsFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceCollection" Data..=)
              Prelude.<$> resourceCollection,
            ("ServiceCollection" Data..=)
              Prelude.<$> serviceCollection,
            ("Severities" Data..=) Prelude.<$> severities,
            ("Statuses" Data..=) Prelude.<$> statuses
          ]
      )
