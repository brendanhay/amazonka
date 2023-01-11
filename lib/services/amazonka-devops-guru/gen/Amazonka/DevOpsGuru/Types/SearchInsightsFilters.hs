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
-- Module      : Amazonka.DevOpsGuru.Types.SearchInsightsFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.SearchInsightsFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.InsightSeverity
import Amazonka.DevOpsGuru.Types.InsightStatus
import Amazonka.DevOpsGuru.Types.ResourceCollection
import Amazonka.DevOpsGuru.Types.ServiceCollection
import qualified Amazonka.Prelude as Prelude

-- | Specifies one or more severity values and one or more status values that
-- are used to search for insights.
--
-- /See:/ 'newSearchInsightsFilters' smart constructor.
data SearchInsightsFilters = SearchInsightsFilters'
  { resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | A collection of the names of Amazon Web Services services.
    serviceCollection :: Prelude.Maybe ServiceCollection,
    -- | An array of severity values used to search for insights.
    severities :: Prelude.Maybe [InsightSeverity],
    -- | An array of status values used to search for insights.
    statuses :: Prelude.Maybe [InsightStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchInsightsFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceCollection', 'searchInsightsFilters_resourceCollection' - Undocumented member.
--
-- 'serviceCollection', 'searchInsightsFilters_serviceCollection' - A collection of the names of Amazon Web Services services.
--
-- 'severities', 'searchInsightsFilters_severities' - An array of severity values used to search for insights.
--
-- 'statuses', 'searchInsightsFilters_statuses' - An array of status values used to search for insights.
newSearchInsightsFilters ::
  SearchInsightsFilters
newSearchInsightsFilters =
  SearchInsightsFilters'
    { resourceCollection =
        Prelude.Nothing,
      serviceCollection = Prelude.Nothing,
      severities = Prelude.Nothing,
      statuses = Prelude.Nothing
    }

-- | Undocumented member.
searchInsightsFilters_resourceCollection :: Lens.Lens' SearchInsightsFilters (Prelude.Maybe ResourceCollection)
searchInsightsFilters_resourceCollection = Lens.lens (\SearchInsightsFilters' {resourceCollection} -> resourceCollection) (\s@SearchInsightsFilters' {} a -> s {resourceCollection = a} :: SearchInsightsFilters)

-- | A collection of the names of Amazon Web Services services.
searchInsightsFilters_serviceCollection :: Lens.Lens' SearchInsightsFilters (Prelude.Maybe ServiceCollection)
searchInsightsFilters_serviceCollection = Lens.lens (\SearchInsightsFilters' {serviceCollection} -> serviceCollection) (\s@SearchInsightsFilters' {} a -> s {serviceCollection = a} :: SearchInsightsFilters)

-- | An array of severity values used to search for insights.
searchInsightsFilters_severities :: Lens.Lens' SearchInsightsFilters (Prelude.Maybe [InsightSeverity])
searchInsightsFilters_severities = Lens.lens (\SearchInsightsFilters' {severities} -> severities) (\s@SearchInsightsFilters' {} a -> s {severities = a} :: SearchInsightsFilters) Prelude.. Lens.mapping Lens.coerced

-- | An array of status values used to search for insights.
searchInsightsFilters_statuses :: Lens.Lens' SearchInsightsFilters (Prelude.Maybe [InsightStatus])
searchInsightsFilters_statuses = Lens.lens (\SearchInsightsFilters' {statuses} -> statuses) (\s@SearchInsightsFilters' {} a -> s {statuses = a} :: SearchInsightsFilters) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable SearchInsightsFilters where
  hashWithSalt _salt SearchInsightsFilters' {..} =
    _salt `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` serviceCollection
      `Prelude.hashWithSalt` severities
      `Prelude.hashWithSalt` statuses

instance Prelude.NFData SearchInsightsFilters where
  rnf SearchInsightsFilters' {..} =
    Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf serviceCollection
      `Prelude.seq` Prelude.rnf severities
      `Prelude.seq` Prelude.rnf statuses

instance Data.ToJSON SearchInsightsFilters where
  toJSON SearchInsightsFilters' {..} =
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
