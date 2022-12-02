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
-- Module      : Amazonka.DevOpsGuru.Types.ListMonitoredResourcesFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ListMonitoredResourcesFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.ResourcePermission
import Amazonka.DevOpsGuru.Types.ResourceTypeFilter
import qualified Amazonka.Prelude as Prelude

-- | Filters to determine which monitored resources you want to retrieve. You
-- can filter by resource type or resource permission status.
--
-- /See:/ 'newListMonitoredResourcesFilters' smart constructor.
data ListMonitoredResourcesFilters = ListMonitoredResourcesFilters'
  { -- | The permission status of a resource.
    resourcePermission :: ResourcePermission,
    -- | The type of resource that you wish to retrieve, such as log groups.
    resourceTypeFilters :: [ResourceTypeFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitoredResourcesFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourcePermission', 'listMonitoredResourcesFilters_resourcePermission' - The permission status of a resource.
--
-- 'resourceTypeFilters', 'listMonitoredResourcesFilters_resourceTypeFilters' - The type of resource that you wish to retrieve, such as log groups.
newListMonitoredResourcesFilters ::
  -- | 'resourcePermission'
  ResourcePermission ->
  ListMonitoredResourcesFilters
newListMonitoredResourcesFilters pResourcePermission_ =
  ListMonitoredResourcesFilters'
    { resourcePermission =
        pResourcePermission_,
      resourceTypeFilters = Prelude.mempty
    }

-- | The permission status of a resource.
listMonitoredResourcesFilters_resourcePermission :: Lens.Lens' ListMonitoredResourcesFilters ResourcePermission
listMonitoredResourcesFilters_resourcePermission = Lens.lens (\ListMonitoredResourcesFilters' {resourcePermission} -> resourcePermission) (\s@ListMonitoredResourcesFilters' {} a -> s {resourcePermission = a} :: ListMonitoredResourcesFilters)

-- | The type of resource that you wish to retrieve, such as log groups.
listMonitoredResourcesFilters_resourceTypeFilters :: Lens.Lens' ListMonitoredResourcesFilters [ResourceTypeFilter]
listMonitoredResourcesFilters_resourceTypeFilters = Lens.lens (\ListMonitoredResourcesFilters' {resourceTypeFilters} -> resourceTypeFilters) (\s@ListMonitoredResourcesFilters' {} a -> s {resourceTypeFilters = a} :: ListMonitoredResourcesFilters) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    ListMonitoredResourcesFilters
  where
  hashWithSalt _salt ListMonitoredResourcesFilters' {..} =
    _salt `Prelude.hashWithSalt` resourcePermission
      `Prelude.hashWithSalt` resourceTypeFilters

instance Prelude.NFData ListMonitoredResourcesFilters where
  rnf ListMonitoredResourcesFilters' {..} =
    Prelude.rnf resourcePermission
      `Prelude.seq` Prelude.rnf resourceTypeFilters

instance Data.ToJSON ListMonitoredResourcesFilters where
  toJSON ListMonitoredResourcesFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourcePermission" Data..= resourcePermission),
            Prelude.Just
              ("ResourceTypeFilters" Data..= resourceTypeFilters)
          ]
      )
