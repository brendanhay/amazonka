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
-- Module      : Network.AWS.GuardDuty.Types.UsageCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageCriteria where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.DataSource
import qualified Network.AWS.Lens as Lens

-- | Contains information about the criteria used to query usage statistics.
--
-- /See:/ 'newUsageCriteria' smart constructor.
data UsageCriteria = UsageCriteria'
  { -- | The account IDs to aggregate usage statistics from.
    accountIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The resources to aggregate usage statistics from. Only accepts exact
    -- resource names.
    resources :: Core.Maybe [Core.Text],
    -- | The data sources to aggregate usage statistics from.
    dataSources :: [DataSource]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UsageCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'usageCriteria_accountIds' - The account IDs to aggregate usage statistics from.
--
-- 'resources', 'usageCriteria_resources' - The resources to aggregate usage statistics from. Only accepts exact
-- resource names.
--
-- 'dataSources', 'usageCriteria_dataSources' - The data sources to aggregate usage statistics from.
newUsageCriteria ::
  UsageCriteria
newUsageCriteria =
  UsageCriteria'
    { accountIds = Core.Nothing,
      resources = Core.Nothing,
      dataSources = Core.mempty
    }

-- | The account IDs to aggregate usage statistics from.
usageCriteria_accountIds :: Lens.Lens' UsageCriteria (Core.Maybe (Core.NonEmpty Core.Text))
usageCriteria_accountIds = Lens.lens (\UsageCriteria' {accountIds} -> accountIds) (\s@UsageCriteria' {} a -> s {accountIds = a} :: UsageCriteria) Core.. Lens.mapping Lens._Coerce

-- | The resources to aggregate usage statistics from. Only accepts exact
-- resource names.
usageCriteria_resources :: Lens.Lens' UsageCriteria (Core.Maybe [Core.Text])
usageCriteria_resources = Lens.lens (\UsageCriteria' {resources} -> resources) (\s@UsageCriteria' {} a -> s {resources = a} :: UsageCriteria) Core.. Lens.mapping Lens._Coerce

-- | The data sources to aggregate usage statistics from.
usageCriteria_dataSources :: Lens.Lens' UsageCriteria [DataSource]
usageCriteria_dataSources = Lens.lens (\UsageCriteria' {dataSources} -> dataSources) (\s@UsageCriteria' {} a -> s {dataSources = a} :: UsageCriteria) Core.. Lens._Coerce

instance Core.Hashable UsageCriteria

instance Core.NFData UsageCriteria

instance Core.ToJSON UsageCriteria where
  toJSON UsageCriteria' {..} =
    Core.object
      ( Core.catMaybes
          [ ("accountIds" Core..=) Core.<$> accountIds,
            ("resources" Core..=) Core.<$> resources,
            Core.Just ("dataSources" Core..= dataSources)
          ]
      )
