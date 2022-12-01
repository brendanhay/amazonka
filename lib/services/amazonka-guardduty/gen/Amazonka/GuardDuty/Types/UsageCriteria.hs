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
-- Module      : Amazonka.GuardDuty.Types.UsageCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.UsageCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.DataSource
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the criteria used to query usage statistics.
--
-- /See:/ 'newUsageCriteria' smart constructor.
data UsageCriteria = UsageCriteria'
  { -- | The account IDs to aggregate usage statistics from.
    accountIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The resources to aggregate usage statistics from. Only accepts exact
    -- resource names.
    resources :: Prelude.Maybe [Prelude.Text],
    -- | The data sources to aggregate usage statistics from.
    dataSources :: [DataSource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { accountIds = Prelude.Nothing,
      resources = Prelude.Nothing,
      dataSources = Prelude.mempty
    }

-- | The account IDs to aggregate usage statistics from.
usageCriteria_accountIds :: Lens.Lens' UsageCriteria (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
usageCriteria_accountIds = Lens.lens (\UsageCriteria' {accountIds} -> accountIds) (\s@UsageCriteria' {} a -> s {accountIds = a} :: UsageCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The resources to aggregate usage statistics from. Only accepts exact
-- resource names.
usageCriteria_resources :: Lens.Lens' UsageCriteria (Prelude.Maybe [Prelude.Text])
usageCriteria_resources = Lens.lens (\UsageCriteria' {resources} -> resources) (\s@UsageCriteria' {} a -> s {resources = a} :: UsageCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The data sources to aggregate usage statistics from.
usageCriteria_dataSources :: Lens.Lens' UsageCriteria [DataSource]
usageCriteria_dataSources = Lens.lens (\UsageCriteria' {dataSources} -> dataSources) (\s@UsageCriteria' {} a -> s {dataSources = a} :: UsageCriteria) Prelude.. Lens.coerced

instance Prelude.Hashable UsageCriteria where
  hashWithSalt _salt UsageCriteria' {..} =
    _salt `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` dataSources

instance Prelude.NFData UsageCriteria where
  rnf UsageCriteria' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf dataSources

instance Core.ToJSON UsageCriteria where
  toJSON UsageCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("accountIds" Core..=) Prelude.<$> accountIds,
            ("resources" Core..=) Prelude.<$> resources,
            Prelude.Just ("dataSources" Core..= dataSources)
          ]
      )
