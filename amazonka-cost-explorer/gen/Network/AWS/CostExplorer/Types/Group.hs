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
-- Module      : Network.AWS.CostExplorer.Types.Group
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Group where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.MetricValue
import qualified Network.AWS.Lens as Lens

-- | One level of grouped data in the results.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The metrics that are included in this group.
    metrics :: Core.Maybe (Core.HashMap Core.Text MetricValue),
    -- | The keys that are included in this group.
    keys :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'group_metrics' - The metrics that are included in this group.
--
-- 'keys', 'group_keys' - The keys that are included in this group.
newGroup ::
  Group
newGroup =
  Group' {metrics = Core.Nothing, keys = Core.Nothing}

-- | The metrics that are included in this group.
group_metrics :: Lens.Lens' Group (Core.Maybe (Core.HashMap Core.Text MetricValue))
group_metrics = Lens.lens (\Group' {metrics} -> metrics) (\s@Group' {} a -> s {metrics = a} :: Group) Core.. Lens.mapping Lens._Coerce

-- | The keys that are included in this group.
group_keys :: Lens.Lens' Group (Core.Maybe [Core.Text])
group_keys = Lens.lens (\Group' {keys} -> keys) (\s@Group' {} a -> s {keys = a} :: Group) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Group where
  parseJSON =
    Core.withObject
      "Group"
      ( \x ->
          Group'
            Core.<$> (x Core..:? "Metrics" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Keys" Core..!= Core.mempty)
      )

instance Core.Hashable Group

instance Core.NFData Group
