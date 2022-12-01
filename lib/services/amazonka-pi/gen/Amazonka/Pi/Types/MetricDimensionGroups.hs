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
-- Module      : Amazonka.Pi.Types.MetricDimensionGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.MetricDimensionGroups where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pi.Types.DimensionGroupDetail
import qualified Amazonka.Prelude as Prelude

-- | The available dimension information for a metric type.
--
-- /See:/ 'newMetricDimensionGroups' smart constructor.
data MetricDimensionGroups = MetricDimensionGroups'
  { -- | The metric type to which the dimension information belongs.
    metric :: Prelude.Maybe Prelude.Text,
    -- | The available dimension groups for a metric type.
    groups :: Prelude.Maybe [DimensionGroupDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDimensionGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metric', 'metricDimensionGroups_metric' - The metric type to which the dimension information belongs.
--
-- 'groups', 'metricDimensionGroups_groups' - The available dimension groups for a metric type.
newMetricDimensionGroups ::
  MetricDimensionGroups
newMetricDimensionGroups =
  MetricDimensionGroups'
    { metric = Prelude.Nothing,
      groups = Prelude.Nothing
    }

-- | The metric type to which the dimension information belongs.
metricDimensionGroups_metric :: Lens.Lens' MetricDimensionGroups (Prelude.Maybe Prelude.Text)
metricDimensionGroups_metric = Lens.lens (\MetricDimensionGroups' {metric} -> metric) (\s@MetricDimensionGroups' {} a -> s {metric = a} :: MetricDimensionGroups)

-- | The available dimension groups for a metric type.
metricDimensionGroups_groups :: Lens.Lens' MetricDimensionGroups (Prelude.Maybe [DimensionGroupDetail])
metricDimensionGroups_groups = Lens.lens (\MetricDimensionGroups' {groups} -> groups) (\s@MetricDimensionGroups' {} a -> s {groups = a} :: MetricDimensionGroups) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON MetricDimensionGroups where
  parseJSON =
    Core.withObject
      "MetricDimensionGroups"
      ( \x ->
          MetricDimensionGroups'
            Prelude.<$> (x Core..:? "Metric")
            Prelude.<*> (x Core..:? "Groups" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable MetricDimensionGroups where
  hashWithSalt _salt MetricDimensionGroups' {..} =
    _salt `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` groups

instance Prelude.NFData MetricDimensionGroups where
  rnf MetricDimensionGroups' {..} =
    Prelude.rnf metric `Prelude.seq` Prelude.rnf groups
