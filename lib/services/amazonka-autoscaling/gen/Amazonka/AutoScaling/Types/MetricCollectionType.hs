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
-- Module      : Amazonka.AutoScaling.Types.MetricCollectionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.MetricCollectionType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a metric.
--
-- /See:/ 'newMetricCollectionType' smart constructor.
data MetricCollectionType = MetricCollectionType'
  { -- | One of the following metrics:
    --
    -- -   @GroupMinSize@
    --
    -- -   @GroupMaxSize@
    --
    -- -   @GroupDesiredCapacity@
    --
    -- -   @GroupInServiceInstances@
    --
    -- -   @GroupPendingInstances@
    --
    -- -   @GroupStandbyInstances@
    --
    -- -   @GroupTerminatingInstances@
    --
    -- -   @GroupTotalInstances@
    --
    -- -   @GroupInServiceCapacity@
    --
    -- -   @GroupPendingCapacity@
    --
    -- -   @GroupStandbyCapacity@
    --
    -- -   @GroupTerminatingCapacity@
    --
    -- -   @GroupTotalCapacity@
    --
    -- -   @WarmPoolDesiredCapacity@
    --
    -- -   @WarmPoolWarmedCapacity@
    --
    -- -   @WarmPoolPendingCapacity@
    --
    -- -   @WarmPoolTerminatingCapacity@
    --
    -- -   @WarmPoolTotalCapacity@
    --
    -- -   @GroupAndWarmPoolDesiredCapacity@
    --
    -- -   @GroupAndWarmPoolTotalCapacity@
    metric :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricCollectionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metric', 'metricCollectionType_metric' - One of the following metrics:
--
-- -   @GroupMinSize@
--
-- -   @GroupMaxSize@
--
-- -   @GroupDesiredCapacity@
--
-- -   @GroupInServiceInstances@
--
-- -   @GroupPendingInstances@
--
-- -   @GroupStandbyInstances@
--
-- -   @GroupTerminatingInstances@
--
-- -   @GroupTotalInstances@
--
-- -   @GroupInServiceCapacity@
--
-- -   @GroupPendingCapacity@
--
-- -   @GroupStandbyCapacity@
--
-- -   @GroupTerminatingCapacity@
--
-- -   @GroupTotalCapacity@
--
-- -   @WarmPoolDesiredCapacity@
--
-- -   @WarmPoolWarmedCapacity@
--
-- -   @WarmPoolPendingCapacity@
--
-- -   @WarmPoolTerminatingCapacity@
--
-- -   @WarmPoolTotalCapacity@
--
-- -   @GroupAndWarmPoolDesiredCapacity@
--
-- -   @GroupAndWarmPoolTotalCapacity@
newMetricCollectionType ::
  MetricCollectionType
newMetricCollectionType =
  MetricCollectionType' {metric = Prelude.Nothing}

-- | One of the following metrics:
--
-- -   @GroupMinSize@
--
-- -   @GroupMaxSize@
--
-- -   @GroupDesiredCapacity@
--
-- -   @GroupInServiceInstances@
--
-- -   @GroupPendingInstances@
--
-- -   @GroupStandbyInstances@
--
-- -   @GroupTerminatingInstances@
--
-- -   @GroupTotalInstances@
--
-- -   @GroupInServiceCapacity@
--
-- -   @GroupPendingCapacity@
--
-- -   @GroupStandbyCapacity@
--
-- -   @GroupTerminatingCapacity@
--
-- -   @GroupTotalCapacity@
--
-- -   @WarmPoolDesiredCapacity@
--
-- -   @WarmPoolWarmedCapacity@
--
-- -   @WarmPoolPendingCapacity@
--
-- -   @WarmPoolTerminatingCapacity@
--
-- -   @WarmPoolTotalCapacity@
--
-- -   @GroupAndWarmPoolDesiredCapacity@
--
-- -   @GroupAndWarmPoolTotalCapacity@
metricCollectionType_metric :: Lens.Lens' MetricCollectionType (Prelude.Maybe Prelude.Text)
metricCollectionType_metric = Lens.lens (\MetricCollectionType' {metric} -> metric) (\s@MetricCollectionType' {} a -> s {metric = a} :: MetricCollectionType)

instance Data.FromXML MetricCollectionType where
  parseXML x =
    MetricCollectionType'
      Prelude.<$> (x Data..@? "Metric")

instance Prelude.Hashable MetricCollectionType where
  hashWithSalt _salt MetricCollectionType' {..} =
    _salt `Prelude.hashWithSalt` metric

instance Prelude.NFData MetricCollectionType where
  rnf MetricCollectionType' {..} = Prelude.rnf metric
