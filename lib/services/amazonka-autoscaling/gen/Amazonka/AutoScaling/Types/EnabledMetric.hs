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
-- Module      : Amazonka.AutoScaling.Types.EnabledMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.EnabledMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an enabled Auto Scaling group metric.
--
-- /See:/ 'newEnabledMetric' smart constructor.
data EnabledMetric = EnabledMetric'
  { -- | The granularity of the metric. The only valid value is @1Minute@.
    granularity :: Prelude.Maybe Prelude.Text,
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
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-cloudwatch-monitoring.html#as-group-metrics Auto Scaling group metrics>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    metric :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnabledMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'granularity', 'enabledMetric_granularity' - The granularity of the metric. The only valid value is @1Minute@.
--
-- 'metric', 'enabledMetric_metric' - One of the following metrics:
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
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-cloudwatch-monitoring.html#as-group-metrics Auto Scaling group metrics>
-- in the /Amazon EC2 Auto Scaling User Guide/.
newEnabledMetric ::
  EnabledMetric
newEnabledMetric =
  EnabledMetric'
    { granularity = Prelude.Nothing,
      metric = Prelude.Nothing
    }

-- | The granularity of the metric. The only valid value is @1Minute@.
enabledMetric_granularity :: Lens.Lens' EnabledMetric (Prelude.Maybe Prelude.Text)
enabledMetric_granularity = Lens.lens (\EnabledMetric' {granularity} -> granularity) (\s@EnabledMetric' {} a -> s {granularity = a} :: EnabledMetric)

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
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-cloudwatch-monitoring.html#as-group-metrics Auto Scaling group metrics>
-- in the /Amazon EC2 Auto Scaling User Guide/.
enabledMetric_metric :: Lens.Lens' EnabledMetric (Prelude.Maybe Prelude.Text)
enabledMetric_metric = Lens.lens (\EnabledMetric' {metric} -> metric) (\s@EnabledMetric' {} a -> s {metric = a} :: EnabledMetric)

instance Data.FromXML EnabledMetric where
  parseXML x =
    EnabledMetric'
      Prelude.<$> (x Data..@? "Granularity")
      Prelude.<*> (x Data..@? "Metric")

instance Prelude.Hashable EnabledMetric where
  hashWithSalt _salt EnabledMetric' {..} =
    _salt
      `Prelude.hashWithSalt` granularity
      `Prelude.hashWithSalt` metric

instance Prelude.NFData EnabledMetric where
  rnf EnabledMetric' {..} =
    Prelude.rnf granularity
      `Prelude.seq` Prelude.rnf metric
