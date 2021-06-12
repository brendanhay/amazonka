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
-- Module      : Network.AWS.AutoScaling.Types.EnabledMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.EnabledMetric where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an enabled metric.
--
-- /See:/ 'newEnabledMetric' smart constructor.
data EnabledMetric = EnabledMetric'
  { -- | The granularity of the metric. The only valid value is @1Minute@.
    granularity :: Core.Maybe Core.Text,
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
    metric :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
newEnabledMetric ::
  EnabledMetric
newEnabledMetric =
  EnabledMetric'
    { granularity = Core.Nothing,
      metric = Core.Nothing
    }

-- | The granularity of the metric. The only valid value is @1Minute@.
enabledMetric_granularity :: Lens.Lens' EnabledMetric (Core.Maybe Core.Text)
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
enabledMetric_metric :: Lens.Lens' EnabledMetric (Core.Maybe Core.Text)
enabledMetric_metric = Lens.lens (\EnabledMetric' {metric} -> metric) (\s@EnabledMetric' {} a -> s {metric = a} :: EnabledMetric)

instance Core.FromXML EnabledMetric where
  parseXML x =
    EnabledMetric'
      Core.<$> (x Core..@? "Granularity")
      Core.<*> (x Core..@? "Metric")

instance Core.Hashable EnabledMetric

instance Core.NFData EnabledMetric
