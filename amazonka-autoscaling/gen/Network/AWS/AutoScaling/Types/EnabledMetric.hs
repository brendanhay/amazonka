{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an enabled metric.
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
    metric :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
enabledMetric_metric :: Lens.Lens' EnabledMetric (Prelude.Maybe Prelude.Text)
enabledMetric_metric = Lens.lens (\EnabledMetric' {metric} -> metric) (\s@EnabledMetric' {} a -> s {metric = a} :: EnabledMetric)

instance Prelude.FromXML EnabledMetric where
  parseXML x =
    EnabledMetric'
      Prelude.<$> (x Prelude..@? "Granularity")
      Prelude.<*> (x Prelude..@? "Metric")

instance Prelude.Hashable EnabledMetric

instance Prelude.NFData EnabledMetric
