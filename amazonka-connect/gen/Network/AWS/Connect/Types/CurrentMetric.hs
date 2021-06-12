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
-- Module      : Network.AWS.Connect.Types.CurrentMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetric where

import Network.AWS.Connect.Types.CurrentMetricName
import Network.AWS.Connect.Types.Unit
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a real-time metric. For a description of each
-- metric, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions>
-- in the /Amazon Connect Administrator Guide/.
--
-- /See:/ 'newCurrentMetric' smart constructor.
data CurrentMetric = CurrentMetric'
  { -- | The unit for the metric.
    unit :: Core.Maybe Unit,
    -- | The name of the metric.
    name :: Core.Maybe CurrentMetricName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CurrentMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'currentMetric_unit' - The unit for the metric.
--
-- 'name', 'currentMetric_name' - The name of the metric.
newCurrentMetric ::
  CurrentMetric
newCurrentMetric =
  CurrentMetric'
    { unit = Core.Nothing,
      name = Core.Nothing
    }

-- | The unit for the metric.
currentMetric_unit :: Lens.Lens' CurrentMetric (Core.Maybe Unit)
currentMetric_unit = Lens.lens (\CurrentMetric' {unit} -> unit) (\s@CurrentMetric' {} a -> s {unit = a} :: CurrentMetric)

-- | The name of the metric.
currentMetric_name :: Lens.Lens' CurrentMetric (Core.Maybe CurrentMetricName)
currentMetric_name = Lens.lens (\CurrentMetric' {name} -> name) (\s@CurrentMetric' {} a -> s {name = a} :: CurrentMetric)

instance Core.FromJSON CurrentMetric where
  parseJSON =
    Core.withObject
      "CurrentMetric"
      ( \x ->
          CurrentMetric'
            Core.<$> (x Core..:? "Unit") Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable CurrentMetric

instance Core.NFData CurrentMetric

instance Core.ToJSON CurrentMetric where
  toJSON CurrentMetric' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Unit" Core..=) Core.<$> unit,
            ("Name" Core..=) Core.<$> name
          ]
      )
