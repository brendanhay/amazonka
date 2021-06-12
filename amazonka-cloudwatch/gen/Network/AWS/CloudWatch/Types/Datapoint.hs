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
-- Module      : Network.AWS.CloudWatch.Types.Datapoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Datapoint where

import Network.AWS.CloudWatch.Types.StandardUnit
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Encapsulates the statistical data that CloudWatch computes from metric
-- data.
--
-- /See:/ 'newDatapoint' smart constructor.
data Datapoint = Datapoint'
  { -- | The standard unit for the data point.
    unit :: Core.Maybe StandardUnit,
    -- | The minimum metric value for the data point.
    minimum :: Core.Maybe Core.Double,
    -- | The sum of the metric values for the data point.
    sum :: Core.Maybe Core.Double,
    -- | The number of metric values that contributed to the aggregate value of
    -- this data point.
    sampleCount :: Core.Maybe Core.Double,
    -- | The time stamp used for the data point.
    timestamp :: Core.Maybe Core.ISO8601,
    -- | The average of the metric values that correspond to the data point.
    average :: Core.Maybe Core.Double,
    -- | The maximum metric value for the data point.
    maximum :: Core.Maybe Core.Double,
    -- | The percentile statistic for the data point.
    extendedStatistics :: Core.Maybe (Core.HashMap Core.Text Core.Double)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Datapoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'datapoint_unit' - The standard unit for the data point.
--
-- 'minimum', 'datapoint_minimum' - The minimum metric value for the data point.
--
-- 'sum', 'datapoint_sum' - The sum of the metric values for the data point.
--
-- 'sampleCount', 'datapoint_sampleCount' - The number of metric values that contributed to the aggregate value of
-- this data point.
--
-- 'timestamp', 'datapoint_timestamp' - The time stamp used for the data point.
--
-- 'average', 'datapoint_average' - The average of the metric values that correspond to the data point.
--
-- 'maximum', 'datapoint_maximum' - The maximum metric value for the data point.
--
-- 'extendedStatistics', 'datapoint_extendedStatistics' - The percentile statistic for the data point.
newDatapoint ::
  Datapoint
newDatapoint =
  Datapoint'
    { unit = Core.Nothing,
      minimum = Core.Nothing,
      sum = Core.Nothing,
      sampleCount = Core.Nothing,
      timestamp = Core.Nothing,
      average = Core.Nothing,
      maximum = Core.Nothing,
      extendedStatistics = Core.Nothing
    }

-- | The standard unit for the data point.
datapoint_unit :: Lens.Lens' Datapoint (Core.Maybe StandardUnit)
datapoint_unit = Lens.lens (\Datapoint' {unit} -> unit) (\s@Datapoint' {} a -> s {unit = a} :: Datapoint)

-- | The minimum metric value for the data point.
datapoint_minimum :: Lens.Lens' Datapoint (Core.Maybe Core.Double)
datapoint_minimum = Lens.lens (\Datapoint' {minimum} -> minimum) (\s@Datapoint' {} a -> s {minimum = a} :: Datapoint)

-- | The sum of the metric values for the data point.
datapoint_sum :: Lens.Lens' Datapoint (Core.Maybe Core.Double)
datapoint_sum = Lens.lens (\Datapoint' {sum} -> sum) (\s@Datapoint' {} a -> s {sum = a} :: Datapoint)

-- | The number of metric values that contributed to the aggregate value of
-- this data point.
datapoint_sampleCount :: Lens.Lens' Datapoint (Core.Maybe Core.Double)
datapoint_sampleCount = Lens.lens (\Datapoint' {sampleCount} -> sampleCount) (\s@Datapoint' {} a -> s {sampleCount = a} :: Datapoint)

-- | The time stamp used for the data point.
datapoint_timestamp :: Lens.Lens' Datapoint (Core.Maybe Core.UTCTime)
datapoint_timestamp = Lens.lens (\Datapoint' {timestamp} -> timestamp) (\s@Datapoint' {} a -> s {timestamp = a} :: Datapoint) Core.. Lens.mapping Core._Time

-- | The average of the metric values that correspond to the data point.
datapoint_average :: Lens.Lens' Datapoint (Core.Maybe Core.Double)
datapoint_average = Lens.lens (\Datapoint' {average} -> average) (\s@Datapoint' {} a -> s {average = a} :: Datapoint)

-- | The maximum metric value for the data point.
datapoint_maximum :: Lens.Lens' Datapoint (Core.Maybe Core.Double)
datapoint_maximum = Lens.lens (\Datapoint' {maximum} -> maximum) (\s@Datapoint' {} a -> s {maximum = a} :: Datapoint)

-- | The percentile statistic for the data point.
datapoint_extendedStatistics :: Lens.Lens' Datapoint (Core.Maybe (Core.HashMap Core.Text Core.Double))
datapoint_extendedStatistics = Lens.lens (\Datapoint' {extendedStatistics} -> extendedStatistics) (\s@Datapoint' {} a -> s {extendedStatistics = a} :: Datapoint) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML Datapoint where
  parseXML x =
    Datapoint'
      Core.<$> (x Core..@? "Unit")
      Core.<*> (x Core..@? "Minimum")
      Core.<*> (x Core..@? "Sum")
      Core.<*> (x Core..@? "SampleCount")
      Core.<*> (x Core..@? "Timestamp")
      Core.<*> (x Core..@? "Average")
      Core.<*> (x Core..@? "Maximum")
      Core.<*> ( x Core..@? "ExtendedStatistics" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
               )

instance Core.Hashable Datapoint

instance Core.NFData Datapoint
