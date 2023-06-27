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
-- Module      : Amazonka.EC2.Types.MetricPoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.MetricPoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether the network was healthy or degraded at a particular
-- point. The value is aggregated from the @startDate@ to the @endDate@.
-- Currently only @five_minutes@ is supported.
--
-- /See:/ 'newMetricPoint' smart constructor.
data MetricPoint = MetricPoint'
  { -- | The end date for the metric point. The ending time must be formatted as
    -- @yyyy-mm-ddThh:mm:ss@. For example, @2022-06-12T12:00:00.000Z@.
    endDate :: Prelude.Maybe Data.ISO8601,
    -- | The start date for the metric point. The starting date for the metric
    -- point. The starting time must be formatted as @yyyy-mm-ddThh:mm:ss@. For
    -- example, @2022-06-10T12:00:00.000Z@.
    startDate :: Prelude.Maybe Data.ISO8601,
    -- | The status of the metric point.
    status :: Prelude.Maybe Prelude.Text,
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endDate', 'metricPoint_endDate' - The end date for the metric point. The ending time must be formatted as
-- @yyyy-mm-ddThh:mm:ss@. For example, @2022-06-12T12:00:00.000Z@.
--
-- 'startDate', 'metricPoint_startDate' - The start date for the metric point. The starting date for the metric
-- point. The starting time must be formatted as @yyyy-mm-ddThh:mm:ss@. For
-- example, @2022-06-10T12:00:00.000Z@.
--
-- 'status', 'metricPoint_status' - The status of the metric point.
--
-- 'value', 'metricPoint_value' - Undocumented member.
newMetricPoint ::
  MetricPoint
newMetricPoint =
  MetricPoint'
    { endDate = Prelude.Nothing,
      startDate = Prelude.Nothing,
      status = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The end date for the metric point. The ending time must be formatted as
-- @yyyy-mm-ddThh:mm:ss@. For example, @2022-06-12T12:00:00.000Z@.
metricPoint_endDate :: Lens.Lens' MetricPoint (Prelude.Maybe Prelude.UTCTime)
metricPoint_endDate = Lens.lens (\MetricPoint' {endDate} -> endDate) (\s@MetricPoint' {} a -> s {endDate = a} :: MetricPoint) Prelude.. Lens.mapping Data._Time

-- | The start date for the metric point. The starting date for the metric
-- point. The starting time must be formatted as @yyyy-mm-ddThh:mm:ss@. For
-- example, @2022-06-10T12:00:00.000Z@.
metricPoint_startDate :: Lens.Lens' MetricPoint (Prelude.Maybe Prelude.UTCTime)
metricPoint_startDate = Lens.lens (\MetricPoint' {startDate} -> startDate) (\s@MetricPoint' {} a -> s {startDate = a} :: MetricPoint) Prelude.. Lens.mapping Data._Time

-- | The status of the metric point.
metricPoint_status :: Lens.Lens' MetricPoint (Prelude.Maybe Prelude.Text)
metricPoint_status = Lens.lens (\MetricPoint' {status} -> status) (\s@MetricPoint' {} a -> s {status = a} :: MetricPoint)

-- | Undocumented member.
metricPoint_value :: Lens.Lens' MetricPoint (Prelude.Maybe Prelude.Double)
metricPoint_value = Lens.lens (\MetricPoint' {value} -> value) (\s@MetricPoint' {} a -> s {value = a} :: MetricPoint)

instance Data.FromXML MetricPoint where
  parseXML x =
    MetricPoint'
      Prelude.<$> (x Data..@? "endDate")
      Prelude.<*> (x Data..@? "startDate")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "value")

instance Prelude.Hashable MetricPoint where
  hashWithSalt _salt MetricPoint' {..} =
    _salt
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` value

instance Prelude.NFData MetricPoint where
  rnf MetricPoint' {..} =
    Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf value
