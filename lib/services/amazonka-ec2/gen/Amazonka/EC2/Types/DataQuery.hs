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
-- Module      : Amazonka.EC2.Types.DataQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DataQuery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.MetricType
import Amazonka.EC2.Types.PeriodType
import Amazonka.EC2.Types.StatisticType
import qualified Amazonka.Prelude as Prelude

-- | A query used for retrieving network health data.
--
-- /See:/ 'newDataQuery' smart constructor.
data DataQuery = DataQuery'
  { -- | The Region or Availability Zone that\'s the target for the data query.
    -- For example, @eu-north-1@.
    destination :: Prelude.Maybe Prelude.Text,
    -- | A user-defined ID associated with a data query that\'s returned in the
    -- @dataResponse@ identifying the query. For example, if you set the Id to
    -- @MyQuery01@in the query, the @dataResponse@ identifies the query as
    -- @MyQuery01@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The metric, @aggregation-latency@, indicating that network latency is
    -- aggregated for the query. This is the only supported metric.
    metric :: Prelude.Maybe MetricType,
    -- | The aggregation period used for the data query.
    period :: Prelude.Maybe PeriodType,
    -- | The Region or Availability Zone that\'s the source for the data query.
    -- For example, @us-east-1@.
    source :: Prelude.Maybe Prelude.Text,
    -- | The metric data aggregation period, @p50@, between the specified
    -- @startDate@ and @endDate@. For example, a metric of @five_minutes@ is
    -- the median of all the data points gathered within those five minutes.
    -- @p50@ is the only supported metric.
    statistic :: Prelude.Maybe StatisticType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'dataQuery_destination' - The Region or Availability Zone that\'s the target for the data query.
-- For example, @eu-north-1@.
--
-- 'id', 'dataQuery_id' - A user-defined ID associated with a data query that\'s returned in the
-- @dataResponse@ identifying the query. For example, if you set the Id to
-- @MyQuery01@in the query, the @dataResponse@ identifies the query as
-- @MyQuery01@.
--
-- 'metric', 'dataQuery_metric' - The metric, @aggregation-latency@, indicating that network latency is
-- aggregated for the query. This is the only supported metric.
--
-- 'period', 'dataQuery_period' - The aggregation period used for the data query.
--
-- 'source', 'dataQuery_source' - The Region or Availability Zone that\'s the source for the data query.
-- For example, @us-east-1@.
--
-- 'statistic', 'dataQuery_statistic' - The metric data aggregation period, @p50@, between the specified
-- @startDate@ and @endDate@. For example, a metric of @five_minutes@ is
-- the median of all the data points gathered within those five minutes.
-- @p50@ is the only supported metric.
newDataQuery ::
  DataQuery
newDataQuery =
  DataQuery'
    { destination = Prelude.Nothing,
      id = Prelude.Nothing,
      metric = Prelude.Nothing,
      period = Prelude.Nothing,
      source = Prelude.Nothing,
      statistic = Prelude.Nothing
    }

-- | The Region or Availability Zone that\'s the target for the data query.
-- For example, @eu-north-1@.
dataQuery_destination :: Lens.Lens' DataQuery (Prelude.Maybe Prelude.Text)
dataQuery_destination = Lens.lens (\DataQuery' {destination} -> destination) (\s@DataQuery' {} a -> s {destination = a} :: DataQuery)

-- | A user-defined ID associated with a data query that\'s returned in the
-- @dataResponse@ identifying the query. For example, if you set the Id to
-- @MyQuery01@in the query, the @dataResponse@ identifies the query as
-- @MyQuery01@.
dataQuery_id :: Lens.Lens' DataQuery (Prelude.Maybe Prelude.Text)
dataQuery_id = Lens.lens (\DataQuery' {id} -> id) (\s@DataQuery' {} a -> s {id = a} :: DataQuery)

-- | The metric, @aggregation-latency@, indicating that network latency is
-- aggregated for the query. This is the only supported metric.
dataQuery_metric :: Lens.Lens' DataQuery (Prelude.Maybe MetricType)
dataQuery_metric = Lens.lens (\DataQuery' {metric} -> metric) (\s@DataQuery' {} a -> s {metric = a} :: DataQuery)

-- | The aggregation period used for the data query.
dataQuery_period :: Lens.Lens' DataQuery (Prelude.Maybe PeriodType)
dataQuery_period = Lens.lens (\DataQuery' {period} -> period) (\s@DataQuery' {} a -> s {period = a} :: DataQuery)

-- | The Region or Availability Zone that\'s the source for the data query.
-- For example, @us-east-1@.
dataQuery_source :: Lens.Lens' DataQuery (Prelude.Maybe Prelude.Text)
dataQuery_source = Lens.lens (\DataQuery' {source} -> source) (\s@DataQuery' {} a -> s {source = a} :: DataQuery)

-- | The metric data aggregation period, @p50@, between the specified
-- @startDate@ and @endDate@. For example, a metric of @five_minutes@ is
-- the median of all the data points gathered within those five minutes.
-- @p50@ is the only supported metric.
dataQuery_statistic :: Lens.Lens' DataQuery (Prelude.Maybe StatisticType)
dataQuery_statistic = Lens.lens (\DataQuery' {statistic} -> statistic) (\s@DataQuery' {} a -> s {statistic = a} :: DataQuery)

instance Prelude.Hashable DataQuery where
  hashWithSalt _salt DataQuery' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` statistic

instance Prelude.NFData DataQuery where
  rnf DataQuery' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf statistic

instance Data.ToQuery DataQuery where
  toQuery DataQuery' {..} =
    Prelude.mconcat
      [ "Destination" Data.=: destination,
        "Id" Data.=: id,
        "Metric" Data.=: metric,
        "Period" Data.=: period,
        "Source" Data.=: source,
        "Statistic" Data.=: statistic
      ]
