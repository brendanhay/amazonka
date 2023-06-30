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
-- Module      : Amazonka.EC2.Types.DataResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DataResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.MetricPoint
import Amazonka.EC2.Types.MetricType
import Amazonka.EC2.Types.PeriodType
import Amazonka.EC2.Types.StatisticType
import qualified Amazonka.Prelude as Prelude

-- | The response to a @DataQuery@.
--
-- /See:/ 'newDataResponse' smart constructor.
data DataResponse = DataResponse'
  { -- | The Region or Availability Zone that\'s the destination for the data
    -- query. For example, @eu-west-1@.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The ID passed in the @DataQuery@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The metric used for the network performance request. Currently only
    -- @aggregate-latency@ is supported, showing network latency during a
    -- specified period.
    metric :: Prelude.Maybe MetricType,
    -- | A list of @MetricPoint@ objects.
    metricPoints :: Prelude.Maybe [MetricPoint],
    -- | The period used for the network performance request.
    period :: Prelude.Maybe PeriodType,
    -- | The Region or Availability Zone that\'s the source for the data query.
    -- For example, @us-east-1@.
    source :: Prelude.Maybe Prelude.Text,
    -- | The statistic used for the network performance request.
    statistic :: Prelude.Maybe StatisticType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'dataResponse_destination' - The Region or Availability Zone that\'s the destination for the data
-- query. For example, @eu-west-1@.
--
-- 'id', 'dataResponse_id' - The ID passed in the @DataQuery@.
--
-- 'metric', 'dataResponse_metric' - The metric used for the network performance request. Currently only
-- @aggregate-latency@ is supported, showing network latency during a
-- specified period.
--
-- 'metricPoints', 'dataResponse_metricPoints' - A list of @MetricPoint@ objects.
--
-- 'period', 'dataResponse_period' - The period used for the network performance request.
--
-- 'source', 'dataResponse_source' - The Region or Availability Zone that\'s the source for the data query.
-- For example, @us-east-1@.
--
-- 'statistic', 'dataResponse_statistic' - The statistic used for the network performance request.
newDataResponse ::
  DataResponse
newDataResponse =
  DataResponse'
    { destination = Prelude.Nothing,
      id = Prelude.Nothing,
      metric = Prelude.Nothing,
      metricPoints = Prelude.Nothing,
      period = Prelude.Nothing,
      source = Prelude.Nothing,
      statistic = Prelude.Nothing
    }

-- | The Region or Availability Zone that\'s the destination for the data
-- query. For example, @eu-west-1@.
dataResponse_destination :: Lens.Lens' DataResponse (Prelude.Maybe Prelude.Text)
dataResponse_destination = Lens.lens (\DataResponse' {destination} -> destination) (\s@DataResponse' {} a -> s {destination = a} :: DataResponse)

-- | The ID passed in the @DataQuery@.
dataResponse_id :: Lens.Lens' DataResponse (Prelude.Maybe Prelude.Text)
dataResponse_id = Lens.lens (\DataResponse' {id} -> id) (\s@DataResponse' {} a -> s {id = a} :: DataResponse)

-- | The metric used for the network performance request. Currently only
-- @aggregate-latency@ is supported, showing network latency during a
-- specified period.
dataResponse_metric :: Lens.Lens' DataResponse (Prelude.Maybe MetricType)
dataResponse_metric = Lens.lens (\DataResponse' {metric} -> metric) (\s@DataResponse' {} a -> s {metric = a} :: DataResponse)

-- | A list of @MetricPoint@ objects.
dataResponse_metricPoints :: Lens.Lens' DataResponse (Prelude.Maybe [MetricPoint])
dataResponse_metricPoints = Lens.lens (\DataResponse' {metricPoints} -> metricPoints) (\s@DataResponse' {} a -> s {metricPoints = a} :: DataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The period used for the network performance request.
dataResponse_period :: Lens.Lens' DataResponse (Prelude.Maybe PeriodType)
dataResponse_period = Lens.lens (\DataResponse' {period} -> period) (\s@DataResponse' {} a -> s {period = a} :: DataResponse)

-- | The Region or Availability Zone that\'s the source for the data query.
-- For example, @us-east-1@.
dataResponse_source :: Lens.Lens' DataResponse (Prelude.Maybe Prelude.Text)
dataResponse_source = Lens.lens (\DataResponse' {source} -> source) (\s@DataResponse' {} a -> s {source = a} :: DataResponse)

-- | The statistic used for the network performance request.
dataResponse_statistic :: Lens.Lens' DataResponse (Prelude.Maybe StatisticType)
dataResponse_statistic = Lens.lens (\DataResponse' {statistic} -> statistic) (\s@DataResponse' {} a -> s {statistic = a} :: DataResponse)

instance Data.FromXML DataResponse where
  parseXML x =
    DataResponse'
      Prelude.<$> (x Data..@? "destination")
      Prelude.<*> (x Data..@? "id")
      Prelude.<*> (x Data..@? "metric")
      Prelude.<*> ( x
                      Data..@? "metricPointSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "period")
      Prelude.<*> (x Data..@? "source")
      Prelude.<*> (x Data..@? "statistic")

instance Prelude.Hashable DataResponse where
  hashWithSalt _salt DataResponse' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` metricPoints
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` statistic

instance Prelude.NFData DataResponse where
  rnf DataResponse' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf metricPoints
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf statistic
