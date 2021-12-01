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
-- Module      : Amazonka.CloudWatch.Types.MetricDataResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.MetricDataResult where

import Amazonka.CloudWatch.Types.MessageData
import Amazonka.CloudWatch.Types.StatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A @GetMetricData@ call returns an array of @MetricDataResult@
-- structures. Each of these structures includes the data points for that
-- metric, along with the timestamps of those data points and other
-- identifying information.
--
-- /See:/ 'newMetricDataResult' smart constructor.
data MetricDataResult = MetricDataResult'
  { -- | The data points for the metric corresponding to @Timestamps@. The number
    -- of values always matches the number of timestamps and the timestamp for
    -- Values[x] is Timestamps[x].
    values :: Prelude.Maybe [Prelude.Double],
    -- | The short name you specified to represent this metric.
    id :: Prelude.Maybe Prelude.Text,
    -- | The timestamps for the data points, formatted in Unix timestamp format.
    -- The number of timestamps always matches the number of values and the
    -- value for Timestamps[x] is Values[x].
    timestamps :: Prelude.Maybe [Core.ISO8601],
    -- | A list of messages with additional information about the data returned.
    messages :: Prelude.Maybe [MessageData],
    -- | The human-readable label associated with the data.
    label :: Prelude.Maybe Prelude.Text,
    -- | The status of the returned data. @Complete@ indicates that all data
    -- points in the requested time range were returned. @PartialData@ means
    -- that an incomplete set of data points were returned. You can use the
    -- @NextToken@ value that was returned and repeat your request to get more
    -- data points. @NextToken@ is not returned if you are performing a math
    -- expression. @InternalError@ indicates that an error occurred. Retry your
    -- request using @NextToken@, if present.
    statusCode :: Prelude.Maybe StatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDataResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'metricDataResult_values' - The data points for the metric corresponding to @Timestamps@. The number
-- of values always matches the number of timestamps and the timestamp for
-- Values[x] is Timestamps[x].
--
-- 'id', 'metricDataResult_id' - The short name you specified to represent this metric.
--
-- 'timestamps', 'metricDataResult_timestamps' - The timestamps for the data points, formatted in Unix timestamp format.
-- The number of timestamps always matches the number of values and the
-- value for Timestamps[x] is Values[x].
--
-- 'messages', 'metricDataResult_messages' - A list of messages with additional information about the data returned.
--
-- 'label', 'metricDataResult_label' - The human-readable label associated with the data.
--
-- 'statusCode', 'metricDataResult_statusCode' - The status of the returned data. @Complete@ indicates that all data
-- points in the requested time range were returned. @PartialData@ means
-- that an incomplete set of data points were returned. You can use the
-- @NextToken@ value that was returned and repeat your request to get more
-- data points. @NextToken@ is not returned if you are performing a math
-- expression. @InternalError@ indicates that an error occurred. Retry your
-- request using @NextToken@, if present.
newMetricDataResult ::
  MetricDataResult
newMetricDataResult =
  MetricDataResult'
    { values = Prelude.Nothing,
      id = Prelude.Nothing,
      timestamps = Prelude.Nothing,
      messages = Prelude.Nothing,
      label = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | The data points for the metric corresponding to @Timestamps@. The number
-- of values always matches the number of timestamps and the timestamp for
-- Values[x] is Timestamps[x].
metricDataResult_values :: Lens.Lens' MetricDataResult (Prelude.Maybe [Prelude.Double])
metricDataResult_values = Lens.lens (\MetricDataResult' {values} -> values) (\s@MetricDataResult' {} a -> s {values = a} :: MetricDataResult) Prelude.. Lens.mapping Lens.coerced

-- | The short name you specified to represent this metric.
metricDataResult_id :: Lens.Lens' MetricDataResult (Prelude.Maybe Prelude.Text)
metricDataResult_id = Lens.lens (\MetricDataResult' {id} -> id) (\s@MetricDataResult' {} a -> s {id = a} :: MetricDataResult)

-- | The timestamps for the data points, formatted in Unix timestamp format.
-- The number of timestamps always matches the number of values and the
-- value for Timestamps[x] is Values[x].
metricDataResult_timestamps :: Lens.Lens' MetricDataResult (Prelude.Maybe [Prelude.UTCTime])
metricDataResult_timestamps = Lens.lens (\MetricDataResult' {timestamps} -> timestamps) (\s@MetricDataResult' {} a -> s {timestamps = a} :: MetricDataResult) Prelude.. Lens.mapping Lens.coerced

-- | A list of messages with additional information about the data returned.
metricDataResult_messages :: Lens.Lens' MetricDataResult (Prelude.Maybe [MessageData])
metricDataResult_messages = Lens.lens (\MetricDataResult' {messages} -> messages) (\s@MetricDataResult' {} a -> s {messages = a} :: MetricDataResult) Prelude.. Lens.mapping Lens.coerced

-- | The human-readable label associated with the data.
metricDataResult_label :: Lens.Lens' MetricDataResult (Prelude.Maybe Prelude.Text)
metricDataResult_label = Lens.lens (\MetricDataResult' {label} -> label) (\s@MetricDataResult' {} a -> s {label = a} :: MetricDataResult)

-- | The status of the returned data. @Complete@ indicates that all data
-- points in the requested time range were returned. @PartialData@ means
-- that an incomplete set of data points were returned. You can use the
-- @NextToken@ value that was returned and repeat your request to get more
-- data points. @NextToken@ is not returned if you are performing a math
-- expression. @InternalError@ indicates that an error occurred. Retry your
-- request using @NextToken@, if present.
metricDataResult_statusCode :: Lens.Lens' MetricDataResult (Prelude.Maybe StatusCode)
metricDataResult_statusCode = Lens.lens (\MetricDataResult' {statusCode} -> statusCode) (\s@MetricDataResult' {} a -> s {statusCode = a} :: MetricDataResult)

instance Core.FromXML MetricDataResult where
  parseXML x =
    MetricDataResult'
      Prelude.<$> ( x Core..@? "Values" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Id")
      Prelude.<*> ( x Core..@? "Timestamps" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "Messages" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Label")
      Prelude.<*> (x Core..@? "StatusCode")

instance Prelude.Hashable MetricDataResult where
  hashWithSalt salt' MetricDataResult' {..} =
    salt' `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` messages
      `Prelude.hashWithSalt` timestamps
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` values

instance Prelude.NFData MetricDataResult where
  rnf MetricDataResult' {..} =
    Prelude.rnf values
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf messages
      `Prelude.seq` Prelude.rnf timestamps
      `Prelude.seq` Prelude.rnf id
