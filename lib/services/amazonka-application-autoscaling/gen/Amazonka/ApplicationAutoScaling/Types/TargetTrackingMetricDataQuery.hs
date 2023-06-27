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
-- Module      : Amazonka.ApplicationAutoScaling.Types.TargetTrackingMetricDataQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.TargetTrackingMetricDataQuery where

import Amazonka.ApplicationAutoScaling.Types.TargetTrackingMetricStat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metric data to return. Also defines whether this call is returning
-- data for one metric only, or whether it is performing a math expression
-- on the values of returned metric statistics to create a new time series.
-- A time series is a series of data points, each of which is associated
-- with a timestamp.
--
-- For more information and examples, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking-metric-math.html Create a target tracking scaling policy for Application Auto Scaling using metric math>
-- in the /Application Auto Scaling User Guide/.
--
-- /See:/ 'newTargetTrackingMetricDataQuery' smart constructor.
data TargetTrackingMetricDataQuery = TargetTrackingMetricDataQuery'
  { -- | The math expression to perform on the returned data, if this object is
    -- performing a math expression. This expression can use the @Id@ of the
    -- other metrics to refer to those metrics, and can also use the @Id@ of
    -- other expressions to use the result of those expressions.
    --
    -- Conditional: Within each @TargetTrackingMetricDataQuery@ object, you
    -- must specify either @Expression@ or @MetricStat@, but not both.
    expression :: Prelude.Maybe Prelude.Text,
    -- | A human-readable label for this metric or expression. This is especially
    -- useful if this is a math expression, so that you know what the value
    -- represents.
    label :: Prelude.Maybe Prelude.Text,
    -- | Information about the metric data to return.
    --
    -- Conditional: Within each @MetricDataQuery@ object, you must specify
    -- either @Expression@ or @MetricStat@, but not both.
    metricStat :: Prelude.Maybe TargetTrackingMetricStat,
    -- | Indicates whether to return the timestamps and raw data values of this
    -- metric.
    --
    -- If you use any math expressions, specify @true@ for this value for only
    -- the final math expression that the metric specification is based on. You
    -- must specify @false@ for @ReturnData@ for all the other metrics and
    -- expressions used in the metric specification.
    --
    -- If you are only retrieving metrics and not performing any math
    -- expressions, do not specify anything for @ReturnData@. This sets it to
    -- its default (@true@).
    returnData :: Prelude.Maybe Prelude.Bool,
    -- | A short name that identifies the object\'s results in the response. This
    -- name must be unique among all @MetricDataQuery@ objects specified for a
    -- single scaling policy. If you are performing math expressions on this
    -- set of data, this name represents that data and can serve as a variable
    -- in the mathematical expression. The valid characters are letters,
    -- numbers, and underscores. The first character must be a lowercase
    -- letter.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetTrackingMetricDataQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'targetTrackingMetricDataQuery_expression' - The math expression to perform on the returned data, if this object is
-- performing a math expression. This expression can use the @Id@ of the
-- other metrics to refer to those metrics, and can also use the @Id@ of
-- other expressions to use the result of those expressions.
--
-- Conditional: Within each @TargetTrackingMetricDataQuery@ object, you
-- must specify either @Expression@ or @MetricStat@, but not both.
--
-- 'label', 'targetTrackingMetricDataQuery_label' - A human-readable label for this metric or expression. This is especially
-- useful if this is a math expression, so that you know what the value
-- represents.
--
-- 'metricStat', 'targetTrackingMetricDataQuery_metricStat' - Information about the metric data to return.
--
-- Conditional: Within each @MetricDataQuery@ object, you must specify
-- either @Expression@ or @MetricStat@, but not both.
--
-- 'returnData', 'targetTrackingMetricDataQuery_returnData' - Indicates whether to return the timestamps and raw data values of this
-- metric.
--
-- If you use any math expressions, specify @true@ for this value for only
-- the final math expression that the metric specification is based on. You
-- must specify @false@ for @ReturnData@ for all the other metrics and
-- expressions used in the metric specification.
--
-- If you are only retrieving metrics and not performing any math
-- expressions, do not specify anything for @ReturnData@. This sets it to
-- its default (@true@).
--
-- 'id', 'targetTrackingMetricDataQuery_id' - A short name that identifies the object\'s results in the response. This
-- name must be unique among all @MetricDataQuery@ objects specified for a
-- single scaling policy. If you are performing math expressions on this
-- set of data, this name represents that data and can serve as a variable
-- in the mathematical expression. The valid characters are letters,
-- numbers, and underscores. The first character must be a lowercase
-- letter.
newTargetTrackingMetricDataQuery ::
  -- | 'id'
  Prelude.Text ->
  TargetTrackingMetricDataQuery
newTargetTrackingMetricDataQuery pId_ =
  TargetTrackingMetricDataQuery'
    { expression =
        Prelude.Nothing,
      label = Prelude.Nothing,
      metricStat = Prelude.Nothing,
      returnData = Prelude.Nothing,
      id = pId_
    }

-- | The math expression to perform on the returned data, if this object is
-- performing a math expression. This expression can use the @Id@ of the
-- other metrics to refer to those metrics, and can also use the @Id@ of
-- other expressions to use the result of those expressions.
--
-- Conditional: Within each @TargetTrackingMetricDataQuery@ object, you
-- must specify either @Expression@ or @MetricStat@, but not both.
targetTrackingMetricDataQuery_expression :: Lens.Lens' TargetTrackingMetricDataQuery (Prelude.Maybe Prelude.Text)
targetTrackingMetricDataQuery_expression = Lens.lens (\TargetTrackingMetricDataQuery' {expression} -> expression) (\s@TargetTrackingMetricDataQuery' {} a -> s {expression = a} :: TargetTrackingMetricDataQuery)

-- | A human-readable label for this metric or expression. This is especially
-- useful if this is a math expression, so that you know what the value
-- represents.
targetTrackingMetricDataQuery_label :: Lens.Lens' TargetTrackingMetricDataQuery (Prelude.Maybe Prelude.Text)
targetTrackingMetricDataQuery_label = Lens.lens (\TargetTrackingMetricDataQuery' {label} -> label) (\s@TargetTrackingMetricDataQuery' {} a -> s {label = a} :: TargetTrackingMetricDataQuery)

-- | Information about the metric data to return.
--
-- Conditional: Within each @MetricDataQuery@ object, you must specify
-- either @Expression@ or @MetricStat@, but not both.
targetTrackingMetricDataQuery_metricStat :: Lens.Lens' TargetTrackingMetricDataQuery (Prelude.Maybe TargetTrackingMetricStat)
targetTrackingMetricDataQuery_metricStat = Lens.lens (\TargetTrackingMetricDataQuery' {metricStat} -> metricStat) (\s@TargetTrackingMetricDataQuery' {} a -> s {metricStat = a} :: TargetTrackingMetricDataQuery)

-- | Indicates whether to return the timestamps and raw data values of this
-- metric.
--
-- If you use any math expressions, specify @true@ for this value for only
-- the final math expression that the metric specification is based on. You
-- must specify @false@ for @ReturnData@ for all the other metrics and
-- expressions used in the metric specification.
--
-- If you are only retrieving metrics and not performing any math
-- expressions, do not specify anything for @ReturnData@. This sets it to
-- its default (@true@).
targetTrackingMetricDataQuery_returnData :: Lens.Lens' TargetTrackingMetricDataQuery (Prelude.Maybe Prelude.Bool)
targetTrackingMetricDataQuery_returnData = Lens.lens (\TargetTrackingMetricDataQuery' {returnData} -> returnData) (\s@TargetTrackingMetricDataQuery' {} a -> s {returnData = a} :: TargetTrackingMetricDataQuery)

-- | A short name that identifies the object\'s results in the response. This
-- name must be unique among all @MetricDataQuery@ objects specified for a
-- single scaling policy. If you are performing math expressions on this
-- set of data, this name represents that data and can serve as a variable
-- in the mathematical expression. The valid characters are letters,
-- numbers, and underscores. The first character must be a lowercase
-- letter.
targetTrackingMetricDataQuery_id :: Lens.Lens' TargetTrackingMetricDataQuery Prelude.Text
targetTrackingMetricDataQuery_id = Lens.lens (\TargetTrackingMetricDataQuery' {id} -> id) (\s@TargetTrackingMetricDataQuery' {} a -> s {id = a} :: TargetTrackingMetricDataQuery)

instance Data.FromJSON TargetTrackingMetricDataQuery where
  parseJSON =
    Data.withObject
      "TargetTrackingMetricDataQuery"
      ( \x ->
          TargetTrackingMetricDataQuery'
            Prelude.<$> (x Data..:? "Expression")
            Prelude.<*> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "MetricStat")
            Prelude.<*> (x Data..:? "ReturnData")
            Prelude.<*> (x Data..: "Id")
      )

instance
  Prelude.Hashable
    TargetTrackingMetricDataQuery
  where
  hashWithSalt _salt TargetTrackingMetricDataQuery' {..} =
    _salt
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` metricStat
      `Prelude.hashWithSalt` returnData
      `Prelude.hashWithSalt` id

instance Prelude.NFData TargetTrackingMetricDataQuery where
  rnf TargetTrackingMetricDataQuery' {..} =
    Prelude.rnf expression
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf metricStat
      `Prelude.seq` Prelude.rnf returnData
      `Prelude.seq` Prelude.rnf id

instance Data.ToJSON TargetTrackingMetricDataQuery where
  toJSON TargetTrackingMetricDataQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Expression" Data..=) Prelude.<$> expression,
            ("Label" Data..=) Prelude.<$> label,
            ("MetricStat" Data..=) Prelude.<$> metricStat,
            ("ReturnData" Data..=) Prelude.<$> returnData,
            Prelude.Just ("Id" Data..= id)
          ]
      )
