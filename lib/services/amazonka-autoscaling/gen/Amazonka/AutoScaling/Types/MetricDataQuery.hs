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
-- Module      : Amazonka.AutoScaling.Types.MetricDataQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.MetricDataQuery where

import Amazonka.AutoScaling.Types.MetricStat
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
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/predictive-scaling-customized-metric-specification.html Advanced predictive scaling policy configurations using custom metrics>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newMetricDataQuery' smart constructor.
data MetricDataQuery = MetricDataQuery'
  { -- | The math expression to perform on the returned data, if this object is
    -- performing a math expression. This expression can use the @Id@ of the
    -- other metrics to refer to those metrics, and can also use the @Id@ of
    -- other expressions to use the result of those expressions.
    --
    -- Conditional: Within each @MetricDataQuery@ object, you must specify
    -- either @Expression@ or @MetricStat@, but not both.
    expression :: Prelude.Maybe Prelude.Text,
    -- | A human-readable label for this metric or expression. This is especially
    -- useful if this is a math expression, so that you know what the value
    -- represents.
    label :: Prelude.Maybe Prelude.Text,
    -- | Information about the metric data to return.
    --
    -- Conditional: Within each @MetricDataQuery@ object, you must specify
    -- either @Expression@ or @MetricStat@, but not both.
    metricStat :: Prelude.Maybe MetricStat,
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
-- Create a value of 'MetricDataQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'metricDataQuery_expression' - The math expression to perform on the returned data, if this object is
-- performing a math expression. This expression can use the @Id@ of the
-- other metrics to refer to those metrics, and can also use the @Id@ of
-- other expressions to use the result of those expressions.
--
-- Conditional: Within each @MetricDataQuery@ object, you must specify
-- either @Expression@ or @MetricStat@, but not both.
--
-- 'label', 'metricDataQuery_label' - A human-readable label for this metric or expression. This is especially
-- useful if this is a math expression, so that you know what the value
-- represents.
--
-- 'metricStat', 'metricDataQuery_metricStat' - Information about the metric data to return.
--
-- Conditional: Within each @MetricDataQuery@ object, you must specify
-- either @Expression@ or @MetricStat@, but not both.
--
-- 'returnData', 'metricDataQuery_returnData' - Indicates whether to return the timestamps and raw data values of this
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
-- 'id', 'metricDataQuery_id' - A short name that identifies the object\'s results in the response. This
-- name must be unique among all @MetricDataQuery@ objects specified for a
-- single scaling policy. If you are performing math expressions on this
-- set of data, this name represents that data and can serve as a variable
-- in the mathematical expression. The valid characters are letters,
-- numbers, and underscores. The first character must be a lowercase
-- letter.
newMetricDataQuery ::
  -- | 'id'
  Prelude.Text ->
  MetricDataQuery
newMetricDataQuery pId_ =
  MetricDataQuery'
    { expression = Prelude.Nothing,
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
-- Conditional: Within each @MetricDataQuery@ object, you must specify
-- either @Expression@ or @MetricStat@, but not both.
metricDataQuery_expression :: Lens.Lens' MetricDataQuery (Prelude.Maybe Prelude.Text)
metricDataQuery_expression = Lens.lens (\MetricDataQuery' {expression} -> expression) (\s@MetricDataQuery' {} a -> s {expression = a} :: MetricDataQuery)

-- | A human-readable label for this metric or expression. This is especially
-- useful if this is a math expression, so that you know what the value
-- represents.
metricDataQuery_label :: Lens.Lens' MetricDataQuery (Prelude.Maybe Prelude.Text)
metricDataQuery_label = Lens.lens (\MetricDataQuery' {label} -> label) (\s@MetricDataQuery' {} a -> s {label = a} :: MetricDataQuery)

-- | Information about the metric data to return.
--
-- Conditional: Within each @MetricDataQuery@ object, you must specify
-- either @Expression@ or @MetricStat@, but not both.
metricDataQuery_metricStat :: Lens.Lens' MetricDataQuery (Prelude.Maybe MetricStat)
metricDataQuery_metricStat = Lens.lens (\MetricDataQuery' {metricStat} -> metricStat) (\s@MetricDataQuery' {} a -> s {metricStat = a} :: MetricDataQuery)

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
metricDataQuery_returnData :: Lens.Lens' MetricDataQuery (Prelude.Maybe Prelude.Bool)
metricDataQuery_returnData = Lens.lens (\MetricDataQuery' {returnData} -> returnData) (\s@MetricDataQuery' {} a -> s {returnData = a} :: MetricDataQuery)

-- | A short name that identifies the object\'s results in the response. This
-- name must be unique among all @MetricDataQuery@ objects specified for a
-- single scaling policy. If you are performing math expressions on this
-- set of data, this name represents that data and can serve as a variable
-- in the mathematical expression. The valid characters are letters,
-- numbers, and underscores. The first character must be a lowercase
-- letter.
metricDataQuery_id :: Lens.Lens' MetricDataQuery Prelude.Text
metricDataQuery_id = Lens.lens (\MetricDataQuery' {id} -> id) (\s@MetricDataQuery' {} a -> s {id = a} :: MetricDataQuery)

instance Data.FromXML MetricDataQuery where
  parseXML x =
    MetricDataQuery'
      Prelude.<$> (x Data..@? "Expression")
      Prelude.<*> (x Data..@? "Label")
      Prelude.<*> (x Data..@? "MetricStat")
      Prelude.<*> (x Data..@? "ReturnData")
      Prelude.<*> (x Data..@ "Id")

instance Prelude.Hashable MetricDataQuery where
  hashWithSalt _salt MetricDataQuery' {..} =
    _salt
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` metricStat
      `Prelude.hashWithSalt` returnData
      `Prelude.hashWithSalt` id

instance Prelude.NFData MetricDataQuery where
  rnf MetricDataQuery' {..} =
    Prelude.rnf expression
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf metricStat
      `Prelude.seq` Prelude.rnf returnData
      `Prelude.seq` Prelude.rnf id

instance Data.ToQuery MetricDataQuery where
  toQuery MetricDataQuery' {..} =
    Prelude.mconcat
      [ "Expression" Data.=: expression,
        "Label" Data.=: label,
        "MetricStat" Data.=: metricStat,
        "ReturnData" Data.=: returnData,
        "Id" Data.=: id
      ]
