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
-- Module      : Amazonka.Personalize.Types.MetricAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.MetricAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information on a metric that a metric attribution reports on.
-- For more information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/measuring-recommendation-impact.html Measuring impact of recommendations>.
--
-- /See:/ 'newMetricAttribute' smart constructor.
data MetricAttribute = MetricAttribute'
  { -- | The metric\'s event type.
    eventType :: Prelude.Text,
    -- | The metric\'s name. The name helps you identify the metric in Amazon
    -- CloudWatch or Amazon S3.
    metricName :: Prelude.Text,
    -- | The attribute\'s expression. Available functions are @SUM()@ or
    -- @SAMPLECOUNT()@. For SUM() functions, provide the dataset type (either
    -- Interactions or Items) and column to sum as a parameter. For example
    -- SUM(Items.PRICE).
    expression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventType', 'metricAttribute_eventType' - The metric\'s event type.
--
-- 'metricName', 'metricAttribute_metricName' - The metric\'s name. The name helps you identify the metric in Amazon
-- CloudWatch or Amazon S3.
--
-- 'expression', 'metricAttribute_expression' - The attribute\'s expression. Available functions are @SUM()@ or
-- @SAMPLECOUNT()@. For SUM() functions, provide the dataset type (either
-- Interactions or Items) and column to sum as a parameter. For example
-- SUM(Items.PRICE).
newMetricAttribute ::
  -- | 'eventType'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  -- | 'expression'
  Prelude.Text ->
  MetricAttribute
newMetricAttribute
  pEventType_
  pMetricName_
  pExpression_ =
    MetricAttribute'
      { eventType = pEventType_,
        metricName = pMetricName_,
        expression = pExpression_
      }

-- | The metric\'s event type.
metricAttribute_eventType :: Lens.Lens' MetricAttribute Prelude.Text
metricAttribute_eventType = Lens.lens (\MetricAttribute' {eventType} -> eventType) (\s@MetricAttribute' {} a -> s {eventType = a} :: MetricAttribute)

-- | The metric\'s name. The name helps you identify the metric in Amazon
-- CloudWatch or Amazon S3.
metricAttribute_metricName :: Lens.Lens' MetricAttribute Prelude.Text
metricAttribute_metricName = Lens.lens (\MetricAttribute' {metricName} -> metricName) (\s@MetricAttribute' {} a -> s {metricName = a} :: MetricAttribute)

-- | The attribute\'s expression. Available functions are @SUM()@ or
-- @SAMPLECOUNT()@. For SUM() functions, provide the dataset type (either
-- Interactions or Items) and column to sum as a parameter. For example
-- SUM(Items.PRICE).
metricAttribute_expression :: Lens.Lens' MetricAttribute Prelude.Text
metricAttribute_expression = Lens.lens (\MetricAttribute' {expression} -> expression) (\s@MetricAttribute' {} a -> s {expression = a} :: MetricAttribute)

instance Data.FromJSON MetricAttribute where
  parseJSON =
    Data.withObject
      "MetricAttribute"
      ( \x ->
          MetricAttribute'
            Prelude.<$> (x Data..: "eventType")
            Prelude.<*> (x Data..: "metricName")
            Prelude.<*> (x Data..: "expression")
      )

instance Prelude.Hashable MetricAttribute where
  hashWithSalt _salt MetricAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` expression

instance Prelude.NFData MetricAttribute where
  rnf MetricAttribute' {..} =
    Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf expression

instance Data.ToJSON MetricAttribute where
  toJSON MetricAttribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("eventType" Data..= eventType),
            Prelude.Just ("metricName" Data..= metricName),
            Prelude.Just ("expression" Data..= expression)
          ]
      )
