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
-- Module      : Amazonka.IoTSiteWise.Types.Metric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Metric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.ExpressionVariable
import Amazonka.IoTSiteWise.Types.MetricProcessingConfig
import Amazonka.IoTSiteWise.Types.MetricWindow
import qualified Amazonka.Prelude as Prelude

-- | Contains an asset metric property. With metrics, you can calculate
-- aggregate functions, such as an average, maximum, or minimum, as
-- specified through an expression. A metric maps several values to a
-- single value (such as a sum).
--
-- The maximum number of dependent\/cascading variables used in any one
-- metric calculation is 10. Therefore, a /root/ metric can have up to 10
-- cascading metrics in its computational dependency tree. Additionally, a
-- metric can only have a data type of @DOUBLE@ and consume properties with
-- data types of @INTEGER@ or @DOUBLE@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-properties.html#metrics Metrics>
-- in the /IoT SiteWise User Guide/.
--
-- /See:/ 'newMetric' smart constructor.
data Metric = Metric'
  { -- | The processing configuration for the given metric property. You can
    -- configure metrics to be computed at the edge or in the Amazon Web
    -- Services Cloud. By default, metrics are forwarded to the cloud.
    processingConfig :: Prelude.Maybe MetricProcessingConfig,
    -- | The mathematical expression that defines the metric aggregation
    -- function. You can specify up to 10 variables per expression. You can
    -- specify up to 10 functions per expression.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
    -- in the /IoT SiteWise User Guide/.
    expression :: Prelude.Text,
    -- | The list of variables used in the expression.
    variables :: [ExpressionVariable],
    -- | The window (time interval) over which IoT SiteWise computes the
    -- metric\'s aggregation expression. IoT SiteWise computes one data point
    -- per @window@.
    window :: MetricWindow
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Metric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingConfig', 'metric_processingConfig' - The processing configuration for the given metric property. You can
-- configure metrics to be computed at the edge or in the Amazon Web
-- Services Cloud. By default, metrics are forwarded to the cloud.
--
-- 'expression', 'metric_expression' - The mathematical expression that defines the metric aggregation
-- function. You can specify up to 10 variables per expression. You can
-- specify up to 10 functions per expression.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
--
-- 'variables', 'metric_variables' - The list of variables used in the expression.
--
-- 'window', 'metric_window' - The window (time interval) over which IoT SiteWise computes the
-- metric\'s aggregation expression. IoT SiteWise computes one data point
-- per @window@.
newMetric ::
  -- | 'expression'
  Prelude.Text ->
  -- | 'window'
  MetricWindow ->
  Metric
newMetric pExpression_ pWindow_ =
  Metric'
    { processingConfig = Prelude.Nothing,
      expression = pExpression_,
      variables = Prelude.mempty,
      window = pWindow_
    }

-- | The processing configuration for the given metric property. You can
-- configure metrics to be computed at the edge or in the Amazon Web
-- Services Cloud. By default, metrics are forwarded to the cloud.
metric_processingConfig :: Lens.Lens' Metric (Prelude.Maybe MetricProcessingConfig)
metric_processingConfig = Lens.lens (\Metric' {processingConfig} -> processingConfig) (\s@Metric' {} a -> s {processingConfig = a} :: Metric)

-- | The mathematical expression that defines the metric aggregation
-- function. You can specify up to 10 variables per expression. You can
-- specify up to 10 functions per expression.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
metric_expression :: Lens.Lens' Metric Prelude.Text
metric_expression = Lens.lens (\Metric' {expression} -> expression) (\s@Metric' {} a -> s {expression = a} :: Metric)

-- | The list of variables used in the expression.
metric_variables :: Lens.Lens' Metric [ExpressionVariable]
metric_variables = Lens.lens (\Metric' {variables} -> variables) (\s@Metric' {} a -> s {variables = a} :: Metric) Prelude.. Lens.coerced

-- | The window (time interval) over which IoT SiteWise computes the
-- metric\'s aggregation expression. IoT SiteWise computes one data point
-- per @window@.
metric_window :: Lens.Lens' Metric MetricWindow
metric_window = Lens.lens (\Metric' {window} -> window) (\s@Metric' {} a -> s {window = a} :: Metric)

instance Data.FromJSON Metric where
  parseJSON =
    Data.withObject
      "Metric"
      ( \x ->
          Metric'
            Prelude.<$> (x Data..:? "processingConfig")
            Prelude.<*> (x Data..: "expression")
            Prelude.<*> (x Data..:? "variables" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "window")
      )

instance Prelude.Hashable Metric where
  hashWithSalt _salt Metric' {..} =
    _salt
      `Prelude.hashWithSalt` processingConfig
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` variables
      `Prelude.hashWithSalt` window

instance Prelude.NFData Metric where
  rnf Metric' {..} =
    Prelude.rnf processingConfig
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf variables
      `Prelude.seq` Prelude.rnf window

instance Data.ToJSON Metric where
  toJSON Metric' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("processingConfig" Data..=)
              Prelude.<$> processingConfig,
            Prelude.Just ("expression" Data..= expression),
            Prelude.Just ("variables" Data..= variables),
            Prelude.Just ("window" Data..= window)
          ]
      )
