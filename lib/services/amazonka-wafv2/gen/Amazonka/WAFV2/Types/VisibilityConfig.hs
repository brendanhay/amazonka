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
-- Module      : Amazonka.WAFV2.Types.VisibilityConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.VisibilityConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
--
-- /See:/ 'newVisibilityConfig' smart constructor.
data VisibilityConfig = VisibilityConfig'
  { -- | A boolean indicating whether WAF should store a sampling of the web
    -- requests that match the rules. You can view the sampled requests through
    -- the WAF console.
    sampledRequestsEnabled :: Prelude.Bool,
    -- | A boolean indicating whether the associated resource sends metrics to
    -- Amazon CloudWatch. For the list of available metrics, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#waf-metrics WAF Metrics>.
    cloudWatchMetricsEnabled :: Prelude.Bool,
    -- | A name of the Amazon CloudWatch metric. The name can contain only the
    -- characters: A-Z, a-z, 0-9, - (hyphen), and _ (underscore). The name can
    -- be from one to 128 characters long. It can\'t contain whitespace or
    -- metric names reserved for WAF, for example @All@ and @Default_Action@.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisibilityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sampledRequestsEnabled', 'visibilityConfig_sampledRequestsEnabled' - A boolean indicating whether WAF should store a sampling of the web
-- requests that match the rules. You can view the sampled requests through
-- the WAF console.
--
-- 'cloudWatchMetricsEnabled', 'visibilityConfig_cloudWatchMetricsEnabled' - A boolean indicating whether the associated resource sends metrics to
-- Amazon CloudWatch. For the list of available metrics, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#waf-metrics WAF Metrics>.
--
-- 'metricName', 'visibilityConfig_metricName' - A name of the Amazon CloudWatch metric. The name can contain only the
-- characters: A-Z, a-z, 0-9, - (hyphen), and _ (underscore). The name can
-- be from one to 128 characters long. It can\'t contain whitespace or
-- metric names reserved for WAF, for example @All@ and @Default_Action@.
newVisibilityConfig ::
  -- | 'sampledRequestsEnabled'
  Prelude.Bool ->
  -- | 'cloudWatchMetricsEnabled'
  Prelude.Bool ->
  -- | 'metricName'
  Prelude.Text ->
  VisibilityConfig
newVisibilityConfig
  pSampledRequestsEnabled_
  pCloudWatchMetricsEnabled_
  pMetricName_ =
    VisibilityConfig'
      { sampledRequestsEnabled =
          pSampledRequestsEnabled_,
        cloudWatchMetricsEnabled =
          pCloudWatchMetricsEnabled_,
        metricName = pMetricName_
      }

-- | A boolean indicating whether WAF should store a sampling of the web
-- requests that match the rules. You can view the sampled requests through
-- the WAF console.
visibilityConfig_sampledRequestsEnabled :: Lens.Lens' VisibilityConfig Prelude.Bool
visibilityConfig_sampledRequestsEnabled = Lens.lens (\VisibilityConfig' {sampledRequestsEnabled} -> sampledRequestsEnabled) (\s@VisibilityConfig' {} a -> s {sampledRequestsEnabled = a} :: VisibilityConfig)

-- | A boolean indicating whether the associated resource sends metrics to
-- Amazon CloudWatch. For the list of available metrics, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#waf-metrics WAF Metrics>.
visibilityConfig_cloudWatchMetricsEnabled :: Lens.Lens' VisibilityConfig Prelude.Bool
visibilityConfig_cloudWatchMetricsEnabled = Lens.lens (\VisibilityConfig' {cloudWatchMetricsEnabled} -> cloudWatchMetricsEnabled) (\s@VisibilityConfig' {} a -> s {cloudWatchMetricsEnabled = a} :: VisibilityConfig)

-- | A name of the Amazon CloudWatch metric. The name can contain only the
-- characters: A-Z, a-z, 0-9, - (hyphen), and _ (underscore). The name can
-- be from one to 128 characters long. It can\'t contain whitespace or
-- metric names reserved for WAF, for example @All@ and @Default_Action@.
visibilityConfig_metricName :: Lens.Lens' VisibilityConfig Prelude.Text
visibilityConfig_metricName = Lens.lens (\VisibilityConfig' {metricName} -> metricName) (\s@VisibilityConfig' {} a -> s {metricName = a} :: VisibilityConfig)

instance Data.FromJSON VisibilityConfig where
  parseJSON =
    Data.withObject
      "VisibilityConfig"
      ( \x ->
          VisibilityConfig'
            Prelude.<$> (x Data..: "SampledRequestsEnabled")
            Prelude.<*> (x Data..: "CloudWatchMetricsEnabled")
            Prelude.<*> (x Data..: "MetricName")
      )

instance Prelude.Hashable VisibilityConfig where
  hashWithSalt _salt VisibilityConfig' {..} =
    _salt
      `Prelude.hashWithSalt` sampledRequestsEnabled
      `Prelude.hashWithSalt` cloudWatchMetricsEnabled
      `Prelude.hashWithSalt` metricName

instance Prelude.NFData VisibilityConfig where
  rnf VisibilityConfig' {..} =
    Prelude.rnf sampledRequestsEnabled
      `Prelude.seq` Prelude.rnf cloudWatchMetricsEnabled
      `Prelude.seq` Prelude.rnf metricName

instance Data.ToJSON VisibilityConfig where
  toJSON VisibilityConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SampledRequestsEnabled"
                  Data..= sampledRequestsEnabled
              ),
            Prelude.Just
              ( "CloudWatchMetricsEnabled"
                  Data..= cloudWatchMetricsEnabled
              ),
            Prelude.Just ("MetricName" Data..= metricName)
          ]
      )
