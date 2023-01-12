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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2VisibilityConfigDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2VisibilityConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
--
-- /See:/ 'newAwsWafv2VisibilityConfigDetails' smart constructor.
data AwsWafv2VisibilityConfigDetails = AwsWafv2VisibilityConfigDetails'
  { -- | A boolean indicating whether the associated resource sends metrics to
    -- Amazon CloudWatch. For the list of available metrics, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#waf-metrics WAF metrics and dimensions>
    -- in the /WAF Developer Guide/.
    cloudWatchMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A name of the Amazon CloudWatch metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | A boolean indicating whether WAF should store a sampling of the web
    -- requests that match the rules. You can view the sampled requests through
    -- the WAF console.
    sampledRequestsEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2VisibilityConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchMetricsEnabled', 'awsWafv2VisibilityConfigDetails_cloudWatchMetricsEnabled' - A boolean indicating whether the associated resource sends metrics to
-- Amazon CloudWatch. For the list of available metrics, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#waf-metrics WAF metrics and dimensions>
-- in the /WAF Developer Guide/.
--
-- 'metricName', 'awsWafv2VisibilityConfigDetails_metricName' - A name of the Amazon CloudWatch metric.
--
-- 'sampledRequestsEnabled', 'awsWafv2VisibilityConfigDetails_sampledRequestsEnabled' - A boolean indicating whether WAF should store a sampling of the web
-- requests that match the rules. You can view the sampled requests through
-- the WAF console.
newAwsWafv2VisibilityConfigDetails ::
  AwsWafv2VisibilityConfigDetails
newAwsWafv2VisibilityConfigDetails =
  AwsWafv2VisibilityConfigDetails'
    { cloudWatchMetricsEnabled =
        Prelude.Nothing,
      metricName = Prelude.Nothing,
      sampledRequestsEnabled = Prelude.Nothing
    }

-- | A boolean indicating whether the associated resource sends metrics to
-- Amazon CloudWatch. For the list of available metrics, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#waf-metrics WAF metrics and dimensions>
-- in the /WAF Developer Guide/.
awsWafv2VisibilityConfigDetails_cloudWatchMetricsEnabled :: Lens.Lens' AwsWafv2VisibilityConfigDetails (Prelude.Maybe Prelude.Bool)
awsWafv2VisibilityConfigDetails_cloudWatchMetricsEnabled = Lens.lens (\AwsWafv2VisibilityConfigDetails' {cloudWatchMetricsEnabled} -> cloudWatchMetricsEnabled) (\s@AwsWafv2VisibilityConfigDetails' {} a -> s {cloudWatchMetricsEnabled = a} :: AwsWafv2VisibilityConfigDetails)

-- | A name of the Amazon CloudWatch metric.
awsWafv2VisibilityConfigDetails_metricName :: Lens.Lens' AwsWafv2VisibilityConfigDetails (Prelude.Maybe Prelude.Text)
awsWafv2VisibilityConfigDetails_metricName = Lens.lens (\AwsWafv2VisibilityConfigDetails' {metricName} -> metricName) (\s@AwsWafv2VisibilityConfigDetails' {} a -> s {metricName = a} :: AwsWafv2VisibilityConfigDetails)

-- | A boolean indicating whether WAF should store a sampling of the web
-- requests that match the rules. You can view the sampled requests through
-- the WAF console.
awsWafv2VisibilityConfigDetails_sampledRequestsEnabled :: Lens.Lens' AwsWafv2VisibilityConfigDetails (Prelude.Maybe Prelude.Bool)
awsWafv2VisibilityConfigDetails_sampledRequestsEnabled = Lens.lens (\AwsWafv2VisibilityConfigDetails' {sampledRequestsEnabled} -> sampledRequestsEnabled) (\s@AwsWafv2VisibilityConfigDetails' {} a -> s {sampledRequestsEnabled = a} :: AwsWafv2VisibilityConfigDetails)

instance
  Data.FromJSON
    AwsWafv2VisibilityConfigDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafv2VisibilityConfigDetails"
      ( \x ->
          AwsWafv2VisibilityConfigDetails'
            Prelude.<$> (x Data..:? "CloudWatchMetricsEnabled")
            Prelude.<*> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "SampledRequestsEnabled")
      )

instance
  Prelude.Hashable
    AwsWafv2VisibilityConfigDetails
  where
  hashWithSalt
    _salt
    AwsWafv2VisibilityConfigDetails' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchMetricsEnabled
        `Prelude.hashWithSalt` metricName
        `Prelude.hashWithSalt` sampledRequestsEnabled

instance
  Prelude.NFData
    AwsWafv2VisibilityConfigDetails
  where
  rnf AwsWafv2VisibilityConfigDetails' {..} =
    Prelude.rnf cloudWatchMetricsEnabled
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf sampledRequestsEnabled

instance Data.ToJSON AwsWafv2VisibilityConfigDetails where
  toJSON AwsWafv2VisibilityConfigDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchMetricsEnabled" Data..=)
              Prelude.<$> cloudWatchMetricsEnabled,
            ("MetricName" Data..=) Prelude.<$> metricName,
            ("SampledRequestsEnabled" Data..=)
              Prelude.<$> sampledRequestsEnabled
          ]
      )
