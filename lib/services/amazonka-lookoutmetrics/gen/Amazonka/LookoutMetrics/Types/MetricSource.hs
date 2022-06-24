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
-- Module      : Amazonka.LookoutMetrics.Types.MetricSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.MetricSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutMetrics.Types.AppFlowConfig
import Amazonka.LookoutMetrics.Types.CloudWatchConfig
import Amazonka.LookoutMetrics.Types.RDSSourceConfig
import Amazonka.LookoutMetrics.Types.RedshiftSourceConfig
import Amazonka.LookoutMetrics.Types.S3SourceConfig
import qualified Amazonka.Prelude as Prelude

-- | Contains information about source data used to generate a metric.
--
-- /See:/ 'newMetricSource' smart constructor.
data MetricSource = MetricSource'
  { s3SourceConfig :: Prelude.Maybe S3SourceConfig,
    -- | An object containing information about the Amazon CloudWatch monitoring
    -- configuration.
    cloudWatchConfig :: Prelude.Maybe CloudWatchConfig,
    -- | An object containing information about the AppFlow configuration.
    appFlowConfig :: Prelude.Maybe AppFlowConfig,
    -- | An object containing information about the Amazon Relational Database
    -- Service (RDS) configuration.
    rDSSourceConfig :: Prelude.Maybe RDSSourceConfig,
    -- | An object containing information about the Amazon Redshift database
    -- configuration.
    redshiftSourceConfig :: Prelude.Maybe RedshiftSourceConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3SourceConfig', 'metricSource_s3SourceConfig' - Undocumented member.
--
-- 'cloudWatchConfig', 'metricSource_cloudWatchConfig' - An object containing information about the Amazon CloudWatch monitoring
-- configuration.
--
-- 'appFlowConfig', 'metricSource_appFlowConfig' - An object containing information about the AppFlow configuration.
--
-- 'rDSSourceConfig', 'metricSource_rDSSourceConfig' - An object containing information about the Amazon Relational Database
-- Service (RDS) configuration.
--
-- 'redshiftSourceConfig', 'metricSource_redshiftSourceConfig' - An object containing information about the Amazon Redshift database
-- configuration.
newMetricSource ::
  MetricSource
newMetricSource =
  MetricSource'
    { s3SourceConfig = Prelude.Nothing,
      cloudWatchConfig = Prelude.Nothing,
      appFlowConfig = Prelude.Nothing,
      rDSSourceConfig = Prelude.Nothing,
      redshiftSourceConfig = Prelude.Nothing
    }

-- | Undocumented member.
metricSource_s3SourceConfig :: Lens.Lens' MetricSource (Prelude.Maybe S3SourceConfig)
metricSource_s3SourceConfig = Lens.lens (\MetricSource' {s3SourceConfig} -> s3SourceConfig) (\s@MetricSource' {} a -> s {s3SourceConfig = a} :: MetricSource)

-- | An object containing information about the Amazon CloudWatch monitoring
-- configuration.
metricSource_cloudWatchConfig :: Lens.Lens' MetricSource (Prelude.Maybe CloudWatchConfig)
metricSource_cloudWatchConfig = Lens.lens (\MetricSource' {cloudWatchConfig} -> cloudWatchConfig) (\s@MetricSource' {} a -> s {cloudWatchConfig = a} :: MetricSource)

-- | An object containing information about the AppFlow configuration.
metricSource_appFlowConfig :: Lens.Lens' MetricSource (Prelude.Maybe AppFlowConfig)
metricSource_appFlowConfig = Lens.lens (\MetricSource' {appFlowConfig} -> appFlowConfig) (\s@MetricSource' {} a -> s {appFlowConfig = a} :: MetricSource)

-- | An object containing information about the Amazon Relational Database
-- Service (RDS) configuration.
metricSource_rDSSourceConfig :: Lens.Lens' MetricSource (Prelude.Maybe RDSSourceConfig)
metricSource_rDSSourceConfig = Lens.lens (\MetricSource' {rDSSourceConfig} -> rDSSourceConfig) (\s@MetricSource' {} a -> s {rDSSourceConfig = a} :: MetricSource)

-- | An object containing information about the Amazon Redshift database
-- configuration.
metricSource_redshiftSourceConfig :: Lens.Lens' MetricSource (Prelude.Maybe RedshiftSourceConfig)
metricSource_redshiftSourceConfig = Lens.lens (\MetricSource' {redshiftSourceConfig} -> redshiftSourceConfig) (\s@MetricSource' {} a -> s {redshiftSourceConfig = a} :: MetricSource)

instance Core.FromJSON MetricSource where
  parseJSON =
    Core.withObject
      "MetricSource"
      ( \x ->
          MetricSource'
            Prelude.<$> (x Core..:? "S3SourceConfig")
            Prelude.<*> (x Core..:? "CloudWatchConfig")
            Prelude.<*> (x Core..:? "AppFlowConfig")
            Prelude.<*> (x Core..:? "RDSSourceConfig")
            Prelude.<*> (x Core..:? "RedshiftSourceConfig")
      )

instance Prelude.Hashable MetricSource where
  hashWithSalt _salt MetricSource' {..} =
    _salt `Prelude.hashWithSalt` s3SourceConfig
      `Prelude.hashWithSalt` cloudWatchConfig
      `Prelude.hashWithSalt` appFlowConfig
      `Prelude.hashWithSalt` rDSSourceConfig
      `Prelude.hashWithSalt` redshiftSourceConfig

instance Prelude.NFData MetricSource where
  rnf MetricSource' {..} =
    Prelude.rnf s3SourceConfig
      `Prelude.seq` Prelude.rnf cloudWatchConfig
      `Prelude.seq` Prelude.rnf appFlowConfig
      `Prelude.seq` Prelude.rnf rDSSourceConfig
      `Prelude.seq` Prelude.rnf redshiftSourceConfig

instance Core.ToJSON MetricSource where
  toJSON MetricSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3SourceConfig" Core..=)
              Prelude.<$> s3SourceConfig,
            ("CloudWatchConfig" Core..=)
              Prelude.<$> cloudWatchConfig,
            ("AppFlowConfig" Core..=) Prelude.<$> appFlowConfig,
            ("RDSSourceConfig" Core..=)
              Prelude.<$> rDSSourceConfig,
            ("RedshiftSourceConfig" Core..=)
              Prelude.<$> redshiftSourceConfig
          ]
      )
