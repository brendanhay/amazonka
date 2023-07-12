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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.MetricSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.AppFlowConfig
import Amazonka.LookoutMetrics.Types.AthenaSourceConfig
import Amazonka.LookoutMetrics.Types.CloudWatchConfig
import Amazonka.LookoutMetrics.Types.RDSSourceConfig
import Amazonka.LookoutMetrics.Types.RedshiftSourceConfig
import Amazonka.LookoutMetrics.Types.S3SourceConfig
import qualified Amazonka.Prelude as Prelude

-- | Contains information about source data used to generate metrics.
--
-- /See:/ 'newMetricSource' smart constructor.
data MetricSource = MetricSource'
  { -- | Details about an AppFlow datasource.
    appFlowConfig :: Prelude.Maybe AppFlowConfig,
    -- | Details about an Amazon Athena datasource.
    athenaSourceConfig :: Prelude.Maybe AthenaSourceConfig,
    -- | Details about an Amazon CloudWatch monitoring datasource.
    cloudWatchConfig :: Prelude.Maybe CloudWatchConfig,
    -- | Details about an Amazon Relational Database Service (RDS) datasource.
    rDSSourceConfig :: Prelude.Maybe RDSSourceConfig,
    -- | Details about an Amazon Redshift database datasource.
    redshiftSourceConfig :: Prelude.Maybe RedshiftSourceConfig,
    s3SourceConfig :: Prelude.Maybe S3SourceConfig
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
-- 'appFlowConfig', 'metricSource_appFlowConfig' - Details about an AppFlow datasource.
--
-- 'athenaSourceConfig', 'metricSource_athenaSourceConfig' - Details about an Amazon Athena datasource.
--
-- 'cloudWatchConfig', 'metricSource_cloudWatchConfig' - Details about an Amazon CloudWatch monitoring datasource.
--
-- 'rDSSourceConfig', 'metricSource_rDSSourceConfig' - Details about an Amazon Relational Database Service (RDS) datasource.
--
-- 'redshiftSourceConfig', 'metricSource_redshiftSourceConfig' - Details about an Amazon Redshift database datasource.
--
-- 's3SourceConfig', 'metricSource_s3SourceConfig' - Undocumented member.
newMetricSource ::
  MetricSource
newMetricSource =
  MetricSource'
    { appFlowConfig = Prelude.Nothing,
      athenaSourceConfig = Prelude.Nothing,
      cloudWatchConfig = Prelude.Nothing,
      rDSSourceConfig = Prelude.Nothing,
      redshiftSourceConfig = Prelude.Nothing,
      s3SourceConfig = Prelude.Nothing
    }

-- | Details about an AppFlow datasource.
metricSource_appFlowConfig :: Lens.Lens' MetricSource (Prelude.Maybe AppFlowConfig)
metricSource_appFlowConfig = Lens.lens (\MetricSource' {appFlowConfig} -> appFlowConfig) (\s@MetricSource' {} a -> s {appFlowConfig = a} :: MetricSource)

-- | Details about an Amazon Athena datasource.
metricSource_athenaSourceConfig :: Lens.Lens' MetricSource (Prelude.Maybe AthenaSourceConfig)
metricSource_athenaSourceConfig = Lens.lens (\MetricSource' {athenaSourceConfig} -> athenaSourceConfig) (\s@MetricSource' {} a -> s {athenaSourceConfig = a} :: MetricSource)

-- | Details about an Amazon CloudWatch monitoring datasource.
metricSource_cloudWatchConfig :: Lens.Lens' MetricSource (Prelude.Maybe CloudWatchConfig)
metricSource_cloudWatchConfig = Lens.lens (\MetricSource' {cloudWatchConfig} -> cloudWatchConfig) (\s@MetricSource' {} a -> s {cloudWatchConfig = a} :: MetricSource)

-- | Details about an Amazon Relational Database Service (RDS) datasource.
metricSource_rDSSourceConfig :: Lens.Lens' MetricSource (Prelude.Maybe RDSSourceConfig)
metricSource_rDSSourceConfig = Lens.lens (\MetricSource' {rDSSourceConfig} -> rDSSourceConfig) (\s@MetricSource' {} a -> s {rDSSourceConfig = a} :: MetricSource)

-- | Details about an Amazon Redshift database datasource.
metricSource_redshiftSourceConfig :: Lens.Lens' MetricSource (Prelude.Maybe RedshiftSourceConfig)
metricSource_redshiftSourceConfig = Lens.lens (\MetricSource' {redshiftSourceConfig} -> redshiftSourceConfig) (\s@MetricSource' {} a -> s {redshiftSourceConfig = a} :: MetricSource)

-- | Undocumented member.
metricSource_s3SourceConfig :: Lens.Lens' MetricSource (Prelude.Maybe S3SourceConfig)
metricSource_s3SourceConfig = Lens.lens (\MetricSource' {s3SourceConfig} -> s3SourceConfig) (\s@MetricSource' {} a -> s {s3SourceConfig = a} :: MetricSource)

instance Data.FromJSON MetricSource where
  parseJSON =
    Data.withObject
      "MetricSource"
      ( \x ->
          MetricSource'
            Prelude.<$> (x Data..:? "AppFlowConfig")
            Prelude.<*> (x Data..:? "AthenaSourceConfig")
            Prelude.<*> (x Data..:? "CloudWatchConfig")
            Prelude.<*> (x Data..:? "RDSSourceConfig")
            Prelude.<*> (x Data..:? "RedshiftSourceConfig")
            Prelude.<*> (x Data..:? "S3SourceConfig")
      )

instance Prelude.Hashable MetricSource where
  hashWithSalt _salt MetricSource' {..} =
    _salt
      `Prelude.hashWithSalt` appFlowConfig
      `Prelude.hashWithSalt` athenaSourceConfig
      `Prelude.hashWithSalt` cloudWatchConfig
      `Prelude.hashWithSalt` rDSSourceConfig
      `Prelude.hashWithSalt` redshiftSourceConfig
      `Prelude.hashWithSalt` s3SourceConfig

instance Prelude.NFData MetricSource where
  rnf MetricSource' {..} =
    Prelude.rnf appFlowConfig
      `Prelude.seq` Prelude.rnf athenaSourceConfig
      `Prelude.seq` Prelude.rnf cloudWatchConfig
      `Prelude.seq` Prelude.rnf rDSSourceConfig
      `Prelude.seq` Prelude.rnf redshiftSourceConfig
      `Prelude.seq` Prelude.rnf s3SourceConfig

instance Data.ToJSON MetricSource where
  toJSON MetricSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppFlowConfig" Data..=) Prelude.<$> appFlowConfig,
            ("AthenaSourceConfig" Data..=)
              Prelude.<$> athenaSourceConfig,
            ("CloudWatchConfig" Data..=)
              Prelude.<$> cloudWatchConfig,
            ("RDSSourceConfig" Data..=)
              Prelude.<$> rDSSourceConfig,
            ("RedshiftSourceConfig" Data..=)
              Prelude.<$> redshiftSourceConfig,
            ("S3SourceConfig" Data..=)
              Prelude.<$> s3SourceConfig
          ]
      )
