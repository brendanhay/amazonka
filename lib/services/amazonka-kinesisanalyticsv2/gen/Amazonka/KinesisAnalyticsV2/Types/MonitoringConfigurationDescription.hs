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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.MonitoringConfigurationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.MonitoringConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.ConfigurationType
import Amazonka.KinesisAnalyticsV2.Types.LogLevel
import Amazonka.KinesisAnalyticsV2.Types.MetricsLevel
import qualified Amazonka.Prelude as Prelude

-- | Describes configuration parameters for CloudWatch logging for an
-- application.
--
-- /See:/ 'newMonitoringConfigurationDescription' smart constructor.
data MonitoringConfigurationDescription = MonitoringConfigurationDescription'
  { -- | Describes the verbosity of the CloudWatch Logs for an application.
    logLevel :: Prelude.Maybe LogLevel,
    -- | Describes the granularity of the CloudWatch Logs for an application.
    metricsLevel :: Prelude.Maybe MetricsLevel,
    -- | Describes whether to use the default CloudWatch logging configuration
    -- for an application.
    configurationType :: Prelude.Maybe ConfigurationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logLevel', 'monitoringConfigurationDescription_logLevel' - Describes the verbosity of the CloudWatch Logs for an application.
--
-- 'metricsLevel', 'monitoringConfigurationDescription_metricsLevel' - Describes the granularity of the CloudWatch Logs for an application.
--
-- 'configurationType', 'monitoringConfigurationDescription_configurationType' - Describes whether to use the default CloudWatch logging configuration
-- for an application.
newMonitoringConfigurationDescription ::
  MonitoringConfigurationDescription
newMonitoringConfigurationDescription =
  MonitoringConfigurationDescription'
    { logLevel =
        Prelude.Nothing,
      metricsLevel = Prelude.Nothing,
      configurationType = Prelude.Nothing
    }

-- | Describes the verbosity of the CloudWatch Logs for an application.
monitoringConfigurationDescription_logLevel :: Lens.Lens' MonitoringConfigurationDescription (Prelude.Maybe LogLevel)
monitoringConfigurationDescription_logLevel = Lens.lens (\MonitoringConfigurationDescription' {logLevel} -> logLevel) (\s@MonitoringConfigurationDescription' {} a -> s {logLevel = a} :: MonitoringConfigurationDescription)

-- | Describes the granularity of the CloudWatch Logs for an application.
monitoringConfigurationDescription_metricsLevel :: Lens.Lens' MonitoringConfigurationDescription (Prelude.Maybe MetricsLevel)
monitoringConfigurationDescription_metricsLevel = Lens.lens (\MonitoringConfigurationDescription' {metricsLevel} -> metricsLevel) (\s@MonitoringConfigurationDescription' {} a -> s {metricsLevel = a} :: MonitoringConfigurationDescription)

-- | Describes whether to use the default CloudWatch logging configuration
-- for an application.
monitoringConfigurationDescription_configurationType :: Lens.Lens' MonitoringConfigurationDescription (Prelude.Maybe ConfigurationType)
monitoringConfigurationDescription_configurationType = Lens.lens (\MonitoringConfigurationDescription' {configurationType} -> configurationType) (\s@MonitoringConfigurationDescription' {} a -> s {configurationType = a} :: MonitoringConfigurationDescription)

instance
  Core.FromJSON
    MonitoringConfigurationDescription
  where
  parseJSON =
    Core.withObject
      "MonitoringConfigurationDescription"
      ( \x ->
          MonitoringConfigurationDescription'
            Prelude.<$> (x Core..:? "LogLevel")
            Prelude.<*> (x Core..:? "MetricsLevel")
            Prelude.<*> (x Core..:? "ConfigurationType")
      )

instance
  Prelude.Hashable
    MonitoringConfigurationDescription
  where
  hashWithSalt
    _salt
    MonitoringConfigurationDescription' {..} =
      _salt `Prelude.hashWithSalt` logLevel
        `Prelude.hashWithSalt` metricsLevel
        `Prelude.hashWithSalt` configurationType

instance
  Prelude.NFData
    MonitoringConfigurationDescription
  where
  rnf MonitoringConfigurationDescription' {..} =
    Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf metricsLevel
      `Prelude.seq` Prelude.rnf configurationType
