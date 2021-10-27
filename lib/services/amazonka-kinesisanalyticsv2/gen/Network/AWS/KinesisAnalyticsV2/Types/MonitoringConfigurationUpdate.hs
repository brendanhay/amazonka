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
-- Module      : Network.AWS.KinesisAnalyticsV2.Types.MonitoringConfigurationUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalyticsV2.Types.MonitoringConfigurationUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalyticsV2.Types.ConfigurationType
import Network.AWS.KinesisAnalyticsV2.Types.LogLevel
import Network.AWS.KinesisAnalyticsV2.Types.MetricsLevel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes updates to configuration parameters for Amazon CloudWatch
-- logging for an application.
--
-- /See:/ 'newMonitoringConfigurationUpdate' smart constructor.
data MonitoringConfigurationUpdate = MonitoringConfigurationUpdate'
  { -- | Describes updates to the granularity of the CloudWatch Logs for an
    -- application. The @Parallelism@ level is not recommended for applications
    -- with a Parallelism over 64 due to excessive costs.
    metricsLevelUpdate :: Prelude.Maybe MetricsLevel,
    -- | Describes updates to the verbosity of the CloudWatch Logs for an
    -- application.
    logLevelUpdate :: Prelude.Maybe LogLevel,
    -- | Describes updates to whether to use the default CloudWatch logging
    -- configuration for an application. You must set this property to @CUSTOM@
    -- in order to set the @LogLevel@ or @MetricsLevel@ parameters.
    configurationTypeUpdate :: Prelude.Maybe ConfigurationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricsLevelUpdate', 'monitoringConfigurationUpdate_metricsLevelUpdate' - Describes updates to the granularity of the CloudWatch Logs for an
-- application. The @Parallelism@ level is not recommended for applications
-- with a Parallelism over 64 due to excessive costs.
--
-- 'logLevelUpdate', 'monitoringConfigurationUpdate_logLevelUpdate' - Describes updates to the verbosity of the CloudWatch Logs for an
-- application.
--
-- 'configurationTypeUpdate', 'monitoringConfigurationUpdate_configurationTypeUpdate' - Describes updates to whether to use the default CloudWatch logging
-- configuration for an application. You must set this property to @CUSTOM@
-- in order to set the @LogLevel@ or @MetricsLevel@ parameters.
newMonitoringConfigurationUpdate ::
  MonitoringConfigurationUpdate
newMonitoringConfigurationUpdate =
  MonitoringConfigurationUpdate'
    { metricsLevelUpdate =
        Prelude.Nothing,
      logLevelUpdate = Prelude.Nothing,
      configurationTypeUpdate = Prelude.Nothing
    }

-- | Describes updates to the granularity of the CloudWatch Logs for an
-- application. The @Parallelism@ level is not recommended for applications
-- with a Parallelism over 64 due to excessive costs.
monitoringConfigurationUpdate_metricsLevelUpdate :: Lens.Lens' MonitoringConfigurationUpdate (Prelude.Maybe MetricsLevel)
monitoringConfigurationUpdate_metricsLevelUpdate = Lens.lens (\MonitoringConfigurationUpdate' {metricsLevelUpdate} -> metricsLevelUpdate) (\s@MonitoringConfigurationUpdate' {} a -> s {metricsLevelUpdate = a} :: MonitoringConfigurationUpdate)

-- | Describes updates to the verbosity of the CloudWatch Logs for an
-- application.
monitoringConfigurationUpdate_logLevelUpdate :: Lens.Lens' MonitoringConfigurationUpdate (Prelude.Maybe LogLevel)
monitoringConfigurationUpdate_logLevelUpdate = Lens.lens (\MonitoringConfigurationUpdate' {logLevelUpdate} -> logLevelUpdate) (\s@MonitoringConfigurationUpdate' {} a -> s {logLevelUpdate = a} :: MonitoringConfigurationUpdate)

-- | Describes updates to whether to use the default CloudWatch logging
-- configuration for an application. You must set this property to @CUSTOM@
-- in order to set the @LogLevel@ or @MetricsLevel@ parameters.
monitoringConfigurationUpdate_configurationTypeUpdate :: Lens.Lens' MonitoringConfigurationUpdate (Prelude.Maybe ConfigurationType)
monitoringConfigurationUpdate_configurationTypeUpdate = Lens.lens (\MonitoringConfigurationUpdate' {configurationTypeUpdate} -> configurationTypeUpdate) (\s@MonitoringConfigurationUpdate' {} a -> s {configurationTypeUpdate = a} :: MonitoringConfigurationUpdate)

instance
  Prelude.Hashable
    MonitoringConfigurationUpdate

instance Prelude.NFData MonitoringConfigurationUpdate

instance Core.ToJSON MonitoringConfigurationUpdate where
  toJSON MonitoringConfigurationUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MetricsLevelUpdate" Core..=)
              Prelude.<$> metricsLevelUpdate,
            ("LogLevelUpdate" Core..=)
              Prelude.<$> logLevelUpdate,
            ("ConfigurationTypeUpdate" Core..=)
              Prelude.<$> configurationTypeUpdate
          ]
      )
