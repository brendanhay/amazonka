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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.MonitoringConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.MonitoringConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.ConfigurationType
import Amazonka.KinesisAnalyticsV2.Types.LogLevel
import Amazonka.KinesisAnalyticsV2.Types.MetricsLevel
import qualified Amazonka.Prelude as Prelude

-- | Describes updates to configuration parameters for Amazon CloudWatch
-- logging for an application.
--
-- /See:/ 'newMonitoringConfigurationUpdate' smart constructor.
data MonitoringConfigurationUpdate = MonitoringConfigurationUpdate'
  { -- | Describes updates to whether to use the default CloudWatch logging
    -- configuration for an application. You must set this property to @CUSTOM@
    -- in order to set the @LogLevel@ or @MetricsLevel@ parameters.
    configurationTypeUpdate :: Prelude.Maybe ConfigurationType,
    -- | Describes updates to the verbosity of the CloudWatch Logs for an
    -- application.
    logLevelUpdate :: Prelude.Maybe LogLevel,
    -- | Describes updates to the granularity of the CloudWatch Logs for an
    -- application. The @Parallelism@ level is not recommended for applications
    -- with a Parallelism over 64 due to excessive costs.
    metricsLevelUpdate :: Prelude.Maybe MetricsLevel
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
-- 'configurationTypeUpdate', 'monitoringConfigurationUpdate_configurationTypeUpdate' - Describes updates to whether to use the default CloudWatch logging
-- configuration for an application. You must set this property to @CUSTOM@
-- in order to set the @LogLevel@ or @MetricsLevel@ parameters.
--
-- 'logLevelUpdate', 'monitoringConfigurationUpdate_logLevelUpdate' - Describes updates to the verbosity of the CloudWatch Logs for an
-- application.
--
-- 'metricsLevelUpdate', 'monitoringConfigurationUpdate_metricsLevelUpdate' - Describes updates to the granularity of the CloudWatch Logs for an
-- application. The @Parallelism@ level is not recommended for applications
-- with a Parallelism over 64 due to excessive costs.
newMonitoringConfigurationUpdate ::
  MonitoringConfigurationUpdate
newMonitoringConfigurationUpdate =
  MonitoringConfigurationUpdate'
    { configurationTypeUpdate =
        Prelude.Nothing,
      logLevelUpdate = Prelude.Nothing,
      metricsLevelUpdate = Prelude.Nothing
    }

-- | Describes updates to whether to use the default CloudWatch logging
-- configuration for an application. You must set this property to @CUSTOM@
-- in order to set the @LogLevel@ or @MetricsLevel@ parameters.
monitoringConfigurationUpdate_configurationTypeUpdate :: Lens.Lens' MonitoringConfigurationUpdate (Prelude.Maybe ConfigurationType)
monitoringConfigurationUpdate_configurationTypeUpdate = Lens.lens (\MonitoringConfigurationUpdate' {configurationTypeUpdate} -> configurationTypeUpdate) (\s@MonitoringConfigurationUpdate' {} a -> s {configurationTypeUpdate = a} :: MonitoringConfigurationUpdate)

-- | Describes updates to the verbosity of the CloudWatch Logs for an
-- application.
monitoringConfigurationUpdate_logLevelUpdate :: Lens.Lens' MonitoringConfigurationUpdate (Prelude.Maybe LogLevel)
monitoringConfigurationUpdate_logLevelUpdate = Lens.lens (\MonitoringConfigurationUpdate' {logLevelUpdate} -> logLevelUpdate) (\s@MonitoringConfigurationUpdate' {} a -> s {logLevelUpdate = a} :: MonitoringConfigurationUpdate)

-- | Describes updates to the granularity of the CloudWatch Logs for an
-- application. The @Parallelism@ level is not recommended for applications
-- with a Parallelism over 64 due to excessive costs.
monitoringConfigurationUpdate_metricsLevelUpdate :: Lens.Lens' MonitoringConfigurationUpdate (Prelude.Maybe MetricsLevel)
monitoringConfigurationUpdate_metricsLevelUpdate = Lens.lens (\MonitoringConfigurationUpdate' {metricsLevelUpdate} -> metricsLevelUpdate) (\s@MonitoringConfigurationUpdate' {} a -> s {metricsLevelUpdate = a} :: MonitoringConfigurationUpdate)

instance
  Prelude.Hashable
    MonitoringConfigurationUpdate
  where
  hashWithSalt _salt MonitoringConfigurationUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` configurationTypeUpdate
      `Prelude.hashWithSalt` logLevelUpdate
      `Prelude.hashWithSalt` metricsLevelUpdate

instance Prelude.NFData MonitoringConfigurationUpdate where
  rnf MonitoringConfigurationUpdate' {..} =
    Prelude.rnf configurationTypeUpdate
      `Prelude.seq` Prelude.rnf logLevelUpdate
      `Prelude.seq` Prelude.rnf metricsLevelUpdate

instance Data.ToJSON MonitoringConfigurationUpdate where
  toJSON MonitoringConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigurationTypeUpdate" Data..=)
              Prelude.<$> configurationTypeUpdate,
            ("LogLevelUpdate" Data..=)
              Prelude.<$> logLevelUpdate,
            ("MetricsLevelUpdate" Data..=)
              Prelude.<$> metricsLevelUpdate
          ]
      )
