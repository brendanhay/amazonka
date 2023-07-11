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
-- Module      : Amazonka.MwAA.Types.ModuleLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.ModuleLoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MwAA.Types.LoggingLevel
import qualified Amazonka.Prelude as Prelude

-- | Describes the Apache Airflow log details for the log type (e.g.
-- @DagProcessingLogs@).
--
-- /See:/ 'newModuleLoggingConfiguration' smart constructor.
data ModuleLoggingConfiguration = ModuleLoggingConfiguration'
  { -- | The Amazon Resource Name (ARN) for the CloudWatch Logs group where the
    -- Apache Airflow log type (e.g. @DagProcessingLogs@) is published. For
    -- example,
    -- @arn:aws:logs:us-east-1:123456789012:log-group:airflow-MyMWAAEnvironment-MwaaEnvironment-DAGProcessing:*@.
    cloudWatchLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the Apache Airflow log type (e.g. @DagProcessingLogs@)
    -- is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The Apache Airflow log level for the log type (e.g.
    -- @DagProcessingLogs@).
    logLevel :: Prelude.Maybe LoggingLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModuleLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogGroupArn', 'moduleLoggingConfiguration_cloudWatchLogGroupArn' - The Amazon Resource Name (ARN) for the CloudWatch Logs group where the
-- Apache Airflow log type (e.g. @DagProcessingLogs@) is published. For
-- example,
-- @arn:aws:logs:us-east-1:123456789012:log-group:airflow-MyMWAAEnvironment-MwaaEnvironment-DAGProcessing:*@.
--
-- 'enabled', 'moduleLoggingConfiguration_enabled' - Indicates whether the Apache Airflow log type (e.g. @DagProcessingLogs@)
-- is enabled.
--
-- 'logLevel', 'moduleLoggingConfiguration_logLevel' - The Apache Airflow log level for the log type (e.g.
-- @DagProcessingLogs@).
newModuleLoggingConfiguration ::
  ModuleLoggingConfiguration
newModuleLoggingConfiguration =
  ModuleLoggingConfiguration'
    { cloudWatchLogGroupArn =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      logLevel = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the CloudWatch Logs group where the
-- Apache Airflow log type (e.g. @DagProcessingLogs@) is published. For
-- example,
-- @arn:aws:logs:us-east-1:123456789012:log-group:airflow-MyMWAAEnvironment-MwaaEnvironment-DAGProcessing:*@.
moduleLoggingConfiguration_cloudWatchLogGroupArn :: Lens.Lens' ModuleLoggingConfiguration (Prelude.Maybe Prelude.Text)
moduleLoggingConfiguration_cloudWatchLogGroupArn = Lens.lens (\ModuleLoggingConfiguration' {cloudWatchLogGroupArn} -> cloudWatchLogGroupArn) (\s@ModuleLoggingConfiguration' {} a -> s {cloudWatchLogGroupArn = a} :: ModuleLoggingConfiguration)

-- | Indicates whether the Apache Airflow log type (e.g. @DagProcessingLogs@)
-- is enabled.
moduleLoggingConfiguration_enabled :: Lens.Lens' ModuleLoggingConfiguration (Prelude.Maybe Prelude.Bool)
moduleLoggingConfiguration_enabled = Lens.lens (\ModuleLoggingConfiguration' {enabled} -> enabled) (\s@ModuleLoggingConfiguration' {} a -> s {enabled = a} :: ModuleLoggingConfiguration)

-- | The Apache Airflow log level for the log type (e.g.
-- @DagProcessingLogs@).
moduleLoggingConfiguration_logLevel :: Lens.Lens' ModuleLoggingConfiguration (Prelude.Maybe LoggingLevel)
moduleLoggingConfiguration_logLevel = Lens.lens (\ModuleLoggingConfiguration' {logLevel} -> logLevel) (\s@ModuleLoggingConfiguration' {} a -> s {logLevel = a} :: ModuleLoggingConfiguration)

instance Data.FromJSON ModuleLoggingConfiguration where
  parseJSON =
    Data.withObject
      "ModuleLoggingConfiguration"
      ( \x ->
          ModuleLoggingConfiguration'
            Prelude.<$> (x Data..:? "CloudWatchLogGroupArn")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "LogLevel")
      )

instance Prelude.Hashable ModuleLoggingConfiguration where
  hashWithSalt _salt ModuleLoggingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogGroupArn
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` logLevel

instance Prelude.NFData ModuleLoggingConfiguration where
  rnf ModuleLoggingConfiguration' {..} =
    Prelude.rnf cloudWatchLogGroupArn
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf logLevel
