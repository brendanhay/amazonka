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
-- Module      : Network.AWS.MwAA.Types.ModuleLoggingConfigurationInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MwAA.Types.ModuleLoggingConfigurationInput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MwAA.Types.LoggingLevel
import qualified Network.AWS.Prelude as Prelude

-- | Defines the type of logs to send for the Apache Airflow log type (e.g.
-- @DagProcessingLogs@). Valid values: @CloudWatchLogGroupArn@, @Enabled@,
-- @LogLevel@.
--
-- /See:/ 'newModuleLoggingConfigurationInput' smart constructor.
data ModuleLoggingConfigurationInput = ModuleLoggingConfigurationInput'
  { -- | Indicates whether to enable the Apache Airflow log type (e.g.
    -- @DagProcessingLogs@) in CloudWatch Logs.
    enabled :: Prelude.Bool,
    -- | Defines the Apache Airflow logs to send for the log type (e.g.
    -- @DagProcessingLogs@) to CloudWatch Logs. Valid values: @CRITICAL@,
    -- @ERROR@, @WARNING@, @INFO@.
    logLevel :: LoggingLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModuleLoggingConfigurationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'moduleLoggingConfigurationInput_enabled' - Indicates whether to enable the Apache Airflow log type (e.g.
-- @DagProcessingLogs@) in CloudWatch Logs.
--
-- 'logLevel', 'moduleLoggingConfigurationInput_logLevel' - Defines the Apache Airflow logs to send for the log type (e.g.
-- @DagProcessingLogs@) to CloudWatch Logs. Valid values: @CRITICAL@,
-- @ERROR@, @WARNING@, @INFO@.
newModuleLoggingConfigurationInput ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'logLevel'
  LoggingLevel ->
  ModuleLoggingConfigurationInput
newModuleLoggingConfigurationInput
  pEnabled_
  pLogLevel_ =
    ModuleLoggingConfigurationInput'
      { enabled =
          pEnabled_,
        logLevel = pLogLevel_
      }

-- | Indicates whether to enable the Apache Airflow log type (e.g.
-- @DagProcessingLogs@) in CloudWatch Logs.
moduleLoggingConfigurationInput_enabled :: Lens.Lens' ModuleLoggingConfigurationInput Prelude.Bool
moduleLoggingConfigurationInput_enabled = Lens.lens (\ModuleLoggingConfigurationInput' {enabled} -> enabled) (\s@ModuleLoggingConfigurationInput' {} a -> s {enabled = a} :: ModuleLoggingConfigurationInput)

-- | Defines the Apache Airflow logs to send for the log type (e.g.
-- @DagProcessingLogs@) to CloudWatch Logs. Valid values: @CRITICAL@,
-- @ERROR@, @WARNING@, @INFO@.
moduleLoggingConfigurationInput_logLevel :: Lens.Lens' ModuleLoggingConfigurationInput LoggingLevel
moduleLoggingConfigurationInput_logLevel = Lens.lens (\ModuleLoggingConfigurationInput' {logLevel} -> logLevel) (\s@ModuleLoggingConfigurationInput' {} a -> s {logLevel = a} :: ModuleLoggingConfigurationInput)

instance
  Prelude.Hashable
    ModuleLoggingConfigurationInput

instance
  Prelude.NFData
    ModuleLoggingConfigurationInput

instance Core.ToJSON ModuleLoggingConfigurationInput where
  toJSON ModuleLoggingConfigurationInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Enabled" Core..= enabled),
            Prelude.Just ("LogLevel" Core..= logLevel)
          ]
      )
