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
-- Module      : Amazonka.MwAA.Types.ModuleLoggingConfigurationInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.ModuleLoggingConfigurationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MwAA.Types.LoggingLevel
import qualified Amazonka.Prelude as Prelude

-- | Enables the Apache Airflow log type (e.g. @DagProcessingLogs@) and
-- defines the log level to send to CloudWatch Logs (e.g. @INFO@).
--
-- /See:/ 'newModuleLoggingConfigurationInput' smart constructor.
data ModuleLoggingConfigurationInput = ModuleLoggingConfigurationInput'
  { -- | Indicates whether to enable the Apache Airflow log type (e.g.
    -- @DagProcessingLogs@).
    enabled :: Prelude.Bool,
    -- | Defines the Apache Airflow log level (e.g. @INFO@) to send to CloudWatch
    -- Logs.
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
-- @DagProcessingLogs@).
--
-- 'logLevel', 'moduleLoggingConfigurationInput_logLevel' - Defines the Apache Airflow log level (e.g. @INFO@) to send to CloudWatch
-- Logs.
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
-- @DagProcessingLogs@).
moduleLoggingConfigurationInput_enabled :: Lens.Lens' ModuleLoggingConfigurationInput Prelude.Bool
moduleLoggingConfigurationInput_enabled = Lens.lens (\ModuleLoggingConfigurationInput' {enabled} -> enabled) (\s@ModuleLoggingConfigurationInput' {} a -> s {enabled = a} :: ModuleLoggingConfigurationInput)

-- | Defines the Apache Airflow log level (e.g. @INFO@) to send to CloudWatch
-- Logs.
moduleLoggingConfigurationInput_logLevel :: Lens.Lens' ModuleLoggingConfigurationInput LoggingLevel
moduleLoggingConfigurationInput_logLevel = Lens.lens (\ModuleLoggingConfigurationInput' {logLevel} -> logLevel) (\s@ModuleLoggingConfigurationInput' {} a -> s {logLevel = a} :: ModuleLoggingConfigurationInput)

instance
  Prelude.Hashable
    ModuleLoggingConfigurationInput
  where
  hashWithSalt
    _salt
    ModuleLoggingConfigurationInput' {..} =
      _salt `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` logLevel

instance
  Prelude.NFData
    ModuleLoggingConfigurationInput
  where
  rnf ModuleLoggingConfigurationInput' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf logLevel

instance Core.ToJSON ModuleLoggingConfigurationInput where
  toJSON ModuleLoggingConfigurationInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Enabled" Core..= enabled),
            Prelude.Just ("LogLevel" Core..= logLevel)
          ]
      )
