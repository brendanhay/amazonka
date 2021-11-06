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
-- Module      : Amazonka.MwAA.Types.LoggingConfigurationInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.LoggingConfigurationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MwAA.Types.ModuleLoggingConfigurationInput
import qualified Amazonka.Prelude as Prelude

-- | Defines the Apache Airflow logs to send to CloudWatch Logs:
-- @DagProcessingLogs@, @SchedulerLogs@, @TaskLogs@, @WebserverLogs@,
-- @WorkerLogs@.
--
-- /See:/ 'newLoggingConfigurationInput' smart constructor.
data LoggingConfigurationInput = LoggingConfigurationInput'
  { taskLogs :: Prelude.Maybe ModuleLoggingConfigurationInput,
    webserverLogs :: Prelude.Maybe ModuleLoggingConfigurationInput,
    schedulerLogs :: Prelude.Maybe ModuleLoggingConfigurationInput,
    dagProcessingLogs :: Prelude.Maybe ModuleLoggingConfigurationInput,
    workerLogs :: Prelude.Maybe ModuleLoggingConfigurationInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingConfigurationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskLogs', 'loggingConfigurationInput_taskLogs' - Undocumented member.
--
-- 'webserverLogs', 'loggingConfigurationInput_webserverLogs' - Undocumented member.
--
-- 'schedulerLogs', 'loggingConfigurationInput_schedulerLogs' - Undocumented member.
--
-- 'dagProcessingLogs', 'loggingConfigurationInput_dagProcessingLogs' - Undocumented member.
--
-- 'workerLogs', 'loggingConfigurationInput_workerLogs' - Undocumented member.
newLoggingConfigurationInput ::
  LoggingConfigurationInput
newLoggingConfigurationInput =
  LoggingConfigurationInput'
    { taskLogs =
        Prelude.Nothing,
      webserverLogs = Prelude.Nothing,
      schedulerLogs = Prelude.Nothing,
      dagProcessingLogs = Prelude.Nothing,
      workerLogs = Prelude.Nothing
    }

-- | Undocumented member.
loggingConfigurationInput_taskLogs :: Lens.Lens' LoggingConfigurationInput (Prelude.Maybe ModuleLoggingConfigurationInput)
loggingConfigurationInput_taskLogs = Lens.lens (\LoggingConfigurationInput' {taskLogs} -> taskLogs) (\s@LoggingConfigurationInput' {} a -> s {taskLogs = a} :: LoggingConfigurationInput)

-- | Undocumented member.
loggingConfigurationInput_webserverLogs :: Lens.Lens' LoggingConfigurationInput (Prelude.Maybe ModuleLoggingConfigurationInput)
loggingConfigurationInput_webserverLogs = Lens.lens (\LoggingConfigurationInput' {webserverLogs} -> webserverLogs) (\s@LoggingConfigurationInput' {} a -> s {webserverLogs = a} :: LoggingConfigurationInput)

-- | Undocumented member.
loggingConfigurationInput_schedulerLogs :: Lens.Lens' LoggingConfigurationInput (Prelude.Maybe ModuleLoggingConfigurationInput)
loggingConfigurationInput_schedulerLogs = Lens.lens (\LoggingConfigurationInput' {schedulerLogs} -> schedulerLogs) (\s@LoggingConfigurationInput' {} a -> s {schedulerLogs = a} :: LoggingConfigurationInput)

-- | Undocumented member.
loggingConfigurationInput_dagProcessingLogs :: Lens.Lens' LoggingConfigurationInput (Prelude.Maybe ModuleLoggingConfigurationInput)
loggingConfigurationInput_dagProcessingLogs = Lens.lens (\LoggingConfigurationInput' {dagProcessingLogs} -> dagProcessingLogs) (\s@LoggingConfigurationInput' {} a -> s {dagProcessingLogs = a} :: LoggingConfigurationInput)

-- | Undocumented member.
loggingConfigurationInput_workerLogs :: Lens.Lens' LoggingConfigurationInput (Prelude.Maybe ModuleLoggingConfigurationInput)
loggingConfigurationInput_workerLogs = Lens.lens (\LoggingConfigurationInput' {workerLogs} -> workerLogs) (\s@LoggingConfigurationInput' {} a -> s {workerLogs = a} :: LoggingConfigurationInput)

instance Prelude.Hashable LoggingConfigurationInput

instance Prelude.NFData LoggingConfigurationInput

instance Core.ToJSON LoggingConfigurationInput where
  toJSON LoggingConfigurationInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TaskLogs" Core..=) Prelude.<$> taskLogs,
            ("WebserverLogs" Core..=) Prelude.<$> webserverLogs,
            ("SchedulerLogs" Core..=) Prelude.<$> schedulerLogs,
            ("DagProcessingLogs" Core..=)
              Prelude.<$> dagProcessingLogs,
            ("WorkerLogs" Core..=) Prelude.<$> workerLogs
          ]
      )
