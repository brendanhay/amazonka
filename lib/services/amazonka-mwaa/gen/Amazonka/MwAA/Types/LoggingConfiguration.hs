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
-- Module      : Amazonka.MwAA.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.LoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MwAA.Types.ModuleLoggingConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Defines the Apache Airflow logs to send to CloudWatch Logs:
-- @DagProcessingLogs@, @SchedulerLogs@, @TaskLogs@, @WebserverLogs@,
-- @WorkerLogs@.
--
-- /See:/ 'newLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { taskLogs :: Prelude.Maybe ModuleLoggingConfiguration,
    webserverLogs :: Prelude.Maybe ModuleLoggingConfiguration,
    schedulerLogs :: Prelude.Maybe ModuleLoggingConfiguration,
    dagProcessingLogs :: Prelude.Maybe ModuleLoggingConfiguration,
    workerLogs :: Prelude.Maybe ModuleLoggingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskLogs', 'loggingConfiguration_taskLogs' - Undocumented member.
--
-- 'webserverLogs', 'loggingConfiguration_webserverLogs' - Undocumented member.
--
-- 'schedulerLogs', 'loggingConfiguration_schedulerLogs' - Undocumented member.
--
-- 'dagProcessingLogs', 'loggingConfiguration_dagProcessingLogs' - Undocumented member.
--
-- 'workerLogs', 'loggingConfiguration_workerLogs' - Undocumented member.
newLoggingConfiguration ::
  LoggingConfiguration
newLoggingConfiguration =
  LoggingConfiguration'
    { taskLogs = Prelude.Nothing,
      webserverLogs = Prelude.Nothing,
      schedulerLogs = Prelude.Nothing,
      dagProcessingLogs = Prelude.Nothing,
      workerLogs = Prelude.Nothing
    }

-- | Undocumented member.
loggingConfiguration_taskLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe ModuleLoggingConfiguration)
loggingConfiguration_taskLogs = Lens.lens (\LoggingConfiguration' {taskLogs} -> taskLogs) (\s@LoggingConfiguration' {} a -> s {taskLogs = a} :: LoggingConfiguration)

-- | Undocumented member.
loggingConfiguration_webserverLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe ModuleLoggingConfiguration)
loggingConfiguration_webserverLogs = Lens.lens (\LoggingConfiguration' {webserverLogs} -> webserverLogs) (\s@LoggingConfiguration' {} a -> s {webserverLogs = a} :: LoggingConfiguration)

-- | Undocumented member.
loggingConfiguration_schedulerLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe ModuleLoggingConfiguration)
loggingConfiguration_schedulerLogs = Lens.lens (\LoggingConfiguration' {schedulerLogs} -> schedulerLogs) (\s@LoggingConfiguration' {} a -> s {schedulerLogs = a} :: LoggingConfiguration)

-- | Undocumented member.
loggingConfiguration_dagProcessingLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe ModuleLoggingConfiguration)
loggingConfiguration_dagProcessingLogs = Lens.lens (\LoggingConfiguration' {dagProcessingLogs} -> dagProcessingLogs) (\s@LoggingConfiguration' {} a -> s {dagProcessingLogs = a} :: LoggingConfiguration)

-- | Undocumented member.
loggingConfiguration_workerLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe ModuleLoggingConfiguration)
loggingConfiguration_workerLogs = Lens.lens (\LoggingConfiguration' {workerLogs} -> workerLogs) (\s@LoggingConfiguration' {} a -> s {workerLogs = a} :: LoggingConfiguration)

instance Core.FromJSON LoggingConfiguration where
  parseJSON =
    Core.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Prelude.<$> (x Core..:? "TaskLogs")
            Prelude.<*> (x Core..:? "WebserverLogs")
            Prelude.<*> (x Core..:? "SchedulerLogs")
            Prelude.<*> (x Core..:? "DagProcessingLogs")
            Prelude.<*> (x Core..:? "WorkerLogs")
      )

instance Prelude.Hashable LoggingConfiguration where
  hashWithSalt salt' LoggingConfiguration' {..} =
    salt' `Prelude.hashWithSalt` workerLogs
      `Prelude.hashWithSalt` dagProcessingLogs
      `Prelude.hashWithSalt` schedulerLogs
      `Prelude.hashWithSalt` webserverLogs
      `Prelude.hashWithSalt` taskLogs

instance Prelude.NFData LoggingConfiguration where
  rnf LoggingConfiguration' {..} =
    Prelude.rnf taskLogs
      `Prelude.seq` Prelude.rnf workerLogs
      `Prelude.seq` Prelude.rnf dagProcessingLogs
      `Prelude.seq` Prelude.rnf schedulerLogs
      `Prelude.seq` Prelude.rnf webserverLogs
