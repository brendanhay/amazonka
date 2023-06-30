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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.LoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MwAA.Types.ModuleLoggingConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the Apache Airflow log types that are published to CloudWatch
-- Logs.
--
-- /See:/ 'newLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { -- | The Airflow DAG processing logs published to CloudWatch Logs and the log
    -- level.
    dagProcessingLogs :: Prelude.Maybe ModuleLoggingConfiguration,
    -- | The Airflow scheduler logs published to CloudWatch Logs and the log
    -- level.
    schedulerLogs :: Prelude.Maybe ModuleLoggingConfiguration,
    -- | The Airflow task logs published to CloudWatch Logs and the log level.
    taskLogs :: Prelude.Maybe ModuleLoggingConfiguration,
    -- | The Airflow web server logs published to CloudWatch Logs and the log
    -- level.
    webserverLogs :: Prelude.Maybe ModuleLoggingConfiguration,
    -- | The Airflow worker logs published to CloudWatch Logs and the log level.
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
-- 'dagProcessingLogs', 'loggingConfiguration_dagProcessingLogs' - The Airflow DAG processing logs published to CloudWatch Logs and the log
-- level.
--
-- 'schedulerLogs', 'loggingConfiguration_schedulerLogs' - The Airflow scheduler logs published to CloudWatch Logs and the log
-- level.
--
-- 'taskLogs', 'loggingConfiguration_taskLogs' - The Airflow task logs published to CloudWatch Logs and the log level.
--
-- 'webserverLogs', 'loggingConfiguration_webserverLogs' - The Airflow web server logs published to CloudWatch Logs and the log
-- level.
--
-- 'workerLogs', 'loggingConfiguration_workerLogs' - The Airflow worker logs published to CloudWatch Logs and the log level.
newLoggingConfiguration ::
  LoggingConfiguration
newLoggingConfiguration =
  LoggingConfiguration'
    { dagProcessingLogs =
        Prelude.Nothing,
      schedulerLogs = Prelude.Nothing,
      taskLogs = Prelude.Nothing,
      webserverLogs = Prelude.Nothing,
      workerLogs = Prelude.Nothing
    }

-- | The Airflow DAG processing logs published to CloudWatch Logs and the log
-- level.
loggingConfiguration_dagProcessingLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe ModuleLoggingConfiguration)
loggingConfiguration_dagProcessingLogs = Lens.lens (\LoggingConfiguration' {dagProcessingLogs} -> dagProcessingLogs) (\s@LoggingConfiguration' {} a -> s {dagProcessingLogs = a} :: LoggingConfiguration)

-- | The Airflow scheduler logs published to CloudWatch Logs and the log
-- level.
loggingConfiguration_schedulerLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe ModuleLoggingConfiguration)
loggingConfiguration_schedulerLogs = Lens.lens (\LoggingConfiguration' {schedulerLogs} -> schedulerLogs) (\s@LoggingConfiguration' {} a -> s {schedulerLogs = a} :: LoggingConfiguration)

-- | The Airflow task logs published to CloudWatch Logs and the log level.
loggingConfiguration_taskLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe ModuleLoggingConfiguration)
loggingConfiguration_taskLogs = Lens.lens (\LoggingConfiguration' {taskLogs} -> taskLogs) (\s@LoggingConfiguration' {} a -> s {taskLogs = a} :: LoggingConfiguration)

-- | The Airflow web server logs published to CloudWatch Logs and the log
-- level.
loggingConfiguration_webserverLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe ModuleLoggingConfiguration)
loggingConfiguration_webserverLogs = Lens.lens (\LoggingConfiguration' {webserverLogs} -> webserverLogs) (\s@LoggingConfiguration' {} a -> s {webserverLogs = a} :: LoggingConfiguration)

-- | The Airflow worker logs published to CloudWatch Logs and the log level.
loggingConfiguration_workerLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe ModuleLoggingConfiguration)
loggingConfiguration_workerLogs = Lens.lens (\LoggingConfiguration' {workerLogs} -> workerLogs) (\s@LoggingConfiguration' {} a -> s {workerLogs = a} :: LoggingConfiguration)

instance Data.FromJSON LoggingConfiguration where
  parseJSON =
    Data.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Prelude.<$> (x Data..:? "DagProcessingLogs")
            Prelude.<*> (x Data..:? "SchedulerLogs")
            Prelude.<*> (x Data..:? "TaskLogs")
            Prelude.<*> (x Data..:? "WebserverLogs")
            Prelude.<*> (x Data..:? "WorkerLogs")
      )

instance Prelude.Hashable LoggingConfiguration where
  hashWithSalt _salt LoggingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` dagProcessingLogs
      `Prelude.hashWithSalt` schedulerLogs
      `Prelude.hashWithSalt` taskLogs
      `Prelude.hashWithSalt` webserverLogs
      `Prelude.hashWithSalt` workerLogs

instance Prelude.NFData LoggingConfiguration where
  rnf LoggingConfiguration' {..} =
    Prelude.rnf dagProcessingLogs
      `Prelude.seq` Prelude.rnf schedulerLogs
      `Prelude.seq` Prelude.rnf taskLogs
      `Prelude.seq` Prelude.rnf webserverLogs
      `Prelude.seq` Prelude.rnf workerLogs
