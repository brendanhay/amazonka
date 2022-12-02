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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.LoggingConfigurationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MwAA.Types.ModuleLoggingConfigurationInput
import qualified Amazonka.Prelude as Prelude

-- | Defines the Apache Airflow log types to send to CloudWatch Logs.
--
-- /See:/ 'newLoggingConfigurationInput' smart constructor.
data LoggingConfigurationInput = LoggingConfigurationInput'
  { -- | Publishes Airflow DAG processing logs to CloudWatch Logs.
    dagProcessingLogs :: Prelude.Maybe ModuleLoggingConfigurationInput,
    -- | Publishes Airflow task logs to CloudWatch Logs.
    taskLogs :: Prelude.Maybe ModuleLoggingConfigurationInput,
    -- | Publishes Airflow worker logs to CloudWatch Logs.
    workerLogs :: Prelude.Maybe ModuleLoggingConfigurationInput,
    -- | Publishes Airflow web server logs to CloudWatch Logs.
    webserverLogs :: Prelude.Maybe ModuleLoggingConfigurationInput,
    -- | Publishes Airflow scheduler logs to CloudWatch Logs.
    schedulerLogs :: Prelude.Maybe ModuleLoggingConfigurationInput
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
-- 'dagProcessingLogs', 'loggingConfigurationInput_dagProcessingLogs' - Publishes Airflow DAG processing logs to CloudWatch Logs.
--
-- 'taskLogs', 'loggingConfigurationInput_taskLogs' - Publishes Airflow task logs to CloudWatch Logs.
--
-- 'workerLogs', 'loggingConfigurationInput_workerLogs' - Publishes Airflow worker logs to CloudWatch Logs.
--
-- 'webserverLogs', 'loggingConfigurationInput_webserverLogs' - Publishes Airflow web server logs to CloudWatch Logs.
--
-- 'schedulerLogs', 'loggingConfigurationInput_schedulerLogs' - Publishes Airflow scheduler logs to CloudWatch Logs.
newLoggingConfigurationInput ::
  LoggingConfigurationInput
newLoggingConfigurationInput =
  LoggingConfigurationInput'
    { dagProcessingLogs =
        Prelude.Nothing,
      taskLogs = Prelude.Nothing,
      workerLogs = Prelude.Nothing,
      webserverLogs = Prelude.Nothing,
      schedulerLogs = Prelude.Nothing
    }

-- | Publishes Airflow DAG processing logs to CloudWatch Logs.
loggingConfigurationInput_dagProcessingLogs :: Lens.Lens' LoggingConfigurationInput (Prelude.Maybe ModuleLoggingConfigurationInput)
loggingConfigurationInput_dagProcessingLogs = Lens.lens (\LoggingConfigurationInput' {dagProcessingLogs} -> dagProcessingLogs) (\s@LoggingConfigurationInput' {} a -> s {dagProcessingLogs = a} :: LoggingConfigurationInput)

-- | Publishes Airflow task logs to CloudWatch Logs.
loggingConfigurationInput_taskLogs :: Lens.Lens' LoggingConfigurationInput (Prelude.Maybe ModuleLoggingConfigurationInput)
loggingConfigurationInput_taskLogs = Lens.lens (\LoggingConfigurationInput' {taskLogs} -> taskLogs) (\s@LoggingConfigurationInput' {} a -> s {taskLogs = a} :: LoggingConfigurationInput)

-- | Publishes Airflow worker logs to CloudWatch Logs.
loggingConfigurationInput_workerLogs :: Lens.Lens' LoggingConfigurationInput (Prelude.Maybe ModuleLoggingConfigurationInput)
loggingConfigurationInput_workerLogs = Lens.lens (\LoggingConfigurationInput' {workerLogs} -> workerLogs) (\s@LoggingConfigurationInput' {} a -> s {workerLogs = a} :: LoggingConfigurationInput)

-- | Publishes Airflow web server logs to CloudWatch Logs.
loggingConfigurationInput_webserverLogs :: Lens.Lens' LoggingConfigurationInput (Prelude.Maybe ModuleLoggingConfigurationInput)
loggingConfigurationInput_webserverLogs = Lens.lens (\LoggingConfigurationInput' {webserverLogs} -> webserverLogs) (\s@LoggingConfigurationInput' {} a -> s {webserverLogs = a} :: LoggingConfigurationInput)

-- | Publishes Airflow scheduler logs to CloudWatch Logs.
loggingConfigurationInput_schedulerLogs :: Lens.Lens' LoggingConfigurationInput (Prelude.Maybe ModuleLoggingConfigurationInput)
loggingConfigurationInput_schedulerLogs = Lens.lens (\LoggingConfigurationInput' {schedulerLogs} -> schedulerLogs) (\s@LoggingConfigurationInput' {} a -> s {schedulerLogs = a} :: LoggingConfigurationInput)

instance Prelude.Hashable LoggingConfigurationInput where
  hashWithSalt _salt LoggingConfigurationInput' {..} =
    _salt `Prelude.hashWithSalt` dagProcessingLogs
      `Prelude.hashWithSalt` taskLogs
      `Prelude.hashWithSalt` workerLogs
      `Prelude.hashWithSalt` webserverLogs
      `Prelude.hashWithSalt` schedulerLogs

instance Prelude.NFData LoggingConfigurationInput where
  rnf LoggingConfigurationInput' {..} =
    Prelude.rnf dagProcessingLogs
      `Prelude.seq` Prelude.rnf taskLogs
      `Prelude.seq` Prelude.rnf workerLogs
      `Prelude.seq` Prelude.rnf webserverLogs
      `Prelude.seq` Prelude.rnf schedulerLogs

instance Data.ToJSON LoggingConfigurationInput where
  toJSON LoggingConfigurationInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DagProcessingLogs" Data..=)
              Prelude.<$> dagProcessingLogs,
            ("TaskLogs" Data..=) Prelude.<$> taskLogs,
            ("WorkerLogs" Data..=) Prelude.<$> workerLogs,
            ("WebserverLogs" Data..=) Prelude.<$> webserverLogs,
            ("SchedulerLogs" Data..=) Prelude.<$> schedulerLogs
          ]
      )
