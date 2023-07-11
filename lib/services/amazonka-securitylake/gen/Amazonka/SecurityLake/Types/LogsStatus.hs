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
-- Module      : Amazonka.SecurityLake.Types.LogsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.LogsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.SourceStatus

-- | Retrieves the Logs status for the Amazon Security Lake account.
--
-- /See:/ 'newLogsStatus' smart constructor.
data LogsStatus = LogsStatus'
  { -- | The health status of services, including error codes and patterns.
    healthStatus :: SourceStatus,
    -- | Defines path the stored logs are available which has information on your
    -- systems, applications, and services.
    pathToLogs :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthStatus', 'logsStatus_healthStatus' - The health status of services, including error codes and patterns.
--
-- 'pathToLogs', 'logsStatus_pathToLogs' - Defines path the stored logs are available which has information on your
-- systems, applications, and services.
newLogsStatus ::
  -- | 'healthStatus'
  SourceStatus ->
  -- | 'pathToLogs'
  Prelude.Text ->
  LogsStatus
newLogsStatus pHealthStatus_ pPathToLogs_ =
  LogsStatus'
    { healthStatus = pHealthStatus_,
      pathToLogs = pPathToLogs_
    }

-- | The health status of services, including error codes and patterns.
logsStatus_healthStatus :: Lens.Lens' LogsStatus SourceStatus
logsStatus_healthStatus = Lens.lens (\LogsStatus' {healthStatus} -> healthStatus) (\s@LogsStatus' {} a -> s {healthStatus = a} :: LogsStatus)

-- | Defines path the stored logs are available which has information on your
-- systems, applications, and services.
logsStatus_pathToLogs :: Lens.Lens' LogsStatus Prelude.Text
logsStatus_pathToLogs = Lens.lens (\LogsStatus' {pathToLogs} -> pathToLogs) (\s@LogsStatus' {} a -> s {pathToLogs = a} :: LogsStatus)

instance Data.FromJSON LogsStatus where
  parseJSON =
    Data.withObject
      "LogsStatus"
      ( \x ->
          LogsStatus'
            Prelude.<$> (x Data..: "healthStatus")
            Prelude.<*> (x Data..: "pathToLogs")
      )

instance Prelude.Hashable LogsStatus where
  hashWithSalt _salt LogsStatus' {..} =
    _salt
      `Prelude.hashWithSalt` healthStatus
      `Prelude.hashWithSalt` pathToLogs

instance Prelude.NFData LogsStatus where
  rnf LogsStatus' {..} =
    Prelude.rnf healthStatus
      `Prelude.seq` Prelude.rnf pathToLogs
