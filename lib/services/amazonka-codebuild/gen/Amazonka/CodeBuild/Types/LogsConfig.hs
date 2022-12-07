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
-- Module      : Amazonka.CodeBuild.Types.LogsConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.LogsConfig where

import Amazonka.CodeBuild.Types.CloudWatchLogsConfig
import Amazonka.CodeBuild.Types.S3LogsConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about logs for a build project. These can be logs in
-- CloudWatch Logs, built in a specified S3 bucket, or both.
--
-- /See:/ 'newLogsConfig' smart constructor.
data LogsConfig = LogsConfig'
  { -- | Information about CloudWatch Logs for a build project. CloudWatch Logs
    -- are enabled by default.
    cloudWatchLogs :: Prelude.Maybe CloudWatchLogsConfig,
    -- | Information about logs built to an S3 bucket for a build project. S3
    -- logs are not enabled by default.
    s3Logs :: Prelude.Maybe S3LogsConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogs', 'logsConfig_cloudWatchLogs' - Information about CloudWatch Logs for a build project. CloudWatch Logs
-- are enabled by default.
--
-- 's3Logs', 'logsConfig_s3Logs' - Information about logs built to an S3 bucket for a build project. S3
-- logs are not enabled by default.
newLogsConfig ::
  LogsConfig
newLogsConfig =
  LogsConfig'
    { cloudWatchLogs = Prelude.Nothing,
      s3Logs = Prelude.Nothing
    }

-- | Information about CloudWatch Logs for a build project. CloudWatch Logs
-- are enabled by default.
logsConfig_cloudWatchLogs :: Lens.Lens' LogsConfig (Prelude.Maybe CloudWatchLogsConfig)
logsConfig_cloudWatchLogs = Lens.lens (\LogsConfig' {cloudWatchLogs} -> cloudWatchLogs) (\s@LogsConfig' {} a -> s {cloudWatchLogs = a} :: LogsConfig)

-- | Information about logs built to an S3 bucket for a build project. S3
-- logs are not enabled by default.
logsConfig_s3Logs :: Lens.Lens' LogsConfig (Prelude.Maybe S3LogsConfig)
logsConfig_s3Logs = Lens.lens (\LogsConfig' {s3Logs} -> s3Logs) (\s@LogsConfig' {} a -> s {s3Logs = a} :: LogsConfig)

instance Data.FromJSON LogsConfig where
  parseJSON =
    Data.withObject
      "LogsConfig"
      ( \x ->
          LogsConfig'
            Prelude.<$> (x Data..:? "cloudWatchLogs")
            Prelude.<*> (x Data..:? "s3Logs")
      )

instance Prelude.Hashable LogsConfig where
  hashWithSalt _salt LogsConfig' {..} =
    _salt `Prelude.hashWithSalt` cloudWatchLogs
      `Prelude.hashWithSalt` s3Logs

instance Prelude.NFData LogsConfig where
  rnf LogsConfig' {..} =
    Prelude.rnf cloudWatchLogs
      `Prelude.seq` Prelude.rnf s3Logs

instance Data.ToJSON LogsConfig where
  toJSON LogsConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cloudWatchLogs" Data..=)
              Prelude.<$> cloudWatchLogs,
            ("s3Logs" Data..=) Prelude.<$> s3Logs
          ]
      )
