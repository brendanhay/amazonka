{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.Types.LogsConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.LogsConfig where

import Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
import Network.AWS.CodeBuild.Types.S3LogsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about logs for a build project. These can be logs in Amazon
-- CloudWatch Logs, built in a specified S3 bucket, or both.
--
-- /See:/ 'newLogsConfig' smart constructor.
data LogsConfig = LogsConfig'
  { -- | Information about logs built to an S3 bucket for a build project. S3
    -- logs are not enabled by default.
    s3Logs :: Prelude.Maybe S3LogsConfig,
    -- | Information about Amazon CloudWatch Logs for a build project. Amazon
    -- CloudWatch Logs are enabled by default.
    cloudWatchLogs :: Prelude.Maybe CloudWatchLogsConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LogsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Logs', 'logsConfig_s3Logs' - Information about logs built to an S3 bucket for a build project. S3
-- logs are not enabled by default.
--
-- 'cloudWatchLogs', 'logsConfig_cloudWatchLogs' - Information about Amazon CloudWatch Logs for a build project. Amazon
-- CloudWatch Logs are enabled by default.
newLogsConfig ::
  LogsConfig
newLogsConfig =
  LogsConfig'
    { s3Logs = Prelude.Nothing,
      cloudWatchLogs = Prelude.Nothing
    }

-- | Information about logs built to an S3 bucket for a build project. S3
-- logs are not enabled by default.
logsConfig_s3Logs :: Lens.Lens' LogsConfig (Prelude.Maybe S3LogsConfig)
logsConfig_s3Logs = Lens.lens (\LogsConfig' {s3Logs} -> s3Logs) (\s@LogsConfig' {} a -> s {s3Logs = a} :: LogsConfig)

-- | Information about Amazon CloudWatch Logs for a build project. Amazon
-- CloudWatch Logs are enabled by default.
logsConfig_cloudWatchLogs :: Lens.Lens' LogsConfig (Prelude.Maybe CloudWatchLogsConfig)
logsConfig_cloudWatchLogs = Lens.lens (\LogsConfig' {cloudWatchLogs} -> cloudWatchLogs) (\s@LogsConfig' {} a -> s {cloudWatchLogs = a} :: LogsConfig)

instance Prelude.FromJSON LogsConfig where
  parseJSON =
    Prelude.withObject
      "LogsConfig"
      ( \x ->
          LogsConfig'
            Prelude.<$> (x Prelude..:? "s3Logs")
            Prelude.<*> (x Prelude..:? "cloudWatchLogs")
      )

instance Prelude.Hashable LogsConfig

instance Prelude.NFData LogsConfig

instance Prelude.ToJSON LogsConfig where
  toJSON LogsConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("s3Logs" Prelude..=) Prelude.<$> s3Logs,
            ("cloudWatchLogs" Prelude..=)
              Prelude.<$> cloudWatchLogs
          ]
      )
