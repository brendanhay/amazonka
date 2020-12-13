{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LogsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.LogsConfig
  ( LogsConfig (..),

    -- * Smart constructor
    mkLogsConfig,

    -- * Lenses
    lcS3Logs,
    lcCloudWatchLogs,
  )
where

import Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
import Network.AWS.CodeBuild.Types.S3LogsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about logs for a build project. These can be logs in Amazon CloudWatch Logs, built in a specified S3 bucket, or both.
--
-- /See:/ 'mkLogsConfig' smart constructor.
data LogsConfig = LogsConfig'
  { -- | Information about logs built to an S3 bucket for a build project. S3 logs are not enabled by default.
    s3Logs :: Lude.Maybe S3LogsConfig,
    -- | Information about Amazon CloudWatch Logs for a build project. Amazon CloudWatch Logs are enabled by default.
    cloudWatchLogs :: Lude.Maybe CloudWatchLogsConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogsConfig' with the minimum fields required to make a request.
--
-- * 's3Logs' - Information about logs built to an S3 bucket for a build project. S3 logs are not enabled by default.
-- * 'cloudWatchLogs' - Information about Amazon CloudWatch Logs for a build project. Amazon CloudWatch Logs are enabled by default.
mkLogsConfig ::
  LogsConfig
mkLogsConfig =
  LogsConfig' {s3Logs = Lude.Nothing, cloudWatchLogs = Lude.Nothing}

-- | Information about logs built to an S3 bucket for a build project. S3 logs are not enabled by default.
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcS3Logs :: Lens.Lens' LogsConfig (Lude.Maybe S3LogsConfig)
lcS3Logs = Lens.lens (s3Logs :: LogsConfig -> Lude.Maybe S3LogsConfig) (\s a -> s {s3Logs = a} :: LogsConfig)
{-# DEPRECATED lcS3Logs "Use generic-lens or generic-optics with 's3Logs' instead." #-}

-- | Information about Amazon CloudWatch Logs for a build project. Amazon CloudWatch Logs are enabled by default.
--
-- /Note:/ Consider using 'cloudWatchLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCloudWatchLogs :: Lens.Lens' LogsConfig (Lude.Maybe CloudWatchLogsConfig)
lcCloudWatchLogs = Lens.lens (cloudWatchLogs :: LogsConfig -> Lude.Maybe CloudWatchLogsConfig) (\s a -> s {cloudWatchLogs = a} :: LogsConfig)
{-# DEPRECATED lcCloudWatchLogs "Use generic-lens or generic-optics with 'cloudWatchLogs' instead." #-}

instance Lude.FromJSON LogsConfig where
  parseJSON =
    Lude.withObject
      "LogsConfig"
      ( \x ->
          LogsConfig'
            Lude.<$> (x Lude..:? "s3Logs") Lude.<*> (x Lude..:? "cloudWatchLogs")
      )

instance Lude.ToJSON LogsConfig where
  toJSON LogsConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("s3Logs" Lude..=) Lude.<$> s3Logs,
            ("cloudWatchLogs" Lude..=) Lude.<$> cloudWatchLogs
          ]
      )
