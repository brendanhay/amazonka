{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LogsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.LogsConfig
  ( LogsConfig (..)
  -- * Smart constructor
  , mkLogsConfig
  -- * Lenses
  , lcCloudWatchLogs
  , lcS3Logs
  ) where

import qualified Network.AWS.CodeBuild.Types.CloudWatchLogsConfig as Types
import qualified Network.AWS.CodeBuild.Types.S3LogsConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about logs for a build project. These can be logs in Amazon CloudWatch Logs, built in a specified S3 bucket, or both. 
--
-- /See:/ 'mkLogsConfig' smart constructor.
data LogsConfig = LogsConfig'
  { cloudWatchLogs :: Core.Maybe Types.CloudWatchLogsConfig
    -- ^ Information about Amazon CloudWatch Logs for a build project. Amazon CloudWatch Logs are enabled by default. 
  , s3Logs :: Core.Maybe Types.S3LogsConfig
    -- ^ Information about logs built to an S3 bucket for a build project. S3 logs are not enabled by default. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogsConfig' value with any optional fields omitted.
mkLogsConfig
    :: LogsConfig
mkLogsConfig
  = LogsConfig'{cloudWatchLogs = Core.Nothing, s3Logs = Core.Nothing}

-- | Information about Amazon CloudWatch Logs for a build project. Amazon CloudWatch Logs are enabled by default. 
--
-- /Note:/ Consider using 'cloudWatchLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCloudWatchLogs :: Lens.Lens' LogsConfig (Core.Maybe Types.CloudWatchLogsConfig)
lcCloudWatchLogs = Lens.field @"cloudWatchLogs"
{-# INLINEABLE lcCloudWatchLogs #-}
{-# DEPRECATED cloudWatchLogs "Use generic-lens or generic-optics with 'cloudWatchLogs' instead"  #-}

-- | Information about logs built to an S3 bucket for a build project. S3 logs are not enabled by default. 
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcS3Logs :: Lens.Lens' LogsConfig (Core.Maybe Types.S3LogsConfig)
lcS3Logs = Lens.field @"s3Logs"
{-# INLINEABLE lcS3Logs #-}
{-# DEPRECATED s3Logs "Use generic-lens or generic-optics with 's3Logs' instead"  #-}

instance Core.FromJSON LogsConfig where
        toJSON LogsConfig{..}
          = Core.object
              (Core.catMaybes
                 [("cloudWatchLogs" Core..=) Core.<$> cloudWatchLogs,
                  ("s3Logs" Core..=) Core.<$> s3Logs])

instance Core.FromJSON LogsConfig where
        parseJSON
          = Core.withObject "LogsConfig" Core.$
              \ x ->
                LogsConfig' Core.<$>
                  (x Core..:? "cloudWatchLogs") Core.<*> x Core..:? "s3Logs"
