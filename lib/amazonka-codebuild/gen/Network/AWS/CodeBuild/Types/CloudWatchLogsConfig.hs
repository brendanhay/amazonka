{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
  ( CloudWatchLogsConfig (..)
  -- * Smart constructor
  , mkCloudWatchLogsConfig
  -- * Lenses
  , cwlcStatus
  , cwlcGroupName
  , cwlcStreamName
  ) where

import qualified Network.AWS.CodeBuild.Types.LogsConfigStatusType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about Amazon CloudWatch Logs for a build project. 
--
-- /See:/ 'mkCloudWatchLogsConfig' smart constructor.
data CloudWatchLogsConfig = CloudWatchLogsConfig'
  { status :: Types.LogsConfigStatusType
    -- ^ The current status of the logs in Amazon CloudWatch Logs for a build project. Valid values are:
--
--
--     * @ENABLED@ : Amazon CloudWatch Logs are enabled for this build project.
--
--
--     * @DISABLED@ : Amazon CloudWatch Logs are not enabled for this build project.
--
--
  , groupName :: Core.Maybe Core.Text
    -- ^ The group name of the logs in Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> . 
  , streamName :: Core.Maybe Core.Text
    -- ^ The prefix of the stream name of the Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchLogsConfig' value with any optional fields omitted.
mkCloudWatchLogsConfig
    :: Types.LogsConfigStatusType -- ^ 'status'
    -> CloudWatchLogsConfig
mkCloudWatchLogsConfig status
  = CloudWatchLogsConfig'{status, groupName = Core.Nothing,
                          streamName = Core.Nothing}

-- | The current status of the logs in Amazon CloudWatch Logs for a build project. Valid values are:
--
--
--     * @ENABLED@ : Amazon CloudWatch Logs are enabled for this build project.
--
--
--     * @DISABLED@ : Amazon CloudWatch Logs are not enabled for this build project.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlcStatus :: Lens.Lens' CloudWatchLogsConfig Types.LogsConfigStatusType
cwlcStatus = Lens.field @"status"
{-# INLINEABLE cwlcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The group name of the logs in Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> . 
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlcGroupName :: Lens.Lens' CloudWatchLogsConfig (Core.Maybe Core.Text)
cwlcGroupName = Lens.field @"groupName"
{-# INLINEABLE cwlcGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The prefix of the stream name of the Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> . 
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlcStreamName :: Lens.Lens' CloudWatchLogsConfig (Core.Maybe Core.Text)
cwlcStreamName = Lens.field @"streamName"
{-# INLINEABLE cwlcStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

instance Core.FromJSON CloudWatchLogsConfig where
        toJSON CloudWatchLogsConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("status" Core..= status),
                  ("groupName" Core..=) Core.<$> groupName,
                  ("streamName" Core..=) Core.<$> streamName])

instance Core.FromJSON CloudWatchLogsConfig where
        parseJSON
          = Core.withObject "CloudWatchLogsConfig" Core.$
              \ x ->
                CloudWatchLogsConfig' Core.<$>
                  (x Core..: "status") Core.<*> x Core..:? "groupName" Core.<*>
                    x Core..:? "streamName"
