{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
  ( CloudWatchLogsConfig (..),

    -- * Smart constructor
    mkCloudWatchLogsConfig,

    -- * Lenses
    cwlcStatus,
    cwlcGroupName,
    cwlcStreamName,
  )
where

import Network.AWS.CodeBuild.Types.LogsConfigStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about Amazon CloudWatch Logs for a build project.
--
-- /See:/ 'mkCloudWatchLogsConfig' smart constructor.
data CloudWatchLogsConfig = CloudWatchLogsConfig'
  { -- | The current status of the logs in Amazon CloudWatch Logs for a build project. Valid values are:
    --
    --
    --     * @ENABLED@ : Amazon CloudWatch Logs are enabled for this build project.
    --
    --
    --     * @DISABLED@ : Amazon CloudWatch Logs are not enabled for this build project.
    status :: LogsConfigStatusType,
    -- | The group name of the logs in Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> .
    groupName :: Lude.Maybe Lude.Text,
    -- | The prefix of the stream name of the Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> .
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchLogsConfig' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the logs in Amazon CloudWatch Logs for a build project. Valid values are:
--
--
--     * @ENABLED@ : Amazon CloudWatch Logs are enabled for this build project.
--
--
--     * @DISABLED@ : Amazon CloudWatch Logs are not enabled for this build project.
--
--
-- * 'groupName' - The group name of the logs in Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> .
-- * 'streamName' - The prefix of the stream name of the Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> .
mkCloudWatchLogsConfig ::
  -- | 'status'
  LogsConfigStatusType ->
  CloudWatchLogsConfig
mkCloudWatchLogsConfig pStatus_ =
  CloudWatchLogsConfig'
    { status = pStatus_,
      groupName = Lude.Nothing,
      streamName = Lude.Nothing
    }

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
cwlcStatus :: Lens.Lens' CloudWatchLogsConfig LogsConfigStatusType
cwlcStatus = Lens.lens (status :: CloudWatchLogsConfig -> LogsConfigStatusType) (\s a -> s {status = a} :: CloudWatchLogsConfig)
{-# DEPRECATED cwlcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The group name of the logs in Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> .
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlcGroupName :: Lens.Lens' CloudWatchLogsConfig (Lude.Maybe Lude.Text)
cwlcGroupName = Lens.lens (groupName :: CloudWatchLogsConfig -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: CloudWatchLogsConfig)
{-# DEPRECATED cwlcGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The prefix of the stream name of the Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> .
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlcStreamName :: Lens.Lens' CloudWatchLogsConfig (Lude.Maybe Lude.Text)
cwlcStreamName = Lens.lens (streamName :: CloudWatchLogsConfig -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: CloudWatchLogsConfig)
{-# DEPRECATED cwlcStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.FromJSON CloudWatchLogsConfig where
  parseJSON =
    Lude.withObject
      "CloudWatchLogsConfig"
      ( \x ->
          CloudWatchLogsConfig'
            Lude.<$> (x Lude..: "status")
            Lude.<*> (x Lude..:? "groupName")
            Lude.<*> (x Lude..:? "streamName")
      )

instance Lude.ToJSON CloudWatchLogsConfig where
  toJSON CloudWatchLogsConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("status" Lude..= status),
            ("groupName" Lude..=) Lude.<$> groupName,
            ("streamName" Lude..=) Lude.<$> streamName
          ]
      )
