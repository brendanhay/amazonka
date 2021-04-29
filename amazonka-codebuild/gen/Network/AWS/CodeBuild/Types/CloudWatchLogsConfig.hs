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
-- Module      : Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CloudWatchLogsConfig where

import Network.AWS.CodeBuild.Types.LogsConfigStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about Amazon CloudWatch Logs for a build project.
--
-- /See:/ 'newCloudWatchLogsConfig' smart constructor.
data CloudWatchLogsConfig = CloudWatchLogsConfig'
  { -- | The group name of the logs in Amazon CloudWatch Logs. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams>.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The prefix of the stream name of the Amazon CloudWatch Logs. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams>.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the logs in Amazon CloudWatch Logs for a build
    -- project. Valid values are:
    --
    -- -   @ENABLED@: Amazon CloudWatch Logs are enabled for this build
    --     project.
    --
    -- -   @DISABLED@: Amazon CloudWatch Logs are not enabled for this build
    --     project.
    status :: LogsConfigStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'cloudWatchLogsConfig_groupName' - The group name of the logs in Amazon CloudWatch Logs. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams>.
--
-- 'streamName', 'cloudWatchLogsConfig_streamName' - The prefix of the stream name of the Amazon CloudWatch Logs. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams>.
--
-- 'status', 'cloudWatchLogsConfig_status' - The current status of the logs in Amazon CloudWatch Logs for a build
-- project. Valid values are:
--
-- -   @ENABLED@: Amazon CloudWatch Logs are enabled for this build
--     project.
--
-- -   @DISABLED@: Amazon CloudWatch Logs are not enabled for this build
--     project.
newCloudWatchLogsConfig ::
  -- | 'status'
  LogsConfigStatusType ->
  CloudWatchLogsConfig
newCloudWatchLogsConfig pStatus_ =
  CloudWatchLogsConfig'
    { groupName = Prelude.Nothing,
      streamName = Prelude.Nothing,
      status = pStatus_
    }

-- | The group name of the logs in Amazon CloudWatch Logs. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams>.
cloudWatchLogsConfig_groupName :: Lens.Lens' CloudWatchLogsConfig (Prelude.Maybe Prelude.Text)
cloudWatchLogsConfig_groupName = Lens.lens (\CloudWatchLogsConfig' {groupName} -> groupName) (\s@CloudWatchLogsConfig' {} a -> s {groupName = a} :: CloudWatchLogsConfig)

-- | The prefix of the stream name of the Amazon CloudWatch Logs. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams>.
cloudWatchLogsConfig_streamName :: Lens.Lens' CloudWatchLogsConfig (Prelude.Maybe Prelude.Text)
cloudWatchLogsConfig_streamName = Lens.lens (\CloudWatchLogsConfig' {streamName} -> streamName) (\s@CloudWatchLogsConfig' {} a -> s {streamName = a} :: CloudWatchLogsConfig)

-- | The current status of the logs in Amazon CloudWatch Logs for a build
-- project. Valid values are:
--
-- -   @ENABLED@: Amazon CloudWatch Logs are enabled for this build
--     project.
--
-- -   @DISABLED@: Amazon CloudWatch Logs are not enabled for this build
--     project.
cloudWatchLogsConfig_status :: Lens.Lens' CloudWatchLogsConfig LogsConfigStatusType
cloudWatchLogsConfig_status = Lens.lens (\CloudWatchLogsConfig' {status} -> status) (\s@CloudWatchLogsConfig' {} a -> s {status = a} :: CloudWatchLogsConfig)

instance Prelude.FromJSON CloudWatchLogsConfig where
  parseJSON =
    Prelude.withObject
      "CloudWatchLogsConfig"
      ( \x ->
          CloudWatchLogsConfig'
            Prelude.<$> (x Prelude..:? "groupName")
            Prelude.<*> (x Prelude..:? "streamName")
            Prelude.<*> (x Prelude..: "status")
      )

instance Prelude.Hashable CloudWatchLogsConfig

instance Prelude.NFData CloudWatchLogsConfig

instance Prelude.ToJSON CloudWatchLogsConfig where
  toJSON CloudWatchLogsConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("groupName" Prelude..=) Prelude.<$> groupName,
            ("streamName" Prelude..=) Prelude.<$> streamName,
            Prelude.Just ("status" Prelude..= status)
          ]
      )
