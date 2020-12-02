{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CloudWatchLogsConfig where

import Network.AWS.CodeBuild.Types.LogsConfigStatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about Amazon CloudWatch Logs for a build project.
--
--
--
-- /See:/ 'cloudWatchLogsConfig' smart constructor.
data CloudWatchLogsConfig = CloudWatchLogsConfig'
  { _cwlcGroupName ::
      !(Maybe Text),
    _cwlcStreamName :: !(Maybe Text),
    _cwlcStatus :: !LogsConfigStatusType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchLogsConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwlcGroupName' - The group name of the logs in Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> .
--
-- * 'cwlcStreamName' - The prefix of the stream name of the Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> .
--
-- * 'cwlcStatus' - The current status of the logs in Amazon CloudWatch Logs for a build project. Valid values are:     * @ENABLED@ : Amazon CloudWatch Logs are enabled for this build project.     * @DISABLED@ : Amazon CloudWatch Logs are not enabled for this build project.
cloudWatchLogsConfig ::
  -- | 'cwlcStatus'
  LogsConfigStatusType ->
  CloudWatchLogsConfig
cloudWatchLogsConfig pStatus_ =
  CloudWatchLogsConfig'
    { _cwlcGroupName = Nothing,
      _cwlcStreamName = Nothing,
      _cwlcStatus = pStatus_
    }

-- | The group name of the logs in Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> .
cwlcGroupName :: Lens' CloudWatchLogsConfig (Maybe Text)
cwlcGroupName = lens _cwlcGroupName (\s a -> s {_cwlcGroupName = a})

-- | The prefix of the stream name of the Amazon CloudWatch Logs. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html Working with Log Groups and Log Streams> .
cwlcStreamName :: Lens' CloudWatchLogsConfig (Maybe Text)
cwlcStreamName = lens _cwlcStreamName (\s a -> s {_cwlcStreamName = a})

-- | The current status of the logs in Amazon CloudWatch Logs for a build project. Valid values are:     * @ENABLED@ : Amazon CloudWatch Logs are enabled for this build project.     * @DISABLED@ : Amazon CloudWatch Logs are not enabled for this build project.
cwlcStatus :: Lens' CloudWatchLogsConfig LogsConfigStatusType
cwlcStatus = lens _cwlcStatus (\s a -> s {_cwlcStatus = a})

instance FromJSON CloudWatchLogsConfig where
  parseJSON =
    withObject
      "CloudWatchLogsConfig"
      ( \x ->
          CloudWatchLogsConfig'
            <$> (x .:? "groupName") <*> (x .:? "streamName") <*> (x .: "status")
      )

instance Hashable CloudWatchLogsConfig

instance NFData CloudWatchLogsConfig

instance ToJSON CloudWatchLogsConfig where
  toJSON CloudWatchLogsConfig' {..} =
    object
      ( catMaybes
          [ ("groupName" .=) <$> _cwlcGroupName,
            ("streamName" .=) <$> _cwlcStreamName,
            Just ("status" .= _cwlcStatus)
          ]
      )
