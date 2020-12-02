{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LogsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.LogsConfig where

import Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
import Network.AWS.CodeBuild.Types.S3LogsConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about logs for a build project. These can be logs in Amazon CloudWatch Logs, built in a specified S3 bucket, or both.
--
--
--
-- /See:/ 'logsConfig' smart constructor.
data LogsConfig = LogsConfig'
  { _lcS3Logs :: !(Maybe S3LogsConfig),
    _lcCloudWatchLogs :: !(Maybe CloudWatchLogsConfig)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogsConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcS3Logs' - Information about logs built to an S3 bucket for a build project. S3 logs are not enabled by default.
--
-- * 'lcCloudWatchLogs' - Information about Amazon CloudWatch Logs for a build project. Amazon CloudWatch Logs are enabled by default.
logsConfig ::
  LogsConfig
logsConfig =
  LogsConfig' {_lcS3Logs = Nothing, _lcCloudWatchLogs = Nothing}

-- | Information about logs built to an S3 bucket for a build project. S3 logs are not enabled by default.
lcS3Logs :: Lens' LogsConfig (Maybe S3LogsConfig)
lcS3Logs = lens _lcS3Logs (\s a -> s {_lcS3Logs = a})

-- | Information about Amazon CloudWatch Logs for a build project. Amazon CloudWatch Logs are enabled by default.
lcCloudWatchLogs :: Lens' LogsConfig (Maybe CloudWatchLogsConfig)
lcCloudWatchLogs = lens _lcCloudWatchLogs (\s a -> s {_lcCloudWatchLogs = a})

instance FromJSON LogsConfig where
  parseJSON =
    withObject
      "LogsConfig"
      ( \x ->
          LogsConfig' <$> (x .:? "s3Logs") <*> (x .:? "cloudWatchLogs")
      )

instance Hashable LogsConfig

instance NFData LogsConfig

instance ToJSON LogsConfig where
  toJSON LogsConfig' {..} =
    object
      ( catMaybes
          [ ("s3Logs" .=) <$> _lcS3Logs,
            ("cloudWatchLogs" .=) <$> _lcCloudWatchLogs
          ]
      )
