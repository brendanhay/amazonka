{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LogsLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.LogsLocation where

import Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
import Network.AWS.CodeBuild.Types.S3LogsConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about build logs in Amazon CloudWatch Logs.
--
--
--
-- /See:/ 'logsLocation' smart constructor.
data LogsLocation = LogsLocation'
  { _llDeepLink :: !(Maybe Text),
    _llS3Logs :: !(Maybe S3LogsConfig),
    _llCloudWatchLogs :: !(Maybe CloudWatchLogsConfig),
    _llS3DeepLink :: !(Maybe Text),
    _llS3LogsARN :: !(Maybe Text),
    _llCloudWatchLogsARN :: !(Maybe Text),
    _llGroupName :: !(Maybe Text),
    _llStreamName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogsLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llDeepLink' - The URL to an individual build log in Amazon CloudWatch Logs.
--
-- * 'llS3Logs' - Information about S3 logs for a build project.
--
-- * 'llCloudWatchLogs' - Information about Amazon CloudWatch Logs for a build project.
--
-- * 'llS3DeepLink' - The URL to a build log in an S3 bucket.
--
-- * 'llS3LogsARN' - The ARN of S3 logs for a build project. Its format is @arn:${Partition}:s3:::${BucketName}/${ObjectName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3> .
--
-- * 'llCloudWatchLogsARN' - The ARN of Amazon CloudWatch Logs for a build project. Its format is @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by Amazon CloudWatch Logs> .
--
-- * 'llGroupName' - The name of the Amazon CloudWatch Logs group for the build logs.
--
-- * 'llStreamName' - The name of the Amazon CloudWatch Logs stream for the build logs.
logsLocation ::
  LogsLocation
logsLocation =
  LogsLocation'
    { _llDeepLink = Nothing,
      _llS3Logs = Nothing,
      _llCloudWatchLogs = Nothing,
      _llS3DeepLink = Nothing,
      _llS3LogsARN = Nothing,
      _llCloudWatchLogsARN = Nothing,
      _llGroupName = Nothing,
      _llStreamName = Nothing
    }

-- | The URL to an individual build log in Amazon CloudWatch Logs.
llDeepLink :: Lens' LogsLocation (Maybe Text)
llDeepLink = lens _llDeepLink (\s a -> s {_llDeepLink = a})

-- | Information about S3 logs for a build project.
llS3Logs :: Lens' LogsLocation (Maybe S3LogsConfig)
llS3Logs = lens _llS3Logs (\s a -> s {_llS3Logs = a})

-- | Information about Amazon CloudWatch Logs for a build project.
llCloudWatchLogs :: Lens' LogsLocation (Maybe CloudWatchLogsConfig)
llCloudWatchLogs = lens _llCloudWatchLogs (\s a -> s {_llCloudWatchLogs = a})

-- | The URL to a build log in an S3 bucket.
llS3DeepLink :: Lens' LogsLocation (Maybe Text)
llS3DeepLink = lens _llS3DeepLink (\s a -> s {_llS3DeepLink = a})

-- | The ARN of S3 logs for a build project. Its format is @arn:${Partition}:s3:::${BucketName}/${ObjectName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3> .
llS3LogsARN :: Lens' LogsLocation (Maybe Text)
llS3LogsARN = lens _llS3LogsARN (\s a -> s {_llS3LogsARN = a})

-- | The ARN of Amazon CloudWatch Logs for a build project. Its format is @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by Amazon CloudWatch Logs> .
llCloudWatchLogsARN :: Lens' LogsLocation (Maybe Text)
llCloudWatchLogsARN = lens _llCloudWatchLogsARN (\s a -> s {_llCloudWatchLogsARN = a})

-- | The name of the Amazon CloudWatch Logs group for the build logs.
llGroupName :: Lens' LogsLocation (Maybe Text)
llGroupName = lens _llGroupName (\s a -> s {_llGroupName = a})

-- | The name of the Amazon CloudWatch Logs stream for the build logs.
llStreamName :: Lens' LogsLocation (Maybe Text)
llStreamName = lens _llStreamName (\s a -> s {_llStreamName = a})

instance FromJSON LogsLocation where
  parseJSON =
    withObject
      "LogsLocation"
      ( \x ->
          LogsLocation'
            <$> (x .:? "deepLink")
            <*> (x .:? "s3Logs")
            <*> (x .:? "cloudWatchLogs")
            <*> (x .:? "s3DeepLink")
            <*> (x .:? "s3LogsArn")
            <*> (x .:? "cloudWatchLogsArn")
            <*> (x .:? "groupName")
            <*> (x .:? "streamName")
      )

instance Hashable LogsLocation

instance NFData LogsLocation
