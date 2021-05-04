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
-- Module      : Network.AWS.CodeBuild.Types.LogsLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.LogsLocation where

import Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
import Network.AWS.CodeBuild.Types.S3LogsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about build logs in Amazon CloudWatch Logs.
--
-- /See:/ 'newLogsLocation' smart constructor.
data LogsLocation = LogsLocation'
  { -- | Information about S3 logs for a build project.
    s3Logs :: Prelude.Maybe S3LogsConfig,
    -- | Information about Amazon CloudWatch Logs for a build project.
    cloudWatchLogs :: Prelude.Maybe CloudWatchLogsConfig,
    -- | The URL to an individual build log in Amazon CloudWatch Logs.
    deepLink :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon CloudWatch Logs group for the build logs.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of Amazon CloudWatch Logs for a build project. Its format is
    -- @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by Amazon CloudWatch Logs>.
    cloudWatchLogsArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of S3 logs for a build project. Its format is
    -- @arn:${Partition}:s3:::${BucketName}\/${ObjectName}@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3>.
    s3LogsArn :: Prelude.Maybe Prelude.Text,
    -- | The URL to a build log in an S3 bucket.
    s3DeepLink :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon CloudWatch Logs stream for the build logs.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LogsLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Logs', 'logsLocation_s3Logs' - Information about S3 logs for a build project.
--
-- 'cloudWatchLogs', 'logsLocation_cloudWatchLogs' - Information about Amazon CloudWatch Logs for a build project.
--
-- 'deepLink', 'logsLocation_deepLink' - The URL to an individual build log in Amazon CloudWatch Logs.
--
-- 'groupName', 'logsLocation_groupName' - The name of the Amazon CloudWatch Logs group for the build logs.
--
-- 'cloudWatchLogsArn', 'logsLocation_cloudWatchLogsArn' - The ARN of Amazon CloudWatch Logs for a build project. Its format is
-- @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@.
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by Amazon CloudWatch Logs>.
--
-- 's3LogsArn', 'logsLocation_s3LogsArn' - The ARN of S3 logs for a build project. Its format is
-- @arn:${Partition}:s3:::${BucketName}\/${ObjectName}@. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3>.
--
-- 's3DeepLink', 'logsLocation_s3DeepLink' - The URL to a build log in an S3 bucket.
--
-- 'streamName', 'logsLocation_streamName' - The name of the Amazon CloudWatch Logs stream for the build logs.
newLogsLocation ::
  LogsLocation
newLogsLocation =
  LogsLocation'
    { s3Logs = Prelude.Nothing,
      cloudWatchLogs = Prelude.Nothing,
      deepLink = Prelude.Nothing,
      groupName = Prelude.Nothing,
      cloudWatchLogsArn = Prelude.Nothing,
      s3LogsArn = Prelude.Nothing,
      s3DeepLink = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | Information about S3 logs for a build project.
logsLocation_s3Logs :: Lens.Lens' LogsLocation (Prelude.Maybe S3LogsConfig)
logsLocation_s3Logs = Lens.lens (\LogsLocation' {s3Logs} -> s3Logs) (\s@LogsLocation' {} a -> s {s3Logs = a} :: LogsLocation)

-- | Information about Amazon CloudWatch Logs for a build project.
logsLocation_cloudWatchLogs :: Lens.Lens' LogsLocation (Prelude.Maybe CloudWatchLogsConfig)
logsLocation_cloudWatchLogs = Lens.lens (\LogsLocation' {cloudWatchLogs} -> cloudWatchLogs) (\s@LogsLocation' {} a -> s {cloudWatchLogs = a} :: LogsLocation)

-- | The URL to an individual build log in Amazon CloudWatch Logs.
logsLocation_deepLink :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_deepLink = Lens.lens (\LogsLocation' {deepLink} -> deepLink) (\s@LogsLocation' {} a -> s {deepLink = a} :: LogsLocation)

-- | The name of the Amazon CloudWatch Logs group for the build logs.
logsLocation_groupName :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_groupName = Lens.lens (\LogsLocation' {groupName} -> groupName) (\s@LogsLocation' {} a -> s {groupName = a} :: LogsLocation)

-- | The ARN of Amazon CloudWatch Logs for a build project. Its format is
-- @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@.
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by Amazon CloudWatch Logs>.
logsLocation_cloudWatchLogsArn :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_cloudWatchLogsArn = Lens.lens (\LogsLocation' {cloudWatchLogsArn} -> cloudWatchLogsArn) (\s@LogsLocation' {} a -> s {cloudWatchLogsArn = a} :: LogsLocation)

-- | The ARN of S3 logs for a build project. Its format is
-- @arn:${Partition}:s3:::${BucketName}\/${ObjectName}@. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3>.
logsLocation_s3LogsArn :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_s3LogsArn = Lens.lens (\LogsLocation' {s3LogsArn} -> s3LogsArn) (\s@LogsLocation' {} a -> s {s3LogsArn = a} :: LogsLocation)

-- | The URL to a build log in an S3 bucket.
logsLocation_s3DeepLink :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_s3DeepLink = Lens.lens (\LogsLocation' {s3DeepLink} -> s3DeepLink) (\s@LogsLocation' {} a -> s {s3DeepLink = a} :: LogsLocation)

-- | The name of the Amazon CloudWatch Logs stream for the build logs.
logsLocation_streamName :: Lens.Lens' LogsLocation (Prelude.Maybe Prelude.Text)
logsLocation_streamName = Lens.lens (\LogsLocation' {streamName} -> streamName) (\s@LogsLocation' {} a -> s {streamName = a} :: LogsLocation)

instance Prelude.FromJSON LogsLocation where
  parseJSON =
    Prelude.withObject
      "LogsLocation"
      ( \x ->
          LogsLocation'
            Prelude.<$> (x Prelude..:? "s3Logs")
            Prelude.<*> (x Prelude..:? "cloudWatchLogs")
            Prelude.<*> (x Prelude..:? "deepLink")
            Prelude.<*> (x Prelude..:? "groupName")
            Prelude.<*> (x Prelude..:? "cloudWatchLogsArn")
            Prelude.<*> (x Prelude..:? "s3LogsArn")
            Prelude.<*> (x Prelude..:? "s3DeepLink")
            Prelude.<*> (x Prelude..:? "streamName")
      )

instance Prelude.Hashable LogsLocation

instance Prelude.NFData LogsLocation
