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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about build logs in Amazon CloudWatch Logs.
--
-- /See:/ 'newLogsLocation' smart constructor.
data LogsLocation = LogsLocation'
  { -- | Information about S3 logs for a build project.
    s3Logs :: Core.Maybe S3LogsConfig,
    -- | Information about Amazon CloudWatch Logs for a build project.
    cloudWatchLogs :: Core.Maybe CloudWatchLogsConfig,
    -- | The URL to an individual build log in Amazon CloudWatch Logs.
    deepLink :: Core.Maybe Core.Text,
    -- | The name of the Amazon CloudWatch Logs group for the build logs.
    groupName :: Core.Maybe Core.Text,
    -- | The ARN of Amazon CloudWatch Logs for a build project. Its format is
    -- @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by Amazon CloudWatch Logs>.
    cloudWatchLogsArn :: Core.Maybe Core.Text,
    -- | The ARN of S3 logs for a build project. Its format is
    -- @arn:${Partition}:s3:::${BucketName}\/${ObjectName}@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3>.
    s3LogsArn :: Core.Maybe Core.Text,
    -- | The URL to a build log in an S3 bucket.
    s3DeepLink :: Core.Maybe Core.Text,
    -- | The name of the Amazon CloudWatch Logs stream for the build logs.
    streamName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { s3Logs = Core.Nothing,
      cloudWatchLogs = Core.Nothing,
      deepLink = Core.Nothing,
      groupName = Core.Nothing,
      cloudWatchLogsArn = Core.Nothing,
      s3LogsArn = Core.Nothing,
      s3DeepLink = Core.Nothing,
      streamName = Core.Nothing
    }

-- | Information about S3 logs for a build project.
logsLocation_s3Logs :: Lens.Lens' LogsLocation (Core.Maybe S3LogsConfig)
logsLocation_s3Logs = Lens.lens (\LogsLocation' {s3Logs} -> s3Logs) (\s@LogsLocation' {} a -> s {s3Logs = a} :: LogsLocation)

-- | Information about Amazon CloudWatch Logs for a build project.
logsLocation_cloudWatchLogs :: Lens.Lens' LogsLocation (Core.Maybe CloudWatchLogsConfig)
logsLocation_cloudWatchLogs = Lens.lens (\LogsLocation' {cloudWatchLogs} -> cloudWatchLogs) (\s@LogsLocation' {} a -> s {cloudWatchLogs = a} :: LogsLocation)

-- | The URL to an individual build log in Amazon CloudWatch Logs.
logsLocation_deepLink :: Lens.Lens' LogsLocation (Core.Maybe Core.Text)
logsLocation_deepLink = Lens.lens (\LogsLocation' {deepLink} -> deepLink) (\s@LogsLocation' {} a -> s {deepLink = a} :: LogsLocation)

-- | The name of the Amazon CloudWatch Logs group for the build logs.
logsLocation_groupName :: Lens.Lens' LogsLocation (Core.Maybe Core.Text)
logsLocation_groupName = Lens.lens (\LogsLocation' {groupName} -> groupName) (\s@LogsLocation' {} a -> s {groupName = a} :: LogsLocation)

-- | The ARN of Amazon CloudWatch Logs for a build project. Its format is
-- @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@.
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by Amazon CloudWatch Logs>.
logsLocation_cloudWatchLogsArn :: Lens.Lens' LogsLocation (Core.Maybe Core.Text)
logsLocation_cloudWatchLogsArn = Lens.lens (\LogsLocation' {cloudWatchLogsArn} -> cloudWatchLogsArn) (\s@LogsLocation' {} a -> s {cloudWatchLogsArn = a} :: LogsLocation)

-- | The ARN of S3 logs for a build project. Its format is
-- @arn:${Partition}:s3:::${BucketName}\/${ObjectName}@. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3>.
logsLocation_s3LogsArn :: Lens.Lens' LogsLocation (Core.Maybe Core.Text)
logsLocation_s3LogsArn = Lens.lens (\LogsLocation' {s3LogsArn} -> s3LogsArn) (\s@LogsLocation' {} a -> s {s3LogsArn = a} :: LogsLocation)

-- | The URL to a build log in an S3 bucket.
logsLocation_s3DeepLink :: Lens.Lens' LogsLocation (Core.Maybe Core.Text)
logsLocation_s3DeepLink = Lens.lens (\LogsLocation' {s3DeepLink} -> s3DeepLink) (\s@LogsLocation' {} a -> s {s3DeepLink = a} :: LogsLocation)

-- | The name of the Amazon CloudWatch Logs stream for the build logs.
logsLocation_streamName :: Lens.Lens' LogsLocation (Core.Maybe Core.Text)
logsLocation_streamName = Lens.lens (\LogsLocation' {streamName} -> streamName) (\s@LogsLocation' {} a -> s {streamName = a} :: LogsLocation)

instance Core.FromJSON LogsLocation where
  parseJSON =
    Core.withObject
      "LogsLocation"
      ( \x ->
          LogsLocation'
            Core.<$> (x Core..:? "s3Logs")
            Core.<*> (x Core..:? "cloudWatchLogs")
            Core.<*> (x Core..:? "deepLink")
            Core.<*> (x Core..:? "groupName")
            Core.<*> (x Core..:? "cloudWatchLogsArn")
            Core.<*> (x Core..:? "s3LogsArn")
            Core.<*> (x Core..:? "s3DeepLink")
            Core.<*> (x Core..:? "streamName")
      )

instance Core.Hashable LogsLocation

instance Core.NFData LogsLocation
