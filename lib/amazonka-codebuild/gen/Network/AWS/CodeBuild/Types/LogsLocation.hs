{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LogsLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.LogsLocation
  ( LogsLocation (..),

    -- * Smart constructor
    mkLogsLocation,

    -- * Lenses
    llDeepLink,
    llS3Logs,
    llCloudWatchLogs,
    llS3DeepLink,
    llS3LogsARN,
    llCloudWatchLogsARN,
    llGroupName,
    llStreamName,
  )
where

import Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
import Network.AWS.CodeBuild.Types.S3LogsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about build logs in Amazon CloudWatch Logs.
--
-- /See:/ 'mkLogsLocation' smart constructor.
data LogsLocation = LogsLocation'
  { deepLink :: Lude.Maybe Lude.Text,
    s3Logs :: Lude.Maybe S3LogsConfig,
    cloudWatchLogs :: Lude.Maybe CloudWatchLogsConfig,
    s3DeepLink :: Lude.Maybe Lude.Text,
    s3LogsARN :: Lude.Maybe Lude.Text,
    cloudWatchLogsARN :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text,
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogsLocation' with the minimum fields required to make a request.
--
-- * 'cloudWatchLogs' - Information about Amazon CloudWatch Logs for a build project.
-- * 'cloudWatchLogsARN' - The ARN of Amazon CloudWatch Logs for a build project. Its format is @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by Amazon CloudWatch Logs> .
-- * 'deepLink' - The URL to an individual build log in Amazon CloudWatch Logs.
-- * 'groupName' - The name of the Amazon CloudWatch Logs group for the build logs.
-- * 's3DeepLink' - The URL to a build log in an S3 bucket.
-- * 's3Logs' - Information about S3 logs for a build project.
-- * 's3LogsARN' - The ARN of S3 logs for a build project. Its format is @arn:${Partition}:s3:::${BucketName}/${ObjectName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3> .
-- * 'streamName' - The name of the Amazon CloudWatch Logs stream for the build logs.
mkLogsLocation ::
  LogsLocation
mkLogsLocation =
  LogsLocation'
    { deepLink = Lude.Nothing,
      s3Logs = Lude.Nothing,
      cloudWatchLogs = Lude.Nothing,
      s3DeepLink = Lude.Nothing,
      s3LogsARN = Lude.Nothing,
      cloudWatchLogsARN = Lude.Nothing,
      groupName = Lude.Nothing,
      streamName = Lude.Nothing
    }

-- | The URL to an individual build log in Amazon CloudWatch Logs.
--
-- /Note:/ Consider using 'deepLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llDeepLink :: Lens.Lens' LogsLocation (Lude.Maybe Lude.Text)
llDeepLink = Lens.lens (deepLink :: LogsLocation -> Lude.Maybe Lude.Text) (\s a -> s {deepLink = a} :: LogsLocation)
{-# DEPRECATED llDeepLink "Use generic-lens or generic-optics with 'deepLink' instead." #-}

-- | Information about S3 logs for a build project.
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llS3Logs :: Lens.Lens' LogsLocation (Lude.Maybe S3LogsConfig)
llS3Logs = Lens.lens (s3Logs :: LogsLocation -> Lude.Maybe S3LogsConfig) (\s a -> s {s3Logs = a} :: LogsLocation)
{-# DEPRECATED llS3Logs "Use generic-lens or generic-optics with 's3Logs' instead." #-}

-- | Information about Amazon CloudWatch Logs for a build project.
--
-- /Note:/ Consider using 'cloudWatchLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llCloudWatchLogs :: Lens.Lens' LogsLocation (Lude.Maybe CloudWatchLogsConfig)
llCloudWatchLogs = Lens.lens (cloudWatchLogs :: LogsLocation -> Lude.Maybe CloudWatchLogsConfig) (\s a -> s {cloudWatchLogs = a} :: LogsLocation)
{-# DEPRECATED llCloudWatchLogs "Use generic-lens or generic-optics with 'cloudWatchLogs' instead." #-}

-- | The URL to a build log in an S3 bucket.
--
-- /Note:/ Consider using 's3DeepLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llS3DeepLink :: Lens.Lens' LogsLocation (Lude.Maybe Lude.Text)
llS3DeepLink = Lens.lens (s3DeepLink :: LogsLocation -> Lude.Maybe Lude.Text) (\s a -> s {s3DeepLink = a} :: LogsLocation)
{-# DEPRECATED llS3DeepLink "Use generic-lens or generic-optics with 's3DeepLink' instead." #-}

-- | The ARN of S3 logs for a build project. Its format is @arn:${Partition}:s3:::${BucketName}/${ObjectName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3> .
--
-- /Note:/ Consider using 's3LogsARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llS3LogsARN :: Lens.Lens' LogsLocation (Lude.Maybe Lude.Text)
llS3LogsARN = Lens.lens (s3LogsARN :: LogsLocation -> Lude.Maybe Lude.Text) (\s a -> s {s3LogsARN = a} :: LogsLocation)
{-# DEPRECATED llS3LogsARN "Use generic-lens or generic-optics with 's3LogsARN' instead." #-}

-- | The ARN of Amazon CloudWatch Logs for a build project. Its format is @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by Amazon CloudWatch Logs> .
--
-- /Note:/ Consider using 'cloudWatchLogsARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llCloudWatchLogsARN :: Lens.Lens' LogsLocation (Lude.Maybe Lude.Text)
llCloudWatchLogsARN = Lens.lens (cloudWatchLogsARN :: LogsLocation -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsARN = a} :: LogsLocation)
{-# DEPRECATED llCloudWatchLogsARN "Use generic-lens or generic-optics with 'cloudWatchLogsARN' instead." #-}

-- | The name of the Amazon CloudWatch Logs group for the build logs.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llGroupName :: Lens.Lens' LogsLocation (Lude.Maybe Lude.Text)
llGroupName = Lens.lens (groupName :: LogsLocation -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: LogsLocation)
{-# DEPRECATED llGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The name of the Amazon CloudWatch Logs stream for the build logs.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llStreamName :: Lens.Lens' LogsLocation (Lude.Maybe Lude.Text)
llStreamName = Lens.lens (streamName :: LogsLocation -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: LogsLocation)
{-# DEPRECATED llStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.FromJSON LogsLocation where
  parseJSON =
    Lude.withObject
      "LogsLocation"
      ( \x ->
          LogsLocation'
            Lude.<$> (x Lude..:? "deepLink")
            Lude.<*> (x Lude..:? "s3Logs")
            Lude.<*> (x Lude..:? "cloudWatchLogs")
            Lude.<*> (x Lude..:? "s3DeepLink")
            Lude.<*> (x Lude..:? "s3LogsArn")
            Lude.<*> (x Lude..:? "cloudWatchLogsArn")
            Lude.<*> (x Lude..:? "groupName")
            Lude.<*> (x Lude..:? "streamName")
      )
