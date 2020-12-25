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
    llCloudWatchLogs,
    llCloudWatchLogsArn,
    llDeepLink,
    llGroupName,
    llS3DeepLink,
    llS3Logs,
    llS3LogsArn,
    llStreamName,
  )
where

import qualified Network.AWS.CodeBuild.Types.CloudWatchLogsConfig as Types
import qualified Network.AWS.CodeBuild.Types.S3LogsConfig as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about build logs in Amazon CloudWatch Logs.
--
-- /See:/ 'mkLogsLocation' smart constructor.
data LogsLocation = LogsLocation'
  { -- | Information about Amazon CloudWatch Logs for a build project.
    cloudWatchLogs :: Core.Maybe Types.CloudWatchLogsConfig,
    -- | The ARN of Amazon CloudWatch Logs for a build project. Its format is @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by Amazon CloudWatch Logs> .
    cloudWatchLogsArn :: Core.Maybe Types.String,
    -- | The URL to an individual build log in Amazon CloudWatch Logs.
    deepLink :: Core.Maybe Types.String,
    -- | The name of the Amazon CloudWatch Logs group for the build logs.
    groupName :: Core.Maybe Types.String,
    -- | The URL to a build log in an S3 bucket.
    s3DeepLink :: Core.Maybe Types.String,
    -- | Information about S3 logs for a build project.
    s3Logs :: Core.Maybe Types.S3LogsConfig,
    -- | The ARN of S3 logs for a build project. Its format is @arn:${Partition}:s3:::${BucketName}/${ObjectName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3> .
    s3LogsArn :: Core.Maybe Types.String,
    -- | The name of the Amazon CloudWatch Logs stream for the build logs.
    streamName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogsLocation' value with any optional fields omitted.
mkLogsLocation ::
  LogsLocation
mkLogsLocation =
  LogsLocation'
    { cloudWatchLogs = Core.Nothing,
      cloudWatchLogsArn = Core.Nothing,
      deepLink = Core.Nothing,
      groupName = Core.Nothing,
      s3DeepLink = Core.Nothing,
      s3Logs = Core.Nothing,
      s3LogsArn = Core.Nothing,
      streamName = Core.Nothing
    }

-- | Information about Amazon CloudWatch Logs for a build project.
--
-- /Note:/ Consider using 'cloudWatchLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llCloudWatchLogs :: Lens.Lens' LogsLocation (Core.Maybe Types.CloudWatchLogsConfig)
llCloudWatchLogs = Lens.field @"cloudWatchLogs"
{-# DEPRECATED llCloudWatchLogs "Use generic-lens or generic-optics with 'cloudWatchLogs' instead." #-}

-- | The ARN of Amazon CloudWatch Logs for a build project. Its format is @arn:${Partition}:logs:${Region}:${Account}:log-group:${LogGroupName}:log-stream:${LogStreamName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatchlogs.html#amazoncloudwatchlogs-resources-for-iam-policies Resources Defined by Amazon CloudWatch Logs> .
--
-- /Note:/ Consider using 'cloudWatchLogsArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llCloudWatchLogsArn :: Lens.Lens' LogsLocation (Core.Maybe Types.String)
llCloudWatchLogsArn = Lens.field @"cloudWatchLogsArn"
{-# DEPRECATED llCloudWatchLogsArn "Use generic-lens or generic-optics with 'cloudWatchLogsArn' instead." #-}

-- | The URL to an individual build log in Amazon CloudWatch Logs.
--
-- /Note:/ Consider using 'deepLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llDeepLink :: Lens.Lens' LogsLocation (Core.Maybe Types.String)
llDeepLink = Lens.field @"deepLink"
{-# DEPRECATED llDeepLink "Use generic-lens or generic-optics with 'deepLink' instead." #-}

-- | The name of the Amazon CloudWatch Logs group for the build logs.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llGroupName :: Lens.Lens' LogsLocation (Core.Maybe Types.String)
llGroupName = Lens.field @"groupName"
{-# DEPRECATED llGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The URL to a build log in an S3 bucket.
--
-- /Note:/ Consider using 's3DeepLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llS3DeepLink :: Lens.Lens' LogsLocation (Core.Maybe Types.String)
llS3DeepLink = Lens.field @"s3DeepLink"
{-# DEPRECATED llS3DeepLink "Use generic-lens or generic-optics with 's3DeepLink' instead." #-}

-- | Information about S3 logs for a build project.
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llS3Logs :: Lens.Lens' LogsLocation (Core.Maybe Types.S3LogsConfig)
llS3Logs = Lens.field @"s3Logs"
{-# DEPRECATED llS3Logs "Use generic-lens or generic-optics with 's3Logs' instead." #-}

-- | The ARN of S3 logs for a build project. Its format is @arn:${Partition}:s3:::${BucketName}/${ObjectName}@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-resources-for-iam-policies Resources Defined by Amazon S3> .
--
-- /Note:/ Consider using 's3LogsArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llS3LogsArn :: Lens.Lens' LogsLocation (Core.Maybe Types.String)
llS3LogsArn = Lens.field @"s3LogsArn"
{-# DEPRECATED llS3LogsArn "Use generic-lens or generic-optics with 's3LogsArn' instead." #-}

-- | The name of the Amazon CloudWatch Logs stream for the build logs.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llStreamName :: Lens.Lens' LogsLocation (Core.Maybe Types.String)
llStreamName = Lens.field @"streamName"
{-# DEPRECATED llStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Core.FromJSON LogsLocation where
  parseJSON =
    Core.withObject "LogsLocation" Core.$
      \x ->
        LogsLocation'
          Core.<$> (x Core..:? "cloudWatchLogs")
          Core.<*> (x Core..:? "cloudWatchLogsArn")
          Core.<*> (x Core..:? "deepLink")
          Core.<*> (x Core..:? "groupName")
          Core.<*> (x Core..:? "s3DeepLink")
          Core.<*> (x Core..:? "s3Logs")
          Core.<*> (x Core..:? "s3LogsArn")
          Core.<*> (x Core..:? "streamName")
