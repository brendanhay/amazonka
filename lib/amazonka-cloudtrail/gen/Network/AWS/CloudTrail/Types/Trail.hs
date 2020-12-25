{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Trail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.Trail
  ( Trail (..),

    -- * Smart constructor
    mkTrail,

    -- * Lenses
    tCloudWatchLogsLogGroupArn,
    tCloudWatchLogsRoleArn,
    tHasCustomEventSelectors,
    tHasInsightSelectors,
    tHomeRegion,
    tIncludeGlobalServiceEvents,
    tIsMultiRegionTrail,
    tIsOrganizationTrail,
    tKmsKeyId,
    tLogFileValidationEnabled,
    tName,
    tS3BucketName,
    tS3KeyPrefix,
    tSnsTopicARN,
    tSnsTopicName,
    tTrailARN,
  )
where

import qualified Network.AWS.CloudTrail.Types.CloudWatchLogsLogGroupArn as Types
import qualified Network.AWS.CloudTrail.Types.CloudWatchLogsRoleArn as Types
import qualified Network.AWS.CloudTrail.Types.HomeRegion as Types
import qualified Network.AWS.CloudTrail.Types.KmsKeyId as Types
import qualified Network.AWS.CloudTrail.Types.Name as Types
import qualified Network.AWS.CloudTrail.Types.S3BucketName as Types
import qualified Network.AWS.CloudTrail.Types.S3KeyPrefix as Types
import qualified Network.AWS.CloudTrail.Types.SnsTopicARN as Types
import qualified Network.AWS.CloudTrail.Types.SnsTopicName as Types
import qualified Network.AWS.CloudTrail.Types.TrailARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The settings for a trail.
--
-- /See:/ 'mkTrail' smart constructor.
data Trail = Trail'
  { -- | Specifies an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered.
    cloudWatchLogsLogGroupArn :: Core.Maybe Types.CloudWatchLogsLogGroupArn,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
    cloudWatchLogsRoleArn :: Core.Maybe Types.CloudWatchLogsRoleArn,
    -- | Specifies if the trail has custom event selectors.
    hasCustomEventSelectors :: Core.Maybe Core.Bool,
    -- | Specifies whether a trail has insight types specified in an @InsightSelector@ list.
    hasInsightSelectors :: Core.Maybe Core.Bool,
    -- | The region in which the trail was created.
    homeRegion :: Core.Maybe Types.HomeRegion,
    -- | Set to __True__ to include AWS API calls from AWS global services such as IAM. Otherwise, __False__ .
    includeGlobalServiceEvents :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail exists only in one region or exists in all regions.
    isMultiRegionTrail :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail is an organization trail.
    isOrganizationTrail :: Core.Maybe Core.Bool,
    -- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
    --
    -- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | Specifies whether log file validation is enabled.
    logFileValidationEnabled :: Core.Maybe Core.Bool,
    -- | Name of the trail set by calling 'CreateTrail' . The maximum length is 128 characters.
    name :: Core.Maybe Types.Name,
    -- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
    s3BucketName :: Core.Maybe Types.S3BucketName,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .The maximum length is 200 characters.
    s3KeyPrefix :: Core.Maybe Types.S3KeyPrefix,
    -- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
    --
    -- @arn:aws:sns:us-east-2:123456789012:MyTopic@
    snsTopicARN :: Core.Maybe Types.SnsTopicARN,
    -- | This field is no longer in use. Use SnsTopicARN.
    snsTopicName :: Core.Maybe Types.SnsTopicName,
    -- | Specifies the ARN of the trail. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    trailARN :: Core.Maybe Types.TrailARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Trail' value with any optional fields omitted.
mkTrail ::
  Trail
mkTrail =
  Trail'
    { cloudWatchLogsLogGroupArn = Core.Nothing,
      cloudWatchLogsRoleArn = Core.Nothing,
      hasCustomEventSelectors = Core.Nothing,
      hasInsightSelectors = Core.Nothing,
      homeRegion = Core.Nothing,
      includeGlobalServiceEvents = Core.Nothing,
      isMultiRegionTrail = Core.Nothing,
      isOrganizationTrail = Core.Nothing,
      kmsKeyId = Core.Nothing,
      logFileValidationEnabled = Core.Nothing,
      name = Core.Nothing,
      s3BucketName = Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      snsTopicARN = Core.Nothing,
      snsTopicName = Core.Nothing,
      trailARN = Core.Nothing
    }

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCloudWatchLogsLogGroupArn :: Lens.Lens' Trail (Core.Maybe Types.CloudWatchLogsLogGroupArn)
tCloudWatchLogsLogGroupArn = Lens.field @"cloudWatchLogsLogGroupArn"
{-# DEPRECATED tCloudWatchLogsLogGroupArn "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupArn' instead." #-}

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCloudWatchLogsRoleArn :: Lens.Lens' Trail (Core.Maybe Types.CloudWatchLogsRoleArn)
tCloudWatchLogsRoleArn = Lens.field @"cloudWatchLogsRoleArn"
{-# DEPRECATED tCloudWatchLogsRoleArn "Use generic-lens or generic-optics with 'cloudWatchLogsRoleArn' instead." #-}

-- | Specifies if the trail has custom event selectors.
--
-- /Note:/ Consider using 'hasCustomEventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHasCustomEventSelectors :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tHasCustomEventSelectors = Lens.field @"hasCustomEventSelectors"
{-# DEPRECATED tHasCustomEventSelectors "Use generic-lens or generic-optics with 'hasCustomEventSelectors' instead." #-}

-- | Specifies whether a trail has insight types specified in an @InsightSelector@ list.
--
-- /Note:/ Consider using 'hasInsightSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHasInsightSelectors :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tHasInsightSelectors = Lens.field @"hasInsightSelectors"
{-# DEPRECATED tHasInsightSelectors "Use generic-lens or generic-optics with 'hasInsightSelectors' instead." #-}

-- | The region in which the trail was created.
--
-- /Note:/ Consider using 'homeRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHomeRegion :: Lens.Lens' Trail (Core.Maybe Types.HomeRegion)
tHomeRegion = Lens.field @"homeRegion"
{-# DEPRECATED tHomeRegion "Use generic-lens or generic-optics with 'homeRegion' instead." #-}

-- | Set to __True__ to include AWS API calls from AWS global services such as IAM. Otherwise, __False__ .
--
-- /Note:/ Consider using 'includeGlobalServiceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tIncludeGlobalServiceEvents :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tIncludeGlobalServiceEvents = Lens.field @"includeGlobalServiceEvents"
{-# DEPRECATED tIncludeGlobalServiceEvents "Use generic-lens or generic-optics with 'includeGlobalServiceEvents' instead." #-}

-- | Specifies whether the trail exists only in one region or exists in all regions.
--
-- /Note:/ Consider using 'isMultiRegionTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tIsMultiRegionTrail :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tIsMultiRegionTrail = Lens.field @"isMultiRegionTrail"
{-# DEPRECATED tIsMultiRegionTrail "Use generic-lens or generic-optics with 'isMultiRegionTrail' instead." #-}

-- | Specifies whether the trail is an organization trail.
--
-- /Note:/ Consider using 'isOrganizationTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tIsOrganizationTrail :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tIsOrganizationTrail = Lens.field @"isOrganizationTrail"
{-# DEPRECATED tIsOrganizationTrail "Use generic-lens or generic-optics with 'isOrganizationTrail' instead." #-}

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKmsKeyId :: Lens.Lens' Trail (Core.Maybe Types.KmsKeyId)
tKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED tKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies whether log file validation is enabled.
--
-- /Note:/ Consider using 'logFileValidationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLogFileValidationEnabled :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tLogFileValidationEnabled = Lens.field @"logFileValidationEnabled"
{-# DEPRECATED tLogFileValidationEnabled "Use generic-lens or generic-optics with 'logFileValidationEnabled' instead." #-}

-- | Name of the trail set by calling 'CreateTrail' . The maximum length is 128 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Trail (Core.Maybe Types.Name)
tName = Lens.field @"name"
{-# DEPRECATED tName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tS3BucketName :: Lens.Lens' Trail (Core.Maybe Types.S3BucketName)
tS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED tS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .The maximum length is 200 characters.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tS3KeyPrefix :: Lens.Lens' Trail (Core.Maybe Types.S3KeyPrefix)
tS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# DEPRECATED tS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSnsTopicARN :: Lens.Lens' Trail (Core.Maybe Types.SnsTopicARN)
tSnsTopicARN = Lens.field @"snsTopicARN"
{-# DEPRECATED tSnsTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | This field is no longer in use. Use SnsTopicARN.
--
-- /Note:/ Consider using 'snsTopicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSnsTopicName :: Lens.Lens' Trail (Core.Maybe Types.SnsTopicName)
tSnsTopicName = Lens.field @"snsTopicName"
{-# DEPRECATED tSnsTopicName "Use generic-lens or generic-optics with 'snsTopicName' instead." #-}

-- | Specifies the ARN of the trail. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrailARN :: Lens.Lens' Trail (Core.Maybe Types.TrailARN)
tTrailARN = Lens.field @"trailARN"
{-# DEPRECATED tTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

instance Core.FromJSON Trail where
  parseJSON =
    Core.withObject "Trail" Core.$
      \x ->
        Trail'
          Core.<$> (x Core..:? "CloudWatchLogsLogGroupArn")
          Core.<*> (x Core..:? "CloudWatchLogsRoleArn")
          Core.<*> (x Core..:? "HasCustomEventSelectors")
          Core.<*> (x Core..:? "HasInsightSelectors")
          Core.<*> (x Core..:? "HomeRegion")
          Core.<*> (x Core..:? "IncludeGlobalServiceEvents")
          Core.<*> (x Core..:? "IsMultiRegionTrail")
          Core.<*> (x Core..:? "IsOrganizationTrail")
          Core.<*> (x Core..:? "KmsKeyId")
          Core.<*> (x Core..:? "LogFileValidationEnabled")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "S3BucketName")
          Core.<*> (x Core..:? "S3KeyPrefix")
          Core.<*> (x Core..:? "SnsTopicARN")
          Core.<*> (x Core..:? "SnsTopicName")
          Core.<*> (x Core..:? "TrailARN")
