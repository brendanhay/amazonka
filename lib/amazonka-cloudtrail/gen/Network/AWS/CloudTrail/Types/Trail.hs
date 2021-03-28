{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Trail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.Trail
  ( Trail (..)
  -- * Smart constructor
  , mkTrail
  -- * Lenses
  , tCloudWatchLogsLogGroupArn
  , tCloudWatchLogsRoleArn
  , tHasCustomEventSelectors
  , tHasInsightSelectors
  , tHomeRegion
  , tIncludeGlobalServiceEvents
  , tIsMultiRegionTrail
  , tIsOrganizationTrail
  , tKmsKeyId
  , tLogFileValidationEnabled
  , tName
  , tS3BucketName
  , tS3KeyPrefix
  , tSnsTopicARN
  , tSnsTopicName
  , tTrailARN
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The settings for a trail.
--
-- /See:/ 'mkTrail' smart constructor.
data Trail = Trail'
  { cloudWatchLogsLogGroupArn :: Core.Maybe Core.Text
    -- ^ Specifies an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered.
  , cloudWatchLogsRoleArn :: Core.Maybe Core.Text
    -- ^ Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
  , hasCustomEventSelectors :: Core.Maybe Core.Bool
    -- ^ Specifies if the trail has custom event selectors.
  , hasInsightSelectors :: Core.Maybe Core.Bool
    -- ^ Specifies whether a trail has insight types specified in an @InsightSelector@ list.
  , homeRegion :: Core.Maybe Core.Text
    -- ^ The region in which the trail was created.
  , includeGlobalServiceEvents :: Core.Maybe Core.Bool
    -- ^ Set to __True__ to include AWS API calls from AWS global services such as IAM. Otherwise, __False__ .
  , isMultiRegionTrail :: Core.Maybe Core.Bool
    -- ^ Specifies whether the trail exists only in one region or exists in all regions.
  , isOrganizationTrail :: Core.Maybe Core.Bool
    -- ^ Specifies whether the trail is an organization trail.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@ 
  , logFileValidationEnabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether log file validation is enabled.
  , name :: Core.Maybe Core.Text
    -- ^ Name of the trail set by calling 'CreateTrail' . The maximum length is 128 characters.
  , s3BucketName :: Core.Maybe Core.Text
    -- ^ Name of the Amazon S3 bucket into which CloudTrail delivers your trail files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
  , s3KeyPrefix :: Core.Maybe Core.Text
    -- ^ Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .The maximum length is 200 characters.
  , snsTopicARN :: Core.Maybe Core.Text
    -- ^ Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@ 
  , snsTopicName :: Core.Maybe Core.Text
    -- ^ This field is no longer in use. Use SnsTopicARN.
  , trailARN :: Core.Maybe Core.Text
    -- ^ Specifies the ARN of the trail. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Trail' value with any optional fields omitted.
mkTrail
    :: Trail
mkTrail
  = Trail'{cloudWatchLogsLogGroupArn = Core.Nothing,
           cloudWatchLogsRoleArn = Core.Nothing,
           hasCustomEventSelectors = Core.Nothing,
           hasInsightSelectors = Core.Nothing, homeRegion = Core.Nothing,
           includeGlobalServiceEvents = Core.Nothing,
           isMultiRegionTrail = Core.Nothing,
           isOrganizationTrail = Core.Nothing, kmsKeyId = Core.Nothing,
           logFileValidationEnabled = Core.Nothing, name = Core.Nothing,
           s3BucketName = Core.Nothing, s3KeyPrefix = Core.Nothing,
           snsTopicARN = Core.Nothing, snsTopicName = Core.Nothing,
           trailARN = Core.Nothing}

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCloudWatchLogsLogGroupArn :: Lens.Lens' Trail (Core.Maybe Core.Text)
tCloudWatchLogsLogGroupArn = Lens.field @"cloudWatchLogsLogGroupArn"
{-# INLINEABLE tCloudWatchLogsLogGroupArn #-}
{-# DEPRECATED cloudWatchLogsLogGroupArn "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupArn' instead"  #-}

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCloudWatchLogsRoleArn :: Lens.Lens' Trail (Core.Maybe Core.Text)
tCloudWatchLogsRoleArn = Lens.field @"cloudWatchLogsRoleArn"
{-# INLINEABLE tCloudWatchLogsRoleArn #-}
{-# DEPRECATED cloudWatchLogsRoleArn "Use generic-lens or generic-optics with 'cloudWatchLogsRoleArn' instead"  #-}

-- | Specifies if the trail has custom event selectors.
--
-- /Note:/ Consider using 'hasCustomEventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHasCustomEventSelectors :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tHasCustomEventSelectors = Lens.field @"hasCustomEventSelectors"
{-# INLINEABLE tHasCustomEventSelectors #-}
{-# DEPRECATED hasCustomEventSelectors "Use generic-lens or generic-optics with 'hasCustomEventSelectors' instead"  #-}

-- | Specifies whether a trail has insight types specified in an @InsightSelector@ list.
--
-- /Note:/ Consider using 'hasInsightSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHasInsightSelectors :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tHasInsightSelectors = Lens.field @"hasInsightSelectors"
{-# INLINEABLE tHasInsightSelectors #-}
{-# DEPRECATED hasInsightSelectors "Use generic-lens or generic-optics with 'hasInsightSelectors' instead"  #-}

-- | The region in which the trail was created.
--
-- /Note:/ Consider using 'homeRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHomeRegion :: Lens.Lens' Trail (Core.Maybe Core.Text)
tHomeRegion = Lens.field @"homeRegion"
{-# INLINEABLE tHomeRegion #-}
{-# DEPRECATED homeRegion "Use generic-lens or generic-optics with 'homeRegion' instead"  #-}

-- | Set to __True__ to include AWS API calls from AWS global services such as IAM. Otherwise, __False__ .
--
-- /Note:/ Consider using 'includeGlobalServiceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tIncludeGlobalServiceEvents :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tIncludeGlobalServiceEvents = Lens.field @"includeGlobalServiceEvents"
{-# INLINEABLE tIncludeGlobalServiceEvents #-}
{-# DEPRECATED includeGlobalServiceEvents "Use generic-lens or generic-optics with 'includeGlobalServiceEvents' instead"  #-}

-- | Specifies whether the trail exists only in one region or exists in all regions.
--
-- /Note:/ Consider using 'isMultiRegionTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tIsMultiRegionTrail :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tIsMultiRegionTrail = Lens.field @"isMultiRegionTrail"
{-# INLINEABLE tIsMultiRegionTrail #-}
{-# DEPRECATED isMultiRegionTrail "Use generic-lens or generic-optics with 'isMultiRegionTrail' instead"  #-}

-- | Specifies whether the trail is an organization trail.
--
-- /Note:/ Consider using 'isOrganizationTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tIsOrganizationTrail :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tIsOrganizationTrail = Lens.field @"isOrganizationTrail"
{-# INLINEABLE tIsOrganizationTrail #-}
{-# DEPRECATED isOrganizationTrail "Use generic-lens or generic-optics with 'isOrganizationTrail' instead"  #-}

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@ 
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKmsKeyId :: Lens.Lens' Trail (Core.Maybe Core.Text)
tKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE tKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | Specifies whether log file validation is enabled.
--
-- /Note:/ Consider using 'logFileValidationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLogFileValidationEnabled :: Lens.Lens' Trail (Core.Maybe Core.Bool)
tLogFileValidationEnabled = Lens.field @"logFileValidationEnabled"
{-# INLINEABLE tLogFileValidationEnabled #-}
{-# DEPRECATED logFileValidationEnabled "Use generic-lens or generic-optics with 'logFileValidationEnabled' instead"  #-}

-- | Name of the trail set by calling 'CreateTrail' . The maximum length is 128 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Trail (Core.Maybe Core.Text)
tName = Lens.field @"name"
{-# INLINEABLE tName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tS3BucketName :: Lens.Lens' Trail (Core.Maybe Core.Text)
tS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE tS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .The maximum length is 200 characters.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tS3KeyPrefix :: Lens.Lens' Trail (Core.Maybe Core.Text)
tS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# INLINEABLE tS3KeyPrefix #-}
{-# DEPRECATED s3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead"  #-}

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@ 
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSnsTopicARN :: Lens.Lens' Trail (Core.Maybe Core.Text)
tSnsTopicARN = Lens.field @"snsTopicARN"
{-# INLINEABLE tSnsTopicARN #-}
{-# DEPRECATED snsTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead"  #-}

-- | This field is no longer in use. Use SnsTopicARN.
--
-- /Note:/ Consider using 'snsTopicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSnsTopicName :: Lens.Lens' Trail (Core.Maybe Core.Text)
tSnsTopicName = Lens.field @"snsTopicName"
{-# INLINEABLE tSnsTopicName #-}
{-# DEPRECATED snsTopicName "Use generic-lens or generic-optics with 'snsTopicName' instead"  #-}

-- | Specifies the ARN of the trail. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrailARN :: Lens.Lens' Trail (Core.Maybe Core.Text)
tTrailARN = Lens.field @"trailARN"
{-# INLINEABLE tTrailARN #-}
{-# DEPRECATED trailARN "Use generic-lens or generic-optics with 'trailARN' instead"  #-}

instance Core.FromJSON Trail where
        parseJSON
          = Core.withObject "Trail" Core.$
              \ x ->
                Trail' Core.<$>
                  (x Core..:? "CloudWatchLogsLogGroupArn") Core.<*>
                    x Core..:? "CloudWatchLogsRoleArn"
                    Core.<*> x Core..:? "HasCustomEventSelectors"
                    Core.<*> x Core..:? "HasInsightSelectors"
                    Core.<*> x Core..:? "HomeRegion"
                    Core.<*> x Core..:? "IncludeGlobalServiceEvents"
                    Core.<*> x Core..:? "IsMultiRegionTrail"
                    Core.<*> x Core..:? "IsOrganizationTrail"
                    Core.<*> x Core..:? "KmsKeyId"
                    Core.<*> x Core..:? "LogFileValidationEnabled"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "S3BucketName"
                    Core.<*> x Core..:? "S3KeyPrefix"
                    Core.<*> x Core..:? "SnsTopicARN"
                    Core.<*> x Core..:? "SnsTopicName"
                    Core.<*> x Core..:? "TrailARN"
