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
    tLogFileValidationEnabled,
    tTrailARN,
    tS3KeyPrefix,
    tHasInsightSelectors,
    tSNSTopicARN,
    tSNSTopicName,
    tCloudWatchLogsLogGroupARN,
    tKMSKeyId,
    tHomeRegion,
    tName,
    tIncludeGlobalServiceEvents,
    tHasCustomEventSelectors,
    tIsOrganizationTrail,
    tCloudWatchLogsRoleARN,
    tS3BucketName,
    tIsMultiRegionTrail,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The settings for a trail.
--
-- /See:/ 'mkTrail' smart constructor.
data Trail = Trail'
  { -- | Specifies whether log file validation is enabled.
    logFileValidationEnabled :: Lude.Maybe Lude.Bool,
    -- | Specifies the ARN of the trail. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    trailARN :: Lude.Maybe Lude.Text,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .The maximum length is 200 characters.
    s3KeyPrefix :: Lude.Maybe Lude.Text,
    -- | Specifies whether a trail has insight types specified in an @InsightSelector@ list.
    hasInsightSelectors :: Lude.Maybe Lude.Bool,
    -- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
    --
    -- @arn:aws:sns:us-east-2:123456789012:MyTopic@
    snsTopicARN :: Lude.Maybe Lude.Text,
    -- | This field is no longer in use. Use SnsTopicARN.
    snsTopicName :: Lude.Maybe Lude.Text,
    -- | Specifies an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered.
    cloudWatchLogsLogGroupARN :: Lude.Maybe Lude.Text,
    -- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
    --
    -- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The region in which the trail was created.
    homeRegion :: Lude.Maybe Lude.Text,
    -- | Name of the trail set by calling 'CreateTrail' . The maximum length is 128 characters.
    name :: Lude.Maybe Lude.Text,
    -- | Set to __True__ to include AWS API calls from AWS global services such as IAM. Otherwise, __False__ .
    includeGlobalServiceEvents :: Lude.Maybe Lude.Bool,
    -- | Specifies if the trail has custom event selectors.
    hasCustomEventSelectors :: Lude.Maybe Lude.Bool,
    -- | Specifies whether the trail is an organization trail.
    isOrganizationTrail :: Lude.Maybe Lude.Bool,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
    cloudWatchLogsRoleARN :: Lude.Maybe Lude.Text,
    -- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
    s3BucketName :: Lude.Maybe Lude.Text,
    -- | Specifies whether the trail exists only in one region or exists in all regions.
    isMultiRegionTrail :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Trail' with the minimum fields required to make a request.
--
-- * 'logFileValidationEnabled' - Specifies whether log file validation is enabled.
-- * 'trailARN' - Specifies the ARN of the trail. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
-- * 's3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .The maximum length is 200 characters.
-- * 'hasInsightSelectors' - Specifies whether a trail has insight types specified in an @InsightSelector@ list.
-- * 'snsTopicARN' - Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
-- * 'snsTopicName' - This field is no longer in use. Use SnsTopicARN.
-- * 'cloudWatchLogsLogGroupARN' - Specifies an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered.
-- * 'kmsKeyId' - Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
-- * 'homeRegion' - The region in which the trail was created.
-- * 'name' - Name of the trail set by calling 'CreateTrail' . The maximum length is 128 characters.
-- * 'includeGlobalServiceEvents' - Set to __True__ to include AWS API calls from AWS global services such as IAM. Otherwise, __False__ .
-- * 'hasCustomEventSelectors' - Specifies if the trail has custom event selectors.
-- * 'isOrganizationTrail' - Specifies whether the trail is an organization trail.
-- * 'cloudWatchLogsRoleARN' - Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
-- * 's3BucketName' - Name of the Amazon S3 bucket into which CloudTrail delivers your trail files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
-- * 'isMultiRegionTrail' - Specifies whether the trail exists only in one region or exists in all regions.
mkTrail ::
  Trail
mkTrail =
  Trail'
    { logFileValidationEnabled = Lude.Nothing,
      trailARN = Lude.Nothing,
      s3KeyPrefix = Lude.Nothing,
      hasInsightSelectors = Lude.Nothing,
      snsTopicARN = Lude.Nothing,
      snsTopicName = Lude.Nothing,
      cloudWatchLogsLogGroupARN = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      homeRegion = Lude.Nothing,
      name = Lude.Nothing,
      includeGlobalServiceEvents = Lude.Nothing,
      hasCustomEventSelectors = Lude.Nothing,
      isOrganizationTrail = Lude.Nothing,
      cloudWatchLogsRoleARN = Lude.Nothing,
      s3BucketName = Lude.Nothing,
      isMultiRegionTrail = Lude.Nothing
    }

-- | Specifies whether log file validation is enabled.
--
-- /Note:/ Consider using 'logFileValidationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLogFileValidationEnabled :: Lens.Lens' Trail (Lude.Maybe Lude.Bool)
tLogFileValidationEnabled = Lens.lens (logFileValidationEnabled :: Trail -> Lude.Maybe Lude.Bool) (\s a -> s {logFileValidationEnabled = a} :: Trail)
{-# DEPRECATED tLogFileValidationEnabled "Use generic-lens or generic-optics with 'logFileValidationEnabled' instead." #-}

-- | Specifies the ARN of the trail. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrailARN :: Lens.Lens' Trail (Lude.Maybe Lude.Text)
tTrailARN = Lens.lens (trailARN :: Trail -> Lude.Maybe Lude.Text) (\s a -> s {trailARN = a} :: Trail)
{-# DEPRECATED tTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .The maximum length is 200 characters.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tS3KeyPrefix :: Lens.Lens' Trail (Lude.Maybe Lude.Text)
tS3KeyPrefix = Lens.lens (s3KeyPrefix :: Trail -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: Trail)
{-# DEPRECATED tS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | Specifies whether a trail has insight types specified in an @InsightSelector@ list.
--
-- /Note:/ Consider using 'hasInsightSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHasInsightSelectors :: Lens.Lens' Trail (Lude.Maybe Lude.Bool)
tHasInsightSelectors = Lens.lens (hasInsightSelectors :: Trail -> Lude.Maybe Lude.Bool) (\s a -> s {hasInsightSelectors = a} :: Trail)
{-# DEPRECATED tHasInsightSelectors "Use generic-lens or generic-optics with 'hasInsightSelectors' instead." #-}

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSNSTopicARN :: Lens.Lens' Trail (Lude.Maybe Lude.Text)
tSNSTopicARN = Lens.lens (snsTopicARN :: Trail -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: Trail)
{-# DEPRECATED tSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | This field is no longer in use. Use SnsTopicARN.
--
-- /Note:/ Consider using 'snsTopicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSNSTopicName :: Lens.Lens' Trail (Lude.Maybe Lude.Text)
tSNSTopicName = Lens.lens (snsTopicName :: Trail -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicName = a} :: Trail)
{-# DEPRECATED tSNSTopicName "Use generic-lens or generic-optics with 'snsTopicName' instead." #-}

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCloudWatchLogsLogGroupARN :: Lens.Lens' Trail (Lude.Maybe Lude.Text)
tCloudWatchLogsLogGroupARN = Lens.lens (cloudWatchLogsLogGroupARN :: Trail -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsLogGroupARN = a} :: Trail)
{-# DEPRECATED tCloudWatchLogsLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupARN' instead." #-}

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKMSKeyId :: Lens.Lens' Trail (Lude.Maybe Lude.Text)
tKMSKeyId = Lens.lens (kmsKeyId :: Trail -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: Trail)
{-# DEPRECATED tKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The region in which the trail was created.
--
-- /Note:/ Consider using 'homeRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHomeRegion :: Lens.Lens' Trail (Lude.Maybe Lude.Text)
tHomeRegion = Lens.lens (homeRegion :: Trail -> Lude.Maybe Lude.Text) (\s a -> s {homeRegion = a} :: Trail)
{-# DEPRECATED tHomeRegion "Use generic-lens or generic-optics with 'homeRegion' instead." #-}

-- | Name of the trail set by calling 'CreateTrail' . The maximum length is 128 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Trail (Lude.Maybe Lude.Text)
tName = Lens.lens (name :: Trail -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Trail)
{-# DEPRECATED tName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Set to __True__ to include AWS API calls from AWS global services such as IAM. Otherwise, __False__ .
--
-- /Note:/ Consider using 'includeGlobalServiceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tIncludeGlobalServiceEvents :: Lens.Lens' Trail (Lude.Maybe Lude.Bool)
tIncludeGlobalServiceEvents = Lens.lens (includeGlobalServiceEvents :: Trail -> Lude.Maybe Lude.Bool) (\s a -> s {includeGlobalServiceEvents = a} :: Trail)
{-# DEPRECATED tIncludeGlobalServiceEvents "Use generic-lens or generic-optics with 'includeGlobalServiceEvents' instead." #-}

-- | Specifies if the trail has custom event selectors.
--
-- /Note:/ Consider using 'hasCustomEventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHasCustomEventSelectors :: Lens.Lens' Trail (Lude.Maybe Lude.Bool)
tHasCustomEventSelectors = Lens.lens (hasCustomEventSelectors :: Trail -> Lude.Maybe Lude.Bool) (\s a -> s {hasCustomEventSelectors = a} :: Trail)
{-# DEPRECATED tHasCustomEventSelectors "Use generic-lens or generic-optics with 'hasCustomEventSelectors' instead." #-}

-- | Specifies whether the trail is an organization trail.
--
-- /Note:/ Consider using 'isOrganizationTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tIsOrganizationTrail :: Lens.Lens' Trail (Lude.Maybe Lude.Bool)
tIsOrganizationTrail = Lens.lens (isOrganizationTrail :: Trail -> Lude.Maybe Lude.Bool) (\s a -> s {isOrganizationTrail = a} :: Trail)
{-# DEPRECATED tIsOrganizationTrail "Use generic-lens or generic-optics with 'isOrganizationTrail' instead." #-}

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCloudWatchLogsRoleARN :: Lens.Lens' Trail (Lude.Maybe Lude.Text)
tCloudWatchLogsRoleARN = Lens.lens (cloudWatchLogsRoleARN :: Trail -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsRoleARN = a} :: Trail)
{-# DEPRECATED tCloudWatchLogsRoleARN "Use generic-lens or generic-optics with 'cloudWatchLogsRoleARN' instead." #-}

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tS3BucketName :: Lens.Lens' Trail (Lude.Maybe Lude.Text)
tS3BucketName = Lens.lens (s3BucketName :: Trail -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: Trail)
{-# DEPRECATED tS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Specifies whether the trail exists only in one region or exists in all regions.
--
-- /Note:/ Consider using 'isMultiRegionTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tIsMultiRegionTrail :: Lens.Lens' Trail (Lude.Maybe Lude.Bool)
tIsMultiRegionTrail = Lens.lens (isMultiRegionTrail :: Trail -> Lude.Maybe Lude.Bool) (\s a -> s {isMultiRegionTrail = a} :: Trail)
{-# DEPRECATED tIsMultiRegionTrail "Use generic-lens or generic-optics with 'isMultiRegionTrail' instead." #-}

instance Lude.FromJSON Trail where
  parseJSON =
    Lude.withObject
      "Trail"
      ( \x ->
          Trail'
            Lude.<$> (x Lude..:? "LogFileValidationEnabled")
            Lude.<*> (x Lude..:? "TrailARN")
            Lude.<*> (x Lude..:? "S3KeyPrefix")
            Lude.<*> (x Lude..:? "HasInsightSelectors")
            Lude.<*> (x Lude..:? "SnsTopicARN")
            Lude.<*> (x Lude..:? "SnsTopicName")
            Lude.<*> (x Lude..:? "CloudWatchLogsLogGroupArn")
            Lude.<*> (x Lude..:? "KmsKeyId")
            Lude.<*> (x Lude..:? "HomeRegion")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "IncludeGlobalServiceEvents")
            Lude.<*> (x Lude..:? "HasCustomEventSelectors")
            Lude.<*> (x Lude..:? "IsOrganizationTrail")
            Lude.<*> (x Lude..:? "CloudWatchLogsRoleArn")
            Lude.<*> (x Lude..:? "S3BucketName")
            Lude.<*> (x Lude..:? "IsMultiRegionTrail")
      )
