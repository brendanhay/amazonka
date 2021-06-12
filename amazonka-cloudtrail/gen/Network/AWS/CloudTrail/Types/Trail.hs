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
-- Module      : Network.AWS.CloudTrail.Types.Trail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.Trail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The settings for a trail.
--
-- /See:/ 'newTrail' smart constructor.
data Trail = Trail'
  { -- | Specifies the ARN of the trail. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    trailARN :: Core.Maybe Core.Text,
    -- | Specifies whether log file validation is enabled.
    logFileValidationEnabled :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail is an organization trail.
    isOrganizationTrail :: Core.Maybe Core.Bool,
    -- | Specifies if the trail has custom event selectors.
    hasCustomEventSelectors :: Core.Maybe Core.Bool,
    -- | This field is no longer in use. Use SnsTopicARN.
    snsTopicName :: Core.Maybe Core.Text,
    -- | Set to __True__ to include AWS API calls from AWS global services such
    -- as IAM. Otherwise, __False__.
    includeGlobalServiceEvents :: Core.Maybe Core.Bool,
    -- | The region in which the trail was created.
    homeRegion :: Core.Maybe Core.Text,
    -- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
    -- The value is a fully specified ARN to a KMS key in the format:
    --
    -- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the
    -- bucket you have designated for log file delivery. For more information,
    -- see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.The
    -- maximum length is 200 characters.
    s3KeyPrefix :: Core.Maybe Core.Text,
    -- | Name of the trail set by calling CreateTrail. The maximum length is 128
    -- characters.
    name :: Core.Maybe Core.Text,
    -- | Specifies an Amazon Resource Name (ARN), a unique identifier that
    -- represents the log group to which CloudTrail logs will be delivered.
    cloudWatchLogsLogGroupArn :: Core.Maybe Core.Text,
    -- | Specifies whether the trail exists only in one region or exists in all
    -- regions.
    isMultiRegionTrail :: Core.Maybe Core.Bool,
    -- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail
    -- files. See
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
    s3BucketName :: Core.Maybe Core.Text,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write
    -- to a user\'s log group.
    cloudWatchLogsRoleArn :: Core.Maybe Core.Text,
    -- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
    -- notifications when log files are delivered. The format of a topic ARN
    -- is:
    --
    -- @arn:aws:sns:us-east-2:123456789012:MyTopic@
    snsTopicARN :: Core.Maybe Core.Text,
    -- | Specifies whether a trail has insight types specified in an
    -- @InsightSelector@ list.
    hasInsightSelectors :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Trail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailARN', 'trail_trailARN' - Specifies the ARN of the trail. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
--
-- 'logFileValidationEnabled', 'trail_logFileValidationEnabled' - Specifies whether log file validation is enabled.
--
-- 'isOrganizationTrail', 'trail_isOrganizationTrail' - Specifies whether the trail is an organization trail.
--
-- 'hasCustomEventSelectors', 'trail_hasCustomEventSelectors' - Specifies if the trail has custom event selectors.
--
-- 'snsTopicName', 'trail_snsTopicName' - This field is no longer in use. Use SnsTopicARN.
--
-- 'includeGlobalServiceEvents', 'trail_includeGlobalServiceEvents' - Set to __True__ to include AWS API calls from AWS global services such
-- as IAM. Otherwise, __False__.
--
-- 'homeRegion', 'trail_homeRegion' - The region in which the trail was created.
--
-- 'kmsKeyId', 'trail_kmsKeyId' - Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- 's3KeyPrefix', 'trail_s3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.The
-- maximum length is 200 characters.
--
-- 'name', 'trail_name' - Name of the trail set by calling CreateTrail. The maximum length is 128
-- characters.
--
-- 'cloudWatchLogsLogGroupArn', 'trail_cloudWatchLogsLogGroupArn' - Specifies an Amazon Resource Name (ARN), a unique identifier that
-- represents the log group to which CloudTrail logs will be delivered.
--
-- 'isMultiRegionTrail', 'trail_isMultiRegionTrail' - Specifies whether the trail exists only in one region or exists in all
-- regions.
--
-- 's3BucketName', 'trail_s3BucketName' - Name of the Amazon S3 bucket into which CloudTrail delivers your trail
-- files. See
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
--
-- 'cloudWatchLogsRoleArn', 'trail_cloudWatchLogsRoleArn' - Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
--
-- 'snsTopicARN', 'trail_snsTopicARN' - Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
-- notifications when log files are delivered. The format of a topic ARN
-- is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
--
-- 'hasInsightSelectors', 'trail_hasInsightSelectors' - Specifies whether a trail has insight types specified in an
-- @InsightSelector@ list.
newTrail ::
  Trail
newTrail =
  Trail'
    { trailARN = Core.Nothing,
      logFileValidationEnabled = Core.Nothing,
      isOrganizationTrail = Core.Nothing,
      hasCustomEventSelectors = Core.Nothing,
      snsTopicName = Core.Nothing,
      includeGlobalServiceEvents = Core.Nothing,
      homeRegion = Core.Nothing,
      kmsKeyId = Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      name = Core.Nothing,
      cloudWatchLogsLogGroupArn = Core.Nothing,
      isMultiRegionTrail = Core.Nothing,
      s3BucketName = Core.Nothing,
      cloudWatchLogsRoleArn = Core.Nothing,
      snsTopicARN = Core.Nothing,
      hasInsightSelectors = Core.Nothing
    }

-- | Specifies the ARN of the trail. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
trail_trailARN :: Lens.Lens' Trail (Core.Maybe Core.Text)
trail_trailARN = Lens.lens (\Trail' {trailARN} -> trailARN) (\s@Trail' {} a -> s {trailARN = a} :: Trail)

-- | Specifies whether log file validation is enabled.
trail_logFileValidationEnabled :: Lens.Lens' Trail (Core.Maybe Core.Bool)
trail_logFileValidationEnabled = Lens.lens (\Trail' {logFileValidationEnabled} -> logFileValidationEnabled) (\s@Trail' {} a -> s {logFileValidationEnabled = a} :: Trail)

-- | Specifies whether the trail is an organization trail.
trail_isOrganizationTrail :: Lens.Lens' Trail (Core.Maybe Core.Bool)
trail_isOrganizationTrail = Lens.lens (\Trail' {isOrganizationTrail} -> isOrganizationTrail) (\s@Trail' {} a -> s {isOrganizationTrail = a} :: Trail)

-- | Specifies if the trail has custom event selectors.
trail_hasCustomEventSelectors :: Lens.Lens' Trail (Core.Maybe Core.Bool)
trail_hasCustomEventSelectors = Lens.lens (\Trail' {hasCustomEventSelectors} -> hasCustomEventSelectors) (\s@Trail' {} a -> s {hasCustomEventSelectors = a} :: Trail)

-- | This field is no longer in use. Use SnsTopicARN.
trail_snsTopicName :: Lens.Lens' Trail (Core.Maybe Core.Text)
trail_snsTopicName = Lens.lens (\Trail' {snsTopicName} -> snsTopicName) (\s@Trail' {} a -> s {snsTopicName = a} :: Trail)

-- | Set to __True__ to include AWS API calls from AWS global services such
-- as IAM. Otherwise, __False__.
trail_includeGlobalServiceEvents :: Lens.Lens' Trail (Core.Maybe Core.Bool)
trail_includeGlobalServiceEvents = Lens.lens (\Trail' {includeGlobalServiceEvents} -> includeGlobalServiceEvents) (\s@Trail' {} a -> s {includeGlobalServiceEvents = a} :: Trail)

-- | The region in which the trail was created.
trail_homeRegion :: Lens.Lens' Trail (Core.Maybe Core.Text)
trail_homeRegion = Lens.lens (\Trail' {homeRegion} -> homeRegion) (\s@Trail' {} a -> s {homeRegion = a} :: Trail)

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
trail_kmsKeyId :: Lens.Lens' Trail (Core.Maybe Core.Text)
trail_kmsKeyId = Lens.lens (\Trail' {kmsKeyId} -> kmsKeyId) (\s@Trail' {} a -> s {kmsKeyId = a} :: Trail)

-- | Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.The
-- maximum length is 200 characters.
trail_s3KeyPrefix :: Lens.Lens' Trail (Core.Maybe Core.Text)
trail_s3KeyPrefix = Lens.lens (\Trail' {s3KeyPrefix} -> s3KeyPrefix) (\s@Trail' {} a -> s {s3KeyPrefix = a} :: Trail)

-- | Name of the trail set by calling CreateTrail. The maximum length is 128
-- characters.
trail_name :: Lens.Lens' Trail (Core.Maybe Core.Text)
trail_name = Lens.lens (\Trail' {name} -> name) (\s@Trail' {} a -> s {name = a} :: Trail)

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that
-- represents the log group to which CloudTrail logs will be delivered.
trail_cloudWatchLogsLogGroupArn :: Lens.Lens' Trail (Core.Maybe Core.Text)
trail_cloudWatchLogsLogGroupArn = Lens.lens (\Trail' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@Trail' {} a -> s {cloudWatchLogsLogGroupArn = a} :: Trail)

-- | Specifies whether the trail exists only in one region or exists in all
-- regions.
trail_isMultiRegionTrail :: Lens.Lens' Trail (Core.Maybe Core.Bool)
trail_isMultiRegionTrail = Lens.lens (\Trail' {isMultiRegionTrail} -> isMultiRegionTrail) (\s@Trail' {} a -> s {isMultiRegionTrail = a} :: Trail)

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail
-- files. See
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
trail_s3BucketName :: Lens.Lens' Trail (Core.Maybe Core.Text)
trail_s3BucketName = Lens.lens (\Trail' {s3BucketName} -> s3BucketName) (\s@Trail' {} a -> s {s3BucketName = a} :: Trail)

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
trail_cloudWatchLogsRoleArn :: Lens.Lens' Trail (Core.Maybe Core.Text)
trail_cloudWatchLogsRoleArn = Lens.lens (\Trail' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@Trail' {} a -> s {cloudWatchLogsRoleArn = a} :: Trail)

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
-- notifications when log files are delivered. The format of a topic ARN
-- is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
trail_snsTopicARN :: Lens.Lens' Trail (Core.Maybe Core.Text)
trail_snsTopicARN = Lens.lens (\Trail' {snsTopicARN} -> snsTopicARN) (\s@Trail' {} a -> s {snsTopicARN = a} :: Trail)

-- | Specifies whether a trail has insight types specified in an
-- @InsightSelector@ list.
trail_hasInsightSelectors :: Lens.Lens' Trail (Core.Maybe Core.Bool)
trail_hasInsightSelectors = Lens.lens (\Trail' {hasInsightSelectors} -> hasInsightSelectors) (\s@Trail' {} a -> s {hasInsightSelectors = a} :: Trail)

instance Core.FromJSON Trail where
  parseJSON =
    Core.withObject
      "Trail"
      ( \x ->
          Trail'
            Core.<$> (x Core..:? "TrailARN")
            Core.<*> (x Core..:? "LogFileValidationEnabled")
            Core.<*> (x Core..:? "IsOrganizationTrail")
            Core.<*> (x Core..:? "HasCustomEventSelectors")
            Core.<*> (x Core..:? "SnsTopicName")
            Core.<*> (x Core..:? "IncludeGlobalServiceEvents")
            Core.<*> (x Core..:? "HomeRegion")
            Core.<*> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..:? "S3KeyPrefix")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "CloudWatchLogsLogGroupArn")
            Core.<*> (x Core..:? "IsMultiRegionTrail")
            Core.<*> (x Core..:? "S3BucketName")
            Core.<*> (x Core..:? "CloudWatchLogsRoleArn")
            Core.<*> (x Core..:? "SnsTopicARN")
            Core.<*> (x Core..:? "HasInsightSelectors")
      )

instance Core.Hashable Trail

instance Core.NFData Trail
