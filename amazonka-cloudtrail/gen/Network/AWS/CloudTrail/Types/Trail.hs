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
-- Module      : Network.AWS.CloudTrail.Types.Trail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.Trail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The settings for a trail.
--
-- /See:/ 'newTrail' smart constructor.
data Trail = Trail'
  { -- | Specifies the ARN of the trail. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    trailARN :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether log file validation is enabled.
    logFileValidationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the trail is an organization trail.
    isOrganizationTrail :: Prelude.Maybe Prelude.Bool,
    -- | Specifies if the trail has custom event selectors.
    hasCustomEventSelectors :: Prelude.Maybe Prelude.Bool,
    -- | This field is no longer in use. Use SnsTopicARN.
    snsTopicName :: Prelude.Maybe Prelude.Text,
    -- | Set to __True__ to include AWS API calls from AWS global services such
    -- as IAM. Otherwise, __False__.
    includeGlobalServiceEvents :: Prelude.Maybe Prelude.Bool,
    -- | The region in which the trail was created.
    homeRegion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
    -- The value is a fully specified ARN to a KMS key in the format:
    --
    -- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the
    -- bucket you have designated for log file delivery. For more information,
    -- see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.The
    -- maximum length is 200 characters.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Name of the trail set by calling CreateTrail. The maximum length is 128
    -- characters.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies an Amazon Resource Name (ARN), a unique identifier that
    -- represents the log group to which CloudTrail logs will be delivered.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the trail exists only in one region or exists in all
    -- regions.
    isMultiRegionTrail :: Prelude.Maybe Prelude.Bool,
    -- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail
    -- files. See
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write
    -- to a user\'s log group.
    cloudWatchLogsRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
    -- notifications when log files are delivered. The format of a topic ARN
    -- is:
    --
    -- @arn:aws:sns:us-east-2:123456789012:MyTopic@
    snsTopicARN :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a trail has insight types specified in an
    -- @InsightSelector@ list.
    hasInsightSelectors :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { trailARN = Prelude.Nothing,
      logFileValidationEnabled = Prelude.Nothing,
      isOrganizationTrail = Prelude.Nothing,
      hasCustomEventSelectors = Prelude.Nothing,
      snsTopicName = Prelude.Nothing,
      includeGlobalServiceEvents = Prelude.Nothing,
      homeRegion = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      name = Prelude.Nothing,
      cloudWatchLogsLogGroupArn = Prelude.Nothing,
      isMultiRegionTrail = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      cloudWatchLogsRoleArn = Prelude.Nothing,
      snsTopicARN = Prelude.Nothing,
      hasInsightSelectors = Prelude.Nothing
    }

-- | Specifies the ARN of the trail. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
trail_trailARN :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_trailARN = Lens.lens (\Trail' {trailARN} -> trailARN) (\s@Trail' {} a -> s {trailARN = a} :: Trail)

-- | Specifies whether log file validation is enabled.
trail_logFileValidationEnabled :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_logFileValidationEnabled = Lens.lens (\Trail' {logFileValidationEnabled} -> logFileValidationEnabled) (\s@Trail' {} a -> s {logFileValidationEnabled = a} :: Trail)

-- | Specifies whether the trail is an organization trail.
trail_isOrganizationTrail :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_isOrganizationTrail = Lens.lens (\Trail' {isOrganizationTrail} -> isOrganizationTrail) (\s@Trail' {} a -> s {isOrganizationTrail = a} :: Trail)

-- | Specifies if the trail has custom event selectors.
trail_hasCustomEventSelectors :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_hasCustomEventSelectors = Lens.lens (\Trail' {hasCustomEventSelectors} -> hasCustomEventSelectors) (\s@Trail' {} a -> s {hasCustomEventSelectors = a} :: Trail)

-- | This field is no longer in use. Use SnsTopicARN.
trail_snsTopicName :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_snsTopicName = Lens.lens (\Trail' {snsTopicName} -> snsTopicName) (\s@Trail' {} a -> s {snsTopicName = a} :: Trail)

-- | Set to __True__ to include AWS API calls from AWS global services such
-- as IAM. Otherwise, __False__.
trail_includeGlobalServiceEvents :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_includeGlobalServiceEvents = Lens.lens (\Trail' {includeGlobalServiceEvents} -> includeGlobalServiceEvents) (\s@Trail' {} a -> s {includeGlobalServiceEvents = a} :: Trail)

-- | The region in which the trail was created.
trail_homeRegion :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_homeRegion = Lens.lens (\Trail' {homeRegion} -> homeRegion) (\s@Trail' {} a -> s {homeRegion = a} :: Trail)

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
trail_kmsKeyId :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_kmsKeyId = Lens.lens (\Trail' {kmsKeyId} -> kmsKeyId) (\s@Trail' {} a -> s {kmsKeyId = a} :: Trail)

-- | Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.The
-- maximum length is 200 characters.
trail_s3KeyPrefix :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_s3KeyPrefix = Lens.lens (\Trail' {s3KeyPrefix} -> s3KeyPrefix) (\s@Trail' {} a -> s {s3KeyPrefix = a} :: Trail)

-- | Name of the trail set by calling CreateTrail. The maximum length is 128
-- characters.
trail_name :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_name = Lens.lens (\Trail' {name} -> name) (\s@Trail' {} a -> s {name = a} :: Trail)

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that
-- represents the log group to which CloudTrail logs will be delivered.
trail_cloudWatchLogsLogGroupArn :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_cloudWatchLogsLogGroupArn = Lens.lens (\Trail' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@Trail' {} a -> s {cloudWatchLogsLogGroupArn = a} :: Trail)

-- | Specifies whether the trail exists only in one region or exists in all
-- regions.
trail_isMultiRegionTrail :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_isMultiRegionTrail = Lens.lens (\Trail' {isMultiRegionTrail} -> isMultiRegionTrail) (\s@Trail' {} a -> s {isMultiRegionTrail = a} :: Trail)

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail
-- files. See
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
trail_s3BucketName :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_s3BucketName = Lens.lens (\Trail' {s3BucketName} -> s3BucketName) (\s@Trail' {} a -> s {s3BucketName = a} :: Trail)

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
trail_cloudWatchLogsRoleArn :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_cloudWatchLogsRoleArn = Lens.lens (\Trail' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@Trail' {} a -> s {cloudWatchLogsRoleArn = a} :: Trail)

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
-- notifications when log files are delivered. The format of a topic ARN
-- is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
trail_snsTopicARN :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_snsTopicARN = Lens.lens (\Trail' {snsTopicARN} -> snsTopicARN) (\s@Trail' {} a -> s {snsTopicARN = a} :: Trail)

-- | Specifies whether a trail has insight types specified in an
-- @InsightSelector@ list.
trail_hasInsightSelectors :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_hasInsightSelectors = Lens.lens (\Trail' {hasInsightSelectors} -> hasInsightSelectors) (\s@Trail' {} a -> s {hasInsightSelectors = a} :: Trail)

instance Prelude.FromJSON Trail where
  parseJSON =
    Prelude.withObject
      "Trail"
      ( \x ->
          Trail'
            Prelude.<$> (x Prelude..:? "TrailARN")
            Prelude.<*> (x Prelude..:? "LogFileValidationEnabled")
            Prelude.<*> (x Prelude..:? "IsOrganizationTrail")
            Prelude.<*> (x Prelude..:? "HasCustomEventSelectors")
            Prelude.<*> (x Prelude..:? "SnsTopicName")
            Prelude.<*> (x Prelude..:? "IncludeGlobalServiceEvents")
            Prelude.<*> (x Prelude..:? "HomeRegion")
            Prelude.<*> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..:? "S3KeyPrefix")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "CloudWatchLogsLogGroupArn")
            Prelude.<*> (x Prelude..:? "IsMultiRegionTrail")
            Prelude.<*> (x Prelude..:? "S3BucketName")
            Prelude.<*> (x Prelude..:? "CloudWatchLogsRoleArn")
            Prelude.<*> (x Prelude..:? "SnsTopicARN")
            Prelude.<*> (x Prelude..:? "HasInsightSelectors")
      )

instance Prelude.Hashable Trail

instance Prelude.NFData Trail
