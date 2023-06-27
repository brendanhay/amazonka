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
-- Module      : Amazonka.CloudTrail.Types.Trail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.Trail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings for a trail.
--
-- /See:/ 'newTrail' smart constructor.
data Trail = Trail'
  { -- | Specifies an Amazon Resource Name (ARN), a unique identifier that
    -- represents the log group to which CloudTrail logs will be delivered.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write
    -- to a user\'s log group.
    cloudWatchLogsRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies if the trail has custom event selectors.
    hasCustomEventSelectors :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a trail has insight types specified in an
    -- @InsightSelector@ list.
    hasInsightSelectors :: Prelude.Maybe Prelude.Bool,
    -- | The Region in which the trail was created.
    homeRegion :: Prelude.Maybe Prelude.Text,
    -- | Set to __True__ to include Amazon Web Services API calls from Amazon Web
    -- Services global services such as IAM. Otherwise, __False__.
    includeGlobalServiceEvents :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the trail exists only in one Region or exists in all
    -- Regions.
    isMultiRegionTrail :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the trail is an organization trail.
    isOrganizationTrail :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
    -- The value is a fully specified ARN to a KMS key in the following format.
    --
    -- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether log file validation is enabled.
    logFileValidationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Name of the trail set by calling CreateTrail. The maximum length is 128
    -- characters.
    name :: Prelude.Maybe Prelude.Text,
    -- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail
    -- files. See
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the
    -- bucket you have designated for log file delivery. For more information,
    -- see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
    -- The maximum length is 200 characters.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
    -- notifications when log files are delivered. The following is the format
    -- of a topic ARN.
    --
    -- @arn:aws:sns:us-east-2:123456789012:MyTopic@
    snsTopicARN :: Prelude.Maybe Prelude.Text,
    -- | This field is no longer in use. Use @SnsTopicARN@.
    snsTopicName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ARN of the trail. The following is the format of a trail
    -- ARN.
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    trailARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Trail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogsLogGroupArn', 'trail_cloudWatchLogsLogGroupArn' - Specifies an Amazon Resource Name (ARN), a unique identifier that
-- represents the log group to which CloudTrail logs will be delivered.
--
-- 'cloudWatchLogsRoleArn', 'trail_cloudWatchLogsRoleArn' - Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
--
-- 'hasCustomEventSelectors', 'trail_hasCustomEventSelectors' - Specifies if the trail has custom event selectors.
--
-- 'hasInsightSelectors', 'trail_hasInsightSelectors' - Specifies whether a trail has insight types specified in an
-- @InsightSelector@ list.
--
-- 'homeRegion', 'trail_homeRegion' - The Region in which the trail was created.
--
-- 'includeGlobalServiceEvents', 'trail_includeGlobalServiceEvents' - Set to __True__ to include Amazon Web Services API calls from Amazon Web
-- Services global services such as IAM. Otherwise, __False__.
--
-- 'isMultiRegionTrail', 'trail_isMultiRegionTrail' - Specifies whether the trail exists only in one Region or exists in all
-- Regions.
--
-- 'isOrganizationTrail', 'trail_isOrganizationTrail' - Specifies whether the trail is an organization trail.
--
-- 'kmsKeyId', 'trail_kmsKeyId' - Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the following format.
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- 'logFileValidationEnabled', 'trail_logFileValidationEnabled' - Specifies whether log file validation is enabled.
--
-- 'name', 'trail_name' - Name of the trail set by calling CreateTrail. The maximum length is 128
-- characters.
--
-- 's3BucketName', 'trail_s3BucketName' - Name of the Amazon S3 bucket into which CloudTrail delivers your trail
-- files. See
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
--
-- 's3KeyPrefix', 'trail_s3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
-- The maximum length is 200 characters.
--
-- 'snsTopicARN', 'trail_snsTopicARN' - Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
-- notifications when log files are delivered. The following is the format
-- of a topic ARN.
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
--
-- 'snsTopicName', 'trail_snsTopicName' - This field is no longer in use. Use @SnsTopicARN@.
--
-- 'trailARN', 'trail_trailARN' - Specifies the ARN of the trail. The following is the format of a trail
-- ARN.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newTrail ::
  Trail
newTrail =
  Trail'
    { cloudWatchLogsLogGroupArn = Prelude.Nothing,
      cloudWatchLogsRoleArn = Prelude.Nothing,
      hasCustomEventSelectors = Prelude.Nothing,
      hasInsightSelectors = Prelude.Nothing,
      homeRegion = Prelude.Nothing,
      includeGlobalServiceEvents = Prelude.Nothing,
      isMultiRegionTrail = Prelude.Nothing,
      isOrganizationTrail = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      logFileValidationEnabled = Prelude.Nothing,
      name = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      snsTopicARN = Prelude.Nothing,
      snsTopicName = Prelude.Nothing,
      trailARN = Prelude.Nothing
    }

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that
-- represents the log group to which CloudTrail logs will be delivered.
trail_cloudWatchLogsLogGroupArn :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_cloudWatchLogsLogGroupArn = Lens.lens (\Trail' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@Trail' {} a -> s {cloudWatchLogsLogGroupArn = a} :: Trail)

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
trail_cloudWatchLogsRoleArn :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_cloudWatchLogsRoleArn = Lens.lens (\Trail' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@Trail' {} a -> s {cloudWatchLogsRoleArn = a} :: Trail)

-- | Specifies if the trail has custom event selectors.
trail_hasCustomEventSelectors :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_hasCustomEventSelectors = Lens.lens (\Trail' {hasCustomEventSelectors} -> hasCustomEventSelectors) (\s@Trail' {} a -> s {hasCustomEventSelectors = a} :: Trail)

-- | Specifies whether a trail has insight types specified in an
-- @InsightSelector@ list.
trail_hasInsightSelectors :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_hasInsightSelectors = Lens.lens (\Trail' {hasInsightSelectors} -> hasInsightSelectors) (\s@Trail' {} a -> s {hasInsightSelectors = a} :: Trail)

-- | The Region in which the trail was created.
trail_homeRegion :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_homeRegion = Lens.lens (\Trail' {homeRegion} -> homeRegion) (\s@Trail' {} a -> s {homeRegion = a} :: Trail)

-- | Set to __True__ to include Amazon Web Services API calls from Amazon Web
-- Services global services such as IAM. Otherwise, __False__.
trail_includeGlobalServiceEvents :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_includeGlobalServiceEvents = Lens.lens (\Trail' {includeGlobalServiceEvents} -> includeGlobalServiceEvents) (\s@Trail' {} a -> s {includeGlobalServiceEvents = a} :: Trail)

-- | Specifies whether the trail exists only in one Region or exists in all
-- Regions.
trail_isMultiRegionTrail :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_isMultiRegionTrail = Lens.lens (\Trail' {isMultiRegionTrail} -> isMultiRegionTrail) (\s@Trail' {} a -> s {isMultiRegionTrail = a} :: Trail)

-- | Specifies whether the trail is an organization trail.
trail_isOrganizationTrail :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_isOrganizationTrail = Lens.lens (\Trail' {isOrganizationTrail} -> isOrganizationTrail) (\s@Trail' {} a -> s {isOrganizationTrail = a} :: Trail)

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the following format.
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
trail_kmsKeyId :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_kmsKeyId = Lens.lens (\Trail' {kmsKeyId} -> kmsKeyId) (\s@Trail' {} a -> s {kmsKeyId = a} :: Trail)

-- | Specifies whether log file validation is enabled.
trail_logFileValidationEnabled :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_logFileValidationEnabled = Lens.lens (\Trail' {logFileValidationEnabled} -> logFileValidationEnabled) (\s@Trail' {} a -> s {logFileValidationEnabled = a} :: Trail)

-- | Name of the trail set by calling CreateTrail. The maximum length is 128
-- characters.
trail_name :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_name = Lens.lens (\Trail' {name} -> name) (\s@Trail' {} a -> s {name = a} :: Trail)

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail
-- files. See
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
trail_s3BucketName :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_s3BucketName = Lens.lens (\Trail' {s3BucketName} -> s3BucketName) (\s@Trail' {} a -> s {s3BucketName = a} :: Trail)

-- | Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
-- The maximum length is 200 characters.
trail_s3KeyPrefix :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_s3KeyPrefix = Lens.lens (\Trail' {s3KeyPrefix} -> s3KeyPrefix) (\s@Trail' {} a -> s {s3KeyPrefix = a} :: Trail)

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
-- notifications when log files are delivered. The following is the format
-- of a topic ARN.
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
trail_snsTopicARN :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_snsTopicARN = Lens.lens (\Trail' {snsTopicARN} -> snsTopicARN) (\s@Trail' {} a -> s {snsTopicARN = a} :: Trail)

-- | This field is no longer in use. Use @SnsTopicARN@.
trail_snsTopicName :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_snsTopicName = Lens.lens (\Trail' {snsTopicName} -> snsTopicName) (\s@Trail' {} a -> s {snsTopicName = a} :: Trail)

-- | Specifies the ARN of the trail. The following is the format of a trail
-- ARN.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
trail_trailARN :: Lens.Lens' Trail (Prelude.Maybe Prelude.Text)
trail_trailARN = Lens.lens (\Trail' {trailARN} -> trailARN) (\s@Trail' {} a -> s {trailARN = a} :: Trail)

instance Data.FromJSON Trail where
  parseJSON =
    Data.withObject
      "Trail"
      ( \x ->
          Trail'
            Prelude.<$> (x Data..:? "CloudWatchLogsLogGroupArn")
            Prelude.<*> (x Data..:? "CloudWatchLogsRoleArn")
            Prelude.<*> (x Data..:? "HasCustomEventSelectors")
            Prelude.<*> (x Data..:? "HasInsightSelectors")
            Prelude.<*> (x Data..:? "HomeRegion")
            Prelude.<*> (x Data..:? "IncludeGlobalServiceEvents")
            Prelude.<*> (x Data..:? "IsMultiRegionTrail")
            Prelude.<*> (x Data..:? "IsOrganizationTrail")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "LogFileValidationEnabled")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "S3BucketName")
            Prelude.<*> (x Data..:? "S3KeyPrefix")
            Prelude.<*> (x Data..:? "SnsTopicARN")
            Prelude.<*> (x Data..:? "SnsTopicName")
            Prelude.<*> (x Data..:? "TrailARN")
      )

instance Prelude.Hashable Trail where
  hashWithSalt _salt Trail' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogsLogGroupArn
      `Prelude.hashWithSalt` cloudWatchLogsRoleArn
      `Prelude.hashWithSalt` hasCustomEventSelectors
      `Prelude.hashWithSalt` hasInsightSelectors
      `Prelude.hashWithSalt` homeRegion
      `Prelude.hashWithSalt` includeGlobalServiceEvents
      `Prelude.hashWithSalt` isMultiRegionTrail
      `Prelude.hashWithSalt` isOrganizationTrail
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` logFileValidationEnabled
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` snsTopicARN
      `Prelude.hashWithSalt` snsTopicName
      `Prelude.hashWithSalt` trailARN

instance Prelude.NFData Trail where
  rnf Trail' {..} =
    Prelude.rnf cloudWatchLogsLogGroupArn
      `Prelude.seq` Prelude.rnf cloudWatchLogsRoleArn
      `Prelude.seq` Prelude.rnf hasCustomEventSelectors
      `Prelude.seq` Prelude.rnf hasInsightSelectors
      `Prelude.seq` Prelude.rnf homeRegion
      `Prelude.seq` Prelude.rnf includeGlobalServiceEvents
      `Prelude.seq` Prelude.rnf isMultiRegionTrail
      `Prelude.seq` Prelude.rnf isOrganizationTrail
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf logFileValidationEnabled
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf snsTopicARN
      `Prelude.seq` Prelude.rnf snsTopicName
      `Prelude.seq` Prelude.rnf trailARN
