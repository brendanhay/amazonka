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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudTrailTrailDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudTrailTrailDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides details about a CloudTrail trail.
--
-- /See:/ 'newAwsCloudTrailTrailDetails' smart constructor.
data AwsCloudTrailTrailDetails = AwsCloudTrailTrailDetails'
  { -- | The S3 key prefix. The key prefix is added after the name of the S3
    -- bucket where the log files are published.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the trail.
    name :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether CloudTrail log file validation is enabled.
    logFileValidationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the SNS topic that is used for notifications of log file
    -- delivery.
    snsTopicName :: Prelude.Maybe Prelude.Text,
    -- | Whether the trail is created for all accounts in an organization in
    -- Organizations, or only for the current Amazon Web Services account.
    isOrganizationTrail :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the trail publishes events from global services such
    -- as IAM to the log files.
    includeGlobalServiceEvents :: Prelude.Maybe Prelude.Bool,
    -- | The name of the S3 bucket where the log files are published.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the trail has custom event selectors.
    hasCustomEventSelectors :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the SNS topic that is used for notifications of log file
    -- delivery.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the trail applies only to the current Region or to all
    -- Regions.
    isMultiRegionTrail :: Prelude.Maybe Prelude.Bool,
    -- | The KMS key ID to use to encrypt the logs.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role that the CloudWatch Events endpoint assumes when it
    -- writes to the log group.
    cloudWatchLogsRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Region where the trail was created.
    homeRegion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the log group that CloudTrail logs are delivered to.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the trail.
    trailArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudTrailTrailDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KeyPrefix', 'awsCloudTrailTrailDetails_s3KeyPrefix' - The S3 key prefix. The key prefix is added after the name of the S3
-- bucket where the log files are published.
--
-- 'name', 'awsCloudTrailTrailDetails_name' - The name of the trail.
--
-- 'logFileValidationEnabled', 'awsCloudTrailTrailDetails_logFileValidationEnabled' - Indicates whether CloudTrail log file validation is enabled.
--
-- 'snsTopicName', 'awsCloudTrailTrailDetails_snsTopicName' - The name of the SNS topic that is used for notifications of log file
-- delivery.
--
-- 'isOrganizationTrail', 'awsCloudTrailTrailDetails_isOrganizationTrail' - Whether the trail is created for all accounts in an organization in
-- Organizations, or only for the current Amazon Web Services account.
--
-- 'includeGlobalServiceEvents', 'awsCloudTrailTrailDetails_includeGlobalServiceEvents' - Indicates whether the trail publishes events from global services such
-- as IAM to the log files.
--
-- 's3BucketName', 'awsCloudTrailTrailDetails_s3BucketName' - The name of the S3 bucket where the log files are published.
--
-- 'hasCustomEventSelectors', 'awsCloudTrailTrailDetails_hasCustomEventSelectors' - Indicates whether the trail has custom event selectors.
--
-- 'snsTopicArn', 'awsCloudTrailTrailDetails_snsTopicArn' - The ARN of the SNS topic that is used for notifications of log file
-- delivery.
--
-- 'isMultiRegionTrail', 'awsCloudTrailTrailDetails_isMultiRegionTrail' - Indicates whether the trail applies only to the current Region or to all
-- Regions.
--
-- 'kmsKeyId', 'awsCloudTrailTrailDetails_kmsKeyId' - The KMS key ID to use to encrypt the logs.
--
-- 'cloudWatchLogsRoleArn', 'awsCloudTrailTrailDetails_cloudWatchLogsRoleArn' - The ARN of the role that the CloudWatch Events endpoint assumes when it
-- writes to the log group.
--
-- 'homeRegion', 'awsCloudTrailTrailDetails_homeRegion' - The Region where the trail was created.
--
-- 'cloudWatchLogsLogGroupArn', 'awsCloudTrailTrailDetails_cloudWatchLogsLogGroupArn' - The ARN of the log group that CloudTrail logs are delivered to.
--
-- 'trailArn', 'awsCloudTrailTrailDetails_trailArn' - The ARN of the trail.
newAwsCloudTrailTrailDetails ::
  AwsCloudTrailTrailDetails
newAwsCloudTrailTrailDetails =
  AwsCloudTrailTrailDetails'
    { s3KeyPrefix =
        Prelude.Nothing,
      name = Prelude.Nothing,
      logFileValidationEnabled = Prelude.Nothing,
      snsTopicName = Prelude.Nothing,
      isOrganizationTrail = Prelude.Nothing,
      includeGlobalServiceEvents = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      hasCustomEventSelectors = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      isMultiRegionTrail = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      cloudWatchLogsRoleArn = Prelude.Nothing,
      homeRegion = Prelude.Nothing,
      cloudWatchLogsLogGroupArn = Prelude.Nothing,
      trailArn = Prelude.Nothing
    }

-- | The S3 key prefix. The key prefix is added after the name of the S3
-- bucket where the log files are published.
awsCloudTrailTrailDetails_s3KeyPrefix :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_s3KeyPrefix = Lens.lens (\AwsCloudTrailTrailDetails' {s3KeyPrefix} -> s3KeyPrefix) (\s@AwsCloudTrailTrailDetails' {} a -> s {s3KeyPrefix = a} :: AwsCloudTrailTrailDetails)

-- | The name of the trail.
awsCloudTrailTrailDetails_name :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_name = Lens.lens (\AwsCloudTrailTrailDetails' {name} -> name) (\s@AwsCloudTrailTrailDetails' {} a -> s {name = a} :: AwsCloudTrailTrailDetails)

-- | Indicates whether CloudTrail log file validation is enabled.
awsCloudTrailTrailDetails_logFileValidationEnabled :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Bool)
awsCloudTrailTrailDetails_logFileValidationEnabled = Lens.lens (\AwsCloudTrailTrailDetails' {logFileValidationEnabled} -> logFileValidationEnabled) (\s@AwsCloudTrailTrailDetails' {} a -> s {logFileValidationEnabled = a} :: AwsCloudTrailTrailDetails)

-- | The name of the SNS topic that is used for notifications of log file
-- delivery.
awsCloudTrailTrailDetails_snsTopicName :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_snsTopicName = Lens.lens (\AwsCloudTrailTrailDetails' {snsTopicName} -> snsTopicName) (\s@AwsCloudTrailTrailDetails' {} a -> s {snsTopicName = a} :: AwsCloudTrailTrailDetails)

-- | Whether the trail is created for all accounts in an organization in
-- Organizations, or only for the current Amazon Web Services account.
awsCloudTrailTrailDetails_isOrganizationTrail :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Bool)
awsCloudTrailTrailDetails_isOrganizationTrail = Lens.lens (\AwsCloudTrailTrailDetails' {isOrganizationTrail} -> isOrganizationTrail) (\s@AwsCloudTrailTrailDetails' {} a -> s {isOrganizationTrail = a} :: AwsCloudTrailTrailDetails)

-- | Indicates whether the trail publishes events from global services such
-- as IAM to the log files.
awsCloudTrailTrailDetails_includeGlobalServiceEvents :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Bool)
awsCloudTrailTrailDetails_includeGlobalServiceEvents = Lens.lens (\AwsCloudTrailTrailDetails' {includeGlobalServiceEvents} -> includeGlobalServiceEvents) (\s@AwsCloudTrailTrailDetails' {} a -> s {includeGlobalServiceEvents = a} :: AwsCloudTrailTrailDetails)

-- | The name of the S3 bucket where the log files are published.
awsCloudTrailTrailDetails_s3BucketName :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_s3BucketName = Lens.lens (\AwsCloudTrailTrailDetails' {s3BucketName} -> s3BucketName) (\s@AwsCloudTrailTrailDetails' {} a -> s {s3BucketName = a} :: AwsCloudTrailTrailDetails)

-- | Indicates whether the trail has custom event selectors.
awsCloudTrailTrailDetails_hasCustomEventSelectors :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Bool)
awsCloudTrailTrailDetails_hasCustomEventSelectors = Lens.lens (\AwsCloudTrailTrailDetails' {hasCustomEventSelectors} -> hasCustomEventSelectors) (\s@AwsCloudTrailTrailDetails' {} a -> s {hasCustomEventSelectors = a} :: AwsCloudTrailTrailDetails)

-- | The ARN of the SNS topic that is used for notifications of log file
-- delivery.
awsCloudTrailTrailDetails_snsTopicArn :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_snsTopicArn = Lens.lens (\AwsCloudTrailTrailDetails' {snsTopicArn} -> snsTopicArn) (\s@AwsCloudTrailTrailDetails' {} a -> s {snsTopicArn = a} :: AwsCloudTrailTrailDetails)

-- | Indicates whether the trail applies only to the current Region or to all
-- Regions.
awsCloudTrailTrailDetails_isMultiRegionTrail :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Bool)
awsCloudTrailTrailDetails_isMultiRegionTrail = Lens.lens (\AwsCloudTrailTrailDetails' {isMultiRegionTrail} -> isMultiRegionTrail) (\s@AwsCloudTrailTrailDetails' {} a -> s {isMultiRegionTrail = a} :: AwsCloudTrailTrailDetails)

-- | The KMS key ID to use to encrypt the logs.
awsCloudTrailTrailDetails_kmsKeyId :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_kmsKeyId = Lens.lens (\AwsCloudTrailTrailDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsCloudTrailTrailDetails' {} a -> s {kmsKeyId = a} :: AwsCloudTrailTrailDetails)

-- | The ARN of the role that the CloudWatch Events endpoint assumes when it
-- writes to the log group.
awsCloudTrailTrailDetails_cloudWatchLogsRoleArn :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_cloudWatchLogsRoleArn = Lens.lens (\AwsCloudTrailTrailDetails' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@AwsCloudTrailTrailDetails' {} a -> s {cloudWatchLogsRoleArn = a} :: AwsCloudTrailTrailDetails)

-- | The Region where the trail was created.
awsCloudTrailTrailDetails_homeRegion :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_homeRegion = Lens.lens (\AwsCloudTrailTrailDetails' {homeRegion} -> homeRegion) (\s@AwsCloudTrailTrailDetails' {} a -> s {homeRegion = a} :: AwsCloudTrailTrailDetails)

-- | The ARN of the log group that CloudTrail logs are delivered to.
awsCloudTrailTrailDetails_cloudWatchLogsLogGroupArn :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_cloudWatchLogsLogGroupArn = Lens.lens (\AwsCloudTrailTrailDetails' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@AwsCloudTrailTrailDetails' {} a -> s {cloudWatchLogsLogGroupArn = a} :: AwsCloudTrailTrailDetails)

-- | The ARN of the trail.
awsCloudTrailTrailDetails_trailArn :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_trailArn = Lens.lens (\AwsCloudTrailTrailDetails' {trailArn} -> trailArn) (\s@AwsCloudTrailTrailDetails' {} a -> s {trailArn = a} :: AwsCloudTrailTrailDetails)

instance Core.FromJSON AwsCloudTrailTrailDetails where
  parseJSON =
    Core.withObject
      "AwsCloudTrailTrailDetails"
      ( \x ->
          AwsCloudTrailTrailDetails'
            Prelude.<$> (x Core..:? "S3KeyPrefix")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LogFileValidationEnabled")
            Prelude.<*> (x Core..:? "SnsTopicName")
            Prelude.<*> (x Core..:? "IsOrganizationTrail")
            Prelude.<*> (x Core..:? "IncludeGlobalServiceEvents")
            Prelude.<*> (x Core..:? "S3BucketName")
            Prelude.<*> (x Core..:? "HasCustomEventSelectors")
            Prelude.<*> (x Core..:? "SnsTopicArn")
            Prelude.<*> (x Core..:? "IsMultiRegionTrail")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "CloudWatchLogsRoleArn")
            Prelude.<*> (x Core..:? "HomeRegion")
            Prelude.<*> (x Core..:? "CloudWatchLogsLogGroupArn")
            Prelude.<*> (x Core..:? "TrailArn")
      )

instance Prelude.Hashable AwsCloudTrailTrailDetails where
  hashWithSalt _salt AwsCloudTrailTrailDetails' {..} =
    _salt `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` logFileValidationEnabled
      `Prelude.hashWithSalt` snsTopicName
      `Prelude.hashWithSalt` isOrganizationTrail
      `Prelude.hashWithSalt` includeGlobalServiceEvents
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` hasCustomEventSelectors
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` isMultiRegionTrail
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` cloudWatchLogsRoleArn
      `Prelude.hashWithSalt` homeRegion
      `Prelude.hashWithSalt` cloudWatchLogsLogGroupArn
      `Prelude.hashWithSalt` trailArn

instance Prelude.NFData AwsCloudTrailTrailDetails where
  rnf AwsCloudTrailTrailDetails' {..} =
    Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf logFileValidationEnabled
      `Prelude.seq` Prelude.rnf snsTopicName
      `Prelude.seq` Prelude.rnf isOrganizationTrail
      `Prelude.seq` Prelude.rnf includeGlobalServiceEvents
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf hasCustomEventSelectors
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf isMultiRegionTrail
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf cloudWatchLogsRoleArn
      `Prelude.seq` Prelude.rnf homeRegion
      `Prelude.seq` Prelude.rnf cloudWatchLogsLogGroupArn
      `Prelude.seq` Prelude.rnf trailArn

instance Core.ToJSON AwsCloudTrailTrailDetails where
  toJSON AwsCloudTrailTrailDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3KeyPrefix" Core..=) Prelude.<$> s3KeyPrefix,
            ("Name" Core..=) Prelude.<$> name,
            ("LogFileValidationEnabled" Core..=)
              Prelude.<$> logFileValidationEnabled,
            ("SnsTopicName" Core..=) Prelude.<$> snsTopicName,
            ("IsOrganizationTrail" Core..=)
              Prelude.<$> isOrganizationTrail,
            ("IncludeGlobalServiceEvents" Core..=)
              Prelude.<$> includeGlobalServiceEvents,
            ("S3BucketName" Core..=) Prelude.<$> s3BucketName,
            ("HasCustomEventSelectors" Core..=)
              Prelude.<$> hasCustomEventSelectors,
            ("SnsTopicArn" Core..=) Prelude.<$> snsTopicArn,
            ("IsMultiRegionTrail" Core..=)
              Prelude.<$> isMultiRegionTrail,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("CloudWatchLogsRoleArn" Core..=)
              Prelude.<$> cloudWatchLogsRoleArn,
            ("HomeRegion" Core..=) Prelude.<$> homeRegion,
            ("CloudWatchLogsLogGroupArn" Core..=)
              Prelude.<$> cloudWatchLogsLogGroupArn,
            ("TrailArn" Core..=) Prelude.<$> trailArn
          ]
      )
