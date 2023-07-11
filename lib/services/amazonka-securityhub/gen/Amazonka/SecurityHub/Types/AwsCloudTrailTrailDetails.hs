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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudTrailTrailDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about a CloudTrail trail.
--
-- /See:/ 'newAwsCloudTrailTrailDetails' smart constructor.
data AwsCloudTrailTrailDetails = AwsCloudTrailTrailDetails'
  { -- | The ARN of the log group that CloudTrail logs are delivered to.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role that the CloudWatch Events endpoint assumes when it
    -- writes to the log group.
    cloudWatchLogsRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the trail has custom event selectors.
    hasCustomEventSelectors :: Prelude.Maybe Prelude.Bool,
    -- | The Region where the trail was created.
    homeRegion :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the trail publishes events from global services such
    -- as IAM to the log files.
    includeGlobalServiceEvents :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the trail applies only to the current Region or to all
    -- Regions.
    isMultiRegionTrail :: Prelude.Maybe Prelude.Bool,
    -- | Whether the trail is created for all accounts in an organization in
    -- Organizations, or only for the current Amazon Web Services account.
    isOrganizationTrail :: Prelude.Maybe Prelude.Bool,
    -- | The KMS key ID to use to encrypt the logs.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether CloudTrail log file validation is enabled.
    logFileValidationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the trail.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket where the log files are published.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The S3 key prefix. The key prefix is added after the name of the S3
    -- bucket where the log files are published.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the SNS topic that is used for notifications of log file
    -- delivery.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the SNS topic that is used for notifications of log file
    -- delivery.
    snsTopicName :: Prelude.Maybe Prelude.Text,
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
-- 'cloudWatchLogsLogGroupArn', 'awsCloudTrailTrailDetails_cloudWatchLogsLogGroupArn' - The ARN of the log group that CloudTrail logs are delivered to.
--
-- 'cloudWatchLogsRoleArn', 'awsCloudTrailTrailDetails_cloudWatchLogsRoleArn' - The ARN of the role that the CloudWatch Events endpoint assumes when it
-- writes to the log group.
--
-- 'hasCustomEventSelectors', 'awsCloudTrailTrailDetails_hasCustomEventSelectors' - Indicates whether the trail has custom event selectors.
--
-- 'homeRegion', 'awsCloudTrailTrailDetails_homeRegion' - The Region where the trail was created.
--
-- 'includeGlobalServiceEvents', 'awsCloudTrailTrailDetails_includeGlobalServiceEvents' - Indicates whether the trail publishes events from global services such
-- as IAM to the log files.
--
-- 'isMultiRegionTrail', 'awsCloudTrailTrailDetails_isMultiRegionTrail' - Indicates whether the trail applies only to the current Region or to all
-- Regions.
--
-- 'isOrganizationTrail', 'awsCloudTrailTrailDetails_isOrganizationTrail' - Whether the trail is created for all accounts in an organization in
-- Organizations, or only for the current Amazon Web Services account.
--
-- 'kmsKeyId', 'awsCloudTrailTrailDetails_kmsKeyId' - The KMS key ID to use to encrypt the logs.
--
-- 'logFileValidationEnabled', 'awsCloudTrailTrailDetails_logFileValidationEnabled' - Indicates whether CloudTrail log file validation is enabled.
--
-- 'name', 'awsCloudTrailTrailDetails_name' - The name of the trail.
--
-- 's3BucketName', 'awsCloudTrailTrailDetails_s3BucketName' - The name of the S3 bucket where the log files are published.
--
-- 's3KeyPrefix', 'awsCloudTrailTrailDetails_s3KeyPrefix' - The S3 key prefix. The key prefix is added after the name of the S3
-- bucket where the log files are published.
--
-- 'snsTopicArn', 'awsCloudTrailTrailDetails_snsTopicArn' - The ARN of the SNS topic that is used for notifications of log file
-- delivery.
--
-- 'snsTopicName', 'awsCloudTrailTrailDetails_snsTopicName' - The name of the SNS topic that is used for notifications of log file
-- delivery.
--
-- 'trailArn', 'awsCloudTrailTrailDetails_trailArn' - The ARN of the trail.
newAwsCloudTrailTrailDetails ::
  AwsCloudTrailTrailDetails
newAwsCloudTrailTrailDetails =
  AwsCloudTrailTrailDetails'
    { cloudWatchLogsLogGroupArn =
        Prelude.Nothing,
      cloudWatchLogsRoleArn = Prelude.Nothing,
      hasCustomEventSelectors = Prelude.Nothing,
      homeRegion = Prelude.Nothing,
      includeGlobalServiceEvents = Prelude.Nothing,
      isMultiRegionTrail = Prelude.Nothing,
      isOrganizationTrail = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      logFileValidationEnabled = Prelude.Nothing,
      name = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      snsTopicName = Prelude.Nothing,
      trailArn = Prelude.Nothing
    }

-- | The ARN of the log group that CloudTrail logs are delivered to.
awsCloudTrailTrailDetails_cloudWatchLogsLogGroupArn :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_cloudWatchLogsLogGroupArn = Lens.lens (\AwsCloudTrailTrailDetails' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@AwsCloudTrailTrailDetails' {} a -> s {cloudWatchLogsLogGroupArn = a} :: AwsCloudTrailTrailDetails)

-- | The ARN of the role that the CloudWatch Events endpoint assumes when it
-- writes to the log group.
awsCloudTrailTrailDetails_cloudWatchLogsRoleArn :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_cloudWatchLogsRoleArn = Lens.lens (\AwsCloudTrailTrailDetails' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@AwsCloudTrailTrailDetails' {} a -> s {cloudWatchLogsRoleArn = a} :: AwsCloudTrailTrailDetails)

-- | Indicates whether the trail has custom event selectors.
awsCloudTrailTrailDetails_hasCustomEventSelectors :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Bool)
awsCloudTrailTrailDetails_hasCustomEventSelectors = Lens.lens (\AwsCloudTrailTrailDetails' {hasCustomEventSelectors} -> hasCustomEventSelectors) (\s@AwsCloudTrailTrailDetails' {} a -> s {hasCustomEventSelectors = a} :: AwsCloudTrailTrailDetails)

-- | The Region where the trail was created.
awsCloudTrailTrailDetails_homeRegion :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_homeRegion = Lens.lens (\AwsCloudTrailTrailDetails' {homeRegion} -> homeRegion) (\s@AwsCloudTrailTrailDetails' {} a -> s {homeRegion = a} :: AwsCloudTrailTrailDetails)

-- | Indicates whether the trail publishes events from global services such
-- as IAM to the log files.
awsCloudTrailTrailDetails_includeGlobalServiceEvents :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Bool)
awsCloudTrailTrailDetails_includeGlobalServiceEvents = Lens.lens (\AwsCloudTrailTrailDetails' {includeGlobalServiceEvents} -> includeGlobalServiceEvents) (\s@AwsCloudTrailTrailDetails' {} a -> s {includeGlobalServiceEvents = a} :: AwsCloudTrailTrailDetails)

-- | Indicates whether the trail applies only to the current Region or to all
-- Regions.
awsCloudTrailTrailDetails_isMultiRegionTrail :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Bool)
awsCloudTrailTrailDetails_isMultiRegionTrail = Lens.lens (\AwsCloudTrailTrailDetails' {isMultiRegionTrail} -> isMultiRegionTrail) (\s@AwsCloudTrailTrailDetails' {} a -> s {isMultiRegionTrail = a} :: AwsCloudTrailTrailDetails)

-- | Whether the trail is created for all accounts in an organization in
-- Organizations, or only for the current Amazon Web Services account.
awsCloudTrailTrailDetails_isOrganizationTrail :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Bool)
awsCloudTrailTrailDetails_isOrganizationTrail = Lens.lens (\AwsCloudTrailTrailDetails' {isOrganizationTrail} -> isOrganizationTrail) (\s@AwsCloudTrailTrailDetails' {} a -> s {isOrganizationTrail = a} :: AwsCloudTrailTrailDetails)

-- | The KMS key ID to use to encrypt the logs.
awsCloudTrailTrailDetails_kmsKeyId :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_kmsKeyId = Lens.lens (\AwsCloudTrailTrailDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsCloudTrailTrailDetails' {} a -> s {kmsKeyId = a} :: AwsCloudTrailTrailDetails)

-- | Indicates whether CloudTrail log file validation is enabled.
awsCloudTrailTrailDetails_logFileValidationEnabled :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Bool)
awsCloudTrailTrailDetails_logFileValidationEnabled = Lens.lens (\AwsCloudTrailTrailDetails' {logFileValidationEnabled} -> logFileValidationEnabled) (\s@AwsCloudTrailTrailDetails' {} a -> s {logFileValidationEnabled = a} :: AwsCloudTrailTrailDetails)

-- | The name of the trail.
awsCloudTrailTrailDetails_name :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_name = Lens.lens (\AwsCloudTrailTrailDetails' {name} -> name) (\s@AwsCloudTrailTrailDetails' {} a -> s {name = a} :: AwsCloudTrailTrailDetails)

-- | The name of the S3 bucket where the log files are published.
awsCloudTrailTrailDetails_s3BucketName :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_s3BucketName = Lens.lens (\AwsCloudTrailTrailDetails' {s3BucketName} -> s3BucketName) (\s@AwsCloudTrailTrailDetails' {} a -> s {s3BucketName = a} :: AwsCloudTrailTrailDetails)

-- | The S3 key prefix. The key prefix is added after the name of the S3
-- bucket where the log files are published.
awsCloudTrailTrailDetails_s3KeyPrefix :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_s3KeyPrefix = Lens.lens (\AwsCloudTrailTrailDetails' {s3KeyPrefix} -> s3KeyPrefix) (\s@AwsCloudTrailTrailDetails' {} a -> s {s3KeyPrefix = a} :: AwsCloudTrailTrailDetails)

-- | The ARN of the SNS topic that is used for notifications of log file
-- delivery.
awsCloudTrailTrailDetails_snsTopicArn :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_snsTopicArn = Lens.lens (\AwsCloudTrailTrailDetails' {snsTopicArn} -> snsTopicArn) (\s@AwsCloudTrailTrailDetails' {} a -> s {snsTopicArn = a} :: AwsCloudTrailTrailDetails)

-- | The name of the SNS topic that is used for notifications of log file
-- delivery.
awsCloudTrailTrailDetails_snsTopicName :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_snsTopicName = Lens.lens (\AwsCloudTrailTrailDetails' {snsTopicName} -> snsTopicName) (\s@AwsCloudTrailTrailDetails' {} a -> s {snsTopicName = a} :: AwsCloudTrailTrailDetails)

-- | The ARN of the trail.
awsCloudTrailTrailDetails_trailArn :: Lens.Lens' AwsCloudTrailTrailDetails (Prelude.Maybe Prelude.Text)
awsCloudTrailTrailDetails_trailArn = Lens.lens (\AwsCloudTrailTrailDetails' {trailArn} -> trailArn) (\s@AwsCloudTrailTrailDetails' {} a -> s {trailArn = a} :: AwsCloudTrailTrailDetails)

instance Data.FromJSON AwsCloudTrailTrailDetails where
  parseJSON =
    Data.withObject
      "AwsCloudTrailTrailDetails"
      ( \x ->
          AwsCloudTrailTrailDetails'
            Prelude.<$> (x Data..:? "CloudWatchLogsLogGroupArn")
            Prelude.<*> (x Data..:? "CloudWatchLogsRoleArn")
            Prelude.<*> (x Data..:? "HasCustomEventSelectors")
            Prelude.<*> (x Data..:? "HomeRegion")
            Prelude.<*> (x Data..:? "IncludeGlobalServiceEvents")
            Prelude.<*> (x Data..:? "IsMultiRegionTrail")
            Prelude.<*> (x Data..:? "IsOrganizationTrail")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "LogFileValidationEnabled")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "S3BucketName")
            Prelude.<*> (x Data..:? "S3KeyPrefix")
            Prelude.<*> (x Data..:? "SnsTopicArn")
            Prelude.<*> (x Data..:? "SnsTopicName")
            Prelude.<*> (x Data..:? "TrailArn")
      )

instance Prelude.Hashable AwsCloudTrailTrailDetails where
  hashWithSalt _salt AwsCloudTrailTrailDetails' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogsLogGroupArn
      `Prelude.hashWithSalt` cloudWatchLogsRoleArn
      `Prelude.hashWithSalt` hasCustomEventSelectors
      `Prelude.hashWithSalt` homeRegion
      `Prelude.hashWithSalt` includeGlobalServiceEvents
      `Prelude.hashWithSalt` isMultiRegionTrail
      `Prelude.hashWithSalt` isOrganizationTrail
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` logFileValidationEnabled
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` snsTopicName
      `Prelude.hashWithSalt` trailArn

instance Prelude.NFData AwsCloudTrailTrailDetails where
  rnf AwsCloudTrailTrailDetails' {..} =
    Prelude.rnf cloudWatchLogsLogGroupArn
      `Prelude.seq` Prelude.rnf cloudWatchLogsRoleArn
      `Prelude.seq` Prelude.rnf hasCustomEventSelectors
      `Prelude.seq` Prelude.rnf homeRegion
      `Prelude.seq` Prelude.rnf includeGlobalServiceEvents
      `Prelude.seq` Prelude.rnf isMultiRegionTrail
      `Prelude.seq` Prelude.rnf isOrganizationTrail
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf logFileValidationEnabled
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf snsTopicName
      `Prelude.seq` Prelude.rnf trailArn

instance Data.ToJSON AwsCloudTrailTrailDetails where
  toJSON AwsCloudTrailTrailDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLogsLogGroupArn" Data..=)
              Prelude.<$> cloudWatchLogsLogGroupArn,
            ("CloudWatchLogsRoleArn" Data..=)
              Prelude.<$> cloudWatchLogsRoleArn,
            ("HasCustomEventSelectors" Data..=)
              Prelude.<$> hasCustomEventSelectors,
            ("HomeRegion" Data..=) Prelude.<$> homeRegion,
            ("IncludeGlobalServiceEvents" Data..=)
              Prelude.<$> includeGlobalServiceEvents,
            ("IsMultiRegionTrail" Data..=)
              Prelude.<$> isMultiRegionTrail,
            ("IsOrganizationTrail" Data..=)
              Prelude.<$> isOrganizationTrail,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("LogFileValidationEnabled" Data..=)
              Prelude.<$> logFileValidationEnabled,
            ("Name" Data..=) Prelude.<$> name,
            ("S3BucketName" Data..=) Prelude.<$> s3BucketName,
            ("S3KeyPrefix" Data..=) Prelude.<$> s3KeyPrefix,
            ("SnsTopicArn" Data..=) Prelude.<$> snsTopicArn,
            ("SnsTopicName" Data..=) Prelude.<$> snsTopicName,
            ("TrailArn" Data..=) Prelude.<$> trailArn
          ]
      )
