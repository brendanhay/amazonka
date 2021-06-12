{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.UpdateTrail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings that specify delivery of log files. Changes to a
-- trail do not require stopping the CloudTrail service. Use this action to
-- designate an existing bucket for log delivery. If the existing bucket
-- has previously been a target for CloudTrail log files, an IAM policy
-- exists for the bucket. @UpdateTrail@ must be called from the region in
-- which the trail was created; otherwise, an @InvalidHomeRegionException@
-- is thrown.
module Network.AWS.CloudTrail.UpdateTrail
  ( -- * Creating a Request
    UpdateTrail (..),
    newUpdateTrail,

    -- * Request Lenses
    updateTrail_isOrganizationTrail,
    updateTrail_snsTopicName,
    updateTrail_includeGlobalServiceEvents,
    updateTrail_kmsKeyId,
    updateTrail_s3KeyPrefix,
    updateTrail_cloudWatchLogsLogGroupArn,
    updateTrail_isMultiRegionTrail,
    updateTrail_s3BucketName,
    updateTrail_cloudWatchLogsRoleArn,
    updateTrail_enableLogFileValidation,
    updateTrail_name,

    -- * Destructuring the Response
    UpdateTrailResponse (..),
    newUpdateTrailResponse,

    -- * Response Lenses
    updateTrailResponse_trailARN,
    updateTrailResponse_logFileValidationEnabled,
    updateTrailResponse_isOrganizationTrail,
    updateTrailResponse_snsTopicName,
    updateTrailResponse_includeGlobalServiceEvents,
    updateTrailResponse_kmsKeyId,
    updateTrailResponse_s3KeyPrefix,
    updateTrailResponse_name,
    updateTrailResponse_cloudWatchLogsLogGroupArn,
    updateTrailResponse_isMultiRegionTrail,
    updateTrailResponse_s3BucketName,
    updateTrailResponse_cloudWatchLogsRoleArn,
    updateTrailResponse_snsTopicARN,
    updateTrailResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Specifies settings to update for the trail.
--
-- /See:/ 'newUpdateTrail' smart constructor.
data UpdateTrail = UpdateTrail'
  { -- | Specifies whether the trail is applied to all accounts in an
    -- organization in AWS Organizations, or only for the current AWS account.
    -- The default is false, and cannot be true unless the call is made on
    -- behalf of an AWS account that is the master account for an organization
    -- in AWS Organizations. If the trail is not an organization trail and this
    -- is set to true, the trail will be created in all AWS accounts that
    -- belong to the organization. If the trail is an organization trail and
    -- this is set to false, the trail will remain in the current AWS account
    -- but be deleted from all member accounts in the organization.
    isOrganizationTrail :: Core.Maybe Core.Bool,
    -- | Specifies the name of the Amazon SNS topic defined for notification of
    -- log file delivery. The maximum length is 256 characters.
    snsTopicName :: Core.Maybe Core.Text,
    -- | Specifies whether the trail is publishing events from global services
    -- such as IAM to the log files.
    includeGlobalServiceEvents :: Core.Maybe Core.Bool,
    -- | Specifies the KMS key ID to use to encrypt the logs delivered by
    -- CloudTrail. The value can be an alias name prefixed by \"alias\/\", a
    -- fully specified ARN to an alias, a fully specified ARN to a key, or a
    -- globally unique identifier.
    --
    -- Examples:
    --
    -- -   alias\/MyAliasName
    --
    -- -   arn:aws:kms:us-east-2:123456789012:alias\/MyAliasName
    --
    -- -   arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012
    --
    -- -   12345678-1234-1234-1234-123456789012
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the
    -- bucket you have designated for log file delivery. For more information,
    -- see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
    -- The maximum length is 200 characters.
    s3KeyPrefix :: Core.Maybe Core.Text,
    -- | Specifies a log group name using an Amazon Resource Name (ARN), a unique
    -- identifier that represents the log group to which CloudTrail logs will
    -- be delivered. Not required unless you specify CloudWatchLogsRoleArn.
    cloudWatchLogsLogGroupArn :: Core.Maybe Core.Text,
    -- | Specifies whether the trail applies only to the current region or to all
    -- regions. The default is false. If the trail exists only in the current
    -- region and this value is set to true, shadow trails (replications of the
    -- trail) will be created in the other regions. If the trail exists in all
    -- regions and this value is set to false, the trail will remain in the
    -- region where it was created, and its shadow trails in other regions will
    -- be deleted. As a best practice, consider using trails that log events in
    -- all regions.
    isMultiRegionTrail :: Core.Maybe Core.Bool,
    -- | Specifies the name of the Amazon S3 bucket designated for publishing log
    -- files. See
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
    s3BucketName :: Core.Maybe Core.Text,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write
    -- to a user\'s log group.
    cloudWatchLogsRoleArn :: Core.Maybe Core.Text,
    -- | Specifies whether log file validation is enabled. The default is false.
    --
    -- When you disable log file integrity validation, the chain of digest
    -- files is broken after one hour. CloudTrail will not create digest files
    -- for log files that were delivered during a period in which log file
    -- integrity validation was disabled. For example, if you enable log file
    -- integrity validation at noon on January 1, disable it at noon on January
    -- 2, and re-enable it at noon on January 10, digest files will not be
    -- created for the log files delivered from noon on January 2 to noon on
    -- January 10. The same applies whenever you stop CloudTrail logging or
    -- delete a trail.
    enableLogFileValidation :: Core.Maybe Core.Bool,
    -- | Specifies the name of the trail or trail ARN. If @Name@ is a trail name,
    -- the string must meet the following requirements:
    --
    -- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.),
    --     underscores (_), or dashes (-)
    --
    -- -   Start with a letter or number, and end with a letter or number
    --
    -- -   Be between 3 and 128 characters
    --
    -- -   Have no adjacent periods, underscores or dashes. Names like
    --     @my-_namespace@ and @my--namespace@ are invalid.
    --
    -- -   Not be in IP address format (for example, 192.168.5.4)
    --
    -- If @Name@ is a trail ARN, it must be in the format:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTrail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isOrganizationTrail', 'updateTrail_isOrganizationTrail' - Specifies whether the trail is applied to all accounts in an
-- organization in AWS Organizations, or only for the current AWS account.
-- The default is false, and cannot be true unless the call is made on
-- behalf of an AWS account that is the master account for an organization
-- in AWS Organizations. If the trail is not an organization trail and this
-- is set to true, the trail will be created in all AWS accounts that
-- belong to the organization. If the trail is an organization trail and
-- this is set to false, the trail will remain in the current AWS account
-- but be deleted from all member accounts in the organization.
--
-- 'snsTopicName', 'updateTrail_snsTopicName' - Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery. The maximum length is 256 characters.
--
-- 'includeGlobalServiceEvents', 'updateTrail_includeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
--
-- 'kmsKeyId', 'updateTrail_kmsKeyId' - Specifies the KMS key ID to use to encrypt the logs delivered by
-- CloudTrail. The value can be an alias name prefixed by \"alias\/\", a
-- fully specified ARN to an alias, a fully specified ARN to a key, or a
-- globally unique identifier.
--
-- Examples:
--
-- -   alias\/MyAliasName
--
-- -   arn:aws:kms:us-east-2:123456789012:alias\/MyAliasName
--
-- -   arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012
--
-- -   12345678-1234-1234-1234-123456789012
--
-- 's3KeyPrefix', 'updateTrail_s3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
-- The maximum length is 200 characters.
--
-- 'cloudWatchLogsLogGroupArn', 'updateTrail_cloudWatchLogsLogGroupArn' - Specifies a log group name using an Amazon Resource Name (ARN), a unique
-- identifier that represents the log group to which CloudTrail logs will
-- be delivered. Not required unless you specify CloudWatchLogsRoleArn.
--
-- 'isMultiRegionTrail', 'updateTrail_isMultiRegionTrail' - Specifies whether the trail applies only to the current region or to all
-- regions. The default is false. If the trail exists only in the current
-- region and this value is set to true, shadow trails (replications of the
-- trail) will be created in the other regions. If the trail exists in all
-- regions and this value is set to false, the trail will remain in the
-- region where it was created, and its shadow trails in other regions will
-- be deleted. As a best practice, consider using trails that log events in
-- all regions.
--
-- 's3BucketName', 'updateTrail_s3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log
-- files. See
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
--
-- 'cloudWatchLogsRoleArn', 'updateTrail_cloudWatchLogsRoleArn' - Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
--
-- 'enableLogFileValidation', 'updateTrail_enableLogFileValidation' - Specifies whether log file validation is enabled. The default is false.
--
-- When you disable log file integrity validation, the chain of digest
-- files is broken after one hour. CloudTrail will not create digest files
-- for log files that were delivered during a period in which log file
-- integrity validation was disabled. For example, if you enable log file
-- integrity validation at noon on January 1, disable it at noon on January
-- 2, and re-enable it at noon on January 10, digest files will not be
-- created for the log files delivered from noon on January 2 to noon on
-- January 10. The same applies whenever you stop CloudTrail logging or
-- delete a trail.
--
-- 'name', 'updateTrail_name' - Specifies the name of the trail or trail ARN. If @Name@ is a trail name,
-- the string must meet the following requirements:
--
-- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.),
--     underscores (_), or dashes (-)
--
-- -   Start with a letter or number, and end with a letter or number
--
-- -   Be between 3 and 128 characters
--
-- -   Have no adjacent periods, underscores or dashes. Names like
--     @my-_namespace@ and @my--namespace@ are invalid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
--
-- If @Name@ is a trail ARN, it must be in the format:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newUpdateTrail ::
  -- | 'name'
  Core.Text ->
  UpdateTrail
newUpdateTrail pName_ =
  UpdateTrail'
    { isOrganizationTrail = Core.Nothing,
      snsTopicName = Core.Nothing,
      includeGlobalServiceEvents = Core.Nothing,
      kmsKeyId = Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      cloudWatchLogsLogGroupArn = Core.Nothing,
      isMultiRegionTrail = Core.Nothing,
      s3BucketName = Core.Nothing,
      cloudWatchLogsRoleArn = Core.Nothing,
      enableLogFileValidation = Core.Nothing,
      name = pName_
    }

-- | Specifies whether the trail is applied to all accounts in an
-- organization in AWS Organizations, or only for the current AWS account.
-- The default is false, and cannot be true unless the call is made on
-- behalf of an AWS account that is the master account for an organization
-- in AWS Organizations. If the trail is not an organization trail and this
-- is set to true, the trail will be created in all AWS accounts that
-- belong to the organization. If the trail is an organization trail and
-- this is set to false, the trail will remain in the current AWS account
-- but be deleted from all member accounts in the organization.
updateTrail_isOrganizationTrail :: Lens.Lens' UpdateTrail (Core.Maybe Core.Bool)
updateTrail_isOrganizationTrail = Lens.lens (\UpdateTrail' {isOrganizationTrail} -> isOrganizationTrail) (\s@UpdateTrail' {} a -> s {isOrganizationTrail = a} :: UpdateTrail)

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery. The maximum length is 256 characters.
updateTrail_snsTopicName :: Lens.Lens' UpdateTrail (Core.Maybe Core.Text)
updateTrail_snsTopicName = Lens.lens (\UpdateTrail' {snsTopicName} -> snsTopicName) (\s@UpdateTrail' {} a -> s {snsTopicName = a} :: UpdateTrail)

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
updateTrail_includeGlobalServiceEvents :: Lens.Lens' UpdateTrail (Core.Maybe Core.Bool)
updateTrail_includeGlobalServiceEvents = Lens.lens (\UpdateTrail' {includeGlobalServiceEvents} -> includeGlobalServiceEvents) (\s@UpdateTrail' {} a -> s {includeGlobalServiceEvents = a} :: UpdateTrail)

-- | Specifies the KMS key ID to use to encrypt the logs delivered by
-- CloudTrail. The value can be an alias name prefixed by \"alias\/\", a
-- fully specified ARN to an alias, a fully specified ARN to a key, or a
-- globally unique identifier.
--
-- Examples:
--
-- -   alias\/MyAliasName
--
-- -   arn:aws:kms:us-east-2:123456789012:alias\/MyAliasName
--
-- -   arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012
--
-- -   12345678-1234-1234-1234-123456789012
updateTrail_kmsKeyId :: Lens.Lens' UpdateTrail (Core.Maybe Core.Text)
updateTrail_kmsKeyId = Lens.lens (\UpdateTrail' {kmsKeyId} -> kmsKeyId) (\s@UpdateTrail' {} a -> s {kmsKeyId = a} :: UpdateTrail)

-- | Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
-- The maximum length is 200 characters.
updateTrail_s3KeyPrefix :: Lens.Lens' UpdateTrail (Core.Maybe Core.Text)
updateTrail_s3KeyPrefix = Lens.lens (\UpdateTrail' {s3KeyPrefix} -> s3KeyPrefix) (\s@UpdateTrail' {} a -> s {s3KeyPrefix = a} :: UpdateTrail)

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique
-- identifier that represents the log group to which CloudTrail logs will
-- be delivered. Not required unless you specify CloudWatchLogsRoleArn.
updateTrail_cloudWatchLogsLogGroupArn :: Lens.Lens' UpdateTrail (Core.Maybe Core.Text)
updateTrail_cloudWatchLogsLogGroupArn = Lens.lens (\UpdateTrail' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@UpdateTrail' {} a -> s {cloudWatchLogsLogGroupArn = a} :: UpdateTrail)

-- | Specifies whether the trail applies only to the current region or to all
-- regions. The default is false. If the trail exists only in the current
-- region and this value is set to true, shadow trails (replications of the
-- trail) will be created in the other regions. If the trail exists in all
-- regions and this value is set to false, the trail will remain in the
-- region where it was created, and its shadow trails in other regions will
-- be deleted. As a best practice, consider using trails that log events in
-- all regions.
updateTrail_isMultiRegionTrail :: Lens.Lens' UpdateTrail (Core.Maybe Core.Bool)
updateTrail_isMultiRegionTrail = Lens.lens (\UpdateTrail' {isMultiRegionTrail} -> isMultiRegionTrail) (\s@UpdateTrail' {} a -> s {isMultiRegionTrail = a} :: UpdateTrail)

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files. See
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
updateTrail_s3BucketName :: Lens.Lens' UpdateTrail (Core.Maybe Core.Text)
updateTrail_s3BucketName = Lens.lens (\UpdateTrail' {s3BucketName} -> s3BucketName) (\s@UpdateTrail' {} a -> s {s3BucketName = a} :: UpdateTrail)

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
updateTrail_cloudWatchLogsRoleArn :: Lens.Lens' UpdateTrail (Core.Maybe Core.Text)
updateTrail_cloudWatchLogsRoleArn = Lens.lens (\UpdateTrail' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@UpdateTrail' {} a -> s {cloudWatchLogsRoleArn = a} :: UpdateTrail)

-- | Specifies whether log file validation is enabled. The default is false.
--
-- When you disable log file integrity validation, the chain of digest
-- files is broken after one hour. CloudTrail will not create digest files
-- for log files that were delivered during a period in which log file
-- integrity validation was disabled. For example, if you enable log file
-- integrity validation at noon on January 1, disable it at noon on January
-- 2, and re-enable it at noon on January 10, digest files will not be
-- created for the log files delivered from noon on January 2 to noon on
-- January 10. The same applies whenever you stop CloudTrail logging or
-- delete a trail.
updateTrail_enableLogFileValidation :: Lens.Lens' UpdateTrail (Core.Maybe Core.Bool)
updateTrail_enableLogFileValidation = Lens.lens (\UpdateTrail' {enableLogFileValidation} -> enableLogFileValidation) (\s@UpdateTrail' {} a -> s {enableLogFileValidation = a} :: UpdateTrail)

-- | Specifies the name of the trail or trail ARN. If @Name@ is a trail name,
-- the string must meet the following requirements:
--
-- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.),
--     underscores (_), or dashes (-)
--
-- -   Start with a letter or number, and end with a letter or number
--
-- -   Be between 3 and 128 characters
--
-- -   Have no adjacent periods, underscores or dashes. Names like
--     @my-_namespace@ and @my--namespace@ are invalid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
--
-- If @Name@ is a trail ARN, it must be in the format:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
updateTrail_name :: Lens.Lens' UpdateTrail Core.Text
updateTrail_name = Lens.lens (\UpdateTrail' {name} -> name) (\s@UpdateTrail' {} a -> s {name = a} :: UpdateTrail)

instance Core.AWSRequest UpdateTrail where
  type AWSResponse UpdateTrail = UpdateTrailResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrailResponse'
            Core.<$> (x Core..?> "TrailARN")
            Core.<*> (x Core..?> "LogFileValidationEnabled")
            Core.<*> (x Core..?> "IsOrganizationTrail")
            Core.<*> (x Core..?> "SnsTopicName")
            Core.<*> (x Core..?> "IncludeGlobalServiceEvents")
            Core.<*> (x Core..?> "KmsKeyId")
            Core.<*> (x Core..?> "S3KeyPrefix")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "CloudWatchLogsLogGroupArn")
            Core.<*> (x Core..?> "IsMultiRegionTrail")
            Core.<*> (x Core..?> "S3BucketName")
            Core.<*> (x Core..?> "CloudWatchLogsRoleArn")
            Core.<*> (x Core..?> "SnsTopicARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateTrail

instance Core.NFData UpdateTrail

instance Core.ToHeaders UpdateTrail where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.UpdateTrail" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateTrail where
  toJSON UpdateTrail' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IsOrganizationTrail" Core..=)
              Core.<$> isOrganizationTrail,
            ("SnsTopicName" Core..=) Core.<$> snsTopicName,
            ("IncludeGlobalServiceEvents" Core..=)
              Core.<$> includeGlobalServiceEvents,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("S3KeyPrefix" Core..=) Core.<$> s3KeyPrefix,
            ("CloudWatchLogsLogGroupArn" Core..=)
              Core.<$> cloudWatchLogsLogGroupArn,
            ("IsMultiRegionTrail" Core..=)
              Core.<$> isMultiRegionTrail,
            ("S3BucketName" Core..=) Core.<$> s3BucketName,
            ("CloudWatchLogsRoleArn" Core..=)
              Core.<$> cloudWatchLogsRoleArn,
            ("EnableLogFileValidation" Core..=)
              Core.<$> enableLogFileValidation,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateTrail where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTrail where
  toQuery = Core.const Core.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newUpdateTrailResponse' smart constructor.
data UpdateTrailResponse = UpdateTrailResponse'
  { -- | Specifies the ARN of the trail that was updated. The format of a trail
    -- ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    trailARN :: Core.Maybe Core.Text,
    -- | Specifies whether log file integrity validation is enabled.
    logFileValidationEnabled :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail is an organization trail.
    isOrganizationTrail :: Core.Maybe Core.Bool,
    -- | This field is no longer in use. Use SnsTopicARN.
    snsTopicName :: Core.Maybe Core.Text,
    -- | Specifies whether the trail is publishing events from global services
    -- such as IAM to the log files.
    includeGlobalServiceEvents :: Core.Maybe Core.Bool,
    -- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
    -- The value is a fully specified ARN to a KMS key in the format:
    --
    -- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the
    -- bucket you have designated for log file delivery. For more information,
    -- see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
    s3KeyPrefix :: Core.Maybe Core.Text,
    -- | Specifies the name of the trail.
    name :: Core.Maybe Core.Text,
    -- | Specifies the Amazon Resource Name (ARN) of the log group to which
    -- CloudTrail logs will be delivered.
    cloudWatchLogsLogGroupArn :: Core.Maybe Core.Text,
    -- | Specifies whether the trail exists in one region or in all regions.
    isMultiRegionTrail :: Core.Maybe Core.Bool,
    -- | Specifies the name of the Amazon S3 bucket designated for publishing log
    -- files.
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
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTrailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailARN', 'updateTrailResponse_trailARN' - Specifies the ARN of the trail that was updated. The format of a trail
-- ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
--
-- 'logFileValidationEnabled', 'updateTrailResponse_logFileValidationEnabled' - Specifies whether log file integrity validation is enabled.
--
-- 'isOrganizationTrail', 'updateTrailResponse_isOrganizationTrail' - Specifies whether the trail is an organization trail.
--
-- 'snsTopicName', 'updateTrailResponse_snsTopicName' - This field is no longer in use. Use SnsTopicARN.
--
-- 'includeGlobalServiceEvents', 'updateTrailResponse_includeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
--
-- 'kmsKeyId', 'updateTrailResponse_kmsKeyId' - Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- 's3KeyPrefix', 'updateTrailResponse_s3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
--
-- 'name', 'updateTrailResponse_name' - Specifies the name of the trail.
--
-- 'cloudWatchLogsLogGroupArn', 'updateTrailResponse_cloudWatchLogsLogGroupArn' - Specifies the Amazon Resource Name (ARN) of the log group to which
-- CloudTrail logs will be delivered.
--
-- 'isMultiRegionTrail', 'updateTrailResponse_isMultiRegionTrail' - Specifies whether the trail exists in one region or in all regions.
--
-- 's3BucketName', 'updateTrailResponse_s3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
--
-- 'cloudWatchLogsRoleArn', 'updateTrailResponse_cloudWatchLogsRoleArn' - Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
--
-- 'snsTopicARN', 'updateTrailResponse_snsTopicARN' - Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
-- notifications when log files are delivered. The format of a topic ARN
-- is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
--
-- 'httpStatus', 'updateTrailResponse_httpStatus' - The response's http status code.
newUpdateTrailResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateTrailResponse
newUpdateTrailResponse pHttpStatus_ =
  UpdateTrailResponse'
    { trailARN = Core.Nothing,
      logFileValidationEnabled = Core.Nothing,
      isOrganizationTrail = Core.Nothing,
      snsTopicName = Core.Nothing,
      includeGlobalServiceEvents = Core.Nothing,
      kmsKeyId = Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      name = Core.Nothing,
      cloudWatchLogsLogGroupArn = Core.Nothing,
      isMultiRegionTrail = Core.Nothing,
      s3BucketName = Core.Nothing,
      cloudWatchLogsRoleArn = Core.Nothing,
      snsTopicARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the ARN of the trail that was updated. The format of a trail
-- ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
updateTrailResponse_trailARN :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Text)
updateTrailResponse_trailARN = Lens.lens (\UpdateTrailResponse' {trailARN} -> trailARN) (\s@UpdateTrailResponse' {} a -> s {trailARN = a} :: UpdateTrailResponse)

-- | Specifies whether log file integrity validation is enabled.
updateTrailResponse_logFileValidationEnabled :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Bool)
updateTrailResponse_logFileValidationEnabled = Lens.lens (\UpdateTrailResponse' {logFileValidationEnabled} -> logFileValidationEnabled) (\s@UpdateTrailResponse' {} a -> s {logFileValidationEnabled = a} :: UpdateTrailResponse)

-- | Specifies whether the trail is an organization trail.
updateTrailResponse_isOrganizationTrail :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Bool)
updateTrailResponse_isOrganizationTrail = Lens.lens (\UpdateTrailResponse' {isOrganizationTrail} -> isOrganizationTrail) (\s@UpdateTrailResponse' {} a -> s {isOrganizationTrail = a} :: UpdateTrailResponse)

-- | This field is no longer in use. Use SnsTopicARN.
updateTrailResponse_snsTopicName :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Text)
updateTrailResponse_snsTopicName = Lens.lens (\UpdateTrailResponse' {snsTopicName} -> snsTopicName) (\s@UpdateTrailResponse' {} a -> s {snsTopicName = a} :: UpdateTrailResponse)

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
updateTrailResponse_includeGlobalServiceEvents :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Bool)
updateTrailResponse_includeGlobalServiceEvents = Lens.lens (\UpdateTrailResponse' {includeGlobalServiceEvents} -> includeGlobalServiceEvents) (\s@UpdateTrailResponse' {} a -> s {includeGlobalServiceEvents = a} :: UpdateTrailResponse)

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
updateTrailResponse_kmsKeyId :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Text)
updateTrailResponse_kmsKeyId = Lens.lens (\UpdateTrailResponse' {kmsKeyId} -> kmsKeyId) (\s@UpdateTrailResponse' {} a -> s {kmsKeyId = a} :: UpdateTrailResponse)

-- | Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
updateTrailResponse_s3KeyPrefix :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Text)
updateTrailResponse_s3KeyPrefix = Lens.lens (\UpdateTrailResponse' {s3KeyPrefix} -> s3KeyPrefix) (\s@UpdateTrailResponse' {} a -> s {s3KeyPrefix = a} :: UpdateTrailResponse)

-- | Specifies the name of the trail.
updateTrailResponse_name :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Text)
updateTrailResponse_name = Lens.lens (\UpdateTrailResponse' {name} -> name) (\s@UpdateTrailResponse' {} a -> s {name = a} :: UpdateTrailResponse)

-- | Specifies the Amazon Resource Name (ARN) of the log group to which
-- CloudTrail logs will be delivered.
updateTrailResponse_cloudWatchLogsLogGroupArn :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Text)
updateTrailResponse_cloudWatchLogsLogGroupArn = Lens.lens (\UpdateTrailResponse' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@UpdateTrailResponse' {} a -> s {cloudWatchLogsLogGroupArn = a} :: UpdateTrailResponse)

-- | Specifies whether the trail exists in one region or in all regions.
updateTrailResponse_isMultiRegionTrail :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Bool)
updateTrailResponse_isMultiRegionTrail = Lens.lens (\UpdateTrailResponse' {isMultiRegionTrail} -> isMultiRegionTrail) (\s@UpdateTrailResponse' {} a -> s {isMultiRegionTrail = a} :: UpdateTrailResponse)

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
updateTrailResponse_s3BucketName :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Text)
updateTrailResponse_s3BucketName = Lens.lens (\UpdateTrailResponse' {s3BucketName} -> s3BucketName) (\s@UpdateTrailResponse' {} a -> s {s3BucketName = a} :: UpdateTrailResponse)

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
updateTrailResponse_cloudWatchLogsRoleArn :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Text)
updateTrailResponse_cloudWatchLogsRoleArn = Lens.lens (\UpdateTrailResponse' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@UpdateTrailResponse' {} a -> s {cloudWatchLogsRoleArn = a} :: UpdateTrailResponse)

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
-- notifications when log files are delivered. The format of a topic ARN
-- is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
updateTrailResponse_snsTopicARN :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Text)
updateTrailResponse_snsTopicARN = Lens.lens (\UpdateTrailResponse' {snsTopicARN} -> snsTopicARN) (\s@UpdateTrailResponse' {} a -> s {snsTopicARN = a} :: UpdateTrailResponse)

-- | The response's http status code.
updateTrailResponse_httpStatus :: Lens.Lens' UpdateTrailResponse Core.Int
updateTrailResponse_httpStatus = Lens.lens (\UpdateTrailResponse' {httpStatus} -> httpStatus) (\s@UpdateTrailResponse' {} a -> s {httpStatus = a} :: UpdateTrailResponse)

instance Core.NFData UpdateTrailResponse
