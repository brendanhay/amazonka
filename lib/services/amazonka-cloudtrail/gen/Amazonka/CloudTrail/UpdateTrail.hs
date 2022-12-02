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
-- Module      : Amazonka.CloudTrail.UpdateTrail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates trail settings that control what events you are logging, and how
-- to handle log files. Changes to a trail do not require stopping the
-- CloudTrail service. Use this action to designate an existing bucket for
-- log delivery. If the existing bucket has previously been a target for
-- CloudTrail log files, an IAM policy exists for the bucket. @UpdateTrail@
-- must be called from the region in which the trail was created;
-- otherwise, an @InvalidHomeRegionException@ is thrown.
module Amazonka.CloudTrail.UpdateTrail
  ( -- * Creating a Request
    UpdateTrail (..),
    newUpdateTrail,

    -- * Request Lenses
    updateTrail_s3KeyPrefix,
    updateTrail_snsTopicName,
    updateTrail_isOrganizationTrail,
    updateTrail_includeGlobalServiceEvents,
    updateTrail_s3BucketName,
    updateTrail_isMultiRegionTrail,
    updateTrail_kmsKeyId,
    updateTrail_enableLogFileValidation,
    updateTrail_cloudWatchLogsRoleArn,
    updateTrail_cloudWatchLogsLogGroupArn,
    updateTrail_name,

    -- * Destructuring the Response
    UpdateTrailResponse (..),
    newUpdateTrailResponse,

    -- * Response Lenses
    updateTrailResponse_s3KeyPrefix,
    updateTrailResponse_name,
    updateTrailResponse_logFileValidationEnabled,
    updateTrailResponse_snsTopicName,
    updateTrailResponse_isOrganizationTrail,
    updateTrailResponse_includeGlobalServiceEvents,
    updateTrailResponse_s3BucketName,
    updateTrailResponse_snsTopicARN,
    updateTrailResponse_isMultiRegionTrail,
    updateTrailResponse_kmsKeyId,
    updateTrailResponse_cloudWatchLogsRoleArn,
    updateTrailResponse_cloudWatchLogsLogGroupArn,
    updateTrailResponse_trailARN,
    updateTrailResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Specifies settings to update for the trail.
--
-- /See:/ 'newUpdateTrail' smart constructor.
data UpdateTrail = UpdateTrail'
  { -- | Specifies the Amazon S3 key prefix that comes after the name of the
    -- bucket you have designated for log file delivery. For more information,
    -- see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
    -- The maximum length is 200 characters.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the Amazon SNS topic defined for notification of
    -- log file delivery. The maximum length is 256 characters.
    snsTopicName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the trail is applied to all accounts in an
    -- organization in Organizations, or only for the current Amazon Web
    -- Services account. The default is false, and cannot be true unless the
    -- call is made on behalf of an Amazon Web Services account that is the
    -- management account for an organization in Organizations. If the trail is
    -- not an organization trail and this is set to @true@, the trail will be
    -- created in all Amazon Web Services accounts that belong to the
    -- organization. If the trail is an organization trail and this is set to
    -- @false@, the trail will remain in the current Amazon Web Services
    -- account but be deleted from all member accounts in the organization.
    isOrganizationTrail :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the trail is publishing events from global services
    -- such as IAM to the log files.
    includeGlobalServiceEvents :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the Amazon S3 bucket designated for publishing log
    -- files. See
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the trail applies only to the current region or to all
    -- regions. The default is false. If the trail exists only in the current
    -- region and this value is set to true, shadow trails (replications of the
    -- trail) will be created in the other regions. If the trail exists in all
    -- regions and this value is set to false, the trail will remain in the
    -- region where it was created, and its shadow trails in other regions will
    -- be deleted. As a best practice, consider using trails that log events in
    -- all regions.
    isMultiRegionTrail :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the KMS key ID to use to encrypt the logs delivered by
    -- CloudTrail. The value can be an alias name prefixed by \"alias\/\", a
    -- fully specified ARN to an alias, a fully specified ARN to a key, or a
    -- globally unique identifier.
    --
    -- CloudTrail also supports KMS multi-Region keys. For more information
    -- about multi-Region keys, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Using multi-Region keys>
    -- in the /Key Management Service Developer Guide/.
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
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether log file validation is enabled. The default is false.
    --
    -- When you disable log file integrity validation, the chain of digest
    -- files is broken after one hour. CloudTrail does not create digest files
    -- for log files that were delivered during a period in which log file
    -- integrity validation was disabled. For example, if you enable log file
    -- integrity validation at noon on January 1, disable it at noon on January
    -- 2, and re-enable it at noon on January 10, digest files will not be
    -- created for the log files delivered from noon on January 2 to noon on
    -- January 10. The same applies whenever you stop CloudTrail logging or
    -- delete a trail.
    enableLogFileValidation :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write
    -- to a user\'s log group.
    cloudWatchLogsRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies a log group name using an Amazon Resource Name (ARN), a unique
    -- identifier that represents the log group to which CloudTrail logs are
    -- delivered. Not required unless you specify @CloudWatchLogsRoleArn@.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
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
    --     @my-_namespace@ and @my--namespace@ are not valid.
    --
    -- -   Not be in IP address format (for example, 192.168.5.4)
    --
    -- If @Name@ is a trail ARN, it must be in the following format.
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KeyPrefix', 'updateTrail_s3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
-- The maximum length is 200 characters.
--
-- 'snsTopicName', 'updateTrail_snsTopicName' - Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery. The maximum length is 256 characters.
--
-- 'isOrganizationTrail', 'updateTrail_isOrganizationTrail' - Specifies whether the trail is applied to all accounts in an
-- organization in Organizations, or only for the current Amazon Web
-- Services account. The default is false, and cannot be true unless the
-- call is made on behalf of an Amazon Web Services account that is the
-- management account for an organization in Organizations. If the trail is
-- not an organization trail and this is set to @true@, the trail will be
-- created in all Amazon Web Services accounts that belong to the
-- organization. If the trail is an organization trail and this is set to
-- @false@, the trail will remain in the current Amazon Web Services
-- account but be deleted from all member accounts in the organization.
--
-- 'includeGlobalServiceEvents', 'updateTrail_includeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
--
-- 's3BucketName', 'updateTrail_s3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log
-- files. See
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
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
-- 'kmsKeyId', 'updateTrail_kmsKeyId' - Specifies the KMS key ID to use to encrypt the logs delivered by
-- CloudTrail. The value can be an alias name prefixed by \"alias\/\", a
-- fully specified ARN to an alias, a fully specified ARN to a key, or a
-- globally unique identifier.
--
-- CloudTrail also supports KMS multi-Region keys. For more information
-- about multi-Region keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Using multi-Region keys>
-- in the /Key Management Service Developer Guide/.
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
-- 'enableLogFileValidation', 'updateTrail_enableLogFileValidation' - Specifies whether log file validation is enabled. The default is false.
--
-- When you disable log file integrity validation, the chain of digest
-- files is broken after one hour. CloudTrail does not create digest files
-- for log files that were delivered during a period in which log file
-- integrity validation was disabled. For example, if you enable log file
-- integrity validation at noon on January 1, disable it at noon on January
-- 2, and re-enable it at noon on January 10, digest files will not be
-- created for the log files delivered from noon on January 2 to noon on
-- January 10. The same applies whenever you stop CloudTrail logging or
-- delete a trail.
--
-- 'cloudWatchLogsRoleArn', 'updateTrail_cloudWatchLogsRoleArn' - Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
--
-- 'cloudWatchLogsLogGroupArn', 'updateTrail_cloudWatchLogsLogGroupArn' - Specifies a log group name using an Amazon Resource Name (ARN), a unique
-- identifier that represents the log group to which CloudTrail logs are
-- delivered. Not required unless you specify @CloudWatchLogsRoleArn@.
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
--     @my-_namespace@ and @my--namespace@ are not valid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
--
-- If @Name@ is a trail ARN, it must be in the following format.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newUpdateTrail ::
  -- | 'name'
  Prelude.Text ->
  UpdateTrail
newUpdateTrail pName_ =
  UpdateTrail'
    { s3KeyPrefix = Prelude.Nothing,
      snsTopicName = Prelude.Nothing,
      isOrganizationTrail = Prelude.Nothing,
      includeGlobalServiceEvents = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      isMultiRegionTrail = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      enableLogFileValidation = Prelude.Nothing,
      cloudWatchLogsRoleArn = Prelude.Nothing,
      cloudWatchLogsLogGroupArn = Prelude.Nothing,
      name = pName_
    }

-- | Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
-- The maximum length is 200 characters.
updateTrail_s3KeyPrefix :: Lens.Lens' UpdateTrail (Prelude.Maybe Prelude.Text)
updateTrail_s3KeyPrefix = Lens.lens (\UpdateTrail' {s3KeyPrefix} -> s3KeyPrefix) (\s@UpdateTrail' {} a -> s {s3KeyPrefix = a} :: UpdateTrail)

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery. The maximum length is 256 characters.
updateTrail_snsTopicName :: Lens.Lens' UpdateTrail (Prelude.Maybe Prelude.Text)
updateTrail_snsTopicName = Lens.lens (\UpdateTrail' {snsTopicName} -> snsTopicName) (\s@UpdateTrail' {} a -> s {snsTopicName = a} :: UpdateTrail)

-- | Specifies whether the trail is applied to all accounts in an
-- organization in Organizations, or only for the current Amazon Web
-- Services account. The default is false, and cannot be true unless the
-- call is made on behalf of an Amazon Web Services account that is the
-- management account for an organization in Organizations. If the trail is
-- not an organization trail and this is set to @true@, the trail will be
-- created in all Amazon Web Services accounts that belong to the
-- organization. If the trail is an organization trail and this is set to
-- @false@, the trail will remain in the current Amazon Web Services
-- account but be deleted from all member accounts in the organization.
updateTrail_isOrganizationTrail :: Lens.Lens' UpdateTrail (Prelude.Maybe Prelude.Bool)
updateTrail_isOrganizationTrail = Lens.lens (\UpdateTrail' {isOrganizationTrail} -> isOrganizationTrail) (\s@UpdateTrail' {} a -> s {isOrganizationTrail = a} :: UpdateTrail)

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
updateTrail_includeGlobalServiceEvents :: Lens.Lens' UpdateTrail (Prelude.Maybe Prelude.Bool)
updateTrail_includeGlobalServiceEvents = Lens.lens (\UpdateTrail' {includeGlobalServiceEvents} -> includeGlobalServiceEvents) (\s@UpdateTrail' {} a -> s {includeGlobalServiceEvents = a} :: UpdateTrail)

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files. See
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
updateTrail_s3BucketName :: Lens.Lens' UpdateTrail (Prelude.Maybe Prelude.Text)
updateTrail_s3BucketName = Lens.lens (\UpdateTrail' {s3BucketName} -> s3BucketName) (\s@UpdateTrail' {} a -> s {s3BucketName = a} :: UpdateTrail)

-- | Specifies whether the trail applies only to the current region or to all
-- regions. The default is false. If the trail exists only in the current
-- region and this value is set to true, shadow trails (replications of the
-- trail) will be created in the other regions. If the trail exists in all
-- regions and this value is set to false, the trail will remain in the
-- region where it was created, and its shadow trails in other regions will
-- be deleted. As a best practice, consider using trails that log events in
-- all regions.
updateTrail_isMultiRegionTrail :: Lens.Lens' UpdateTrail (Prelude.Maybe Prelude.Bool)
updateTrail_isMultiRegionTrail = Lens.lens (\UpdateTrail' {isMultiRegionTrail} -> isMultiRegionTrail) (\s@UpdateTrail' {} a -> s {isMultiRegionTrail = a} :: UpdateTrail)

-- | Specifies the KMS key ID to use to encrypt the logs delivered by
-- CloudTrail. The value can be an alias name prefixed by \"alias\/\", a
-- fully specified ARN to an alias, a fully specified ARN to a key, or a
-- globally unique identifier.
--
-- CloudTrail also supports KMS multi-Region keys. For more information
-- about multi-Region keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Using multi-Region keys>
-- in the /Key Management Service Developer Guide/.
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
updateTrail_kmsKeyId :: Lens.Lens' UpdateTrail (Prelude.Maybe Prelude.Text)
updateTrail_kmsKeyId = Lens.lens (\UpdateTrail' {kmsKeyId} -> kmsKeyId) (\s@UpdateTrail' {} a -> s {kmsKeyId = a} :: UpdateTrail)

-- | Specifies whether log file validation is enabled. The default is false.
--
-- When you disable log file integrity validation, the chain of digest
-- files is broken after one hour. CloudTrail does not create digest files
-- for log files that were delivered during a period in which log file
-- integrity validation was disabled. For example, if you enable log file
-- integrity validation at noon on January 1, disable it at noon on January
-- 2, and re-enable it at noon on January 10, digest files will not be
-- created for the log files delivered from noon on January 2 to noon on
-- January 10. The same applies whenever you stop CloudTrail logging or
-- delete a trail.
updateTrail_enableLogFileValidation :: Lens.Lens' UpdateTrail (Prelude.Maybe Prelude.Bool)
updateTrail_enableLogFileValidation = Lens.lens (\UpdateTrail' {enableLogFileValidation} -> enableLogFileValidation) (\s@UpdateTrail' {} a -> s {enableLogFileValidation = a} :: UpdateTrail)

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
updateTrail_cloudWatchLogsRoleArn :: Lens.Lens' UpdateTrail (Prelude.Maybe Prelude.Text)
updateTrail_cloudWatchLogsRoleArn = Lens.lens (\UpdateTrail' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@UpdateTrail' {} a -> s {cloudWatchLogsRoleArn = a} :: UpdateTrail)

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique
-- identifier that represents the log group to which CloudTrail logs are
-- delivered. Not required unless you specify @CloudWatchLogsRoleArn@.
updateTrail_cloudWatchLogsLogGroupArn :: Lens.Lens' UpdateTrail (Prelude.Maybe Prelude.Text)
updateTrail_cloudWatchLogsLogGroupArn = Lens.lens (\UpdateTrail' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@UpdateTrail' {} a -> s {cloudWatchLogsLogGroupArn = a} :: UpdateTrail)

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
--     @my-_namespace@ and @my--namespace@ are not valid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
--
-- If @Name@ is a trail ARN, it must be in the following format.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
updateTrail_name :: Lens.Lens' UpdateTrail Prelude.Text
updateTrail_name = Lens.lens (\UpdateTrail' {name} -> name) (\s@UpdateTrail' {} a -> s {name = a} :: UpdateTrail)

instance Core.AWSRequest UpdateTrail where
  type AWSResponse UpdateTrail = UpdateTrailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrailResponse'
            Prelude.<$> (x Data..?> "S3KeyPrefix")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "LogFileValidationEnabled")
            Prelude.<*> (x Data..?> "SnsTopicName")
            Prelude.<*> (x Data..?> "IsOrganizationTrail")
            Prelude.<*> (x Data..?> "IncludeGlobalServiceEvents")
            Prelude.<*> (x Data..?> "S3BucketName")
            Prelude.<*> (x Data..?> "SnsTopicARN")
            Prelude.<*> (x Data..?> "IsMultiRegionTrail")
            Prelude.<*> (x Data..?> "KmsKeyId")
            Prelude.<*> (x Data..?> "CloudWatchLogsRoleArn")
            Prelude.<*> (x Data..?> "CloudWatchLogsLogGroupArn")
            Prelude.<*> (x Data..?> "TrailARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTrail where
  hashWithSalt _salt UpdateTrail' {..} =
    _salt `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` snsTopicName
      `Prelude.hashWithSalt` isOrganizationTrail
      `Prelude.hashWithSalt` includeGlobalServiceEvents
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` isMultiRegionTrail
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` enableLogFileValidation
      `Prelude.hashWithSalt` cloudWatchLogsRoleArn
      `Prelude.hashWithSalt` cloudWatchLogsLogGroupArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateTrail where
  rnf UpdateTrail' {..} =
    Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf snsTopicName
      `Prelude.seq` Prelude.rnf isOrganizationTrail
      `Prelude.seq` Prelude.rnf includeGlobalServiceEvents
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf isMultiRegionTrail
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf enableLogFileValidation
      `Prelude.seq` Prelude.rnf cloudWatchLogsRoleArn
      `Prelude.seq` Prelude.rnf cloudWatchLogsLogGroupArn
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateTrail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.UpdateTrail" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTrail where
  toJSON UpdateTrail' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3KeyPrefix" Data..=) Prelude.<$> s3KeyPrefix,
            ("SnsTopicName" Data..=) Prelude.<$> snsTopicName,
            ("IsOrganizationTrail" Data..=)
              Prelude.<$> isOrganizationTrail,
            ("IncludeGlobalServiceEvents" Data..=)
              Prelude.<$> includeGlobalServiceEvents,
            ("S3BucketName" Data..=) Prelude.<$> s3BucketName,
            ("IsMultiRegionTrail" Data..=)
              Prelude.<$> isMultiRegionTrail,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("EnableLogFileValidation" Data..=)
              Prelude.<$> enableLogFileValidation,
            ("CloudWatchLogsRoleArn" Data..=)
              Prelude.<$> cloudWatchLogsRoleArn,
            ("CloudWatchLogsLogGroupArn" Data..=)
              Prelude.<$> cloudWatchLogsLogGroupArn,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateTrail where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTrail where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newUpdateTrailResponse' smart constructor.
data UpdateTrailResponse = UpdateTrailResponse'
  { -- | Specifies the Amazon S3 key prefix that comes after the name of the
    -- bucket you have designated for log file delivery. For more information,
    -- see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your IAM Log Files>.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the trail.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether log file integrity validation is enabled.
    logFileValidationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | This field is no longer in use. Use UpdateTrailResponse$SnsTopicARN.
    snsTopicName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the trail is an organization trail.
    isOrganizationTrail :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the trail is publishing events from global services
    -- such as IAM to the log files.
    includeGlobalServiceEvents :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the Amazon S3 bucket designated for publishing log
    -- files.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
    -- notifications when log files are delivered. The following is the format
    -- of a topic ARN.
    --
    -- @arn:aws:sns:us-east-2:123456789012:MyTopic@
    snsTopicARN :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the trail exists in one region or in all regions.
    isMultiRegionTrail :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
    -- The value is a fully specified ARN to a KMS key in the following format.
    --
    -- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write
    -- to a user\'s log group.
    cloudWatchLogsRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon Resource Name (ARN) of the log group to which
    -- CloudTrail logs are delivered.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ARN of the trail that was updated. The following is the
    -- format of a trail ARN.
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    trailARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KeyPrefix', 'updateTrailResponse_s3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your IAM Log Files>.
--
-- 'name', 'updateTrailResponse_name' - Specifies the name of the trail.
--
-- 'logFileValidationEnabled', 'updateTrailResponse_logFileValidationEnabled' - Specifies whether log file integrity validation is enabled.
--
-- 'snsTopicName', 'updateTrailResponse_snsTopicName' - This field is no longer in use. Use UpdateTrailResponse$SnsTopicARN.
--
-- 'isOrganizationTrail', 'updateTrailResponse_isOrganizationTrail' - Specifies whether the trail is an organization trail.
--
-- 'includeGlobalServiceEvents', 'updateTrailResponse_includeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
--
-- 's3BucketName', 'updateTrailResponse_s3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
--
-- 'snsTopicARN', 'updateTrailResponse_snsTopicARN' - Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
-- notifications when log files are delivered. The following is the format
-- of a topic ARN.
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
--
-- 'isMultiRegionTrail', 'updateTrailResponse_isMultiRegionTrail' - Specifies whether the trail exists in one region or in all regions.
--
-- 'kmsKeyId', 'updateTrailResponse_kmsKeyId' - Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the following format.
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- 'cloudWatchLogsRoleArn', 'updateTrailResponse_cloudWatchLogsRoleArn' - Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
--
-- 'cloudWatchLogsLogGroupArn', 'updateTrailResponse_cloudWatchLogsLogGroupArn' - Specifies the Amazon Resource Name (ARN) of the log group to which
-- CloudTrail logs are delivered.
--
-- 'trailARN', 'updateTrailResponse_trailARN' - Specifies the ARN of the trail that was updated. The following is the
-- format of a trail ARN.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
--
-- 'httpStatus', 'updateTrailResponse_httpStatus' - The response's http status code.
newUpdateTrailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTrailResponse
newUpdateTrailResponse pHttpStatus_ =
  UpdateTrailResponse'
    { s3KeyPrefix = Prelude.Nothing,
      name = Prelude.Nothing,
      logFileValidationEnabled = Prelude.Nothing,
      snsTopicName = Prelude.Nothing,
      isOrganizationTrail = Prelude.Nothing,
      includeGlobalServiceEvents = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      snsTopicARN = Prelude.Nothing,
      isMultiRegionTrail = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      cloudWatchLogsRoleArn = Prelude.Nothing,
      cloudWatchLogsLogGroupArn = Prelude.Nothing,
      trailARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your IAM Log Files>.
updateTrailResponse_s3KeyPrefix :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Text)
updateTrailResponse_s3KeyPrefix = Lens.lens (\UpdateTrailResponse' {s3KeyPrefix} -> s3KeyPrefix) (\s@UpdateTrailResponse' {} a -> s {s3KeyPrefix = a} :: UpdateTrailResponse)

-- | Specifies the name of the trail.
updateTrailResponse_name :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Text)
updateTrailResponse_name = Lens.lens (\UpdateTrailResponse' {name} -> name) (\s@UpdateTrailResponse' {} a -> s {name = a} :: UpdateTrailResponse)

-- | Specifies whether log file integrity validation is enabled.
updateTrailResponse_logFileValidationEnabled :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Bool)
updateTrailResponse_logFileValidationEnabled = Lens.lens (\UpdateTrailResponse' {logFileValidationEnabled} -> logFileValidationEnabled) (\s@UpdateTrailResponse' {} a -> s {logFileValidationEnabled = a} :: UpdateTrailResponse)

-- | This field is no longer in use. Use UpdateTrailResponse$SnsTopicARN.
updateTrailResponse_snsTopicName :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Text)
updateTrailResponse_snsTopicName = Lens.lens (\UpdateTrailResponse' {snsTopicName} -> snsTopicName) (\s@UpdateTrailResponse' {} a -> s {snsTopicName = a} :: UpdateTrailResponse)

-- | Specifies whether the trail is an organization trail.
updateTrailResponse_isOrganizationTrail :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Bool)
updateTrailResponse_isOrganizationTrail = Lens.lens (\UpdateTrailResponse' {isOrganizationTrail} -> isOrganizationTrail) (\s@UpdateTrailResponse' {} a -> s {isOrganizationTrail = a} :: UpdateTrailResponse)

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
updateTrailResponse_includeGlobalServiceEvents :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Bool)
updateTrailResponse_includeGlobalServiceEvents = Lens.lens (\UpdateTrailResponse' {includeGlobalServiceEvents} -> includeGlobalServiceEvents) (\s@UpdateTrailResponse' {} a -> s {includeGlobalServiceEvents = a} :: UpdateTrailResponse)

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
updateTrailResponse_s3BucketName :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Text)
updateTrailResponse_s3BucketName = Lens.lens (\UpdateTrailResponse' {s3BucketName} -> s3BucketName) (\s@UpdateTrailResponse' {} a -> s {s3BucketName = a} :: UpdateTrailResponse)

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
-- notifications when log files are delivered. The following is the format
-- of a topic ARN.
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
updateTrailResponse_snsTopicARN :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Text)
updateTrailResponse_snsTopicARN = Lens.lens (\UpdateTrailResponse' {snsTopicARN} -> snsTopicARN) (\s@UpdateTrailResponse' {} a -> s {snsTopicARN = a} :: UpdateTrailResponse)

-- | Specifies whether the trail exists in one region or in all regions.
updateTrailResponse_isMultiRegionTrail :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Bool)
updateTrailResponse_isMultiRegionTrail = Lens.lens (\UpdateTrailResponse' {isMultiRegionTrail} -> isMultiRegionTrail) (\s@UpdateTrailResponse' {} a -> s {isMultiRegionTrail = a} :: UpdateTrailResponse)

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the following format.
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
updateTrailResponse_kmsKeyId :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Text)
updateTrailResponse_kmsKeyId = Lens.lens (\UpdateTrailResponse' {kmsKeyId} -> kmsKeyId) (\s@UpdateTrailResponse' {} a -> s {kmsKeyId = a} :: UpdateTrailResponse)

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
updateTrailResponse_cloudWatchLogsRoleArn :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Text)
updateTrailResponse_cloudWatchLogsRoleArn = Lens.lens (\UpdateTrailResponse' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@UpdateTrailResponse' {} a -> s {cloudWatchLogsRoleArn = a} :: UpdateTrailResponse)

-- | Specifies the Amazon Resource Name (ARN) of the log group to which
-- CloudTrail logs are delivered.
updateTrailResponse_cloudWatchLogsLogGroupArn :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Text)
updateTrailResponse_cloudWatchLogsLogGroupArn = Lens.lens (\UpdateTrailResponse' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@UpdateTrailResponse' {} a -> s {cloudWatchLogsLogGroupArn = a} :: UpdateTrailResponse)

-- | Specifies the ARN of the trail that was updated. The following is the
-- format of a trail ARN.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
updateTrailResponse_trailARN :: Lens.Lens' UpdateTrailResponse (Prelude.Maybe Prelude.Text)
updateTrailResponse_trailARN = Lens.lens (\UpdateTrailResponse' {trailARN} -> trailARN) (\s@UpdateTrailResponse' {} a -> s {trailARN = a} :: UpdateTrailResponse)

-- | The response's http status code.
updateTrailResponse_httpStatus :: Lens.Lens' UpdateTrailResponse Prelude.Int
updateTrailResponse_httpStatus = Lens.lens (\UpdateTrailResponse' {httpStatus} -> httpStatus) (\s@UpdateTrailResponse' {} a -> s {httpStatus = a} :: UpdateTrailResponse)

instance Prelude.NFData UpdateTrailResponse where
  rnf UpdateTrailResponse' {..} =
    Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf logFileValidationEnabled
      `Prelude.seq` Prelude.rnf snsTopicName
      `Prelude.seq` Prelude.rnf isOrganizationTrail
      `Prelude.seq` Prelude.rnf includeGlobalServiceEvents
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf snsTopicARN
      `Prelude.seq` Prelude.rnf isMultiRegionTrail
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf cloudWatchLogsRoleArn
      `Prelude.seq` Prelude.rnf cloudWatchLogsLogGroupArn
      `Prelude.seq` Prelude.rnf trailARN
      `Prelude.seq` Prelude.rnf httpStatus
