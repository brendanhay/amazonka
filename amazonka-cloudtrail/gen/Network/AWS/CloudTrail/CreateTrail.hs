{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudTrail.CreateTrail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a trail that specifies the settings for delivery of log data to
-- an Amazon S3 bucket.
module Network.AWS.CloudTrail.CreateTrail
  ( -- * Creating a Request
    CreateTrail (..),
    newCreateTrail,

    -- * Request Lenses
    createTrail_isOrganizationTrail,
    createTrail_snsTopicName,
    createTrail_includeGlobalServiceEvents,
    createTrail_kmsKeyId,
    createTrail_s3KeyPrefix,
    createTrail_cloudWatchLogsLogGroupArn,
    createTrail_isMultiRegionTrail,
    createTrail_cloudWatchLogsRoleArn,
    createTrail_tagsList,
    createTrail_enableLogFileValidation,
    createTrail_name,
    createTrail_s3BucketName,

    -- * Destructuring the Response
    CreateTrailResponse (..),
    newCreateTrailResponse,

    -- * Response Lenses
    createTrailResponse_trailARN,
    createTrailResponse_logFileValidationEnabled,
    createTrailResponse_isOrganizationTrail,
    createTrailResponse_snsTopicName,
    createTrailResponse_includeGlobalServiceEvents,
    createTrailResponse_kmsKeyId,
    createTrailResponse_s3KeyPrefix,
    createTrailResponse_name,
    createTrailResponse_cloudWatchLogsLogGroupArn,
    createTrailResponse_isMultiRegionTrail,
    createTrailResponse_s3BucketName,
    createTrailResponse_cloudWatchLogsRoleArn,
    createTrailResponse_snsTopicARN,
    createTrailResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Specifies the settings for each trail.
--
-- /See:/ 'newCreateTrail' smart constructor.
data CreateTrail = CreateTrail'
  { -- | Specifies whether the trail is created for all accounts in an
    -- organization in AWS Organizations, or only for the current AWS account.
    -- The default is false, and cannot be true unless the call is made on
    -- behalf of an AWS account that is the master account for an organization
    -- in AWS Organizations.
    isOrganizationTrail :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the Amazon SNS topic defined for notification of
    -- log file delivery. The maximum length is 256 characters.
    snsTopicName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the trail is publishing events from global services
    -- such as IAM to the log files.
    includeGlobalServiceEvents :: Prelude.Maybe Prelude.Bool,
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
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the
    -- bucket you have designated for log file delivery. For more information,
    -- see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
    -- The maximum length is 200 characters.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Specifies a log group name using an Amazon Resource Name (ARN), a unique
    -- identifier that represents the log group to which CloudTrail logs will
    -- be delivered. Not required unless you specify CloudWatchLogsRoleArn.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the trail is created in the current region or in all
    -- regions. The default is false, which creates a trail only in the region
    -- where you are signed in. As a best practice, consider creating trails
    -- that log events in all regions.
    isMultiRegionTrail :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write
    -- to a user\'s log group.
    cloudWatchLogsRoleArn :: Prelude.Maybe Prelude.Text,
    tagsList :: Prelude.Maybe [Tag],
    -- | Specifies whether log file integrity validation is enabled. The default
    -- is false.
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
    enableLogFileValidation :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the trail. The name must meet the following
    -- requirements:
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
    name :: Prelude.Text,
    -- | Specifies the name of the Amazon S3 bucket designated for publishing log
    -- files. See
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
    s3BucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateTrail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isOrganizationTrail', 'createTrail_isOrganizationTrail' - Specifies whether the trail is created for all accounts in an
-- organization in AWS Organizations, or only for the current AWS account.
-- The default is false, and cannot be true unless the call is made on
-- behalf of an AWS account that is the master account for an organization
-- in AWS Organizations.
--
-- 'snsTopicName', 'createTrail_snsTopicName' - Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery. The maximum length is 256 characters.
--
-- 'includeGlobalServiceEvents', 'createTrail_includeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
--
-- 'kmsKeyId', 'createTrail_kmsKeyId' - Specifies the KMS key ID to use to encrypt the logs delivered by
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
-- 's3KeyPrefix', 'createTrail_s3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
-- The maximum length is 200 characters.
--
-- 'cloudWatchLogsLogGroupArn', 'createTrail_cloudWatchLogsLogGroupArn' - Specifies a log group name using an Amazon Resource Name (ARN), a unique
-- identifier that represents the log group to which CloudTrail logs will
-- be delivered. Not required unless you specify CloudWatchLogsRoleArn.
--
-- 'isMultiRegionTrail', 'createTrail_isMultiRegionTrail' - Specifies whether the trail is created in the current region or in all
-- regions. The default is false, which creates a trail only in the region
-- where you are signed in. As a best practice, consider creating trails
-- that log events in all regions.
--
-- 'cloudWatchLogsRoleArn', 'createTrail_cloudWatchLogsRoleArn' - Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
--
-- 'tagsList', 'createTrail_tagsList' - Undocumented member.
--
-- 'enableLogFileValidation', 'createTrail_enableLogFileValidation' - Specifies whether log file integrity validation is enabled. The default
-- is false.
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
-- 'name', 'createTrail_name' - Specifies the name of the trail. The name must meet the following
-- requirements:
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
-- 's3BucketName', 'createTrail_s3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log
-- files. See
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
newCreateTrail ::
  -- | 'name'
  Prelude.Text ->
  -- | 's3BucketName'
  Prelude.Text ->
  CreateTrail
newCreateTrail pName_ pS3BucketName_ =
  CreateTrail'
    { isOrganizationTrail = Prelude.Nothing,
      snsTopicName = Prelude.Nothing,
      includeGlobalServiceEvents = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      cloudWatchLogsLogGroupArn = Prelude.Nothing,
      isMultiRegionTrail = Prelude.Nothing,
      cloudWatchLogsRoleArn = Prelude.Nothing,
      tagsList = Prelude.Nothing,
      enableLogFileValidation = Prelude.Nothing,
      name = pName_,
      s3BucketName = pS3BucketName_
    }

-- | Specifies whether the trail is created for all accounts in an
-- organization in AWS Organizations, or only for the current AWS account.
-- The default is false, and cannot be true unless the call is made on
-- behalf of an AWS account that is the master account for an organization
-- in AWS Organizations.
createTrail_isOrganizationTrail :: Lens.Lens' CreateTrail (Prelude.Maybe Prelude.Bool)
createTrail_isOrganizationTrail = Lens.lens (\CreateTrail' {isOrganizationTrail} -> isOrganizationTrail) (\s@CreateTrail' {} a -> s {isOrganizationTrail = a} :: CreateTrail)

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery. The maximum length is 256 characters.
createTrail_snsTopicName :: Lens.Lens' CreateTrail (Prelude.Maybe Prelude.Text)
createTrail_snsTopicName = Lens.lens (\CreateTrail' {snsTopicName} -> snsTopicName) (\s@CreateTrail' {} a -> s {snsTopicName = a} :: CreateTrail)

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
createTrail_includeGlobalServiceEvents :: Lens.Lens' CreateTrail (Prelude.Maybe Prelude.Bool)
createTrail_includeGlobalServiceEvents = Lens.lens (\CreateTrail' {includeGlobalServiceEvents} -> includeGlobalServiceEvents) (\s@CreateTrail' {} a -> s {includeGlobalServiceEvents = a} :: CreateTrail)

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
createTrail_kmsKeyId :: Lens.Lens' CreateTrail (Prelude.Maybe Prelude.Text)
createTrail_kmsKeyId = Lens.lens (\CreateTrail' {kmsKeyId} -> kmsKeyId) (\s@CreateTrail' {} a -> s {kmsKeyId = a} :: CreateTrail)

-- | Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
-- The maximum length is 200 characters.
createTrail_s3KeyPrefix :: Lens.Lens' CreateTrail (Prelude.Maybe Prelude.Text)
createTrail_s3KeyPrefix = Lens.lens (\CreateTrail' {s3KeyPrefix} -> s3KeyPrefix) (\s@CreateTrail' {} a -> s {s3KeyPrefix = a} :: CreateTrail)

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique
-- identifier that represents the log group to which CloudTrail logs will
-- be delivered. Not required unless you specify CloudWatchLogsRoleArn.
createTrail_cloudWatchLogsLogGroupArn :: Lens.Lens' CreateTrail (Prelude.Maybe Prelude.Text)
createTrail_cloudWatchLogsLogGroupArn = Lens.lens (\CreateTrail' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@CreateTrail' {} a -> s {cloudWatchLogsLogGroupArn = a} :: CreateTrail)

-- | Specifies whether the trail is created in the current region or in all
-- regions. The default is false, which creates a trail only in the region
-- where you are signed in. As a best practice, consider creating trails
-- that log events in all regions.
createTrail_isMultiRegionTrail :: Lens.Lens' CreateTrail (Prelude.Maybe Prelude.Bool)
createTrail_isMultiRegionTrail = Lens.lens (\CreateTrail' {isMultiRegionTrail} -> isMultiRegionTrail) (\s@CreateTrail' {} a -> s {isMultiRegionTrail = a} :: CreateTrail)

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
createTrail_cloudWatchLogsRoleArn :: Lens.Lens' CreateTrail (Prelude.Maybe Prelude.Text)
createTrail_cloudWatchLogsRoleArn = Lens.lens (\CreateTrail' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@CreateTrail' {} a -> s {cloudWatchLogsRoleArn = a} :: CreateTrail)

-- | Undocumented member.
createTrail_tagsList :: Lens.Lens' CreateTrail (Prelude.Maybe [Tag])
createTrail_tagsList = Lens.lens (\CreateTrail' {tagsList} -> tagsList) (\s@CreateTrail' {} a -> s {tagsList = a} :: CreateTrail) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies whether log file integrity validation is enabled. The default
-- is false.
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
createTrail_enableLogFileValidation :: Lens.Lens' CreateTrail (Prelude.Maybe Prelude.Bool)
createTrail_enableLogFileValidation = Lens.lens (\CreateTrail' {enableLogFileValidation} -> enableLogFileValidation) (\s@CreateTrail' {} a -> s {enableLogFileValidation = a} :: CreateTrail)

-- | Specifies the name of the trail. The name must meet the following
-- requirements:
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
createTrail_name :: Lens.Lens' CreateTrail Prelude.Text
createTrail_name = Lens.lens (\CreateTrail' {name} -> name) (\s@CreateTrail' {} a -> s {name = a} :: CreateTrail)

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files. See
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
createTrail_s3BucketName :: Lens.Lens' CreateTrail Prelude.Text
createTrail_s3BucketName = Lens.lens (\CreateTrail' {s3BucketName} -> s3BucketName) (\s@CreateTrail' {} a -> s {s3BucketName = a} :: CreateTrail)

instance Prelude.AWSRequest CreateTrail where
  type Rs CreateTrail = CreateTrailResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrailResponse'
            Prelude.<$> (x Prelude..?> "TrailARN")
            Prelude.<*> (x Prelude..?> "LogFileValidationEnabled")
            Prelude.<*> (x Prelude..?> "IsOrganizationTrail")
            Prelude.<*> (x Prelude..?> "SnsTopicName")
            Prelude.<*> (x Prelude..?> "IncludeGlobalServiceEvents")
            Prelude.<*> (x Prelude..?> "KmsKeyId")
            Prelude.<*> (x Prelude..?> "S3KeyPrefix")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (x Prelude..?> "CloudWatchLogsLogGroupArn")
            Prelude.<*> (x Prelude..?> "IsMultiRegionTrail")
            Prelude.<*> (x Prelude..?> "S3BucketName")
            Prelude.<*> (x Prelude..?> "CloudWatchLogsRoleArn")
            Prelude.<*> (x Prelude..?> "SnsTopicARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrail

instance Prelude.NFData CreateTrail

instance Prelude.ToHeaders CreateTrail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.CreateTrail" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateTrail where
  toJSON CreateTrail' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IsOrganizationTrail" Prelude..=)
              Prelude.<$> isOrganizationTrail,
            ("SnsTopicName" Prelude..=) Prelude.<$> snsTopicName,
            ("IncludeGlobalServiceEvents" Prelude..=)
              Prelude.<$> includeGlobalServiceEvents,
            ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            ("S3KeyPrefix" Prelude..=) Prelude.<$> s3KeyPrefix,
            ("CloudWatchLogsLogGroupArn" Prelude..=)
              Prelude.<$> cloudWatchLogsLogGroupArn,
            ("IsMultiRegionTrail" Prelude..=)
              Prelude.<$> isMultiRegionTrail,
            ("CloudWatchLogsRoleArn" Prelude..=)
              Prelude.<$> cloudWatchLogsRoleArn,
            ("TagsList" Prelude..=) Prelude.<$> tagsList,
            ("EnableLogFileValidation" Prelude..=)
              Prelude.<$> enableLogFileValidation,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just
              ("S3BucketName" Prelude..= s3BucketName)
          ]
      )

instance Prelude.ToPath CreateTrail where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateTrail where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newCreateTrailResponse' smart constructor.
data CreateTrailResponse = CreateTrailResponse'
  { -- | Specifies the ARN of the trail that was created. The format of a trail
    -- ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    trailARN :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether log file integrity validation is enabled.
    logFileValidationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the trail is an organization trail.
    isOrganizationTrail :: Prelude.Maybe Prelude.Bool,
    -- | This field is no longer in use. Use SnsTopicARN.
    snsTopicName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the trail is publishing events from global services
    -- such as IAM to the log files.
    includeGlobalServiceEvents :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
    -- The value is a fully specified ARN to a KMS key in the format:
    --
    -- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the
    -- bucket you have designated for log file delivery. For more information,
    -- see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the trail.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon Resource Name (ARN) of the log group to which
    -- CloudTrail logs will be delivered.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the trail exists in one region or in all regions.
    isMultiRegionTrail :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the Amazon S3 bucket designated for publishing log
    -- files.
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
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateTrailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailARN', 'createTrailResponse_trailARN' - Specifies the ARN of the trail that was created. The format of a trail
-- ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
--
-- 'logFileValidationEnabled', 'createTrailResponse_logFileValidationEnabled' - Specifies whether log file integrity validation is enabled.
--
-- 'isOrganizationTrail', 'createTrailResponse_isOrganizationTrail' - Specifies whether the trail is an organization trail.
--
-- 'snsTopicName', 'createTrailResponse_snsTopicName' - This field is no longer in use. Use SnsTopicARN.
--
-- 'includeGlobalServiceEvents', 'createTrailResponse_includeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
--
-- 'kmsKeyId', 'createTrailResponse_kmsKeyId' - Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- 's3KeyPrefix', 'createTrailResponse_s3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
--
-- 'name', 'createTrailResponse_name' - Specifies the name of the trail.
--
-- 'cloudWatchLogsLogGroupArn', 'createTrailResponse_cloudWatchLogsLogGroupArn' - Specifies the Amazon Resource Name (ARN) of the log group to which
-- CloudTrail logs will be delivered.
--
-- 'isMultiRegionTrail', 'createTrailResponse_isMultiRegionTrail' - Specifies whether the trail exists in one region or in all regions.
--
-- 's3BucketName', 'createTrailResponse_s3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
--
-- 'cloudWatchLogsRoleArn', 'createTrailResponse_cloudWatchLogsRoleArn' - Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
--
-- 'snsTopicARN', 'createTrailResponse_snsTopicARN' - Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
-- notifications when log files are delivered. The format of a topic ARN
-- is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
--
-- 'httpStatus', 'createTrailResponse_httpStatus' - The response's http status code.
newCreateTrailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTrailResponse
newCreateTrailResponse pHttpStatus_ =
  CreateTrailResponse'
    { trailARN = Prelude.Nothing,
      logFileValidationEnabled = Prelude.Nothing,
      isOrganizationTrail = Prelude.Nothing,
      snsTopicName = Prelude.Nothing,
      includeGlobalServiceEvents = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      name = Prelude.Nothing,
      cloudWatchLogsLogGroupArn = Prelude.Nothing,
      isMultiRegionTrail = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      cloudWatchLogsRoleArn = Prelude.Nothing,
      snsTopicARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the ARN of the trail that was created. The format of a trail
-- ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
createTrailResponse_trailARN :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Text)
createTrailResponse_trailARN = Lens.lens (\CreateTrailResponse' {trailARN} -> trailARN) (\s@CreateTrailResponse' {} a -> s {trailARN = a} :: CreateTrailResponse)

-- | Specifies whether log file integrity validation is enabled.
createTrailResponse_logFileValidationEnabled :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Bool)
createTrailResponse_logFileValidationEnabled = Lens.lens (\CreateTrailResponse' {logFileValidationEnabled} -> logFileValidationEnabled) (\s@CreateTrailResponse' {} a -> s {logFileValidationEnabled = a} :: CreateTrailResponse)

-- | Specifies whether the trail is an organization trail.
createTrailResponse_isOrganizationTrail :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Bool)
createTrailResponse_isOrganizationTrail = Lens.lens (\CreateTrailResponse' {isOrganizationTrail} -> isOrganizationTrail) (\s@CreateTrailResponse' {} a -> s {isOrganizationTrail = a} :: CreateTrailResponse)

-- | This field is no longer in use. Use SnsTopicARN.
createTrailResponse_snsTopicName :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Text)
createTrailResponse_snsTopicName = Lens.lens (\CreateTrailResponse' {snsTopicName} -> snsTopicName) (\s@CreateTrailResponse' {} a -> s {snsTopicName = a} :: CreateTrailResponse)

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
createTrailResponse_includeGlobalServiceEvents :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Bool)
createTrailResponse_includeGlobalServiceEvents = Lens.lens (\CreateTrailResponse' {includeGlobalServiceEvents} -> includeGlobalServiceEvents) (\s@CreateTrailResponse' {} a -> s {includeGlobalServiceEvents = a} :: CreateTrailResponse)

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
createTrailResponse_kmsKeyId :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Text)
createTrailResponse_kmsKeyId = Lens.lens (\CreateTrailResponse' {kmsKeyId} -> kmsKeyId) (\s@CreateTrailResponse' {} a -> s {kmsKeyId = a} :: CreateTrailResponse)

-- | Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.
createTrailResponse_s3KeyPrefix :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Text)
createTrailResponse_s3KeyPrefix = Lens.lens (\CreateTrailResponse' {s3KeyPrefix} -> s3KeyPrefix) (\s@CreateTrailResponse' {} a -> s {s3KeyPrefix = a} :: CreateTrailResponse)

-- | Specifies the name of the trail.
createTrailResponse_name :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Text)
createTrailResponse_name = Lens.lens (\CreateTrailResponse' {name} -> name) (\s@CreateTrailResponse' {} a -> s {name = a} :: CreateTrailResponse)

-- | Specifies the Amazon Resource Name (ARN) of the log group to which
-- CloudTrail logs will be delivered.
createTrailResponse_cloudWatchLogsLogGroupArn :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Text)
createTrailResponse_cloudWatchLogsLogGroupArn = Lens.lens (\CreateTrailResponse' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@CreateTrailResponse' {} a -> s {cloudWatchLogsLogGroupArn = a} :: CreateTrailResponse)

-- | Specifies whether the trail exists in one region or in all regions.
createTrailResponse_isMultiRegionTrail :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Bool)
createTrailResponse_isMultiRegionTrail = Lens.lens (\CreateTrailResponse' {isMultiRegionTrail} -> isMultiRegionTrail) (\s@CreateTrailResponse' {} a -> s {isMultiRegionTrail = a} :: CreateTrailResponse)

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
createTrailResponse_s3BucketName :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Text)
createTrailResponse_s3BucketName = Lens.lens (\CreateTrailResponse' {s3BucketName} -> s3BucketName) (\s@CreateTrailResponse' {} a -> s {s3BucketName = a} :: CreateTrailResponse)

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
createTrailResponse_cloudWatchLogsRoleArn :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Text)
createTrailResponse_cloudWatchLogsRoleArn = Lens.lens (\CreateTrailResponse' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@CreateTrailResponse' {} a -> s {cloudWatchLogsRoleArn = a} :: CreateTrailResponse)

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send
-- notifications when log files are delivered. The format of a topic ARN
-- is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
createTrailResponse_snsTopicARN :: Lens.Lens' CreateTrailResponse (Prelude.Maybe Prelude.Text)
createTrailResponse_snsTopicARN = Lens.lens (\CreateTrailResponse' {snsTopicARN} -> snsTopicARN) (\s@CreateTrailResponse' {} a -> s {snsTopicARN = a} :: CreateTrailResponse)

-- | The response's http status code.
createTrailResponse_httpStatus :: Lens.Lens' CreateTrailResponse Prelude.Int
createTrailResponse_httpStatus = Lens.lens (\CreateTrailResponse' {httpStatus} -> httpStatus) (\s@CreateTrailResponse' {} a -> s {httpStatus = a} :: CreateTrailResponse)

instance Prelude.NFData CreateTrailResponse
