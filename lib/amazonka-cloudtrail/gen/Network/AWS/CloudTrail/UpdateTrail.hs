{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.UpdateTrail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings that specify delivery of log files. Changes to a trail do not require stopping the CloudTrail service. Use this action to designate an existing bucket for log delivery. If the existing bucket has previously been a target for CloudTrail log files, an IAM policy exists for the bucket. @UpdateTrail@ must be called from the region in which the trail was created; otherwise, an @InvalidHomeRegionException@ is thrown.
module Network.AWS.CloudTrail.UpdateTrail
  ( -- * Creating a request
    UpdateTrail (..),
    mkUpdateTrail,

    -- ** Request lenses
    utS3KeyPrefix,
    utSNSTopicName,
    utEnableLogFileValidation,
    utCloudWatchLogsLogGroupARN,
    utKMSKeyId,
    utName,
    utIncludeGlobalServiceEvents,
    utIsOrganizationTrail,
    utCloudWatchLogsRoleARN,
    utS3BucketName,
    utIsMultiRegionTrail,

    -- * Destructuring the response
    UpdateTrailResponse (..),
    mkUpdateTrailResponse,

    -- ** Response lenses
    utrsLogFileValidationEnabled,
    utrsTrailARN,
    utrsS3KeyPrefix,
    utrsSNSTopicARN,
    utrsSNSTopicName,
    utrsCloudWatchLogsLogGroupARN,
    utrsKMSKeyId,
    utrsName,
    utrsIncludeGlobalServiceEvents,
    utrsIsOrganizationTrail,
    utrsCloudWatchLogsRoleARN,
    utrsS3BucketName,
    utrsIsMultiRegionTrail,
    utrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Specifies settings to update for the trail.
--
-- /See:/ 'mkUpdateTrail' smart constructor.
data UpdateTrail = UpdateTrail'
  { -- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
    s3KeyPrefix :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
    snsTopicName :: Lude.Maybe Lude.Text,
    -- | Specifies whether log file validation is enabled. The default is false.
    enableLogFileValidation :: Lude.Maybe Lude.Bool,
    -- | Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
    cloudWatchLogsLogGroupARN :: Lude.Maybe Lude.Text,
    -- | Specifies the KMS key ID to use to encrypt the logs delivered by CloudTrail. The value can be an alias name prefixed by "alias/", a fully specified ARN to an alias, a fully specified ARN to a key, or a globally unique identifier.
    --
    -- Examples:
    --
    --     * alias/MyAliasName
    --
    --
    --     * arn:aws:kms:us-east-2:123456789012:alias/MyAliasName
    --
    --
    --     * arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012
    --
    --
    --     * 12345678-1234-1234-1234-123456789012
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the trail or trail ARN. If @Name@ is a trail name, the string must meet the following requirements:
    --
    --
    --     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)
    --
    --
    --     * Start with a letter or number, and end with a letter or number
    --
    --
    --     * Be between 3 and 128 characters
    --
    --
    --     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.
    --
    --
    --     * Not be in IP address format (for example, 192.168.5.4)
    --
    --
    -- If @Name@ is a trail ARN, it must be in the format:
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    name :: Lude.Text,
    -- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
    includeGlobalServiceEvents :: Lude.Maybe Lude.Bool,
    -- | Specifies whether the trail is applied to all accounts in an organization in AWS Organizations, or only for the current AWS account. The default is false, and cannot be true unless the call is made on behalf of an AWS account that is the master account for an organization in AWS Organizations. If the trail is not an organization trail and this is set to true, the trail will be created in all AWS accounts that belong to the organization. If the trail is an organization trail and this is set to false, the trail will remain in the current AWS account but be deleted from all member accounts in the organization.
    isOrganizationTrail :: Lude.Maybe Lude.Bool,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
    cloudWatchLogsRoleARN :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the Amazon S3 bucket designated for publishing log files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
    s3BucketName :: Lude.Maybe Lude.Text,
    -- | Specifies whether the trail applies only to the current region or to all regions. The default is false. If the trail exists only in the current region and this value is set to true, shadow trails (replications of the trail) will be created in the other regions. If the trail exists in all regions and this value is set to false, the trail will remain in the region where it was created, and its shadow trails in other regions will be deleted. As a best practice, consider using trails that log events in all regions.
    isMultiRegionTrail :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTrail' with the minimum fields required to make a request.
--
-- * 's3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
-- * 'snsTopicName' - Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
-- * 'enableLogFileValidation' - Specifies whether log file validation is enabled. The default is false.
-- * 'cloudWatchLogsLogGroupARN' - Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
-- * 'kmsKeyId' - Specifies the KMS key ID to use to encrypt the logs delivered by CloudTrail. The value can be an alias name prefixed by "alias/", a fully specified ARN to an alias, a fully specified ARN to a key, or a globally unique identifier.
--
-- Examples:
--
--     * alias/MyAliasName
--
--
--     * arn:aws:kms:us-east-2:123456789012:alias/MyAliasName
--
--
--     * arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012
--
--
--     * 12345678-1234-1234-1234-123456789012
--
--
-- * 'name' - Specifies the name of the trail or trail ARN. If @Name@ is a trail name, the string must meet the following requirements:
--
--
--     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)
--
--
--     * Start with a letter or number, and end with a letter or number
--
--
--     * Be between 3 and 128 characters
--
--
--     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.
--
--
--     * Not be in IP address format (for example, 192.168.5.4)
--
--
-- If @Name@ is a trail ARN, it must be in the format:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
-- * 'includeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services such as IAM to the log files.
-- * 'isOrganizationTrail' - Specifies whether the trail is applied to all accounts in an organization in AWS Organizations, or only for the current AWS account. The default is false, and cannot be true unless the call is made on behalf of an AWS account that is the master account for an organization in AWS Organizations. If the trail is not an organization trail and this is set to true, the trail will be created in all AWS accounts that belong to the organization. If the trail is an organization trail and this is set to false, the trail will remain in the current AWS account but be deleted from all member accounts in the organization.
-- * 'cloudWatchLogsRoleARN' - Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
-- * 's3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
-- * 'isMultiRegionTrail' - Specifies whether the trail applies only to the current region or to all regions. The default is false. If the trail exists only in the current region and this value is set to true, shadow trails (replications of the trail) will be created in the other regions. If the trail exists in all regions and this value is set to false, the trail will remain in the region where it was created, and its shadow trails in other regions will be deleted. As a best practice, consider using trails that log events in all regions.
mkUpdateTrail ::
  -- | 'name'
  Lude.Text ->
  UpdateTrail
mkUpdateTrail pName_ =
  UpdateTrail'
    { s3KeyPrefix = Lude.Nothing,
      snsTopicName = Lude.Nothing,
      enableLogFileValidation = Lude.Nothing,
      cloudWatchLogsLogGroupARN = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      name = pName_,
      includeGlobalServiceEvents = Lude.Nothing,
      isOrganizationTrail = Lude.Nothing,
      cloudWatchLogsRoleARN = Lude.Nothing,
      s3BucketName = Lude.Nothing,
      isMultiRegionTrail = Lude.Nothing
    }

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utS3KeyPrefix :: Lens.Lens' UpdateTrail (Lude.Maybe Lude.Text)
utS3KeyPrefix = Lens.lens (s3KeyPrefix :: UpdateTrail -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: UpdateTrail)
{-# DEPRECATED utS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
--
-- /Note:/ Consider using 'snsTopicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utSNSTopicName :: Lens.Lens' UpdateTrail (Lude.Maybe Lude.Text)
utSNSTopicName = Lens.lens (snsTopicName :: UpdateTrail -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicName = a} :: UpdateTrail)
{-# DEPRECATED utSNSTopicName "Use generic-lens or generic-optics with 'snsTopicName' instead." #-}

-- | Specifies whether log file validation is enabled. The default is false.
--
-- /Note:/ Consider using 'enableLogFileValidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utEnableLogFileValidation :: Lens.Lens' UpdateTrail (Lude.Maybe Lude.Bool)
utEnableLogFileValidation = Lens.lens (enableLogFileValidation :: UpdateTrail -> Lude.Maybe Lude.Bool) (\s a -> s {enableLogFileValidation = a} :: UpdateTrail)
{-# DEPRECATED utEnableLogFileValidation "Use generic-lens or generic-optics with 'enableLogFileValidation' instead." #-}

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utCloudWatchLogsLogGroupARN :: Lens.Lens' UpdateTrail (Lude.Maybe Lude.Text)
utCloudWatchLogsLogGroupARN = Lens.lens (cloudWatchLogsLogGroupARN :: UpdateTrail -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsLogGroupARN = a} :: UpdateTrail)
{-# DEPRECATED utCloudWatchLogsLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupARN' instead." #-}

-- | Specifies the KMS key ID to use to encrypt the logs delivered by CloudTrail. The value can be an alias name prefixed by "alias/", a fully specified ARN to an alias, a fully specified ARN to a key, or a globally unique identifier.
--
-- Examples:
--
--     * alias/MyAliasName
--
--
--     * arn:aws:kms:us-east-2:123456789012:alias/MyAliasName
--
--
--     * arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012
--
--
--     * 12345678-1234-1234-1234-123456789012
--
--
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utKMSKeyId :: Lens.Lens' UpdateTrail (Lude.Maybe Lude.Text)
utKMSKeyId = Lens.lens (kmsKeyId :: UpdateTrail -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: UpdateTrail)
{-# DEPRECATED utKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies the name of the trail or trail ARN. If @Name@ is a trail name, the string must meet the following requirements:
--
--
--     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)
--
--
--     * Start with a letter or number, and end with a letter or number
--
--
--     * Be between 3 and 128 characters
--
--
--     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.
--
--
--     * Not be in IP address format (for example, 192.168.5.4)
--
--
-- If @Name@ is a trail ARN, it must be in the format:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utName :: Lens.Lens' UpdateTrail Lude.Text
utName = Lens.lens (name :: UpdateTrail -> Lude.Text) (\s a -> s {name = a} :: UpdateTrail)
{-# DEPRECATED utName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- /Note:/ Consider using 'includeGlobalServiceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utIncludeGlobalServiceEvents :: Lens.Lens' UpdateTrail (Lude.Maybe Lude.Bool)
utIncludeGlobalServiceEvents = Lens.lens (includeGlobalServiceEvents :: UpdateTrail -> Lude.Maybe Lude.Bool) (\s a -> s {includeGlobalServiceEvents = a} :: UpdateTrail)
{-# DEPRECATED utIncludeGlobalServiceEvents "Use generic-lens or generic-optics with 'includeGlobalServiceEvents' instead." #-}

-- | Specifies whether the trail is applied to all accounts in an organization in AWS Organizations, or only for the current AWS account. The default is false, and cannot be true unless the call is made on behalf of an AWS account that is the master account for an organization in AWS Organizations. If the trail is not an organization trail and this is set to true, the trail will be created in all AWS accounts that belong to the organization. If the trail is an organization trail and this is set to false, the trail will remain in the current AWS account but be deleted from all member accounts in the organization.
--
-- /Note:/ Consider using 'isOrganizationTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utIsOrganizationTrail :: Lens.Lens' UpdateTrail (Lude.Maybe Lude.Bool)
utIsOrganizationTrail = Lens.lens (isOrganizationTrail :: UpdateTrail -> Lude.Maybe Lude.Bool) (\s a -> s {isOrganizationTrail = a} :: UpdateTrail)
{-# DEPRECATED utIsOrganizationTrail "Use generic-lens or generic-optics with 'isOrganizationTrail' instead." #-}

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utCloudWatchLogsRoleARN :: Lens.Lens' UpdateTrail (Lude.Maybe Lude.Text)
utCloudWatchLogsRoleARN = Lens.lens (cloudWatchLogsRoleARN :: UpdateTrail -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsRoleARN = a} :: UpdateTrail)
{-# DEPRECATED utCloudWatchLogsRoleARN "Use generic-lens or generic-optics with 'cloudWatchLogsRoleARN' instead." #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utS3BucketName :: Lens.Lens' UpdateTrail (Lude.Maybe Lude.Text)
utS3BucketName = Lens.lens (s3BucketName :: UpdateTrail -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: UpdateTrail)
{-# DEPRECATED utS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Specifies whether the trail applies only to the current region or to all regions. The default is false. If the trail exists only in the current region and this value is set to true, shadow trails (replications of the trail) will be created in the other regions. If the trail exists in all regions and this value is set to false, the trail will remain in the region where it was created, and its shadow trails in other regions will be deleted. As a best practice, consider using trails that log events in all regions.
--
-- /Note:/ Consider using 'isMultiRegionTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utIsMultiRegionTrail :: Lens.Lens' UpdateTrail (Lude.Maybe Lude.Bool)
utIsMultiRegionTrail = Lens.lens (isMultiRegionTrail :: UpdateTrail -> Lude.Maybe Lude.Bool) (\s a -> s {isMultiRegionTrail = a} :: UpdateTrail)
{-# DEPRECATED utIsMultiRegionTrail "Use generic-lens or generic-optics with 'isMultiRegionTrail' instead." #-}

instance Lude.AWSRequest UpdateTrail where
  type Rs UpdateTrail = UpdateTrailResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTrailResponse'
            Lude.<$> (x Lude..?> "LogFileValidationEnabled")
            Lude.<*> (x Lude..?> "TrailARN")
            Lude.<*> (x Lude..?> "S3KeyPrefix")
            Lude.<*> (x Lude..?> "SnsTopicARN")
            Lude.<*> (x Lude..?> "SnsTopicName")
            Lude.<*> (x Lude..?> "CloudWatchLogsLogGroupArn")
            Lude.<*> (x Lude..?> "KmsKeyId")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "IncludeGlobalServiceEvents")
            Lude.<*> (x Lude..?> "IsOrganizationTrail")
            Lude.<*> (x Lude..?> "CloudWatchLogsRoleArn")
            Lude.<*> (x Lude..?> "S3BucketName")
            Lude.<*> (x Lude..?> "IsMultiRegionTrail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTrail where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.UpdateTrail" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTrail where
  toJSON UpdateTrail' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3KeyPrefix" Lude..=) Lude.<$> s3KeyPrefix,
            ("SnsTopicName" Lude..=) Lude.<$> snsTopicName,
            ("EnableLogFileValidation" Lude..=)
              Lude.<$> enableLogFileValidation,
            ("CloudWatchLogsLogGroupArn" Lude..=)
              Lude.<$> cloudWatchLogsLogGroupARN,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            Lude.Just ("Name" Lude..= name),
            ("IncludeGlobalServiceEvents" Lude..=)
              Lude.<$> includeGlobalServiceEvents,
            ("IsOrganizationTrail" Lude..=) Lude.<$> isOrganizationTrail,
            ("CloudWatchLogsRoleArn" Lude..=) Lude.<$> cloudWatchLogsRoleARN,
            ("S3BucketName" Lude..=) Lude.<$> s3BucketName,
            ("IsMultiRegionTrail" Lude..=) Lude.<$> isMultiRegionTrail
          ]
      )

instance Lude.ToPath UpdateTrail where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTrail where
  toQuery = Lude.const Lude.mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkUpdateTrailResponse' smart constructor.
data UpdateTrailResponse = UpdateTrailResponse'
  { -- | Specifies whether log file integrity validation is enabled.
    logFileValidationEnabled :: Lude.Maybe Lude.Bool,
    -- | Specifies the ARN of the trail that was updated. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    trailARN :: Lude.Maybe Lude.Text,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
    s3KeyPrefix :: Lude.Maybe Lude.Text,
    -- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
    --
    -- @arn:aws:sns:us-east-2:123456789012:MyTopic@
    snsTopicARN :: Lude.Maybe Lude.Text,
    -- | This field is no longer in use. Use SnsTopicARN.
    snsTopicName :: Lude.Maybe Lude.Text,
    -- | Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail logs will be delivered.
    cloudWatchLogsLogGroupARN :: Lude.Maybe Lude.Text,
    -- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
    --
    -- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the trail.
    name :: Lude.Maybe Lude.Text,
    -- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
    includeGlobalServiceEvents :: Lude.Maybe Lude.Bool,
    -- | Specifies whether the trail is an organization trail.
    isOrganizationTrail :: Lude.Maybe Lude.Bool,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
    cloudWatchLogsRoleARN :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the Amazon S3 bucket designated for publishing log files.
    s3BucketName :: Lude.Maybe Lude.Text,
    -- | Specifies whether the trail exists in one region or in all regions.
    isMultiRegionTrail :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTrailResponse' with the minimum fields required to make a request.
--
-- * 'logFileValidationEnabled' - Specifies whether log file integrity validation is enabled.
-- * 'trailARN' - Specifies the ARN of the trail that was updated. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
-- * 's3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
-- * 'snsTopicARN' - Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
-- * 'snsTopicName' - This field is no longer in use. Use SnsTopicARN.
-- * 'cloudWatchLogsLogGroupARN' - Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail logs will be delivered.
-- * 'kmsKeyId' - Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
-- * 'name' - Specifies the name of the trail.
-- * 'includeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services such as IAM to the log files.
-- * 'isOrganizationTrail' - Specifies whether the trail is an organization trail.
-- * 'cloudWatchLogsRoleARN' - Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
-- * 's3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log files.
-- * 'isMultiRegionTrail' - Specifies whether the trail exists in one region or in all regions.
-- * 'responseStatus' - The response status code.
mkUpdateTrailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTrailResponse
mkUpdateTrailResponse pResponseStatus_ =
  UpdateTrailResponse'
    { logFileValidationEnabled = Lude.Nothing,
      trailARN = Lude.Nothing,
      s3KeyPrefix = Lude.Nothing,
      snsTopicARN = Lude.Nothing,
      snsTopicName = Lude.Nothing,
      cloudWatchLogsLogGroupARN = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      name = Lude.Nothing,
      includeGlobalServiceEvents = Lude.Nothing,
      isOrganizationTrail = Lude.Nothing,
      cloudWatchLogsRoleARN = Lude.Nothing,
      s3BucketName = Lude.Nothing,
      isMultiRegionTrail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Specifies whether log file integrity validation is enabled.
--
-- /Note:/ Consider using 'logFileValidationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsLogFileValidationEnabled :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Bool)
utrsLogFileValidationEnabled = Lens.lens (logFileValidationEnabled :: UpdateTrailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {logFileValidationEnabled = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsLogFileValidationEnabled "Use generic-lens or generic-optics with 'logFileValidationEnabled' instead." #-}

-- | Specifies the ARN of the trail that was updated. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsTrailARN :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Text)
utrsTrailARN = Lens.lens (trailARN :: UpdateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {trailARN = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsS3KeyPrefix :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Text)
utrsS3KeyPrefix = Lens.lens (s3KeyPrefix :: UpdateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsSNSTopicARN :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Text)
utrsSNSTopicARN = Lens.lens (snsTopicARN :: UpdateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | This field is no longer in use. Use SnsTopicARN.
--
-- /Note:/ Consider using 'snsTopicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsSNSTopicName :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Text)
utrsSNSTopicName = Lens.lens (snsTopicName :: UpdateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicName = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsSNSTopicName "Use generic-lens or generic-optics with 'snsTopicName' instead." #-}

-- | Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail logs will be delivered.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsCloudWatchLogsLogGroupARN :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Text)
utrsCloudWatchLogsLogGroupARN = Lens.lens (cloudWatchLogsLogGroupARN :: UpdateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsLogGroupARN = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsCloudWatchLogsLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupARN' instead." #-}

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsKMSKeyId :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Text)
utrsKMSKeyId = Lens.lens (kmsKeyId :: UpdateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies the name of the trail.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsName :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Text)
utrsName = Lens.lens (name :: UpdateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- /Note:/ Consider using 'includeGlobalServiceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsIncludeGlobalServiceEvents :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Bool)
utrsIncludeGlobalServiceEvents = Lens.lens (includeGlobalServiceEvents :: UpdateTrailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {includeGlobalServiceEvents = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsIncludeGlobalServiceEvents "Use generic-lens or generic-optics with 'includeGlobalServiceEvents' instead." #-}

-- | Specifies whether the trail is an organization trail.
--
-- /Note:/ Consider using 'isOrganizationTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsIsOrganizationTrail :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Bool)
utrsIsOrganizationTrail = Lens.lens (isOrganizationTrail :: UpdateTrailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isOrganizationTrail = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsIsOrganizationTrail "Use generic-lens or generic-optics with 'isOrganizationTrail' instead." #-}

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsCloudWatchLogsRoleARN :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Text)
utrsCloudWatchLogsRoleARN = Lens.lens (cloudWatchLogsRoleARN :: UpdateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsRoleARN = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsCloudWatchLogsRoleARN "Use generic-lens or generic-optics with 'cloudWatchLogsRoleARN' instead." #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsS3BucketName :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Text)
utrsS3BucketName = Lens.lens (s3BucketName :: UpdateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Specifies whether the trail exists in one region or in all regions.
--
-- /Note:/ Consider using 'isMultiRegionTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsIsMultiRegionTrail :: Lens.Lens' UpdateTrailResponse (Lude.Maybe Lude.Bool)
utrsIsMultiRegionTrail = Lens.lens (isMultiRegionTrail :: UpdateTrailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isMultiRegionTrail = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsIsMultiRegionTrail "Use generic-lens or generic-optics with 'isMultiRegionTrail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsResponseStatus :: Lens.Lens' UpdateTrailResponse Lude.Int
utrsResponseStatus = Lens.lens (responseStatus :: UpdateTrailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTrailResponse)
{-# DEPRECATED utrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
