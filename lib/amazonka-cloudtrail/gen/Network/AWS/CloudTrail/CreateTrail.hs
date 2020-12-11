{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.CreateTrail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a trail that specifies the settings for delivery of log data to an Amazon S3 bucket.
module Network.AWS.CloudTrail.CreateTrail
  ( -- * Creating a request
    CreateTrail (..),
    mkCreateTrail,

    -- ** Request lenses
    ctS3KeyPrefix,
    ctSNSTopicName,
    ctEnableLogFileValidation,
    ctCloudWatchLogsLogGroupARN,
    ctKMSKeyId,
    ctIncludeGlobalServiceEvents,
    ctTagsList,
    ctIsOrganizationTrail,
    ctCloudWatchLogsRoleARN,
    ctIsMultiRegionTrail,
    ctName,
    ctS3BucketName,

    -- * Destructuring the response
    CreateTrailResponse (..),
    mkCreateTrailResponse,

    -- ** Response lenses
    ctrsLogFileValidationEnabled,
    ctrsTrailARN,
    ctrsS3KeyPrefix,
    ctrsSNSTopicARN,
    ctrsSNSTopicName,
    ctrsCloudWatchLogsLogGroupARN,
    ctrsKMSKeyId,
    ctrsName,
    ctrsIncludeGlobalServiceEvents,
    ctrsIsOrganizationTrail,
    ctrsCloudWatchLogsRoleARN,
    ctrsS3BucketName,
    ctrsIsMultiRegionTrail,
    ctrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Specifies the settings for each trail.
--
-- /See:/ 'mkCreateTrail' smart constructor.
data CreateTrail = CreateTrail'
  { s3KeyPrefix ::
      Lude.Maybe Lude.Text,
    snsTopicName :: Lude.Maybe Lude.Text,
    enableLogFileValidation :: Lude.Maybe Lude.Bool,
    cloudWatchLogsLogGroupARN :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    includeGlobalServiceEvents :: Lude.Maybe Lude.Bool,
    tagsList :: Lude.Maybe [Tag],
    isOrganizationTrail :: Lude.Maybe Lude.Bool,
    cloudWatchLogsRoleARN :: Lude.Maybe Lude.Text,
    isMultiRegionTrail :: Lude.Maybe Lude.Bool,
    name :: Lude.Text,
    s3BucketName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrail' with the minimum fields required to make a request.
--
-- * 'cloudWatchLogsLogGroupARN' - Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
-- * 'cloudWatchLogsRoleARN' - Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
-- * 'enableLogFileValidation' - Specifies whether log file integrity validation is enabled. The default is false.
-- * 'includeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services such as IAM to the log files.
-- * 'isMultiRegionTrail' - Specifies whether the trail is created in the current region or in all regions. The default is false, which creates a trail only in the region where you are signed in. As a best practice, consider creating trails that log events in all regions.
-- * 'isOrganizationTrail' - Specifies whether the trail is created for all accounts in an organization in AWS Organizations, or only for the current AWS account. The default is false, and cannot be true unless the call is made on behalf of an AWS account that is the master account for an organization in AWS Organizations.
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
-- * 'name' - Specifies the name of the trail. The name must meet the following requirements:
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
-- * 's3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
-- * 's3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
-- * 'snsTopicName' - Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
-- * 'tagsList' - Undocumented field.
mkCreateTrail ::
  -- | 'name'
  Lude.Text ->
  -- | 's3BucketName'
  Lude.Text ->
  CreateTrail
mkCreateTrail pName_ pS3BucketName_ =
  CreateTrail'
    { s3KeyPrefix = Lude.Nothing,
      snsTopicName = Lude.Nothing,
      enableLogFileValidation = Lude.Nothing,
      cloudWatchLogsLogGroupARN = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      includeGlobalServiceEvents = Lude.Nothing,
      tagsList = Lude.Nothing,
      isOrganizationTrail = Lude.Nothing,
      cloudWatchLogsRoleARN = Lude.Nothing,
      isMultiRegionTrail = Lude.Nothing,
      name = pName_,
      s3BucketName = pS3BucketName_
    }

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctS3KeyPrefix :: Lens.Lens' CreateTrail (Lude.Maybe Lude.Text)
ctS3KeyPrefix = Lens.lens (s3KeyPrefix :: CreateTrail -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: CreateTrail)
{-# DEPRECATED ctS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
--
-- /Note:/ Consider using 'snsTopicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctSNSTopicName :: Lens.Lens' CreateTrail (Lude.Maybe Lude.Text)
ctSNSTopicName = Lens.lens (snsTopicName :: CreateTrail -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicName = a} :: CreateTrail)
{-# DEPRECATED ctSNSTopicName "Use generic-lens or generic-optics with 'snsTopicName' instead." #-}

-- | Specifies whether log file integrity validation is enabled. The default is false.
--
-- /Note:/ Consider using 'enableLogFileValidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctEnableLogFileValidation :: Lens.Lens' CreateTrail (Lude.Maybe Lude.Bool)
ctEnableLogFileValidation = Lens.lens (enableLogFileValidation :: CreateTrail -> Lude.Maybe Lude.Bool) (\s a -> s {enableLogFileValidation = a} :: CreateTrail)
{-# DEPRECATED ctEnableLogFileValidation "Use generic-lens or generic-optics with 'enableLogFileValidation' instead." #-}

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctCloudWatchLogsLogGroupARN :: Lens.Lens' CreateTrail (Lude.Maybe Lude.Text)
ctCloudWatchLogsLogGroupARN = Lens.lens (cloudWatchLogsLogGroupARN :: CreateTrail -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsLogGroupARN = a} :: CreateTrail)
{-# DEPRECATED ctCloudWatchLogsLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupARN' instead." #-}

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
ctKMSKeyId :: Lens.Lens' CreateTrail (Lude.Maybe Lude.Text)
ctKMSKeyId = Lens.lens (kmsKeyId :: CreateTrail -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateTrail)
{-# DEPRECATED ctKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- /Note:/ Consider using 'includeGlobalServiceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeGlobalServiceEvents :: Lens.Lens' CreateTrail (Lude.Maybe Lude.Bool)
ctIncludeGlobalServiceEvents = Lens.lens (includeGlobalServiceEvents :: CreateTrail -> Lude.Maybe Lude.Bool) (\s a -> s {includeGlobalServiceEvents = a} :: CreateTrail)
{-# DEPRECATED ctIncludeGlobalServiceEvents "Use generic-lens or generic-optics with 'includeGlobalServiceEvents' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tagsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTagsList :: Lens.Lens' CreateTrail (Lude.Maybe [Tag])
ctTagsList = Lens.lens (tagsList :: CreateTrail -> Lude.Maybe [Tag]) (\s a -> s {tagsList = a} :: CreateTrail)
{-# DEPRECATED ctTagsList "Use generic-lens or generic-optics with 'tagsList' instead." #-}

-- | Specifies whether the trail is created for all accounts in an organization in AWS Organizations, or only for the current AWS account. The default is false, and cannot be true unless the call is made on behalf of an AWS account that is the master account for an organization in AWS Organizations.
--
-- /Note:/ Consider using 'isOrganizationTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIsOrganizationTrail :: Lens.Lens' CreateTrail (Lude.Maybe Lude.Bool)
ctIsOrganizationTrail = Lens.lens (isOrganizationTrail :: CreateTrail -> Lude.Maybe Lude.Bool) (\s a -> s {isOrganizationTrail = a} :: CreateTrail)
{-# DEPRECATED ctIsOrganizationTrail "Use generic-lens or generic-optics with 'isOrganizationTrail' instead." #-}

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctCloudWatchLogsRoleARN :: Lens.Lens' CreateTrail (Lude.Maybe Lude.Text)
ctCloudWatchLogsRoleARN = Lens.lens (cloudWatchLogsRoleARN :: CreateTrail -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsRoleARN = a} :: CreateTrail)
{-# DEPRECATED ctCloudWatchLogsRoleARN "Use generic-lens or generic-optics with 'cloudWatchLogsRoleARN' instead." #-}

-- | Specifies whether the trail is created in the current region or in all regions. The default is false, which creates a trail only in the region where you are signed in. As a best practice, consider creating trails that log events in all regions.
--
-- /Note:/ Consider using 'isMultiRegionTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIsMultiRegionTrail :: Lens.Lens' CreateTrail (Lude.Maybe Lude.Bool)
ctIsMultiRegionTrail = Lens.lens (isMultiRegionTrail :: CreateTrail -> Lude.Maybe Lude.Bool) (\s a -> s {isMultiRegionTrail = a} :: CreateTrail)
{-# DEPRECATED ctIsMultiRegionTrail "Use generic-lens or generic-optics with 'isMultiRegionTrail' instead." #-}

-- | Specifies the name of the trail. The name must meet the following requirements:
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
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctName :: Lens.Lens' CreateTrail Lude.Text
ctName = Lens.lens (name :: CreateTrail -> Lude.Text) (\s a -> s {name = a} :: CreateTrail)
{-# DEPRECATED ctName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctS3BucketName :: Lens.Lens' CreateTrail Lude.Text
ctS3BucketName = Lens.lens (s3BucketName :: CreateTrail -> Lude.Text) (\s a -> s {s3BucketName = a} :: CreateTrail)
{-# DEPRECATED ctS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

instance Lude.AWSRequest CreateTrail where
  type Rs CreateTrail = CreateTrailResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTrailResponse'
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

instance Lude.ToHeaders CreateTrail where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.CreateTrail" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTrail where
  toJSON CreateTrail' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3KeyPrefix" Lude..=) Lude.<$> s3KeyPrefix,
            ("SnsTopicName" Lude..=) Lude.<$> snsTopicName,
            ("EnableLogFileValidation" Lude..=)
              Lude.<$> enableLogFileValidation,
            ("CloudWatchLogsLogGroupArn" Lude..=)
              Lude.<$> cloudWatchLogsLogGroupARN,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("IncludeGlobalServiceEvents" Lude..=)
              Lude.<$> includeGlobalServiceEvents,
            ("TagsList" Lude..=) Lude.<$> tagsList,
            ("IsOrganizationTrail" Lude..=) Lude.<$> isOrganizationTrail,
            ("CloudWatchLogsRoleArn" Lude..=) Lude.<$> cloudWatchLogsRoleARN,
            ("IsMultiRegionTrail" Lude..=) Lude.<$> isMultiRegionTrail,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("S3BucketName" Lude..= s3BucketName)
          ]
      )

instance Lude.ToPath CreateTrail where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTrail where
  toQuery = Lude.const Lude.mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkCreateTrailResponse' smart constructor.
data CreateTrailResponse = CreateTrailResponse'
  { logFileValidationEnabled ::
      Lude.Maybe Lude.Bool,
    trailARN :: Lude.Maybe Lude.Text,
    s3KeyPrefix :: Lude.Maybe Lude.Text,
    snsTopicARN :: Lude.Maybe Lude.Text,
    snsTopicName :: Lude.Maybe Lude.Text,
    cloudWatchLogsLogGroupARN :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    includeGlobalServiceEvents :: Lude.Maybe Lude.Bool,
    isOrganizationTrail :: Lude.Maybe Lude.Bool,
    cloudWatchLogsRoleARN :: Lude.Maybe Lude.Text,
    s3BucketName :: Lude.Maybe Lude.Text,
    isMultiRegionTrail :: Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrailResponse' with the minimum fields required to make a request.
--
-- * 'cloudWatchLogsLogGroupARN' - Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail logs will be delivered.
-- * 'cloudWatchLogsRoleARN' - Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
-- * 'includeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services such as IAM to the log files.
-- * 'isMultiRegionTrail' - Specifies whether the trail exists in one region or in all regions.
-- * 'isOrganizationTrail' - Specifies whether the trail is an organization trail.
-- * 'kmsKeyId' - Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
-- * 'logFileValidationEnabled' - Specifies whether log file integrity validation is enabled.
-- * 'name' - Specifies the name of the trail.
-- * 'responseStatus' - The response status code.
-- * 's3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log files.
-- * 's3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
-- * 'snsTopicARN' - Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
-- * 'snsTopicName' - This field is no longer in use. Use SnsTopicARN.
-- * 'trailARN' - Specifies the ARN of the trail that was created. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
mkCreateTrailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTrailResponse
mkCreateTrailResponse pResponseStatus_ =
  CreateTrailResponse'
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
ctrsLogFileValidationEnabled :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Bool)
ctrsLogFileValidationEnabled = Lens.lens (logFileValidationEnabled :: CreateTrailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {logFileValidationEnabled = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsLogFileValidationEnabled "Use generic-lens or generic-optics with 'logFileValidationEnabled' instead." #-}

-- | Specifies the ARN of the trail that was created. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsTrailARN :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Text)
ctrsTrailARN = Lens.lens (trailARN :: CreateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {trailARN = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsS3KeyPrefix :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Text)
ctrsS3KeyPrefix = Lens.lens (s3KeyPrefix :: CreateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsSNSTopicARN :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Text)
ctrsSNSTopicARN = Lens.lens (snsTopicARN :: CreateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | This field is no longer in use. Use SnsTopicARN.
--
-- /Note:/ Consider using 'snsTopicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsSNSTopicName :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Text)
ctrsSNSTopicName = Lens.lens (snsTopicName :: CreateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicName = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsSNSTopicName "Use generic-lens or generic-optics with 'snsTopicName' instead." #-}

-- | Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail logs will be delivered.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsCloudWatchLogsLogGroupARN :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Text)
ctrsCloudWatchLogsLogGroupARN = Lens.lens (cloudWatchLogsLogGroupARN :: CreateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsLogGroupARN = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsCloudWatchLogsLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupARN' instead." #-}

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsKMSKeyId :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Text)
ctrsKMSKeyId = Lens.lens (kmsKeyId :: CreateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies the name of the trail.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsName :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Text)
ctrsName = Lens.lens (name :: CreateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- /Note:/ Consider using 'includeGlobalServiceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsIncludeGlobalServiceEvents :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Bool)
ctrsIncludeGlobalServiceEvents = Lens.lens (includeGlobalServiceEvents :: CreateTrailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {includeGlobalServiceEvents = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsIncludeGlobalServiceEvents "Use generic-lens or generic-optics with 'includeGlobalServiceEvents' instead." #-}

-- | Specifies whether the trail is an organization trail.
--
-- /Note:/ Consider using 'isOrganizationTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsIsOrganizationTrail :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Bool)
ctrsIsOrganizationTrail = Lens.lens (isOrganizationTrail :: CreateTrailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isOrganizationTrail = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsIsOrganizationTrail "Use generic-lens or generic-optics with 'isOrganizationTrail' instead." #-}

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsCloudWatchLogsRoleARN :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Text)
ctrsCloudWatchLogsRoleARN = Lens.lens (cloudWatchLogsRoleARN :: CreateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsRoleARN = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsCloudWatchLogsRoleARN "Use generic-lens or generic-optics with 'cloudWatchLogsRoleARN' instead." #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsS3BucketName :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Text)
ctrsS3BucketName = Lens.lens (s3BucketName :: CreateTrailResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Specifies whether the trail exists in one region or in all regions.
--
-- /Note:/ Consider using 'isMultiRegionTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsIsMultiRegionTrail :: Lens.Lens' CreateTrailResponse (Lude.Maybe Lude.Bool)
ctrsIsMultiRegionTrail = Lens.lens (isMultiRegionTrail :: CreateTrailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isMultiRegionTrail = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsIsMultiRegionTrail "Use generic-lens or generic-optics with 'isMultiRegionTrail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateTrailResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateTrailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrailResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
