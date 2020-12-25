{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ctName,
    ctS3BucketName,
    ctCloudWatchLogsLogGroupArn,
    ctCloudWatchLogsRoleArn,
    ctEnableLogFileValidation,
    ctIncludeGlobalServiceEvents,
    ctIsMultiRegionTrail,
    ctIsOrganizationTrail,
    ctKmsKeyId,
    ctS3KeyPrefix,
    ctSnsTopicName,
    ctTagsList,

    -- * Destructuring the response
    CreateTrailResponse (..),
    mkCreateTrailResponse,

    -- ** Response lenses
    ctrrsCloudWatchLogsLogGroupArn,
    ctrrsCloudWatchLogsRoleArn,
    ctrrsIncludeGlobalServiceEvents,
    ctrrsIsMultiRegionTrail,
    ctrrsIsOrganizationTrail,
    ctrrsKmsKeyId,
    ctrrsLogFileValidationEnabled,
    ctrrsName,
    ctrrsS3BucketName,
    ctrrsS3KeyPrefix,
    ctrrsSnsTopicARN,
    ctrrsSnsTopicName,
    ctrrsTrailARN,
    ctrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Specifies the settings for each trail.
--
-- /See:/ 'mkCreateTrail' smart constructor.
data CreateTrail = CreateTrail'
  { -- | Specifies the name of the trail. The name must meet the following requirements:
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
    name :: Types.Name,
    -- | Specifies the name of the Amazon S3 bucket designated for publishing log files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
    s3BucketName :: Types.S3BucketName,
    -- | Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
    cloudWatchLogsLogGroupArn :: Core.Maybe Types.CloudWatchLogsLogGroupArn,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
    cloudWatchLogsRoleArn :: Core.Maybe Types.CloudWatchLogsRoleArn,
    -- | Specifies whether log file integrity validation is enabled. The default is false.
    enableLogFileValidation :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
    includeGlobalServiceEvents :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail is created in the current region or in all regions. The default is false, which creates a trail only in the region where you are signed in. As a best practice, consider creating trails that log events in all regions.
    isMultiRegionTrail :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail is created for all accounts in an organization in AWS Organizations, or only for the current AWS account. The default is false, and cannot be true unless the call is made on behalf of an AWS account that is the master account for an organization in AWS Organizations.
    isOrganizationTrail :: Core.Maybe Core.Bool,
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
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
    s3KeyPrefix :: Core.Maybe Types.S3KeyPrefix,
    -- | Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
    snsTopicName :: Core.Maybe Types.SnsTopicName,
    tagsList :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrail' value with any optional fields omitted.
mkCreateTrail ::
  -- | 'name'
  Types.Name ->
  -- | 's3BucketName'
  Types.S3BucketName ->
  CreateTrail
mkCreateTrail name s3BucketName =
  CreateTrail'
    { name,
      s3BucketName,
      cloudWatchLogsLogGroupArn = Core.Nothing,
      cloudWatchLogsRoleArn = Core.Nothing,
      enableLogFileValidation = Core.Nothing,
      includeGlobalServiceEvents = Core.Nothing,
      isMultiRegionTrail = Core.Nothing,
      isOrganizationTrail = Core.Nothing,
      kmsKeyId = Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      snsTopicName = Core.Nothing,
      tagsList = Core.Nothing
    }

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
ctName :: Lens.Lens' CreateTrail Types.Name
ctName = Lens.field @"name"
{-# DEPRECATED ctName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctS3BucketName :: Lens.Lens' CreateTrail Types.S3BucketName
ctS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED ctS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctCloudWatchLogsLogGroupArn :: Lens.Lens' CreateTrail (Core.Maybe Types.CloudWatchLogsLogGroupArn)
ctCloudWatchLogsLogGroupArn = Lens.field @"cloudWatchLogsLogGroupArn"
{-# DEPRECATED ctCloudWatchLogsLogGroupArn "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupArn' instead." #-}

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctCloudWatchLogsRoleArn :: Lens.Lens' CreateTrail (Core.Maybe Types.CloudWatchLogsRoleArn)
ctCloudWatchLogsRoleArn = Lens.field @"cloudWatchLogsRoleArn"
{-# DEPRECATED ctCloudWatchLogsRoleArn "Use generic-lens or generic-optics with 'cloudWatchLogsRoleArn' instead." #-}

-- | Specifies whether log file integrity validation is enabled. The default is false.
--
-- /Note:/ Consider using 'enableLogFileValidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctEnableLogFileValidation :: Lens.Lens' CreateTrail (Core.Maybe Core.Bool)
ctEnableLogFileValidation = Lens.field @"enableLogFileValidation"
{-# DEPRECATED ctEnableLogFileValidation "Use generic-lens or generic-optics with 'enableLogFileValidation' instead." #-}

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- /Note:/ Consider using 'includeGlobalServiceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeGlobalServiceEvents :: Lens.Lens' CreateTrail (Core.Maybe Core.Bool)
ctIncludeGlobalServiceEvents = Lens.field @"includeGlobalServiceEvents"
{-# DEPRECATED ctIncludeGlobalServiceEvents "Use generic-lens or generic-optics with 'includeGlobalServiceEvents' instead." #-}

-- | Specifies whether the trail is created in the current region or in all regions. The default is false, which creates a trail only in the region where you are signed in. As a best practice, consider creating trails that log events in all regions.
--
-- /Note:/ Consider using 'isMultiRegionTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIsMultiRegionTrail :: Lens.Lens' CreateTrail (Core.Maybe Core.Bool)
ctIsMultiRegionTrail = Lens.field @"isMultiRegionTrail"
{-# DEPRECATED ctIsMultiRegionTrail "Use generic-lens or generic-optics with 'isMultiRegionTrail' instead." #-}

-- | Specifies whether the trail is created for all accounts in an organization in AWS Organizations, or only for the current AWS account. The default is false, and cannot be true unless the call is made on behalf of an AWS account that is the master account for an organization in AWS Organizations.
--
-- /Note:/ Consider using 'isOrganizationTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIsOrganizationTrail :: Lens.Lens' CreateTrail (Core.Maybe Core.Bool)
ctIsOrganizationTrail = Lens.field @"isOrganizationTrail"
{-# DEPRECATED ctIsOrganizationTrail "Use generic-lens or generic-optics with 'isOrganizationTrail' instead." #-}

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
ctKmsKeyId :: Lens.Lens' CreateTrail (Core.Maybe Types.KmsKeyId)
ctKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED ctKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctS3KeyPrefix :: Lens.Lens' CreateTrail (Core.Maybe Types.S3KeyPrefix)
ctS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# DEPRECATED ctS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
--
-- /Note:/ Consider using 'snsTopicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctSnsTopicName :: Lens.Lens' CreateTrail (Core.Maybe Types.SnsTopicName)
ctSnsTopicName = Lens.field @"snsTopicName"
{-# DEPRECATED ctSnsTopicName "Use generic-lens or generic-optics with 'snsTopicName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tagsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTagsList :: Lens.Lens' CreateTrail (Core.Maybe [Types.Tag])
ctTagsList = Lens.field @"tagsList"
{-# DEPRECATED ctTagsList "Use generic-lens or generic-optics with 'tagsList' instead." #-}

instance Core.FromJSON CreateTrail where
  toJSON CreateTrail {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("S3BucketName" Core..= s3BucketName),
            ("CloudWatchLogsLogGroupArn" Core..=)
              Core.<$> cloudWatchLogsLogGroupArn,
            ("CloudWatchLogsRoleArn" Core..=) Core.<$> cloudWatchLogsRoleArn,
            ("EnableLogFileValidation" Core..=)
              Core.<$> enableLogFileValidation,
            ("IncludeGlobalServiceEvents" Core..=)
              Core.<$> includeGlobalServiceEvents,
            ("IsMultiRegionTrail" Core..=) Core.<$> isMultiRegionTrail,
            ("IsOrganizationTrail" Core..=) Core.<$> isOrganizationTrail,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("S3KeyPrefix" Core..=) Core.<$> s3KeyPrefix,
            ("SnsTopicName" Core..=) Core.<$> snsTopicName,
            ("TagsList" Core..=) Core.<$> tagsList
          ]
      )

instance Core.AWSRequest CreateTrail where
  type Rs CreateTrail = CreateTrailResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.CreateTrail"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrailResponse'
            Core.<$> (x Core..:? "CloudWatchLogsLogGroupArn")
            Core.<*> (x Core..:? "CloudWatchLogsRoleArn")
            Core.<*> (x Core..:? "IncludeGlobalServiceEvents")
            Core.<*> (x Core..:? "IsMultiRegionTrail")
            Core.<*> (x Core..:? "IsOrganizationTrail")
            Core.<*> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..:? "LogFileValidationEnabled")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "S3BucketName")
            Core.<*> (x Core..:? "S3KeyPrefix")
            Core.<*> (x Core..:? "SnsTopicARN")
            Core.<*> (x Core..:? "SnsTopicName")
            Core.<*> (x Core..:? "TrailARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkCreateTrailResponse' smart constructor.
data CreateTrailResponse = CreateTrailResponse'
  { -- | Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail logs will be delivered.
    cloudWatchLogsLogGroupArn :: Core.Maybe Types.String,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
    cloudWatchLogsRoleArn :: Core.Maybe Types.String,
    -- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
    includeGlobalServiceEvents :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail exists in one region or in all regions.
    isMultiRegionTrail :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail is an organization trail.
    isOrganizationTrail :: Core.Maybe Core.Bool,
    -- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
    --
    -- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Core.Maybe Types.String,
    -- | Specifies whether log file integrity validation is enabled.
    logFileValidationEnabled :: Core.Maybe Core.Bool,
    -- | Specifies the name of the trail.
    name :: Core.Maybe Types.String,
    -- | Specifies the name of the Amazon S3 bucket designated for publishing log files.
    s3BucketName :: Core.Maybe Types.String,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
    s3KeyPrefix :: Core.Maybe Types.String,
    -- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
    --
    -- @arn:aws:sns:us-east-2:123456789012:MyTopic@
    snsTopicARN :: Core.Maybe Types.String,
    -- | This field is no longer in use. Use SnsTopicARN.
    snsTopicName :: Core.Maybe Types.String,
    -- | Specifies the ARN of the trail that was created. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    trailARN :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrailResponse' value with any optional fields omitted.
mkCreateTrailResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTrailResponse
mkCreateTrailResponse responseStatus =
  CreateTrailResponse'
    { cloudWatchLogsLogGroupArn = Core.Nothing,
      cloudWatchLogsRoleArn = Core.Nothing,
      includeGlobalServiceEvents = Core.Nothing,
      isMultiRegionTrail = Core.Nothing,
      isOrganizationTrail = Core.Nothing,
      kmsKeyId = Core.Nothing,
      logFileValidationEnabled = Core.Nothing,
      name = Core.Nothing,
      s3BucketName = Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      snsTopicARN = Core.Nothing,
      snsTopicName = Core.Nothing,
      trailARN = Core.Nothing,
      responseStatus
    }

-- | Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail logs will be delivered.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsCloudWatchLogsLogGroupArn :: Lens.Lens' CreateTrailResponse (Core.Maybe Types.String)
ctrrsCloudWatchLogsLogGroupArn = Lens.field @"cloudWatchLogsLogGroupArn"
{-# DEPRECATED ctrrsCloudWatchLogsLogGroupArn "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupArn' instead." #-}

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsCloudWatchLogsRoleArn :: Lens.Lens' CreateTrailResponse (Core.Maybe Types.String)
ctrrsCloudWatchLogsRoleArn = Lens.field @"cloudWatchLogsRoleArn"
{-# DEPRECATED ctrrsCloudWatchLogsRoleArn "Use generic-lens or generic-optics with 'cloudWatchLogsRoleArn' instead." #-}

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- /Note:/ Consider using 'includeGlobalServiceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsIncludeGlobalServiceEvents :: Lens.Lens' CreateTrailResponse (Core.Maybe Core.Bool)
ctrrsIncludeGlobalServiceEvents = Lens.field @"includeGlobalServiceEvents"
{-# DEPRECATED ctrrsIncludeGlobalServiceEvents "Use generic-lens or generic-optics with 'includeGlobalServiceEvents' instead." #-}

-- | Specifies whether the trail exists in one region or in all regions.
--
-- /Note:/ Consider using 'isMultiRegionTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsIsMultiRegionTrail :: Lens.Lens' CreateTrailResponse (Core.Maybe Core.Bool)
ctrrsIsMultiRegionTrail = Lens.field @"isMultiRegionTrail"
{-# DEPRECATED ctrrsIsMultiRegionTrail "Use generic-lens or generic-optics with 'isMultiRegionTrail' instead." #-}

-- | Specifies whether the trail is an organization trail.
--
-- /Note:/ Consider using 'isOrganizationTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsIsOrganizationTrail :: Lens.Lens' CreateTrailResponse (Core.Maybe Core.Bool)
ctrrsIsOrganizationTrail = Lens.field @"isOrganizationTrail"
{-# DEPRECATED ctrrsIsOrganizationTrail "Use generic-lens or generic-optics with 'isOrganizationTrail' instead." #-}

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsKmsKeyId :: Lens.Lens' CreateTrailResponse (Core.Maybe Types.String)
ctrrsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED ctrrsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies whether log file integrity validation is enabled.
--
-- /Note:/ Consider using 'logFileValidationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsLogFileValidationEnabled :: Lens.Lens' CreateTrailResponse (Core.Maybe Core.Bool)
ctrrsLogFileValidationEnabled = Lens.field @"logFileValidationEnabled"
{-# DEPRECATED ctrrsLogFileValidationEnabled "Use generic-lens or generic-optics with 'logFileValidationEnabled' instead." #-}

-- | Specifies the name of the trail.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsName :: Lens.Lens' CreateTrailResponse (Core.Maybe Types.String)
ctrrsName = Lens.field @"name"
{-# DEPRECATED ctrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsS3BucketName :: Lens.Lens' CreateTrailResponse (Core.Maybe Types.String)
ctrrsS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED ctrrsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsS3KeyPrefix :: Lens.Lens' CreateTrailResponse (Core.Maybe Types.String)
ctrrsS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# DEPRECATED ctrrsS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsSnsTopicARN :: Lens.Lens' CreateTrailResponse (Core.Maybe Types.String)
ctrrsSnsTopicARN = Lens.field @"snsTopicARN"
{-# DEPRECATED ctrrsSnsTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | This field is no longer in use. Use SnsTopicARN.
--
-- /Note:/ Consider using 'snsTopicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsSnsTopicName :: Lens.Lens' CreateTrailResponse (Core.Maybe Types.String)
ctrrsSnsTopicName = Lens.field @"snsTopicName"
{-# DEPRECATED ctrrsSnsTopicName "Use generic-lens or generic-optics with 'snsTopicName' instead." #-}

-- | Specifies the ARN of the trail that was created. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsTrailARN :: Lens.Lens' CreateTrailResponse (Core.Maybe Types.String)
ctrrsTrailARN = Lens.field @"trailARN"
{-# DEPRECATED ctrrsTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTrailResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
