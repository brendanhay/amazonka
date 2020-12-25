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
    utName,
    utCloudWatchLogsLogGroupArn,
    utCloudWatchLogsRoleArn,
    utEnableLogFileValidation,
    utIncludeGlobalServiceEvents,
    utIsMultiRegionTrail,
    utIsOrganizationTrail,
    utKmsKeyId,
    utS3BucketName,
    utS3KeyPrefix,
    utSnsTopicName,

    -- * Destructuring the response
    UpdateTrailResponse (..),
    mkUpdateTrailResponse,

    -- ** Response lenses
    utrrsCloudWatchLogsLogGroupArn,
    utrrsCloudWatchLogsRoleArn,
    utrrsIncludeGlobalServiceEvents,
    utrrsIsMultiRegionTrail,
    utrrsIsOrganizationTrail,
    utrrsKmsKeyId,
    utrrsLogFileValidationEnabled,
    utrrsName,
    utrrsS3BucketName,
    utrrsS3KeyPrefix,
    utrrsSnsTopicARN,
    utrrsSnsTopicName,
    utrrsTrailARN,
    utrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Specifies settings to update for the trail.
--
-- /See:/ 'mkUpdateTrail' smart constructor.
data UpdateTrail = UpdateTrail'
  { -- | Specifies the name of the trail or trail ARN. If @Name@ is a trail name, the string must meet the following requirements:
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
    name :: Types.Name,
    -- | Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
    cloudWatchLogsLogGroupArn :: Core.Maybe Types.CloudWatchLogsLogGroupArn,
    -- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
    cloudWatchLogsRoleArn :: Core.Maybe Types.CloudWatchLogsRoleArn,
    -- | Specifies whether log file validation is enabled. The default is false.
    enableLogFileValidation :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
    includeGlobalServiceEvents :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail applies only to the current region or to all regions. The default is false. If the trail exists only in the current region and this value is set to true, shadow trails (replications of the trail) will be created in the other regions. If the trail exists in all regions and this value is set to false, the trail will remain in the region where it was created, and its shadow trails in other regions will be deleted. As a best practice, consider using trails that log events in all regions.
    isMultiRegionTrail :: Core.Maybe Core.Bool,
    -- | Specifies whether the trail is applied to all accounts in an organization in AWS Organizations, or only for the current AWS account. The default is false, and cannot be true unless the call is made on behalf of an AWS account that is the master account for an organization in AWS Organizations. If the trail is not an organization trail and this is set to true, the trail will be created in all AWS accounts that belong to the organization. If the trail is an organization trail and this is set to false, the trail will remain in the current AWS account but be deleted from all member accounts in the organization.
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
    -- | Specifies the name of the Amazon S3 bucket designated for publishing log files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
    s3BucketName :: Core.Maybe Types.S3BucketName,
    -- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
    s3KeyPrefix :: Core.Maybe Types.S3KeyPrefix,
    -- | Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
    snsTopicName :: Core.Maybe Types.SnsTopicName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrail' value with any optional fields omitted.
mkUpdateTrail ::
  -- | 'name'
  Types.Name ->
  UpdateTrail
mkUpdateTrail name =
  UpdateTrail'
    { name,
      cloudWatchLogsLogGroupArn = Core.Nothing,
      cloudWatchLogsRoleArn = Core.Nothing,
      enableLogFileValidation = Core.Nothing,
      includeGlobalServiceEvents = Core.Nothing,
      isMultiRegionTrail = Core.Nothing,
      isOrganizationTrail = Core.Nothing,
      kmsKeyId = Core.Nothing,
      s3BucketName = Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      snsTopicName = Core.Nothing
    }

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
utName :: Lens.Lens' UpdateTrail Types.Name
utName = Lens.field @"name"
{-# DEPRECATED utName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utCloudWatchLogsLogGroupArn :: Lens.Lens' UpdateTrail (Core.Maybe Types.CloudWatchLogsLogGroupArn)
utCloudWatchLogsLogGroupArn = Lens.field @"cloudWatchLogsLogGroupArn"
{-# DEPRECATED utCloudWatchLogsLogGroupArn "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupArn' instead." #-}

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utCloudWatchLogsRoleArn :: Lens.Lens' UpdateTrail (Core.Maybe Types.CloudWatchLogsRoleArn)
utCloudWatchLogsRoleArn = Lens.field @"cloudWatchLogsRoleArn"
{-# DEPRECATED utCloudWatchLogsRoleArn "Use generic-lens or generic-optics with 'cloudWatchLogsRoleArn' instead." #-}

-- | Specifies whether log file validation is enabled. The default is false.
--
-- /Note:/ Consider using 'enableLogFileValidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utEnableLogFileValidation :: Lens.Lens' UpdateTrail (Core.Maybe Core.Bool)
utEnableLogFileValidation = Lens.field @"enableLogFileValidation"
{-# DEPRECATED utEnableLogFileValidation "Use generic-lens or generic-optics with 'enableLogFileValidation' instead." #-}

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- /Note:/ Consider using 'includeGlobalServiceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utIncludeGlobalServiceEvents :: Lens.Lens' UpdateTrail (Core.Maybe Core.Bool)
utIncludeGlobalServiceEvents = Lens.field @"includeGlobalServiceEvents"
{-# DEPRECATED utIncludeGlobalServiceEvents "Use generic-lens or generic-optics with 'includeGlobalServiceEvents' instead." #-}

-- | Specifies whether the trail applies only to the current region or to all regions. The default is false. If the trail exists only in the current region and this value is set to true, shadow trails (replications of the trail) will be created in the other regions. If the trail exists in all regions and this value is set to false, the trail will remain in the region where it was created, and its shadow trails in other regions will be deleted. As a best practice, consider using trails that log events in all regions.
--
-- /Note:/ Consider using 'isMultiRegionTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utIsMultiRegionTrail :: Lens.Lens' UpdateTrail (Core.Maybe Core.Bool)
utIsMultiRegionTrail = Lens.field @"isMultiRegionTrail"
{-# DEPRECATED utIsMultiRegionTrail "Use generic-lens or generic-optics with 'isMultiRegionTrail' instead." #-}

-- | Specifies whether the trail is applied to all accounts in an organization in AWS Organizations, or only for the current AWS account. The default is false, and cannot be true unless the call is made on behalf of an AWS account that is the master account for an organization in AWS Organizations. If the trail is not an organization trail and this is set to true, the trail will be created in all AWS accounts that belong to the organization. If the trail is an organization trail and this is set to false, the trail will remain in the current AWS account but be deleted from all member accounts in the organization.
--
-- /Note:/ Consider using 'isOrganizationTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utIsOrganizationTrail :: Lens.Lens' UpdateTrail (Core.Maybe Core.Bool)
utIsOrganizationTrail = Lens.field @"isOrganizationTrail"
{-# DEPRECATED utIsOrganizationTrail "Use generic-lens or generic-optics with 'isOrganizationTrail' instead." #-}

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
utKmsKeyId :: Lens.Lens' UpdateTrail (Core.Maybe Types.KmsKeyId)
utKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED utKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files. See <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utS3BucketName :: Lens.Lens' UpdateTrail (Core.Maybe Types.S3BucketName)
utS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED utS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utS3KeyPrefix :: Lens.Lens' UpdateTrail (Core.Maybe Types.S3KeyPrefix)
utS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# DEPRECATED utS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
--
-- /Note:/ Consider using 'snsTopicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utSnsTopicName :: Lens.Lens' UpdateTrail (Core.Maybe Types.SnsTopicName)
utSnsTopicName = Lens.field @"snsTopicName"
{-# DEPRECATED utSnsTopicName "Use generic-lens or generic-optics with 'snsTopicName' instead." #-}

instance Core.FromJSON UpdateTrail where
  toJSON UpdateTrail {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
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
            ("S3BucketName" Core..=) Core.<$> s3BucketName,
            ("S3KeyPrefix" Core..=) Core.<$> s3KeyPrefix,
            ("SnsTopicName" Core..=) Core.<$> snsTopicName
          ]
      )

instance Core.AWSRequest UpdateTrail where
  type Rs UpdateTrail = UpdateTrailResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.UpdateTrail"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrailResponse'
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
-- /See:/ 'mkUpdateTrailResponse' smart constructor.
data UpdateTrailResponse = UpdateTrailResponse'
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
    -- | Specifies the ARN of the trail that was updated. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    trailARN :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrailResponse' value with any optional fields omitted.
mkUpdateTrailResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateTrailResponse
mkUpdateTrailResponse responseStatus =
  UpdateTrailResponse'
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
utrrsCloudWatchLogsLogGroupArn :: Lens.Lens' UpdateTrailResponse (Core.Maybe Types.String)
utrrsCloudWatchLogsLogGroupArn = Lens.field @"cloudWatchLogsLogGroupArn"
{-# DEPRECATED utrrsCloudWatchLogsLogGroupArn "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupArn' instead." #-}

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsCloudWatchLogsRoleArn :: Lens.Lens' UpdateTrailResponse (Core.Maybe Types.String)
utrrsCloudWatchLogsRoleArn = Lens.field @"cloudWatchLogsRoleArn"
{-# DEPRECATED utrrsCloudWatchLogsRoleArn "Use generic-lens or generic-optics with 'cloudWatchLogsRoleArn' instead." #-}

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- /Note:/ Consider using 'includeGlobalServiceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsIncludeGlobalServiceEvents :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Bool)
utrrsIncludeGlobalServiceEvents = Lens.field @"includeGlobalServiceEvents"
{-# DEPRECATED utrrsIncludeGlobalServiceEvents "Use generic-lens or generic-optics with 'includeGlobalServiceEvents' instead." #-}

-- | Specifies whether the trail exists in one region or in all regions.
--
-- /Note:/ Consider using 'isMultiRegionTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsIsMultiRegionTrail :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Bool)
utrrsIsMultiRegionTrail = Lens.field @"isMultiRegionTrail"
{-# DEPRECATED utrrsIsMultiRegionTrail "Use generic-lens or generic-optics with 'isMultiRegionTrail' instead." #-}

-- | Specifies whether the trail is an organization trail.
--
-- /Note:/ Consider using 'isOrganizationTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsIsOrganizationTrail :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Bool)
utrrsIsOrganizationTrail = Lens.field @"isOrganizationTrail"
{-# DEPRECATED utrrsIsOrganizationTrail "Use generic-lens or generic-optics with 'isOrganizationTrail' instead." #-}

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format:
--
-- @arn:aws:kms:us-east-2:123456789012:key/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsKmsKeyId :: Lens.Lens' UpdateTrailResponse (Core.Maybe Types.String)
utrrsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED utrrsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies whether log file integrity validation is enabled.
--
-- /Note:/ Consider using 'logFileValidationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsLogFileValidationEnabled :: Lens.Lens' UpdateTrailResponse (Core.Maybe Core.Bool)
utrrsLogFileValidationEnabled = Lens.field @"logFileValidationEnabled"
{-# DEPRECATED utrrsLogFileValidationEnabled "Use generic-lens or generic-optics with 'logFileValidationEnabled' instead." #-}

-- | Specifies the name of the trail.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsName :: Lens.Lens' UpdateTrailResponse (Core.Maybe Types.String)
utrrsName = Lens.field @"name"
{-# DEPRECATED utrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsS3BucketName :: Lens.Lens' UpdateTrailResponse (Core.Maybe Types.String)
utrrsS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED utrrsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsS3KeyPrefix :: Lens.Lens' UpdateTrailResponse (Core.Maybe Types.String)
utrrsS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# DEPRECATED utrrsS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is:
--
-- @arn:aws:sns:us-east-2:123456789012:MyTopic@
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsSnsTopicARN :: Lens.Lens' UpdateTrailResponse (Core.Maybe Types.String)
utrrsSnsTopicARN = Lens.field @"snsTopicARN"
{-# DEPRECATED utrrsSnsTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | This field is no longer in use. Use SnsTopicARN.
--
-- /Note:/ Consider using 'snsTopicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsSnsTopicName :: Lens.Lens' UpdateTrailResponse (Core.Maybe Types.String)
utrrsSnsTopicName = Lens.field @"snsTopicName"
{-# DEPRECATED utrrsSnsTopicName "Use generic-lens or generic-optics with 'snsTopicName' instead." #-}

-- | Specifies the ARN of the trail that was updated. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsTrailARN :: Lens.Lens' UpdateTrailResponse (Core.Maybe Types.String)
utrrsTrailARN = Lens.field @"trailARN"
{-# DEPRECATED utrrsTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsResponseStatus :: Lens.Lens' UpdateTrailResponse Core.Int
utrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
