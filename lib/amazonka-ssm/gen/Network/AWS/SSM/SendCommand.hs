{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.SendCommand
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs commands on one or more managed instances.
module Network.AWS.SSM.SendCommand
  ( -- * Creating a request
    SendCommand (..),
    mkSendCommand,

    -- ** Request lenses
    scDocumentName,
    scCloudWatchOutputConfig,
    scComment,
    scDocumentHash,
    scDocumentHashType,
    scDocumentVersion,
    scInstanceIds,
    scMaxConcurrency,
    scMaxErrors,
    scNotificationConfig,
    scOutputS3BucketName,
    scOutputS3KeyPrefix,
    scOutputS3Region,
    scParameters,
    scServiceRoleArn,
    scTargets,
    scTimeoutSeconds,

    -- * Destructuring the response
    SendCommandResponse (..),
    mkSendCommandResponse,

    -- ** Response lenses
    scrrsCommand,
    scrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkSendCommand' smart constructor.
data SendCommand = SendCommand'
  { -- | Required. The name of the Systems Manager document to run. This can be a public document or a custom document.
    documentName :: Types.DocumentARN,
    -- | Enables Systems Manager to send Run Command output to Amazon CloudWatch Logs.
    cloudWatchOutputConfig :: Core.Maybe Types.CloudWatchOutputConfig,
    -- | User-specified information about the command, such as a brief description of what the command should do.
    comment :: Core.Maybe Types.Comment,
    -- | The Sha256 or Sha1 hash created by the system when the document was created.
    documentHash :: Core.Maybe Types.DocumentHash,
    -- | Sha256 or Sha1.
    documentHashType :: Core.Maybe Types.DocumentHashType,
    -- | The SSM document version to use in the request. You can specify $DEFAULT, $LATEST, or a specific version number. If you run commands by using the AWS CLI, then you must escape the first two options by using a backslash. If you specify a version number, then you don't need to use the backslash. For example:
    --
    -- --document-version "\$DEFAULT"
    -- --document-version "\$LATEST"
    -- --document-version "3"
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The IDs of the instances where the command should run. Specifying instance IDs is most useful when you are targeting a limited number of instances, though you can specify up to 50 IDs.
    --
    -- To target a larger number of instances, or if you prefer not to list individual instance IDs, we recommend using the @Targets@ option instead. Using @Targets@ , which accepts tag key-value pairs to identify the instances to send commands to, you can a send command to tens, hundreds, or thousands of instances at once.
    -- For more information about how to use targets, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Using targets and rate controls to send commands to a fleet> in the /AWS Systems Manager User Guide/ .
    instanceIds :: Core.Maybe [Types.InstanceId],
    -- | (Optional) The maximum number of instances that are allowed to run the command at the same time. You can specify a number such as 10 or a percentage such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls> in the /AWS Systems Manager User Guide/ .
    maxConcurrency :: Core.Maybe Types.MaxConcurrency,
    -- | The maximum number of errors allowed without the command failing. When the command fails one more time beyond the value of MaxErrors, the systems stops sending the command to additional targets. You can specify a number like 10 or a percentage like 10%. The default value is 0. For more information about how to use MaxErrors, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls> in the /AWS Systems Manager User Guide/ .
    maxErrors :: Core.Maybe Types.MaxErrors,
    -- | Configurations for sending notifications.
    notificationConfig :: Core.Maybe Types.NotificationConfig,
    -- | The name of the S3 bucket where command execution responses should be stored.
    outputS3BucketName :: Core.Maybe Types.OutputS3BucketName,
    -- | The directory structure within the S3 bucket where the responses should be stored.
    outputS3KeyPrefix :: Core.Maybe Types.S3KeyPrefix,
    -- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
    outputS3Region :: Core.Maybe Types.S3Region,
    -- | The required and optional parameters specified in the document being run.
    parameters :: Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]),
    -- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for Run Command commands.
    serviceRoleArn :: Core.Maybe Types.ServiceRoleArn,
    -- | An array of search criteria that targets instances using a @Key,Value@ combination that you specify. Specifying targets is most useful when you want to send a command to a large number of instances at once. Using @Targets@ , which accepts tag key-value pairs to identify instances, you can send a command to tens, hundreds, or thousands of instances at once.
    --
    -- To send a command to a smaller number of instances, you can use the @InstanceIds@ option instead.
    -- For more information about how to use targets, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending commands to a fleet> in the /AWS Systems Manager User Guide/ .
    targets :: Core.Maybe [Types.Target],
    -- | If this time is reached and the command has not already started running, it will not run.
    timeoutSeconds :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendCommand' value with any optional fields omitted.
mkSendCommand ::
  -- | 'documentName'
  Types.DocumentARN ->
  SendCommand
mkSendCommand documentName =
  SendCommand'
    { documentName,
      cloudWatchOutputConfig = Core.Nothing,
      comment = Core.Nothing,
      documentHash = Core.Nothing,
      documentHashType = Core.Nothing,
      documentVersion = Core.Nothing,
      instanceIds = Core.Nothing,
      maxConcurrency = Core.Nothing,
      maxErrors = Core.Nothing,
      notificationConfig = Core.Nothing,
      outputS3BucketName = Core.Nothing,
      outputS3KeyPrefix = Core.Nothing,
      outputS3Region = Core.Nothing,
      parameters = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      targets = Core.Nothing,
      timeoutSeconds = Core.Nothing
    }

-- | Required. The name of the Systems Manager document to run. This can be a public document or a custom document.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDocumentName :: Lens.Lens' SendCommand Types.DocumentARN
scDocumentName = Lens.field @"documentName"
{-# DEPRECATED scDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | Enables Systems Manager to send Run Command output to Amazon CloudWatch Logs.
--
-- /Note:/ Consider using 'cloudWatchOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scCloudWatchOutputConfig :: Lens.Lens' SendCommand (Core.Maybe Types.CloudWatchOutputConfig)
scCloudWatchOutputConfig = Lens.field @"cloudWatchOutputConfig"
{-# DEPRECATED scCloudWatchOutputConfig "Use generic-lens or generic-optics with 'cloudWatchOutputConfig' instead." #-}

-- | User-specified information about the command, such as a brief description of what the command should do.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scComment :: Lens.Lens' SendCommand (Core.Maybe Types.Comment)
scComment = Lens.field @"comment"
{-# DEPRECATED scComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The Sha256 or Sha1 hash created by the system when the document was created.
--
-- /Note:/ Consider using 'documentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDocumentHash :: Lens.Lens' SendCommand (Core.Maybe Types.DocumentHash)
scDocumentHash = Lens.field @"documentHash"
{-# DEPRECATED scDocumentHash "Use generic-lens or generic-optics with 'documentHash' instead." #-}

-- | Sha256 or Sha1.
--
-- /Note:/ Consider using 'documentHashType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDocumentHashType :: Lens.Lens' SendCommand (Core.Maybe Types.DocumentHashType)
scDocumentHashType = Lens.field @"documentHashType"
{-# DEPRECATED scDocumentHashType "Use generic-lens or generic-optics with 'documentHashType' instead." #-}

-- | The SSM document version to use in the request. You can specify $DEFAULT, $LATEST, or a specific version number. If you run commands by using the AWS CLI, then you must escape the first two options by using a backslash. If you specify a version number, then you don't need to use the backslash. For example:
--
-- --document-version "\$DEFAULT"
-- --document-version "\$LATEST"
-- --document-version "3"
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDocumentVersion :: Lens.Lens' SendCommand (Core.Maybe Types.DocumentVersion)
scDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED scDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The IDs of the instances where the command should run. Specifying instance IDs is most useful when you are targeting a limited number of instances, though you can specify up to 50 IDs.
--
-- To target a larger number of instances, or if you prefer not to list individual instance IDs, we recommend using the @Targets@ option instead. Using @Targets@ , which accepts tag key-value pairs to identify the instances to send commands to, you can a send command to tens, hundreds, or thousands of instances at once.
-- For more information about how to use targets, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Using targets and rate controls to send commands to a fleet> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scInstanceIds :: Lens.Lens' SendCommand (Core.Maybe [Types.InstanceId])
scInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED scInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | (Optional) The maximum number of instances that are allowed to run the command at the same time. You can specify a number such as 10 or a percentage such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxConcurrency :: Lens.Lens' SendCommand (Core.Maybe Types.MaxConcurrency)
scMaxConcurrency = Lens.field @"maxConcurrency"
{-# DEPRECATED scMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The maximum number of errors allowed without the command failing. When the command fails one more time beyond the value of MaxErrors, the systems stops sending the command to additional targets. You can specify a number like 10 or a percentage like 10%. The default value is 0. For more information about how to use MaxErrors, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxErrors :: Lens.Lens' SendCommand (Core.Maybe Types.MaxErrors)
scMaxErrors = Lens.field @"maxErrors"
{-# DEPRECATED scMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | Configurations for sending notifications.
--
-- /Note:/ Consider using 'notificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scNotificationConfig :: Lens.Lens' SendCommand (Core.Maybe Types.NotificationConfig)
scNotificationConfig = Lens.field @"notificationConfig"
{-# DEPRECATED scNotificationConfig "Use generic-lens or generic-optics with 'notificationConfig' instead." #-}

-- | The name of the S3 bucket where command execution responses should be stored.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scOutputS3BucketName :: Lens.Lens' SendCommand (Core.Maybe Types.OutputS3BucketName)
scOutputS3BucketName = Lens.field @"outputS3BucketName"
{-# DEPRECATED scOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

-- | The directory structure within the S3 bucket where the responses should be stored.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scOutputS3KeyPrefix :: Lens.Lens' SendCommand (Core.Maybe Types.S3KeyPrefix)
scOutputS3KeyPrefix = Lens.field @"outputS3KeyPrefix"
{-# DEPRECATED scOutputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead." #-}

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
--
-- /Note:/ Consider using 'outputS3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scOutputS3Region :: Lens.Lens' SendCommand (Core.Maybe Types.S3Region)
scOutputS3Region = Lens.field @"outputS3Region"
{-# DEPRECATED scOutputS3Region "Use generic-lens or generic-optics with 'outputS3Region' instead." #-}

-- | The required and optional parameters specified in the document being run.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scParameters :: Lens.Lens' SendCommand (Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]))
scParameters = Lens.field @"parameters"
{-# DEPRECATED scParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for Run Command commands.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceRoleArn :: Lens.Lens' SendCommand (Core.Maybe Types.ServiceRoleArn)
scServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED scServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

-- | An array of search criteria that targets instances using a @Key,Value@ combination that you specify. Specifying targets is most useful when you want to send a command to a large number of instances at once. Using @Targets@ , which accepts tag key-value pairs to identify instances, you can send a command to tens, hundreds, or thousands of instances at once.
--
-- To send a command to a smaller number of instances, you can use the @InstanceIds@ option instead.
-- For more information about how to use targets, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending commands to a fleet> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTargets :: Lens.Lens' SendCommand (Core.Maybe [Types.Target])
scTargets = Lens.field @"targets"
{-# DEPRECATED scTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | If this time is reached and the command has not already started running, it will not run.
--
-- /Note:/ Consider using 'timeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTimeoutSeconds :: Lens.Lens' SendCommand (Core.Maybe Core.Natural)
scTimeoutSeconds = Lens.field @"timeoutSeconds"
{-# DEPRECATED scTimeoutSeconds "Use generic-lens or generic-optics with 'timeoutSeconds' instead." #-}

instance Core.FromJSON SendCommand where
  toJSON SendCommand {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DocumentName" Core..= documentName),
            ("CloudWatchOutputConfig" Core..=) Core.<$> cloudWatchOutputConfig,
            ("Comment" Core..=) Core.<$> comment,
            ("DocumentHash" Core..=) Core.<$> documentHash,
            ("DocumentHashType" Core..=) Core.<$> documentHashType,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("InstanceIds" Core..=) Core.<$> instanceIds,
            ("MaxConcurrency" Core..=) Core.<$> maxConcurrency,
            ("MaxErrors" Core..=) Core.<$> maxErrors,
            ("NotificationConfig" Core..=) Core.<$> notificationConfig,
            ("OutputS3BucketName" Core..=) Core.<$> outputS3BucketName,
            ("OutputS3KeyPrefix" Core..=) Core.<$> outputS3KeyPrefix,
            ("OutputS3Region" Core..=) Core.<$> outputS3Region,
            ("Parameters" Core..=) Core.<$> parameters,
            ("ServiceRoleArn" Core..=) Core.<$> serviceRoleArn,
            ("Targets" Core..=) Core.<$> targets,
            ("TimeoutSeconds" Core..=) Core.<$> timeoutSeconds
          ]
      )

instance Core.AWSRequest SendCommand where
  type Rs SendCommand = SendCommandResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.SendCommand")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SendCommandResponse'
            Core.<$> (x Core..:? "Command") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSendCommandResponse' smart constructor.
data SendCommandResponse = SendCommandResponse'
  { -- | The request as it was received by Systems Manager. Also provides the command ID which can be used future references to this request.
    command :: Core.Maybe Types.Command,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SendCommandResponse' value with any optional fields omitted.
mkSendCommandResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SendCommandResponse
mkSendCommandResponse responseStatus =
  SendCommandResponse' {command = Core.Nothing, responseStatus}

-- | The request as it was received by Systems Manager. Also provides the command ID which can be used future references to this request.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsCommand :: Lens.Lens' SendCommandResponse (Core.Maybe Types.Command)
scrrsCommand = Lens.field @"command"
{-# DEPRECATED scrrsCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsResponseStatus :: Lens.Lens' SendCommandResponse Core.Int
scrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED scrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
