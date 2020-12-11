{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    scServiceRoleARN,
    scNotificationConfig,
    scDocumentHashType,
    scCloudWatchOutputConfig,
    scOutputS3KeyPrefix,
    scMaxErrors,
    scInstanceIds,
    scOutputS3Region,
    scTargets,
    scParameters,
    scDocumentHash,
    scDocumentVersion,
    scTimeoutSeconds,
    scComment,
    scOutputS3BucketName,
    scMaxConcurrency,
    scDocumentName,

    -- * Destructuring the response
    SendCommandResponse (..),
    mkSendCommandResponse,

    -- ** Response lenses
    scrsCommand,
    scrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkSendCommand' smart constructor.
data SendCommand = SendCommand'
  { serviceRoleARN ::
      Lude.Maybe Lude.Text,
    notificationConfig :: Lude.Maybe NotificationConfig,
    documentHashType :: Lude.Maybe DocumentHashType,
    cloudWatchOutputConfig :: Lude.Maybe CloudWatchOutputConfig,
    outputS3KeyPrefix :: Lude.Maybe Lude.Text,
    maxErrors :: Lude.Maybe Lude.Text,
    instanceIds :: Lude.Maybe [Lude.Text],
    outputS3Region :: Lude.Maybe Lude.Text,
    targets :: Lude.Maybe [Target],
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    documentHash :: Lude.Maybe Lude.Text,
    documentVersion :: Lude.Maybe Lude.Text,
    timeoutSeconds :: Lude.Maybe Lude.Natural,
    comment :: Lude.Maybe Lude.Text,
    outputS3BucketName :: Lude.Maybe Lude.Text,
    maxConcurrency :: Lude.Maybe Lude.Text,
    documentName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendCommand' with the minimum fields required to make a request.
--
-- * 'cloudWatchOutputConfig' - Enables Systems Manager to send Run Command output to Amazon CloudWatch Logs.
-- * 'comment' - User-specified information about the command, such as a brief description of what the command should do.
-- * 'documentHash' - The Sha256 or Sha1 hash created by the system when the document was created.
-- * 'documentHashType' - Sha256 or Sha1.
-- * 'documentName' - Required. The name of the Systems Manager document to run. This can be a public document or a custom document.
-- * 'documentVersion' - The SSM document version to use in the request. You can specify $DEFAULT, $LATEST, or a specific version number. If you run commands by using the AWS CLI, then you must escape the first two options by using a backslash. If you specify a version number, then you don't need to use the backslash. For example:
--
-- --document-version "\$DEFAULT"
-- --document-version "\$LATEST"
-- --document-version "3"
-- * 'instanceIds' - The IDs of the instances where the command should run. Specifying instance IDs is most useful when you are targeting a limited number of instances, though you can specify up to 50 IDs.
--
-- To target a larger number of instances, or if you prefer not to list individual instance IDs, we recommend using the @Targets@ option instead. Using @Targets@ , which accepts tag key-value pairs to identify the instances to send commands to, you can a send command to tens, hundreds, or thousands of instances at once.
-- For more information about how to use targets, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Using targets and rate controls to send commands to a fleet> in the /AWS Systems Manager User Guide/ .
-- * 'maxConcurrency' - (Optional) The maximum number of instances that are allowed to run the command at the same time. You can specify a number such as 10 or a percentage such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls> in the /AWS Systems Manager User Guide/ .
-- * 'maxErrors' - The maximum number of errors allowed without the command failing. When the command fails one more time beyond the value of MaxErrors, the systems stops sending the command to additional targets. You can specify a number like 10 or a percentage like 10%. The default value is 0. For more information about how to use MaxErrors, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls> in the /AWS Systems Manager User Guide/ .
-- * 'notificationConfig' - Configurations for sending notifications.
-- * 'outputS3BucketName' - The name of the S3 bucket where command execution responses should be stored.
-- * 'outputS3KeyPrefix' - The directory structure within the S3 bucket where the responses should be stored.
-- * 'outputS3Region' - (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
-- * 'parameters' - The required and optional parameters specified in the document being run.
-- * 'serviceRoleARN' - The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for Run Command commands.
-- * 'targets' - An array of search criteria that targets instances using a @Key,Value@ combination that you specify. Specifying targets is most useful when you want to send a command to a large number of instances at once. Using @Targets@ , which accepts tag key-value pairs to identify instances, you can send a command to tens, hundreds, or thousands of instances at once.
--
-- To send a command to a smaller number of instances, you can use the @InstanceIds@ option instead.
-- For more information about how to use targets, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending commands to a fleet> in the /AWS Systems Manager User Guide/ .
-- * 'timeoutSeconds' - If this time is reached and the command has not already started running, it will not run.
mkSendCommand ::
  -- | 'documentName'
  Lude.Text ->
  SendCommand
mkSendCommand pDocumentName_ =
  SendCommand'
    { serviceRoleARN = Lude.Nothing,
      notificationConfig = Lude.Nothing,
      documentHashType = Lude.Nothing,
      cloudWatchOutputConfig = Lude.Nothing,
      outputS3KeyPrefix = Lude.Nothing,
      maxErrors = Lude.Nothing,
      instanceIds = Lude.Nothing,
      outputS3Region = Lude.Nothing,
      targets = Lude.Nothing,
      parameters = Lude.Nothing,
      documentHash = Lude.Nothing,
      documentVersion = Lude.Nothing,
      timeoutSeconds = Lude.Nothing,
      comment = Lude.Nothing,
      outputS3BucketName = Lude.Nothing,
      maxConcurrency = Lude.Nothing,
      documentName = pDocumentName_
    }

-- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for Run Command commands.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scServiceRoleARN :: Lens.Lens' SendCommand (Lude.Maybe Lude.Text)
scServiceRoleARN = Lens.lens (serviceRoleARN :: SendCommand -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: SendCommand)
{-# DEPRECATED scServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | Configurations for sending notifications.
--
-- /Note:/ Consider using 'notificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scNotificationConfig :: Lens.Lens' SendCommand (Lude.Maybe NotificationConfig)
scNotificationConfig = Lens.lens (notificationConfig :: SendCommand -> Lude.Maybe NotificationConfig) (\s a -> s {notificationConfig = a} :: SendCommand)
{-# DEPRECATED scNotificationConfig "Use generic-lens or generic-optics with 'notificationConfig' instead." #-}

-- | Sha256 or Sha1.
--
-- /Note:/ Consider using 'documentHashType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDocumentHashType :: Lens.Lens' SendCommand (Lude.Maybe DocumentHashType)
scDocumentHashType = Lens.lens (documentHashType :: SendCommand -> Lude.Maybe DocumentHashType) (\s a -> s {documentHashType = a} :: SendCommand)
{-# DEPRECATED scDocumentHashType "Use generic-lens or generic-optics with 'documentHashType' instead." #-}

-- | Enables Systems Manager to send Run Command output to Amazon CloudWatch Logs.
--
-- /Note:/ Consider using 'cloudWatchOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scCloudWatchOutputConfig :: Lens.Lens' SendCommand (Lude.Maybe CloudWatchOutputConfig)
scCloudWatchOutputConfig = Lens.lens (cloudWatchOutputConfig :: SendCommand -> Lude.Maybe CloudWatchOutputConfig) (\s a -> s {cloudWatchOutputConfig = a} :: SendCommand)
{-# DEPRECATED scCloudWatchOutputConfig "Use generic-lens or generic-optics with 'cloudWatchOutputConfig' instead." #-}

-- | The directory structure within the S3 bucket where the responses should be stored.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scOutputS3KeyPrefix :: Lens.Lens' SendCommand (Lude.Maybe Lude.Text)
scOutputS3KeyPrefix = Lens.lens (outputS3KeyPrefix :: SendCommand -> Lude.Maybe Lude.Text) (\s a -> s {outputS3KeyPrefix = a} :: SendCommand)
{-# DEPRECATED scOutputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead." #-}

-- | The maximum number of errors allowed without the command failing. When the command fails one more time beyond the value of MaxErrors, the systems stops sending the command to additional targets. You can specify a number like 10 or a percentage like 10%. The default value is 0. For more information about how to use MaxErrors, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxErrors :: Lens.Lens' SendCommand (Lude.Maybe Lude.Text)
scMaxErrors = Lens.lens (maxErrors :: SendCommand -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: SendCommand)
{-# DEPRECATED scMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The IDs of the instances where the command should run. Specifying instance IDs is most useful when you are targeting a limited number of instances, though you can specify up to 50 IDs.
--
-- To target a larger number of instances, or if you prefer not to list individual instance IDs, we recommend using the @Targets@ option instead. Using @Targets@ , which accepts tag key-value pairs to identify the instances to send commands to, you can a send command to tens, hundreds, or thousands of instances at once.
-- For more information about how to use targets, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Using targets and rate controls to send commands to a fleet> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scInstanceIds :: Lens.Lens' SendCommand (Lude.Maybe [Lude.Text])
scInstanceIds = Lens.lens (instanceIds :: SendCommand -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: SendCommand)
{-# DEPRECATED scInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
--
-- /Note:/ Consider using 'outputS3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scOutputS3Region :: Lens.Lens' SendCommand (Lude.Maybe Lude.Text)
scOutputS3Region = Lens.lens (outputS3Region :: SendCommand -> Lude.Maybe Lude.Text) (\s a -> s {outputS3Region = a} :: SendCommand)
{-# DEPRECATED scOutputS3Region "Use generic-lens or generic-optics with 'outputS3Region' instead." #-}

-- | An array of search criteria that targets instances using a @Key,Value@ combination that you specify. Specifying targets is most useful when you want to send a command to a large number of instances at once. Using @Targets@ , which accepts tag key-value pairs to identify instances, you can send a command to tens, hundreds, or thousands of instances at once.
--
-- To send a command to a smaller number of instances, you can use the @InstanceIds@ option instead.
-- For more information about how to use targets, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending commands to a fleet> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTargets :: Lens.Lens' SendCommand (Lude.Maybe [Target])
scTargets = Lens.lens (targets :: SendCommand -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: SendCommand)
{-# DEPRECATED scTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The required and optional parameters specified in the document being run.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scParameters :: Lens.Lens' SendCommand (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
scParameters = Lens.lens (parameters :: SendCommand -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: SendCommand)
{-# DEPRECATED scParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The Sha256 or Sha1 hash created by the system when the document was created.
--
-- /Note:/ Consider using 'documentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDocumentHash :: Lens.Lens' SendCommand (Lude.Maybe Lude.Text)
scDocumentHash = Lens.lens (documentHash :: SendCommand -> Lude.Maybe Lude.Text) (\s a -> s {documentHash = a} :: SendCommand)
{-# DEPRECATED scDocumentHash "Use generic-lens or generic-optics with 'documentHash' instead." #-}

-- | The SSM document version to use in the request. You can specify $DEFAULT, $LATEST, or a specific version number. If you run commands by using the AWS CLI, then you must escape the first two options by using a backslash. If you specify a version number, then you don't need to use the backslash. For example:
--
-- --document-version "\$DEFAULT"
-- --document-version "\$LATEST"
-- --document-version "3"
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDocumentVersion :: Lens.Lens' SendCommand (Lude.Maybe Lude.Text)
scDocumentVersion = Lens.lens (documentVersion :: SendCommand -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: SendCommand)
{-# DEPRECATED scDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | If this time is reached and the command has not already started running, it will not run.
--
-- /Note:/ Consider using 'timeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTimeoutSeconds :: Lens.Lens' SendCommand (Lude.Maybe Lude.Natural)
scTimeoutSeconds = Lens.lens (timeoutSeconds :: SendCommand -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutSeconds = a} :: SendCommand)
{-# DEPRECATED scTimeoutSeconds "Use generic-lens or generic-optics with 'timeoutSeconds' instead." #-}

-- | User-specified information about the command, such as a brief description of what the command should do.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scComment :: Lens.Lens' SendCommand (Lude.Maybe Lude.Text)
scComment = Lens.lens (comment :: SendCommand -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: SendCommand)
{-# DEPRECATED scComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The name of the S3 bucket where command execution responses should be stored.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scOutputS3BucketName :: Lens.Lens' SendCommand (Lude.Maybe Lude.Text)
scOutputS3BucketName = Lens.lens (outputS3BucketName :: SendCommand -> Lude.Maybe Lude.Text) (\s a -> s {outputS3BucketName = a} :: SendCommand)
{-# DEPRECATED scOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

-- | (Optional) The maximum number of instances that are allowed to run the command at the same time. You can specify a number such as 10 or a percentage such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxConcurrency :: Lens.Lens' SendCommand (Lude.Maybe Lude.Text)
scMaxConcurrency = Lens.lens (maxConcurrency :: SendCommand -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: SendCommand)
{-# DEPRECATED scMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | Required. The name of the Systems Manager document to run. This can be a public document or a custom document.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDocumentName :: Lens.Lens' SendCommand Lude.Text
scDocumentName = Lens.lens (documentName :: SendCommand -> Lude.Text) (\s a -> s {documentName = a} :: SendCommand)
{-# DEPRECATED scDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

instance Lude.AWSRequest SendCommand where
  type Rs SendCommand = SendCommandResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          SendCommandResponse'
            Lude.<$> (x Lude..?> "Command") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendCommand where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.SendCommand" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SendCommand where
  toJSON SendCommand' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServiceRoleArn" Lude..=) Lude.<$> serviceRoleARN,
            ("NotificationConfig" Lude..=) Lude.<$> notificationConfig,
            ("DocumentHashType" Lude..=) Lude.<$> documentHashType,
            ("CloudWatchOutputConfig" Lude..=) Lude.<$> cloudWatchOutputConfig,
            ("OutputS3KeyPrefix" Lude..=) Lude.<$> outputS3KeyPrefix,
            ("MaxErrors" Lude..=) Lude.<$> maxErrors,
            ("InstanceIds" Lude..=) Lude.<$> instanceIds,
            ("OutputS3Region" Lude..=) Lude.<$> outputS3Region,
            ("Targets" Lude..=) Lude.<$> targets,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("DocumentHash" Lude..=) Lude.<$> documentHash,
            ("DocumentVersion" Lude..=) Lude.<$> documentVersion,
            ("TimeoutSeconds" Lude..=) Lude.<$> timeoutSeconds,
            ("Comment" Lude..=) Lude.<$> comment,
            ("OutputS3BucketName" Lude..=) Lude.<$> outputS3BucketName,
            ("MaxConcurrency" Lude..=) Lude.<$> maxConcurrency,
            Lude.Just ("DocumentName" Lude..= documentName)
          ]
      )

instance Lude.ToPath SendCommand where
  toPath = Lude.const "/"

instance Lude.ToQuery SendCommand where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSendCommandResponse' smart constructor.
data SendCommandResponse = SendCommandResponse'
  { command ::
      Lude.Maybe Command,
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

-- | Creates a value of 'SendCommandResponse' with the minimum fields required to make a request.
--
-- * 'command' - The request as it was received by Systems Manager. Also provides the command ID which can be used future references to this request.
-- * 'responseStatus' - The response status code.
mkSendCommandResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendCommandResponse
mkSendCommandResponse pResponseStatus_ =
  SendCommandResponse'
    { command = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The request as it was received by Systems Manager. Also provides the command ID which can be used future references to this request.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsCommand :: Lens.Lens' SendCommandResponse (Lude.Maybe Command)
scrsCommand = Lens.lens (command :: SendCommandResponse -> Lude.Maybe Command) (\s a -> s {command = a} :: SendCommandResponse)
{-# DEPRECATED scrsCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsResponseStatus :: Lens.Lens' SendCommandResponse Lude.Int
scrsResponseStatus = Lens.lens (responseStatus :: SendCommandResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendCommandResponse)
{-# DEPRECATED scrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
