{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandInvocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandInvocation where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.CommandInvocationStatus
import Network.AWS.SSM.Types.CommandPlugin
import Network.AWS.SSM.Types.NotificationConfig

-- | An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user runs SendCommand against three instances, then a command invocation is created for each requested instance ID. A command invocation returns status and detail information about a command you ran.
--
--
--
-- /See:/ 'commandInvocation' smart constructor.
data CommandInvocation = CommandInvocation'
  { _comInstanceId ::
      !(Maybe Text),
    _comStatus :: !(Maybe CommandInvocationStatus),
    _comNotificationConfig :: !(Maybe NotificationConfig),
    _comCommandPlugins :: !(Maybe [CommandPlugin]),
    _comCloudWatchOutputConfig ::
      !(Maybe CloudWatchOutputConfig),
    _comDocumentName :: !(Maybe Text),
    _comStandardErrorURL :: !(Maybe Text),
    _comStatusDetails :: !(Maybe Text),
    _comStandardOutputURL :: !(Maybe Text),
    _comCommandId :: !(Maybe Text),
    _comDocumentVersion :: !(Maybe Text),
    _comComment :: !(Maybe Text),
    _comTraceOutput :: !(Maybe Text),
    _comInstanceName :: !(Maybe Text),
    _comRequestedDateTime :: !(Maybe POSIX),
    _comServiceRole :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CommandInvocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'comInstanceId' - The instance ID in which this invocation was requested.
--
-- * 'comStatus' - Whether or not the invocation succeeded, failed, or is pending.
--
-- * 'comNotificationConfig' - Configurations for sending notifications about command status changes on a per instance basis.
--
-- * 'comCommandPlugins' - Undocumented member.
--
-- * 'comCloudWatchOutputConfig' - CloudWatch Logs information where you want Systems Manager to send the command output.
--
-- * 'comDocumentName' - The document name that was requested for execution.
--
-- * 'comStandardErrorURL' - The URL to the plugin's StdErr file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardErrorUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
--
-- * 'comStatusDetails' - A detailed status of the command execution for each invocation (each instance targeted by the command). StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:     * Pending: The command has not been sent to the instance.     * In Progress: The command has been sent to the instance but has not reached a terminal state.     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Undeliverable: The command can't be delivered to the instance. The instance might not exist or might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit and don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
--
-- * 'comStandardOutputURL' - The URL to the plugin's StdOut file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardOutputUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
--
-- * 'comCommandId' - The command against which this invocation was requested.
--
-- * 'comDocumentVersion' - The SSM document version.
--
-- * 'comComment' - User-specified information about the command, such as a brief description of what the command should do.
--
-- * 'comTraceOutput' - Gets the trace output sent by the agent.
--
-- * 'comInstanceName' - The name of the invocation target. For EC2 instances this is the value for the aws:Name tag. For on-premises instances, this is the name of the instance.
--
-- * 'comRequestedDateTime' - The time and date the request was sent to this instance.
--
-- * 'comServiceRole' - The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
commandInvocation ::
  CommandInvocation
commandInvocation =
  CommandInvocation'
    { _comInstanceId = Nothing,
      _comStatus = Nothing,
      _comNotificationConfig = Nothing,
      _comCommandPlugins = Nothing,
      _comCloudWatchOutputConfig = Nothing,
      _comDocumentName = Nothing,
      _comStandardErrorURL = Nothing,
      _comStatusDetails = Nothing,
      _comStandardOutputURL = Nothing,
      _comCommandId = Nothing,
      _comDocumentVersion = Nothing,
      _comComment = Nothing,
      _comTraceOutput = Nothing,
      _comInstanceName = Nothing,
      _comRequestedDateTime = Nothing,
      _comServiceRole = Nothing
    }

-- | The instance ID in which this invocation was requested.
comInstanceId :: Lens' CommandInvocation (Maybe Text)
comInstanceId = lens _comInstanceId (\s a -> s {_comInstanceId = a})

-- | Whether or not the invocation succeeded, failed, or is pending.
comStatus :: Lens' CommandInvocation (Maybe CommandInvocationStatus)
comStatus = lens _comStatus (\s a -> s {_comStatus = a})

-- | Configurations for sending notifications about command status changes on a per instance basis.
comNotificationConfig :: Lens' CommandInvocation (Maybe NotificationConfig)
comNotificationConfig = lens _comNotificationConfig (\s a -> s {_comNotificationConfig = a})

-- | Undocumented member.
comCommandPlugins :: Lens' CommandInvocation [CommandPlugin]
comCommandPlugins = lens _comCommandPlugins (\s a -> s {_comCommandPlugins = a}) . _Default . _Coerce

-- | CloudWatch Logs information where you want Systems Manager to send the command output.
comCloudWatchOutputConfig :: Lens' CommandInvocation (Maybe CloudWatchOutputConfig)
comCloudWatchOutputConfig = lens _comCloudWatchOutputConfig (\s a -> s {_comCloudWatchOutputConfig = a})

-- | The document name that was requested for execution.
comDocumentName :: Lens' CommandInvocation (Maybe Text)
comDocumentName = lens _comDocumentName (\s a -> s {_comDocumentName = a})

-- | The URL to the plugin's StdErr file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardErrorUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
comStandardErrorURL :: Lens' CommandInvocation (Maybe Text)
comStandardErrorURL = lens _comStandardErrorURL (\s a -> s {_comStandardErrorURL = a})

-- | A detailed status of the command execution for each invocation (each instance targeted by the command). StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:     * Pending: The command has not been sent to the instance.     * In Progress: The command has been sent to the instance but has not reached a terminal state.     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Undeliverable: The command can't be delivered to the instance. The instance might not exist or might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit and don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
comStatusDetails :: Lens' CommandInvocation (Maybe Text)
comStatusDetails = lens _comStatusDetails (\s a -> s {_comStatusDetails = a})

-- | The URL to the plugin's StdOut file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardOutputUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
comStandardOutputURL :: Lens' CommandInvocation (Maybe Text)
comStandardOutputURL = lens _comStandardOutputURL (\s a -> s {_comStandardOutputURL = a})

-- | The command against which this invocation was requested.
comCommandId :: Lens' CommandInvocation (Maybe Text)
comCommandId = lens _comCommandId (\s a -> s {_comCommandId = a})

-- | The SSM document version.
comDocumentVersion :: Lens' CommandInvocation (Maybe Text)
comDocumentVersion = lens _comDocumentVersion (\s a -> s {_comDocumentVersion = a})

-- | User-specified information about the command, such as a brief description of what the command should do.
comComment :: Lens' CommandInvocation (Maybe Text)
comComment = lens _comComment (\s a -> s {_comComment = a})

-- | Gets the trace output sent by the agent.
comTraceOutput :: Lens' CommandInvocation (Maybe Text)
comTraceOutput = lens _comTraceOutput (\s a -> s {_comTraceOutput = a})

-- | The name of the invocation target. For EC2 instances this is the value for the aws:Name tag. For on-premises instances, this is the name of the instance.
comInstanceName :: Lens' CommandInvocation (Maybe Text)
comInstanceName = lens _comInstanceName (\s a -> s {_comInstanceName = a})

-- | The time and date the request was sent to this instance.
comRequestedDateTime :: Lens' CommandInvocation (Maybe UTCTime)
comRequestedDateTime = lens _comRequestedDateTime (\s a -> s {_comRequestedDateTime = a}) . mapping _Time

-- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
comServiceRole :: Lens' CommandInvocation (Maybe Text)
comServiceRole = lens _comServiceRole (\s a -> s {_comServiceRole = a})

instance FromJSON CommandInvocation where
  parseJSON =
    withObject
      "CommandInvocation"
      ( \x ->
          CommandInvocation'
            <$> (x .:? "InstanceId")
            <*> (x .:? "Status")
            <*> (x .:? "NotificationConfig")
            <*> (x .:? "CommandPlugins" .!= mempty)
            <*> (x .:? "CloudWatchOutputConfig")
            <*> (x .:? "DocumentName")
            <*> (x .:? "StandardErrorUrl")
            <*> (x .:? "StatusDetails")
            <*> (x .:? "StandardOutputUrl")
            <*> (x .:? "CommandId")
            <*> (x .:? "DocumentVersion")
            <*> (x .:? "Comment")
            <*> (x .:? "TraceOutput")
            <*> (x .:? "InstanceName")
            <*> (x .:? "RequestedDateTime")
            <*> (x .:? "ServiceRole")
      )

instance Hashable CommandInvocation

instance NFData CommandInvocation
