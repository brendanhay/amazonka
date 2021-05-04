{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandInvocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandInvocation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.CommandInvocationStatus
import Network.AWS.SSM.Types.CommandPlugin
import Network.AWS.SSM.Types.NotificationConfig

-- | An invocation is copy of a command sent to a specific instance. A
-- command can apply to one or more instances. A command invocation applies
-- to one instance. For example, if a user runs SendCommand against three
-- instances, then a command invocation is created for each requested
-- instance ID. A command invocation returns status and detail information
-- about a command you ran.
--
-- /See:/ 'newCommandInvocation' smart constructor.
data CommandInvocation = CommandInvocation'
  { -- | Configurations for sending notifications about command status changes on
    -- a per instance basis.
    notificationConfig :: Prelude.Maybe NotificationConfig,
    -- | The URL to the plugin\'s StdOut file in Amazon S3, if the S3 bucket was
    -- defined for the parent command. For an invocation, StandardOutputUrl is
    -- populated if there is just one plugin defined for the command, and the
    -- S3 bucket was defined for the command.
    standardOutputUrl :: Prelude.Maybe Prelude.Text,
    -- | Whether or not the invocation succeeded, failed, or is pending.
    status :: Prelude.Maybe CommandInvocationStatus,
    -- | The instance ID in which this invocation was requested.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The IAM service role that Run Command uses to act on your behalf when
    -- sending notifications about command status changes on a per instance
    -- basis.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The time and date the request was sent to this instance.
    requestedDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | A detailed status of the command execution for each invocation (each
    -- instance targeted by the command). StatusDetails includes more
    -- information than Status because it includes states resulting from error
    -- and concurrency control parameters. StatusDetails can show different
    -- results than Status. For more information about these statuses, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
    -- in the /AWS Systems Manager User Guide/. StatusDetails can be one of the
    -- following values:
    --
    -- -   Pending: The command has not been sent to the instance.
    --
    -- -   In Progress: The command has been sent to the instance but has not
    --     reached a terminal state.
    --
    -- -   Success: The execution of the command or plugin was successfully
    --     completed. This is a terminal state.
    --
    -- -   Delivery Timed Out: The command was not delivered to the instance
    --     before the delivery timeout expired. Delivery timeouts do not count
    --     against the parent command\'s MaxErrors limit, but they do
    --     contribute to whether the parent command status is Success or
    --     Incomplete. This is a terminal state.
    --
    -- -   Execution Timed Out: Command execution started on the instance, but
    --     the execution was not complete before the execution timeout expired.
    --     Execution timeouts count against the MaxErrors limit of the parent
    --     command. This is a terminal state.
    --
    -- -   Failed: The command was not successful on the instance. For a
    --     plugin, this indicates that the result code was not zero. For a
    --     command invocation, this indicates that the result code for one or
    --     more plugins was not zero. Invocation failures count against the
    --     MaxErrors limit of the parent command. This is a terminal state.
    --
    -- -   Canceled: The command was terminated before it was completed. This
    --     is a terminal state.
    --
    -- -   Undeliverable: The command can\'t be delivered to the instance. The
    --     instance might not exist or might not be responding. Undeliverable
    --     invocations don\'t count against the parent command\'s MaxErrors
    --     limit and don\'t contribute to whether the parent command status is
    --     Success or Incomplete. This is a terminal state.
    --
    -- -   Terminated: The parent command exceeded its MaxErrors limit and
    --     subsequent command invocations were canceled by the system. This is
    --     a terminal state.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The name of the invocation target. For EC2 instances this is the value
    -- for the aws:Name tag. For on-premises instances, this is the name of the
    -- instance.
    instanceName :: Prelude.Maybe Prelude.Text,
    -- | User-specified information about the command, such as a brief
    -- description of what the command should do.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The URL to the plugin\'s StdErr file in Amazon S3, if the S3 bucket was
    -- defined for the parent command. For an invocation, StandardErrorUrl is
    -- populated if there is just one plugin defined for the command, and the
    -- S3 bucket was defined for the command.
    standardErrorUrl :: Prelude.Maybe Prelude.Text,
    -- | The document name that was requested for execution.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The command against which this invocation was requested.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | Gets the trace output sent by the agent.
    traceOutput :: Prelude.Maybe Prelude.Text,
    -- | CloudWatch Logs information where you want Systems Manager to send the
    -- command output.
    cloudWatchOutputConfig :: Prelude.Maybe CloudWatchOutputConfig,
    commandPlugins :: Prelude.Maybe [CommandPlugin],
    -- | The SSM document version.
    documentVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CommandInvocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationConfig', 'commandInvocation_notificationConfig' - Configurations for sending notifications about command status changes on
-- a per instance basis.
--
-- 'standardOutputUrl', 'commandInvocation_standardOutputUrl' - The URL to the plugin\'s StdOut file in Amazon S3, if the S3 bucket was
-- defined for the parent command. For an invocation, StandardOutputUrl is
-- populated if there is just one plugin defined for the command, and the
-- S3 bucket was defined for the command.
--
-- 'status', 'commandInvocation_status' - Whether or not the invocation succeeded, failed, or is pending.
--
-- 'instanceId', 'commandInvocation_instanceId' - The instance ID in which this invocation was requested.
--
-- 'serviceRole', 'commandInvocation_serviceRole' - The IAM service role that Run Command uses to act on your behalf when
-- sending notifications about command status changes on a per instance
-- basis.
--
-- 'requestedDateTime', 'commandInvocation_requestedDateTime' - The time and date the request was sent to this instance.
--
-- 'statusDetails', 'commandInvocation_statusDetails' - A detailed status of the command execution for each invocation (each
-- instance targeted by the command). StatusDetails includes more
-- information than Status because it includes states resulting from error
-- and concurrency control parameters. StatusDetails can show different
-- results than Status. For more information about these statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /AWS Systems Manager User Guide/. StatusDetails can be one of the
-- following values:
--
-- -   Pending: The command has not been sent to the instance.
--
-- -   In Progress: The command has been sent to the instance but has not
--     reached a terminal state.
--
-- -   Success: The execution of the command or plugin was successfully
--     completed. This is a terminal state.
--
-- -   Delivery Timed Out: The command was not delivered to the instance
--     before the delivery timeout expired. Delivery timeouts do not count
--     against the parent command\'s MaxErrors limit, but they do
--     contribute to whether the parent command status is Success or
--     Incomplete. This is a terminal state.
--
-- -   Execution Timed Out: Command execution started on the instance, but
--     the execution was not complete before the execution timeout expired.
--     Execution timeouts count against the MaxErrors limit of the parent
--     command. This is a terminal state.
--
-- -   Failed: The command was not successful on the instance. For a
--     plugin, this indicates that the result code was not zero. For a
--     command invocation, this indicates that the result code for one or
--     more plugins was not zero. Invocation failures count against the
--     MaxErrors limit of the parent command. This is a terminal state.
--
-- -   Canceled: The command was terminated before it was completed. This
--     is a terminal state.
--
-- -   Undeliverable: The command can\'t be delivered to the instance. The
--     instance might not exist or might not be responding. Undeliverable
--     invocations don\'t count against the parent command\'s MaxErrors
--     limit and don\'t contribute to whether the parent command status is
--     Success or Incomplete. This is a terminal state.
--
-- -   Terminated: The parent command exceeded its MaxErrors limit and
--     subsequent command invocations were canceled by the system. This is
--     a terminal state.
--
-- 'instanceName', 'commandInvocation_instanceName' - The name of the invocation target. For EC2 instances this is the value
-- for the aws:Name tag. For on-premises instances, this is the name of the
-- instance.
--
-- 'comment', 'commandInvocation_comment' - User-specified information about the command, such as a brief
-- description of what the command should do.
--
-- 'standardErrorUrl', 'commandInvocation_standardErrorUrl' - The URL to the plugin\'s StdErr file in Amazon S3, if the S3 bucket was
-- defined for the parent command. For an invocation, StandardErrorUrl is
-- populated if there is just one plugin defined for the command, and the
-- S3 bucket was defined for the command.
--
-- 'documentName', 'commandInvocation_documentName' - The document name that was requested for execution.
--
-- 'commandId', 'commandInvocation_commandId' - The command against which this invocation was requested.
--
-- 'traceOutput', 'commandInvocation_traceOutput' - Gets the trace output sent by the agent.
--
-- 'cloudWatchOutputConfig', 'commandInvocation_cloudWatchOutputConfig' - CloudWatch Logs information where you want Systems Manager to send the
-- command output.
--
-- 'commandPlugins', 'commandInvocation_commandPlugins' - Undocumented member.
--
-- 'documentVersion', 'commandInvocation_documentVersion' - The SSM document version.
newCommandInvocation ::
  CommandInvocation
newCommandInvocation =
  CommandInvocation'
    { notificationConfig =
        Prelude.Nothing,
      standardOutputUrl = Prelude.Nothing,
      status = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      requestedDateTime = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      instanceName = Prelude.Nothing,
      comment = Prelude.Nothing,
      standardErrorUrl = Prelude.Nothing,
      documentName = Prelude.Nothing,
      commandId = Prelude.Nothing,
      traceOutput = Prelude.Nothing,
      cloudWatchOutputConfig = Prelude.Nothing,
      commandPlugins = Prelude.Nothing,
      documentVersion = Prelude.Nothing
    }

-- | Configurations for sending notifications about command status changes on
-- a per instance basis.
commandInvocation_notificationConfig :: Lens.Lens' CommandInvocation (Prelude.Maybe NotificationConfig)
commandInvocation_notificationConfig = Lens.lens (\CommandInvocation' {notificationConfig} -> notificationConfig) (\s@CommandInvocation' {} a -> s {notificationConfig = a} :: CommandInvocation)

-- | The URL to the plugin\'s StdOut file in Amazon S3, if the S3 bucket was
-- defined for the parent command. For an invocation, StandardOutputUrl is
-- populated if there is just one plugin defined for the command, and the
-- S3 bucket was defined for the command.
commandInvocation_standardOutputUrl :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_standardOutputUrl = Lens.lens (\CommandInvocation' {standardOutputUrl} -> standardOutputUrl) (\s@CommandInvocation' {} a -> s {standardOutputUrl = a} :: CommandInvocation)

-- | Whether or not the invocation succeeded, failed, or is pending.
commandInvocation_status :: Lens.Lens' CommandInvocation (Prelude.Maybe CommandInvocationStatus)
commandInvocation_status = Lens.lens (\CommandInvocation' {status} -> status) (\s@CommandInvocation' {} a -> s {status = a} :: CommandInvocation)

-- | The instance ID in which this invocation was requested.
commandInvocation_instanceId :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_instanceId = Lens.lens (\CommandInvocation' {instanceId} -> instanceId) (\s@CommandInvocation' {} a -> s {instanceId = a} :: CommandInvocation)

-- | The IAM service role that Run Command uses to act on your behalf when
-- sending notifications about command status changes on a per instance
-- basis.
commandInvocation_serviceRole :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_serviceRole = Lens.lens (\CommandInvocation' {serviceRole} -> serviceRole) (\s@CommandInvocation' {} a -> s {serviceRole = a} :: CommandInvocation)

-- | The time and date the request was sent to this instance.
commandInvocation_requestedDateTime :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.UTCTime)
commandInvocation_requestedDateTime = Lens.lens (\CommandInvocation' {requestedDateTime} -> requestedDateTime) (\s@CommandInvocation' {} a -> s {requestedDateTime = a} :: CommandInvocation) Prelude.. Lens.mapping Prelude._Time

-- | A detailed status of the command execution for each invocation (each
-- instance targeted by the command). StatusDetails includes more
-- information than Status because it includes states resulting from error
-- and concurrency control parameters. StatusDetails can show different
-- results than Status. For more information about these statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /AWS Systems Manager User Guide/. StatusDetails can be one of the
-- following values:
--
-- -   Pending: The command has not been sent to the instance.
--
-- -   In Progress: The command has been sent to the instance but has not
--     reached a terminal state.
--
-- -   Success: The execution of the command or plugin was successfully
--     completed. This is a terminal state.
--
-- -   Delivery Timed Out: The command was not delivered to the instance
--     before the delivery timeout expired. Delivery timeouts do not count
--     against the parent command\'s MaxErrors limit, but they do
--     contribute to whether the parent command status is Success or
--     Incomplete. This is a terminal state.
--
-- -   Execution Timed Out: Command execution started on the instance, but
--     the execution was not complete before the execution timeout expired.
--     Execution timeouts count against the MaxErrors limit of the parent
--     command. This is a terminal state.
--
-- -   Failed: The command was not successful on the instance. For a
--     plugin, this indicates that the result code was not zero. For a
--     command invocation, this indicates that the result code for one or
--     more plugins was not zero. Invocation failures count against the
--     MaxErrors limit of the parent command. This is a terminal state.
--
-- -   Canceled: The command was terminated before it was completed. This
--     is a terminal state.
--
-- -   Undeliverable: The command can\'t be delivered to the instance. The
--     instance might not exist or might not be responding. Undeliverable
--     invocations don\'t count against the parent command\'s MaxErrors
--     limit and don\'t contribute to whether the parent command status is
--     Success or Incomplete. This is a terminal state.
--
-- -   Terminated: The parent command exceeded its MaxErrors limit and
--     subsequent command invocations were canceled by the system. This is
--     a terminal state.
commandInvocation_statusDetails :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_statusDetails = Lens.lens (\CommandInvocation' {statusDetails} -> statusDetails) (\s@CommandInvocation' {} a -> s {statusDetails = a} :: CommandInvocation)

-- | The name of the invocation target. For EC2 instances this is the value
-- for the aws:Name tag. For on-premises instances, this is the name of the
-- instance.
commandInvocation_instanceName :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_instanceName = Lens.lens (\CommandInvocation' {instanceName} -> instanceName) (\s@CommandInvocation' {} a -> s {instanceName = a} :: CommandInvocation)

-- | User-specified information about the command, such as a brief
-- description of what the command should do.
commandInvocation_comment :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_comment = Lens.lens (\CommandInvocation' {comment} -> comment) (\s@CommandInvocation' {} a -> s {comment = a} :: CommandInvocation)

-- | The URL to the plugin\'s StdErr file in Amazon S3, if the S3 bucket was
-- defined for the parent command. For an invocation, StandardErrorUrl is
-- populated if there is just one plugin defined for the command, and the
-- S3 bucket was defined for the command.
commandInvocation_standardErrorUrl :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_standardErrorUrl = Lens.lens (\CommandInvocation' {standardErrorUrl} -> standardErrorUrl) (\s@CommandInvocation' {} a -> s {standardErrorUrl = a} :: CommandInvocation)

-- | The document name that was requested for execution.
commandInvocation_documentName :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_documentName = Lens.lens (\CommandInvocation' {documentName} -> documentName) (\s@CommandInvocation' {} a -> s {documentName = a} :: CommandInvocation)

-- | The command against which this invocation was requested.
commandInvocation_commandId :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_commandId = Lens.lens (\CommandInvocation' {commandId} -> commandId) (\s@CommandInvocation' {} a -> s {commandId = a} :: CommandInvocation)

-- | Gets the trace output sent by the agent.
commandInvocation_traceOutput :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_traceOutput = Lens.lens (\CommandInvocation' {traceOutput} -> traceOutput) (\s@CommandInvocation' {} a -> s {traceOutput = a} :: CommandInvocation)

-- | CloudWatch Logs information where you want Systems Manager to send the
-- command output.
commandInvocation_cloudWatchOutputConfig :: Lens.Lens' CommandInvocation (Prelude.Maybe CloudWatchOutputConfig)
commandInvocation_cloudWatchOutputConfig = Lens.lens (\CommandInvocation' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@CommandInvocation' {} a -> s {cloudWatchOutputConfig = a} :: CommandInvocation)

-- | Undocumented member.
commandInvocation_commandPlugins :: Lens.Lens' CommandInvocation (Prelude.Maybe [CommandPlugin])
commandInvocation_commandPlugins = Lens.lens (\CommandInvocation' {commandPlugins} -> commandPlugins) (\s@CommandInvocation' {} a -> s {commandPlugins = a} :: CommandInvocation) Prelude.. Lens.mapping Prelude._Coerce

-- | The SSM document version.
commandInvocation_documentVersion :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_documentVersion = Lens.lens (\CommandInvocation' {documentVersion} -> documentVersion) (\s@CommandInvocation' {} a -> s {documentVersion = a} :: CommandInvocation)

instance Prelude.FromJSON CommandInvocation where
  parseJSON =
    Prelude.withObject
      "CommandInvocation"
      ( \x ->
          CommandInvocation'
            Prelude.<$> (x Prelude..:? "NotificationConfig")
            Prelude.<*> (x Prelude..:? "StandardOutputUrl")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "InstanceId")
            Prelude.<*> (x Prelude..:? "ServiceRole")
            Prelude.<*> (x Prelude..:? "RequestedDateTime")
            Prelude.<*> (x Prelude..:? "StatusDetails")
            Prelude.<*> (x Prelude..:? "InstanceName")
            Prelude.<*> (x Prelude..:? "Comment")
            Prelude.<*> (x Prelude..:? "StandardErrorUrl")
            Prelude.<*> (x Prelude..:? "DocumentName")
            Prelude.<*> (x Prelude..:? "CommandId")
            Prelude.<*> (x Prelude..:? "TraceOutput")
            Prelude.<*> (x Prelude..:? "CloudWatchOutputConfig")
            Prelude.<*> ( x Prelude..:? "CommandPlugins"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "DocumentVersion")
      )

instance Prelude.Hashable CommandInvocation

instance Prelude.NFData CommandInvocation
