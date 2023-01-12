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
-- Module      : Amazonka.SSM.Types.CommandInvocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.CommandInvocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.CloudWatchOutputConfig
import Amazonka.SSM.Types.CommandInvocationStatus
import Amazonka.SSM.Types.CommandPlugin
import Amazonka.SSM.Types.NotificationConfig

-- | An invocation is a copy of a command sent to a specific managed node. A
-- command can apply to one or more managed nodes. A command invocation
-- applies to one managed node. For example, if a user runs @SendCommand@
-- against three managed nodes, then a command invocation is created for
-- each requested managed node ID. A command invocation returns status and
-- detail information about a command you ran.
--
-- /See:/ 'newCommandInvocation' smart constructor.
data CommandInvocation = CommandInvocation'
  { -- | Amazon CloudWatch Logs information where you want Amazon Web Services
    -- Systems Manager to send the command output.
    cloudWatchOutputConfig :: Prelude.Maybe CloudWatchOutputConfig,
    -- | The command against which this invocation was requested.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | Plugins processed by the command.
    commandPlugins :: Prelude.Maybe [CommandPlugin],
    -- | User-specified information about the command, such as a brief
    -- description of what the command should do.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The document name that was requested for execution.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The Systems Manager document (SSM document) version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The managed node ID in which this invocation was requested.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified host name of the managed node.
    instanceName :: Prelude.Maybe Prelude.Text,
    -- | Configurations for sending notifications about command status changes on
    -- a per managed node basis.
    notificationConfig :: Prelude.Maybe NotificationConfig,
    -- | The time and date the request was sent to this managed node.
    requestedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Identity and Access Management (IAM) service role that Run Command,
    -- a capability of Amazon Web Services Systems Manager, uses to act on your
    -- behalf when sending notifications about command status changes on a per
    -- managed node basis.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The URL to the plugin\'s StdErr file in Amazon Simple Storage Service
    -- (Amazon S3), if the S3 bucket was defined for the parent command. For an
    -- invocation, @StandardErrorUrl@ is populated if there is just one plugin
    -- defined for the command, and the S3 bucket was defined for the command.
    standardErrorUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL to the plugin\'s StdOut file in Amazon Simple Storage Service
    -- (Amazon S3), if the S3 bucket was defined for the parent command. For an
    -- invocation, @StandardOutputUrl@ is populated if there is just one plugin
    -- defined for the command, and the S3 bucket was defined for the command.
    standardOutputUrl :: Prelude.Maybe Prelude.Text,
    -- | Whether or not the invocation succeeded, failed, or is pending.
    status :: Prelude.Maybe CommandInvocationStatus,
    -- | A detailed status of the command execution for each invocation (each
    -- managed node targeted by the command). StatusDetails includes more
    -- information than Status because it includes states resulting from error
    -- and concurrency control parameters. StatusDetails can show different
    -- results than Status. For more information about these statuses, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
    -- in the /Amazon Web Services Systems Manager User Guide/. StatusDetails
    -- can be one of the following values:
    --
    -- -   Pending: The command hasn\'t been sent to the managed node.
    --
    -- -   In Progress: The command has been sent to the managed node but
    --     hasn\'t reached a terminal state.
    --
    -- -   Success: The execution of the command or plugin was successfully
    --     completed. This is a terminal state.
    --
    -- -   Delivery Timed Out: The command wasn\'t delivered to the managed
    --     node before the delivery timeout expired. Delivery timeouts don\'t
    --     count against the parent command\'s @MaxErrors@ limit, but they do
    --     contribute to whether the parent command status is Success or
    --     Incomplete. This is a terminal state.
    --
    -- -   Execution Timed Out: Command execution started on the managed node,
    --     but the execution wasn\'t complete before the execution timeout
    --     expired. Execution timeouts count against the @MaxErrors@ limit of
    --     the parent command. This is a terminal state.
    --
    -- -   Failed: The command wasn\'t successful on the managed node. For a
    --     plugin, this indicates that the result code wasn\'t zero. For a
    --     command invocation, this indicates that the result code for one or
    --     more plugins wasn\'t zero. Invocation failures count against the
    --     @MaxErrors@ limit of the parent command. This is a terminal state.
    --
    -- -   Cancelled: The command was terminated before it was completed. This
    --     is a terminal state.
    --
    -- -   Undeliverable: The command can\'t be delivered to the managed node.
    --     The managed node might not exist or might not be responding.
    --     Undeliverable invocations don\'t count against the parent command\'s
    --     MaxErrors limit and don\'t contribute to whether the parent command
    --     status is Success or Incomplete. This is a terminal state.
    --
    -- -   Terminated: The parent command exceeded its MaxErrors limit and
    --     subsequent command invocations were canceled by the system. This is
    --     a terminal state.
    --
    -- -   Delayed: The system attempted to send the command to the managed
    --     node but wasn\'t successful. The system retries again.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | Gets the trace output sent by the agent.
    traceOutput :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommandInvocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchOutputConfig', 'commandInvocation_cloudWatchOutputConfig' - Amazon CloudWatch Logs information where you want Amazon Web Services
-- Systems Manager to send the command output.
--
-- 'commandId', 'commandInvocation_commandId' - The command against which this invocation was requested.
--
-- 'commandPlugins', 'commandInvocation_commandPlugins' - Plugins processed by the command.
--
-- 'comment', 'commandInvocation_comment' - User-specified information about the command, such as a brief
-- description of what the command should do.
--
-- 'documentName', 'commandInvocation_documentName' - The document name that was requested for execution.
--
-- 'documentVersion', 'commandInvocation_documentVersion' - The Systems Manager document (SSM document) version.
--
-- 'instanceId', 'commandInvocation_instanceId' - The managed node ID in which this invocation was requested.
--
-- 'instanceName', 'commandInvocation_instanceName' - The fully qualified host name of the managed node.
--
-- 'notificationConfig', 'commandInvocation_notificationConfig' - Configurations for sending notifications about command status changes on
-- a per managed node basis.
--
-- 'requestedDateTime', 'commandInvocation_requestedDateTime' - The time and date the request was sent to this managed node.
--
-- 'serviceRole', 'commandInvocation_serviceRole' - The Identity and Access Management (IAM) service role that Run Command,
-- a capability of Amazon Web Services Systems Manager, uses to act on your
-- behalf when sending notifications about command status changes on a per
-- managed node basis.
--
-- 'standardErrorUrl', 'commandInvocation_standardErrorUrl' - The URL to the plugin\'s StdErr file in Amazon Simple Storage Service
-- (Amazon S3), if the S3 bucket was defined for the parent command. For an
-- invocation, @StandardErrorUrl@ is populated if there is just one plugin
-- defined for the command, and the S3 bucket was defined for the command.
--
-- 'standardOutputUrl', 'commandInvocation_standardOutputUrl' - The URL to the plugin\'s StdOut file in Amazon Simple Storage Service
-- (Amazon S3), if the S3 bucket was defined for the parent command. For an
-- invocation, @StandardOutputUrl@ is populated if there is just one plugin
-- defined for the command, and the S3 bucket was defined for the command.
--
-- 'status', 'commandInvocation_status' - Whether or not the invocation succeeded, failed, or is pending.
--
-- 'statusDetails', 'commandInvocation_statusDetails' - A detailed status of the command execution for each invocation (each
-- managed node targeted by the command). StatusDetails includes more
-- information than Status because it includes states resulting from error
-- and concurrency control parameters. StatusDetails can show different
-- results than Status. For more information about these statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /Amazon Web Services Systems Manager User Guide/. StatusDetails
-- can be one of the following values:
--
-- -   Pending: The command hasn\'t been sent to the managed node.
--
-- -   In Progress: The command has been sent to the managed node but
--     hasn\'t reached a terminal state.
--
-- -   Success: The execution of the command or plugin was successfully
--     completed. This is a terminal state.
--
-- -   Delivery Timed Out: The command wasn\'t delivered to the managed
--     node before the delivery timeout expired. Delivery timeouts don\'t
--     count against the parent command\'s @MaxErrors@ limit, but they do
--     contribute to whether the parent command status is Success or
--     Incomplete. This is a terminal state.
--
-- -   Execution Timed Out: Command execution started on the managed node,
--     but the execution wasn\'t complete before the execution timeout
--     expired. Execution timeouts count against the @MaxErrors@ limit of
--     the parent command. This is a terminal state.
--
-- -   Failed: The command wasn\'t successful on the managed node. For a
--     plugin, this indicates that the result code wasn\'t zero. For a
--     command invocation, this indicates that the result code for one or
--     more plugins wasn\'t zero. Invocation failures count against the
--     @MaxErrors@ limit of the parent command. This is a terminal state.
--
-- -   Cancelled: The command was terminated before it was completed. This
--     is a terminal state.
--
-- -   Undeliverable: The command can\'t be delivered to the managed node.
--     The managed node might not exist or might not be responding.
--     Undeliverable invocations don\'t count against the parent command\'s
--     MaxErrors limit and don\'t contribute to whether the parent command
--     status is Success or Incomplete. This is a terminal state.
--
-- -   Terminated: The parent command exceeded its MaxErrors limit and
--     subsequent command invocations were canceled by the system. This is
--     a terminal state.
--
-- -   Delayed: The system attempted to send the command to the managed
--     node but wasn\'t successful. The system retries again.
--
-- 'traceOutput', 'commandInvocation_traceOutput' - Gets the trace output sent by the agent.
newCommandInvocation ::
  CommandInvocation
newCommandInvocation =
  CommandInvocation'
    { cloudWatchOutputConfig =
        Prelude.Nothing,
      commandId = Prelude.Nothing,
      commandPlugins = Prelude.Nothing,
      comment = Prelude.Nothing,
      documentName = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceName = Prelude.Nothing,
      notificationConfig = Prelude.Nothing,
      requestedDateTime = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      standardErrorUrl = Prelude.Nothing,
      standardOutputUrl = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      traceOutput = Prelude.Nothing
    }

-- | Amazon CloudWatch Logs information where you want Amazon Web Services
-- Systems Manager to send the command output.
commandInvocation_cloudWatchOutputConfig :: Lens.Lens' CommandInvocation (Prelude.Maybe CloudWatchOutputConfig)
commandInvocation_cloudWatchOutputConfig = Lens.lens (\CommandInvocation' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@CommandInvocation' {} a -> s {cloudWatchOutputConfig = a} :: CommandInvocation)

-- | The command against which this invocation was requested.
commandInvocation_commandId :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_commandId = Lens.lens (\CommandInvocation' {commandId} -> commandId) (\s@CommandInvocation' {} a -> s {commandId = a} :: CommandInvocation)

-- | Plugins processed by the command.
commandInvocation_commandPlugins :: Lens.Lens' CommandInvocation (Prelude.Maybe [CommandPlugin])
commandInvocation_commandPlugins = Lens.lens (\CommandInvocation' {commandPlugins} -> commandPlugins) (\s@CommandInvocation' {} a -> s {commandPlugins = a} :: CommandInvocation) Prelude.. Lens.mapping Lens.coerced

-- | User-specified information about the command, such as a brief
-- description of what the command should do.
commandInvocation_comment :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_comment = Lens.lens (\CommandInvocation' {comment} -> comment) (\s@CommandInvocation' {} a -> s {comment = a} :: CommandInvocation)

-- | The document name that was requested for execution.
commandInvocation_documentName :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_documentName = Lens.lens (\CommandInvocation' {documentName} -> documentName) (\s@CommandInvocation' {} a -> s {documentName = a} :: CommandInvocation)

-- | The Systems Manager document (SSM document) version.
commandInvocation_documentVersion :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_documentVersion = Lens.lens (\CommandInvocation' {documentVersion} -> documentVersion) (\s@CommandInvocation' {} a -> s {documentVersion = a} :: CommandInvocation)

-- | The managed node ID in which this invocation was requested.
commandInvocation_instanceId :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_instanceId = Lens.lens (\CommandInvocation' {instanceId} -> instanceId) (\s@CommandInvocation' {} a -> s {instanceId = a} :: CommandInvocation)

-- | The fully qualified host name of the managed node.
commandInvocation_instanceName :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_instanceName = Lens.lens (\CommandInvocation' {instanceName} -> instanceName) (\s@CommandInvocation' {} a -> s {instanceName = a} :: CommandInvocation)

-- | Configurations for sending notifications about command status changes on
-- a per managed node basis.
commandInvocation_notificationConfig :: Lens.Lens' CommandInvocation (Prelude.Maybe NotificationConfig)
commandInvocation_notificationConfig = Lens.lens (\CommandInvocation' {notificationConfig} -> notificationConfig) (\s@CommandInvocation' {} a -> s {notificationConfig = a} :: CommandInvocation)

-- | The time and date the request was sent to this managed node.
commandInvocation_requestedDateTime :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.UTCTime)
commandInvocation_requestedDateTime = Lens.lens (\CommandInvocation' {requestedDateTime} -> requestedDateTime) (\s@CommandInvocation' {} a -> s {requestedDateTime = a} :: CommandInvocation) Prelude.. Lens.mapping Data._Time

-- | The Identity and Access Management (IAM) service role that Run Command,
-- a capability of Amazon Web Services Systems Manager, uses to act on your
-- behalf when sending notifications about command status changes on a per
-- managed node basis.
commandInvocation_serviceRole :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_serviceRole = Lens.lens (\CommandInvocation' {serviceRole} -> serviceRole) (\s@CommandInvocation' {} a -> s {serviceRole = a} :: CommandInvocation)

-- | The URL to the plugin\'s StdErr file in Amazon Simple Storage Service
-- (Amazon S3), if the S3 bucket was defined for the parent command. For an
-- invocation, @StandardErrorUrl@ is populated if there is just one plugin
-- defined for the command, and the S3 bucket was defined for the command.
commandInvocation_standardErrorUrl :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_standardErrorUrl = Lens.lens (\CommandInvocation' {standardErrorUrl} -> standardErrorUrl) (\s@CommandInvocation' {} a -> s {standardErrorUrl = a} :: CommandInvocation)

-- | The URL to the plugin\'s StdOut file in Amazon Simple Storage Service
-- (Amazon S3), if the S3 bucket was defined for the parent command. For an
-- invocation, @StandardOutputUrl@ is populated if there is just one plugin
-- defined for the command, and the S3 bucket was defined for the command.
commandInvocation_standardOutputUrl :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_standardOutputUrl = Lens.lens (\CommandInvocation' {standardOutputUrl} -> standardOutputUrl) (\s@CommandInvocation' {} a -> s {standardOutputUrl = a} :: CommandInvocation)

-- | Whether or not the invocation succeeded, failed, or is pending.
commandInvocation_status :: Lens.Lens' CommandInvocation (Prelude.Maybe CommandInvocationStatus)
commandInvocation_status = Lens.lens (\CommandInvocation' {status} -> status) (\s@CommandInvocation' {} a -> s {status = a} :: CommandInvocation)

-- | A detailed status of the command execution for each invocation (each
-- managed node targeted by the command). StatusDetails includes more
-- information than Status because it includes states resulting from error
-- and concurrency control parameters. StatusDetails can show different
-- results than Status. For more information about these statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /Amazon Web Services Systems Manager User Guide/. StatusDetails
-- can be one of the following values:
--
-- -   Pending: The command hasn\'t been sent to the managed node.
--
-- -   In Progress: The command has been sent to the managed node but
--     hasn\'t reached a terminal state.
--
-- -   Success: The execution of the command or plugin was successfully
--     completed. This is a terminal state.
--
-- -   Delivery Timed Out: The command wasn\'t delivered to the managed
--     node before the delivery timeout expired. Delivery timeouts don\'t
--     count against the parent command\'s @MaxErrors@ limit, but they do
--     contribute to whether the parent command status is Success or
--     Incomplete. This is a terminal state.
--
-- -   Execution Timed Out: Command execution started on the managed node,
--     but the execution wasn\'t complete before the execution timeout
--     expired. Execution timeouts count against the @MaxErrors@ limit of
--     the parent command. This is a terminal state.
--
-- -   Failed: The command wasn\'t successful on the managed node. For a
--     plugin, this indicates that the result code wasn\'t zero. For a
--     command invocation, this indicates that the result code for one or
--     more plugins wasn\'t zero. Invocation failures count against the
--     @MaxErrors@ limit of the parent command. This is a terminal state.
--
-- -   Cancelled: The command was terminated before it was completed. This
--     is a terminal state.
--
-- -   Undeliverable: The command can\'t be delivered to the managed node.
--     The managed node might not exist or might not be responding.
--     Undeliverable invocations don\'t count against the parent command\'s
--     MaxErrors limit and don\'t contribute to whether the parent command
--     status is Success or Incomplete. This is a terminal state.
--
-- -   Terminated: The parent command exceeded its MaxErrors limit and
--     subsequent command invocations were canceled by the system. This is
--     a terminal state.
--
-- -   Delayed: The system attempted to send the command to the managed
--     node but wasn\'t successful. The system retries again.
commandInvocation_statusDetails :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_statusDetails = Lens.lens (\CommandInvocation' {statusDetails} -> statusDetails) (\s@CommandInvocation' {} a -> s {statusDetails = a} :: CommandInvocation)

-- | Gets the trace output sent by the agent.
commandInvocation_traceOutput :: Lens.Lens' CommandInvocation (Prelude.Maybe Prelude.Text)
commandInvocation_traceOutput = Lens.lens (\CommandInvocation' {traceOutput} -> traceOutput) (\s@CommandInvocation' {} a -> s {traceOutput = a} :: CommandInvocation)

instance Data.FromJSON CommandInvocation where
  parseJSON =
    Data.withObject
      "CommandInvocation"
      ( \x ->
          CommandInvocation'
            Prelude.<$> (x Data..:? "CloudWatchOutputConfig")
            Prelude.<*> (x Data..:? "CommandId")
            Prelude.<*> (x Data..:? "CommandPlugins" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Comment")
            Prelude.<*> (x Data..:? "DocumentName")
            Prelude.<*> (x Data..:? "DocumentVersion")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "InstanceName")
            Prelude.<*> (x Data..:? "NotificationConfig")
            Prelude.<*> (x Data..:? "RequestedDateTime")
            Prelude.<*> (x Data..:? "ServiceRole")
            Prelude.<*> (x Data..:? "StandardErrorUrl")
            Prelude.<*> (x Data..:? "StandardOutputUrl")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusDetails")
            Prelude.<*> (x Data..:? "TraceOutput")
      )

instance Prelude.Hashable CommandInvocation where
  hashWithSalt _salt CommandInvocation' {..} =
    _salt `Prelude.hashWithSalt` cloudWatchOutputConfig
      `Prelude.hashWithSalt` commandId
      `Prelude.hashWithSalt` commandPlugins
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` documentName
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceName
      `Prelude.hashWithSalt` notificationConfig
      `Prelude.hashWithSalt` requestedDateTime
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` standardErrorUrl
      `Prelude.hashWithSalt` standardOutputUrl
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusDetails
      `Prelude.hashWithSalt` traceOutput

instance Prelude.NFData CommandInvocation where
  rnf CommandInvocation' {..} =
    Prelude.rnf cloudWatchOutputConfig
      `Prelude.seq` Prelude.rnf commandId
      `Prelude.seq` Prelude.rnf commandPlugins
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf documentName
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf instanceName
      `Prelude.seq` Prelude.rnf notificationConfig
      `Prelude.seq` Prelude.rnf requestedDateTime
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf standardErrorUrl
      `Prelude.seq` Prelude.rnf standardOutputUrl
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusDetails
      `Prelude.seq` Prelude.rnf traceOutput
