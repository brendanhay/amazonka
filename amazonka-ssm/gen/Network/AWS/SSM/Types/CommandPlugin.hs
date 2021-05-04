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
-- Module      : Network.AWS.SSM.Types.CommandPlugin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandPlugin where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.CommandPluginStatus

-- | Describes plugin details.
--
-- /See:/ 'newCommandPlugin' smart constructor.
data CommandPlugin = CommandPlugin'
  { -- | The URL for the complete text written by the plugin to stdout in Amazon
    -- S3. If the S3 bucket for the command was not specified, then this string
    -- is empty.
    standardOutputUrl :: Prelude.Maybe Prelude.Text,
    -- | The status of this plugin. You can run a document with multiple plugins.
    status :: Prelude.Maybe CommandPluginStatus,
    -- | A detailed status of the plugin execution. StatusDetails includes more
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
    --     instance might not exist, or it might not be responding.
    --     Undeliverable invocations don\'t count against the parent command\'s
    --     MaxErrors limit, and they don\'t contribute to whether the parent
    --     command status is Success or Incomplete. This is a terminal state.
    --
    -- -   Terminated: The parent command exceeded its MaxErrors limit and
    --     subsequent command invocations were canceled by the system. This is
    --     a terminal state.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket where the responses to the command executions should be
    -- stored. This was requested when issuing the command. For example, in the
    -- following response:
    --
    -- doc-example-bucket\/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix\/i-02573cafcfEXAMPLE\/awsrunShellScript
    --
    -- doc-example-bucket is the name of the S3 bucket;
    --
    -- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
    --
    -- i-02573cafcfEXAMPLE is the instance ID;
    --
    -- awsrunShellScript is the name of the plugin.
    outputS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The URL for the complete text written by the plugin to stderr. If
    -- execution is not yet complete, then this string is empty.
    standardErrorUrl :: Prelude.Maybe Prelude.Text,
    -- | Output of the plugin execution.
    output :: Prelude.Maybe Prelude.Text,
    -- | (Deprecated) You can no longer specify this parameter. The system
    -- ignores it. Instead, Systems Manager automatically determines the S3
    -- bucket region.
    outputS3Region :: Prelude.Maybe Prelude.Text,
    -- | The name of the plugin. Must be one of the following: aws:updateAgent,
    -- aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule,
    -- aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time the plugin stopped running. Could stop prematurely if, for
    -- example, a cancel command was sent.
    responseFinishDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | A numeric response code generated after running the plugin.
    responseCode :: Prelude.Maybe Prelude.Int,
    -- | The S3 directory path inside the bucket where the responses to the
    -- command executions should be stored. This was requested when issuing the
    -- command. For example, in the following response:
    --
    -- doc-example-bucket\/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix\/i-02573cafcfEXAMPLE\/awsrunShellScript
    --
    -- doc-example-bucket is the name of the S3 bucket;
    --
    -- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
    --
    -- i-02573cafcfEXAMPLE is the instance ID;
    --
    -- awsrunShellScript is the name of the plugin.
    outputS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The time the plugin started running.
    responseStartDateTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CommandPlugin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardOutputUrl', 'commandPlugin_standardOutputUrl' - The URL for the complete text written by the plugin to stdout in Amazon
-- S3. If the S3 bucket for the command was not specified, then this string
-- is empty.
--
-- 'status', 'commandPlugin_status' - The status of this plugin. You can run a document with multiple plugins.
--
-- 'statusDetails', 'commandPlugin_statusDetails' - A detailed status of the plugin execution. StatusDetails includes more
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
--     instance might not exist, or it might not be responding.
--     Undeliverable invocations don\'t count against the parent command\'s
--     MaxErrors limit, and they don\'t contribute to whether the parent
--     command status is Success or Incomplete. This is a terminal state.
--
-- -   Terminated: The parent command exceeded its MaxErrors limit and
--     subsequent command invocations were canceled by the system. This is
--     a terminal state.
--
-- 'outputS3BucketName', 'commandPlugin_outputS3BucketName' - The S3 bucket where the responses to the command executions should be
-- stored. This was requested when issuing the command. For example, in the
-- following response:
--
-- doc-example-bucket\/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix\/i-02573cafcfEXAMPLE\/awsrunShellScript
--
-- doc-example-bucket is the name of the S3 bucket;
--
-- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
--
-- i-02573cafcfEXAMPLE is the instance ID;
--
-- awsrunShellScript is the name of the plugin.
--
-- 'standardErrorUrl', 'commandPlugin_standardErrorUrl' - The URL for the complete text written by the plugin to stderr. If
-- execution is not yet complete, then this string is empty.
--
-- 'output', 'commandPlugin_output' - Output of the plugin execution.
--
-- 'outputS3Region', 'commandPlugin_outputS3Region' - (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the S3
-- bucket region.
--
-- 'name', 'commandPlugin_name' - The name of the plugin. Must be one of the following: aws:updateAgent,
-- aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule,
-- aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
--
-- 'responseFinishDateTime', 'commandPlugin_responseFinishDateTime' - The time the plugin stopped running. Could stop prematurely if, for
-- example, a cancel command was sent.
--
-- 'responseCode', 'commandPlugin_responseCode' - A numeric response code generated after running the plugin.
--
-- 'outputS3KeyPrefix', 'commandPlugin_outputS3KeyPrefix' - The S3 directory path inside the bucket where the responses to the
-- command executions should be stored. This was requested when issuing the
-- command. For example, in the following response:
--
-- doc-example-bucket\/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix\/i-02573cafcfEXAMPLE\/awsrunShellScript
--
-- doc-example-bucket is the name of the S3 bucket;
--
-- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
--
-- i-02573cafcfEXAMPLE is the instance ID;
--
-- awsrunShellScript is the name of the plugin.
--
-- 'responseStartDateTime', 'commandPlugin_responseStartDateTime' - The time the plugin started running.
newCommandPlugin ::
  CommandPlugin
newCommandPlugin =
  CommandPlugin'
    { standardOutputUrl = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      outputS3BucketName = Prelude.Nothing,
      standardErrorUrl = Prelude.Nothing,
      output = Prelude.Nothing,
      outputS3Region = Prelude.Nothing,
      name = Prelude.Nothing,
      responseFinishDateTime = Prelude.Nothing,
      responseCode = Prelude.Nothing,
      outputS3KeyPrefix = Prelude.Nothing,
      responseStartDateTime = Prelude.Nothing
    }

-- | The URL for the complete text written by the plugin to stdout in Amazon
-- S3. If the S3 bucket for the command was not specified, then this string
-- is empty.
commandPlugin_standardOutputUrl :: Lens.Lens' CommandPlugin (Prelude.Maybe Prelude.Text)
commandPlugin_standardOutputUrl = Lens.lens (\CommandPlugin' {standardOutputUrl} -> standardOutputUrl) (\s@CommandPlugin' {} a -> s {standardOutputUrl = a} :: CommandPlugin)

-- | The status of this plugin. You can run a document with multiple plugins.
commandPlugin_status :: Lens.Lens' CommandPlugin (Prelude.Maybe CommandPluginStatus)
commandPlugin_status = Lens.lens (\CommandPlugin' {status} -> status) (\s@CommandPlugin' {} a -> s {status = a} :: CommandPlugin)

-- | A detailed status of the plugin execution. StatusDetails includes more
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
--     instance might not exist, or it might not be responding.
--     Undeliverable invocations don\'t count against the parent command\'s
--     MaxErrors limit, and they don\'t contribute to whether the parent
--     command status is Success or Incomplete. This is a terminal state.
--
-- -   Terminated: The parent command exceeded its MaxErrors limit and
--     subsequent command invocations were canceled by the system. This is
--     a terminal state.
commandPlugin_statusDetails :: Lens.Lens' CommandPlugin (Prelude.Maybe Prelude.Text)
commandPlugin_statusDetails = Lens.lens (\CommandPlugin' {statusDetails} -> statusDetails) (\s@CommandPlugin' {} a -> s {statusDetails = a} :: CommandPlugin)

-- | The S3 bucket where the responses to the command executions should be
-- stored. This was requested when issuing the command. For example, in the
-- following response:
--
-- doc-example-bucket\/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix\/i-02573cafcfEXAMPLE\/awsrunShellScript
--
-- doc-example-bucket is the name of the S3 bucket;
--
-- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
--
-- i-02573cafcfEXAMPLE is the instance ID;
--
-- awsrunShellScript is the name of the plugin.
commandPlugin_outputS3BucketName :: Lens.Lens' CommandPlugin (Prelude.Maybe Prelude.Text)
commandPlugin_outputS3BucketName = Lens.lens (\CommandPlugin' {outputS3BucketName} -> outputS3BucketName) (\s@CommandPlugin' {} a -> s {outputS3BucketName = a} :: CommandPlugin)

-- | The URL for the complete text written by the plugin to stderr. If
-- execution is not yet complete, then this string is empty.
commandPlugin_standardErrorUrl :: Lens.Lens' CommandPlugin (Prelude.Maybe Prelude.Text)
commandPlugin_standardErrorUrl = Lens.lens (\CommandPlugin' {standardErrorUrl} -> standardErrorUrl) (\s@CommandPlugin' {} a -> s {standardErrorUrl = a} :: CommandPlugin)

-- | Output of the plugin execution.
commandPlugin_output :: Lens.Lens' CommandPlugin (Prelude.Maybe Prelude.Text)
commandPlugin_output = Lens.lens (\CommandPlugin' {output} -> output) (\s@CommandPlugin' {} a -> s {output = a} :: CommandPlugin)

-- | (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the S3
-- bucket region.
commandPlugin_outputS3Region :: Lens.Lens' CommandPlugin (Prelude.Maybe Prelude.Text)
commandPlugin_outputS3Region = Lens.lens (\CommandPlugin' {outputS3Region} -> outputS3Region) (\s@CommandPlugin' {} a -> s {outputS3Region = a} :: CommandPlugin)

-- | The name of the plugin. Must be one of the following: aws:updateAgent,
-- aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule,
-- aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
commandPlugin_name :: Lens.Lens' CommandPlugin (Prelude.Maybe Prelude.Text)
commandPlugin_name = Lens.lens (\CommandPlugin' {name} -> name) (\s@CommandPlugin' {} a -> s {name = a} :: CommandPlugin)

-- | The time the plugin stopped running. Could stop prematurely if, for
-- example, a cancel command was sent.
commandPlugin_responseFinishDateTime :: Lens.Lens' CommandPlugin (Prelude.Maybe Prelude.UTCTime)
commandPlugin_responseFinishDateTime = Lens.lens (\CommandPlugin' {responseFinishDateTime} -> responseFinishDateTime) (\s@CommandPlugin' {} a -> s {responseFinishDateTime = a} :: CommandPlugin) Prelude.. Lens.mapping Prelude._Time

-- | A numeric response code generated after running the plugin.
commandPlugin_responseCode :: Lens.Lens' CommandPlugin (Prelude.Maybe Prelude.Int)
commandPlugin_responseCode = Lens.lens (\CommandPlugin' {responseCode} -> responseCode) (\s@CommandPlugin' {} a -> s {responseCode = a} :: CommandPlugin)

-- | The S3 directory path inside the bucket where the responses to the
-- command executions should be stored. This was requested when issuing the
-- command. For example, in the following response:
--
-- doc-example-bucket\/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix\/i-02573cafcfEXAMPLE\/awsrunShellScript
--
-- doc-example-bucket is the name of the S3 bucket;
--
-- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
--
-- i-02573cafcfEXAMPLE is the instance ID;
--
-- awsrunShellScript is the name of the plugin.
commandPlugin_outputS3KeyPrefix :: Lens.Lens' CommandPlugin (Prelude.Maybe Prelude.Text)
commandPlugin_outputS3KeyPrefix = Lens.lens (\CommandPlugin' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@CommandPlugin' {} a -> s {outputS3KeyPrefix = a} :: CommandPlugin)

-- | The time the plugin started running.
commandPlugin_responseStartDateTime :: Lens.Lens' CommandPlugin (Prelude.Maybe Prelude.UTCTime)
commandPlugin_responseStartDateTime = Lens.lens (\CommandPlugin' {responseStartDateTime} -> responseStartDateTime) (\s@CommandPlugin' {} a -> s {responseStartDateTime = a} :: CommandPlugin) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON CommandPlugin where
  parseJSON =
    Prelude.withObject
      "CommandPlugin"
      ( \x ->
          CommandPlugin'
            Prelude.<$> (x Prelude..:? "StandardOutputUrl")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "StatusDetails")
            Prelude.<*> (x Prelude..:? "OutputS3BucketName")
            Prelude.<*> (x Prelude..:? "StandardErrorUrl")
            Prelude.<*> (x Prelude..:? "Output")
            Prelude.<*> (x Prelude..:? "OutputS3Region")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "ResponseFinishDateTime")
            Prelude.<*> (x Prelude..:? "ResponseCode")
            Prelude.<*> (x Prelude..:? "OutputS3KeyPrefix")
            Prelude.<*> (x Prelude..:? "ResponseStartDateTime")
      )

instance Prelude.Hashable CommandPlugin

instance Prelude.NFData CommandPlugin
