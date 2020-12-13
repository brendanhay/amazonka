{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandPlugin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandPlugin
  ( CommandPlugin (..),

    -- * Smart constructor
    mkCommandPlugin,

    -- * Lenses
    cpStatus,
    cpResponseStartDateTime,
    cpOutputS3KeyPrefix,
    cpStandardErrorURL,
    cpResponseCode,
    cpStatusDetails,
    cpOutput,
    cpStandardOutputURL,
    cpName,
    cpOutputS3Region,
    cpOutputS3BucketName,
    cpResponseFinishDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.CommandPluginStatus

-- | Describes plugin details.
--
-- /See:/ 'mkCommandPlugin' smart constructor.
data CommandPlugin = CommandPlugin'
  { -- | The status of this plugin. You can run a document with multiple plugins.
    status :: Lude.Maybe CommandPluginStatus,
    -- | The time the plugin started running.
    responseStartDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response:
    --
    -- doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript
    -- doc-example-bucket is the name of the S3 bucket;
    -- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
    -- i-02573cafcfEXAMPLE is the instance ID;
    -- awsrunShellScript is the name of the plugin.
    outputS3KeyPrefix :: Lude.Maybe Lude.Text,
    -- | The URL for the complete text written by the plugin to stderr. If execution is not yet complete, then this string is empty.
    standardErrorURL :: Lude.Maybe Lude.Text,
    -- | A numeric response code generated after running the plugin.
    responseCode :: Lude.Maybe Lude.Int,
    -- | A detailed status of the plugin execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:
    --
    --
    --     * Pending: The command has not been sent to the instance.
    --
    --
    --     * In Progress: The command has been sent to the instance but has not reached a terminal state.
    --
    --
    --     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.
    --
    --
    --     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
    --
    --
    --     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.
    --
    --
    --     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.
    --
    --
    --     * Canceled: The command was terminated before it was completed. This is a terminal state.
    --
    --
    --     * Undeliverable: The command can't be delivered to the instance. The instance might not exist, or it might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit, and they don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
    --
    --
    --     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
    statusDetails :: Lude.Maybe Lude.Text,
    -- | Output of the plugin execution.
    output :: Lude.Maybe Lude.Text,
    -- | The URL for the complete text written by the plugin to stdout in Amazon S3. If the S3 bucket for the command was not specified, then this string is empty.
    standardOutputURL :: Lude.Maybe Lude.Text,
    -- | The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
    name :: Lude.Maybe Lude.Text,
    -- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the S3 bucket region.
    outputS3Region :: Lude.Maybe Lude.Text,
    -- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response:
    --
    -- doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript
    -- doc-example-bucket is the name of the S3 bucket;
    -- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
    -- i-02573cafcfEXAMPLE is the instance ID;
    -- awsrunShellScript is the name of the plugin.
    outputS3BucketName :: Lude.Maybe Lude.Text,
    -- | The time the plugin stopped running. Could stop prematurely if, for example, a cancel command was sent.
    responseFinishDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CommandPlugin' with the minimum fields required to make a request.
--
-- * 'status' - The status of this plugin. You can run a document with multiple plugins.
-- * 'responseStartDateTime' - The time the plugin started running.
-- * 'outputS3KeyPrefix' - The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response:
--
-- doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript
-- doc-example-bucket is the name of the S3 bucket;
-- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
-- i-02573cafcfEXAMPLE is the instance ID;
-- awsrunShellScript is the name of the plugin.
-- * 'standardErrorURL' - The URL for the complete text written by the plugin to stderr. If execution is not yet complete, then this string is empty.
-- * 'responseCode' - A numeric response code generated after running the plugin.
-- * 'statusDetails' - A detailed status of the plugin execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:
--
--
--     * Pending: The command has not been sent to the instance.
--
--
--     * In Progress: The command has been sent to the instance but has not reached a terminal state.
--
--
--     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.
--
--
--     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
--
--
--     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.
--
--
--     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.
--
--
--     * Canceled: The command was terminated before it was completed. This is a terminal state.
--
--
--     * Undeliverable: The command can't be delivered to the instance. The instance might not exist, or it might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit, and they don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
--
--
--     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
--
--
-- * 'output' - Output of the plugin execution.
-- * 'standardOutputURL' - The URL for the complete text written by the plugin to stdout in Amazon S3. If the S3 bucket for the command was not specified, then this string is empty.
-- * 'name' - The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
-- * 'outputS3Region' - (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the S3 bucket region.
-- * 'outputS3BucketName' - The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response:
--
-- doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript
-- doc-example-bucket is the name of the S3 bucket;
-- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
-- i-02573cafcfEXAMPLE is the instance ID;
-- awsrunShellScript is the name of the plugin.
-- * 'responseFinishDateTime' - The time the plugin stopped running. Could stop prematurely if, for example, a cancel command was sent.
mkCommandPlugin ::
  CommandPlugin
mkCommandPlugin =
  CommandPlugin'
    { status = Lude.Nothing,
      responseStartDateTime = Lude.Nothing,
      outputS3KeyPrefix = Lude.Nothing,
      standardErrorURL = Lude.Nothing,
      responseCode = Lude.Nothing,
      statusDetails = Lude.Nothing,
      output = Lude.Nothing,
      standardOutputURL = Lude.Nothing,
      name = Lude.Nothing,
      outputS3Region = Lude.Nothing,
      outputS3BucketName = Lude.Nothing,
      responseFinishDateTime = Lude.Nothing
    }

-- | The status of this plugin. You can run a document with multiple plugins.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpStatus :: Lens.Lens' CommandPlugin (Lude.Maybe CommandPluginStatus)
cpStatus = Lens.lens (status :: CommandPlugin -> Lude.Maybe CommandPluginStatus) (\s a -> s {status = a} :: CommandPlugin)
{-# DEPRECATED cpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time the plugin started running.
--
-- /Note:/ Consider using 'responseStartDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpResponseStartDateTime :: Lens.Lens' CommandPlugin (Lude.Maybe Lude.Timestamp)
cpResponseStartDateTime = Lens.lens (responseStartDateTime :: CommandPlugin -> Lude.Maybe Lude.Timestamp) (\s a -> s {responseStartDateTime = a} :: CommandPlugin)
{-# DEPRECATED cpResponseStartDateTime "Use generic-lens or generic-optics with 'responseStartDateTime' instead." #-}

-- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response:
--
-- doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript
-- doc-example-bucket is the name of the S3 bucket;
-- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
-- i-02573cafcfEXAMPLE is the instance ID;
-- awsrunShellScript is the name of the plugin.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOutputS3KeyPrefix :: Lens.Lens' CommandPlugin (Lude.Maybe Lude.Text)
cpOutputS3KeyPrefix = Lens.lens (outputS3KeyPrefix :: CommandPlugin -> Lude.Maybe Lude.Text) (\s a -> s {outputS3KeyPrefix = a} :: CommandPlugin)
{-# DEPRECATED cpOutputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead." #-}

-- | The URL for the complete text written by the plugin to stderr. If execution is not yet complete, then this string is empty.
--
-- /Note:/ Consider using 'standardErrorURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpStandardErrorURL :: Lens.Lens' CommandPlugin (Lude.Maybe Lude.Text)
cpStandardErrorURL = Lens.lens (standardErrorURL :: CommandPlugin -> Lude.Maybe Lude.Text) (\s a -> s {standardErrorURL = a} :: CommandPlugin)
{-# DEPRECATED cpStandardErrorURL "Use generic-lens or generic-optics with 'standardErrorURL' instead." #-}

-- | A numeric response code generated after running the plugin.
--
-- /Note:/ Consider using 'responseCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpResponseCode :: Lens.Lens' CommandPlugin (Lude.Maybe Lude.Int)
cpResponseCode = Lens.lens (responseCode :: CommandPlugin -> Lude.Maybe Lude.Int) (\s a -> s {responseCode = a} :: CommandPlugin)
{-# DEPRECATED cpResponseCode "Use generic-lens or generic-optics with 'responseCode' instead." #-}

-- | A detailed status of the plugin execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:
--
--
--     * Pending: The command has not been sent to the instance.
--
--
--     * In Progress: The command has been sent to the instance but has not reached a terminal state.
--
--
--     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.
--
--
--     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
--
--
--     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.
--
--
--     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.
--
--
--     * Canceled: The command was terminated before it was completed. This is a terminal state.
--
--
--     * Undeliverable: The command can't be delivered to the instance. The instance might not exist, or it might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit, and they don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
--
--
--     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
--
--
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpStatusDetails :: Lens.Lens' CommandPlugin (Lude.Maybe Lude.Text)
cpStatusDetails = Lens.lens (statusDetails :: CommandPlugin -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: CommandPlugin)
{-# DEPRECATED cpStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | Output of the plugin execution.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOutput :: Lens.Lens' CommandPlugin (Lude.Maybe Lude.Text)
cpOutput = Lens.lens (output :: CommandPlugin -> Lude.Maybe Lude.Text) (\s a -> s {output = a} :: CommandPlugin)
{-# DEPRECATED cpOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | The URL for the complete text written by the plugin to stdout in Amazon S3. If the S3 bucket for the command was not specified, then this string is empty.
--
-- /Note:/ Consider using 'standardOutputURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpStandardOutputURL :: Lens.Lens' CommandPlugin (Lude.Maybe Lude.Text)
cpStandardOutputURL = Lens.lens (standardOutputURL :: CommandPlugin -> Lude.Maybe Lude.Text) (\s a -> s {standardOutputURL = a} :: CommandPlugin)
{-# DEPRECATED cpStandardOutputURL "Use generic-lens or generic-optics with 'standardOutputURL' instead." #-}

-- | The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CommandPlugin (Lude.Maybe Lude.Text)
cpName = Lens.lens (name :: CommandPlugin -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CommandPlugin)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the S3 bucket region.
--
-- /Note:/ Consider using 'outputS3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOutputS3Region :: Lens.Lens' CommandPlugin (Lude.Maybe Lude.Text)
cpOutputS3Region = Lens.lens (outputS3Region :: CommandPlugin -> Lude.Maybe Lude.Text) (\s a -> s {outputS3Region = a} :: CommandPlugin)
{-# DEPRECATED cpOutputS3Region "Use generic-lens or generic-optics with 'outputS3Region' instead." #-}

-- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response:
--
-- doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript
-- doc-example-bucket is the name of the S3 bucket;
-- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
-- i-02573cafcfEXAMPLE is the instance ID;
-- awsrunShellScript is the name of the plugin.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOutputS3BucketName :: Lens.Lens' CommandPlugin (Lude.Maybe Lude.Text)
cpOutputS3BucketName = Lens.lens (outputS3BucketName :: CommandPlugin -> Lude.Maybe Lude.Text) (\s a -> s {outputS3BucketName = a} :: CommandPlugin)
{-# DEPRECATED cpOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

-- | The time the plugin stopped running. Could stop prematurely if, for example, a cancel command was sent.
--
-- /Note:/ Consider using 'responseFinishDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpResponseFinishDateTime :: Lens.Lens' CommandPlugin (Lude.Maybe Lude.Timestamp)
cpResponseFinishDateTime = Lens.lens (responseFinishDateTime :: CommandPlugin -> Lude.Maybe Lude.Timestamp) (\s a -> s {responseFinishDateTime = a} :: CommandPlugin)
{-# DEPRECATED cpResponseFinishDateTime "Use generic-lens or generic-optics with 'responseFinishDateTime' instead." #-}

instance Lude.FromJSON CommandPlugin where
  parseJSON =
    Lude.withObject
      "CommandPlugin"
      ( \x ->
          CommandPlugin'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ResponseStartDateTime")
            Lude.<*> (x Lude..:? "OutputS3KeyPrefix")
            Lude.<*> (x Lude..:? "StandardErrorUrl")
            Lude.<*> (x Lude..:? "ResponseCode")
            Lude.<*> (x Lude..:? "StatusDetails")
            Lude.<*> (x Lude..:? "Output")
            Lude.<*> (x Lude..:? "StandardOutputUrl")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "OutputS3Region")
            Lude.<*> (x Lude..:? "OutputS3BucketName")
            Lude.<*> (x Lude..:? "ResponseFinishDateTime")
      )
