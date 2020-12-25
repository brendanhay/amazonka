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
    cpName,
    cpOutput,
    cpOutputS3BucketName,
    cpOutputS3KeyPrefix,
    cpOutputS3Region,
    cpResponseCode,
    cpResponseFinishDateTime,
    cpResponseStartDateTime,
    cpStandardErrorUrl,
    cpStandardOutputUrl,
    cpStatus,
    cpStatusDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.CommandPluginStatus as Types
import qualified Network.AWS.SSM.Types.Name as Types
import qualified Network.AWS.SSM.Types.Output as Types
import qualified Network.AWS.SSM.Types.OutputS3BucketName as Types
import qualified Network.AWS.SSM.Types.S3KeyPrefix as Types
import qualified Network.AWS.SSM.Types.S3Region as Types
import qualified Network.AWS.SSM.Types.StatusDetails as Types
import qualified Network.AWS.SSM.Types.Url as Types

-- | Describes plugin details.
--
-- /See:/ 'mkCommandPlugin' smart constructor.
data CommandPlugin = CommandPlugin'
  { -- | The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
    name :: Core.Maybe Types.Name,
    -- | Output of the plugin execution.
    output :: Core.Maybe Types.Output,
    -- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response:
    --
    -- doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript
    -- doc-example-bucket is the name of the S3 bucket;
    -- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
    -- i-02573cafcfEXAMPLE is the instance ID;
    -- awsrunShellScript is the name of the plugin.
    outputS3BucketName :: Core.Maybe Types.OutputS3BucketName,
    -- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response:
    --
    -- doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript
    -- doc-example-bucket is the name of the S3 bucket;
    -- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
    -- i-02573cafcfEXAMPLE is the instance ID;
    -- awsrunShellScript is the name of the plugin.
    outputS3KeyPrefix :: Core.Maybe Types.S3KeyPrefix,
    -- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the S3 bucket region.
    outputS3Region :: Core.Maybe Types.S3Region,
    -- | A numeric response code generated after running the plugin.
    responseCode :: Core.Maybe Core.Int,
    -- | The time the plugin stopped running. Could stop prematurely if, for example, a cancel command was sent.
    responseFinishDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time the plugin started running.
    responseStartDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The URL for the complete text written by the plugin to stderr. If execution is not yet complete, then this string is empty.
    standardErrorUrl :: Core.Maybe Types.Url,
    -- | The URL for the complete text written by the plugin to stdout in Amazon S3. If the S3 bucket for the command was not specified, then this string is empty.
    standardOutputUrl :: Core.Maybe Types.Url,
    -- | The status of this plugin. You can run a document with multiple plugins.
    status :: Core.Maybe Types.CommandPluginStatus,
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
    statusDetails :: Core.Maybe Types.StatusDetails
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CommandPlugin' value with any optional fields omitted.
mkCommandPlugin ::
  CommandPlugin
mkCommandPlugin =
  CommandPlugin'
    { name = Core.Nothing,
      output = Core.Nothing,
      outputS3BucketName = Core.Nothing,
      outputS3KeyPrefix = Core.Nothing,
      outputS3Region = Core.Nothing,
      responseCode = Core.Nothing,
      responseFinishDateTime = Core.Nothing,
      responseStartDateTime = Core.Nothing,
      standardErrorUrl = Core.Nothing,
      standardOutputUrl = Core.Nothing,
      status = Core.Nothing,
      statusDetails = Core.Nothing
    }

-- | The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CommandPlugin (Core.Maybe Types.Name)
cpName = Lens.field @"name"
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Output of the plugin execution.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOutput :: Lens.Lens' CommandPlugin (Core.Maybe Types.Output)
cpOutput = Lens.field @"output"
{-# DEPRECATED cpOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response:
--
-- doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript
-- doc-example-bucket is the name of the S3 bucket;
-- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
-- i-02573cafcfEXAMPLE is the instance ID;
-- awsrunShellScript is the name of the plugin.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOutputS3BucketName :: Lens.Lens' CommandPlugin (Core.Maybe Types.OutputS3BucketName)
cpOutputS3BucketName = Lens.field @"outputS3BucketName"
{-# DEPRECATED cpOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

-- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response:
--
-- doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript
-- doc-example-bucket is the name of the S3 bucket;
-- ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix;
-- i-02573cafcfEXAMPLE is the instance ID;
-- awsrunShellScript is the name of the plugin.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOutputS3KeyPrefix :: Lens.Lens' CommandPlugin (Core.Maybe Types.S3KeyPrefix)
cpOutputS3KeyPrefix = Lens.field @"outputS3KeyPrefix"
{-# DEPRECATED cpOutputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead." #-}

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the S3 bucket region.
--
-- /Note:/ Consider using 'outputS3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOutputS3Region :: Lens.Lens' CommandPlugin (Core.Maybe Types.S3Region)
cpOutputS3Region = Lens.field @"outputS3Region"
{-# DEPRECATED cpOutputS3Region "Use generic-lens or generic-optics with 'outputS3Region' instead." #-}

-- | A numeric response code generated after running the plugin.
--
-- /Note:/ Consider using 'responseCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpResponseCode :: Lens.Lens' CommandPlugin (Core.Maybe Core.Int)
cpResponseCode = Lens.field @"responseCode"
{-# DEPRECATED cpResponseCode "Use generic-lens or generic-optics with 'responseCode' instead." #-}

-- | The time the plugin stopped running. Could stop prematurely if, for example, a cancel command was sent.
--
-- /Note:/ Consider using 'responseFinishDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpResponseFinishDateTime :: Lens.Lens' CommandPlugin (Core.Maybe Core.NominalDiffTime)
cpResponseFinishDateTime = Lens.field @"responseFinishDateTime"
{-# DEPRECATED cpResponseFinishDateTime "Use generic-lens or generic-optics with 'responseFinishDateTime' instead." #-}

-- | The time the plugin started running.
--
-- /Note:/ Consider using 'responseStartDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpResponseStartDateTime :: Lens.Lens' CommandPlugin (Core.Maybe Core.NominalDiffTime)
cpResponseStartDateTime = Lens.field @"responseStartDateTime"
{-# DEPRECATED cpResponseStartDateTime "Use generic-lens or generic-optics with 'responseStartDateTime' instead." #-}

-- | The URL for the complete text written by the plugin to stderr. If execution is not yet complete, then this string is empty.
--
-- /Note:/ Consider using 'standardErrorUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpStandardErrorUrl :: Lens.Lens' CommandPlugin (Core.Maybe Types.Url)
cpStandardErrorUrl = Lens.field @"standardErrorUrl"
{-# DEPRECATED cpStandardErrorUrl "Use generic-lens or generic-optics with 'standardErrorUrl' instead." #-}

-- | The URL for the complete text written by the plugin to stdout in Amazon S3. If the S3 bucket for the command was not specified, then this string is empty.
--
-- /Note:/ Consider using 'standardOutputUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpStandardOutputUrl :: Lens.Lens' CommandPlugin (Core.Maybe Types.Url)
cpStandardOutputUrl = Lens.field @"standardOutputUrl"
{-# DEPRECATED cpStandardOutputUrl "Use generic-lens or generic-optics with 'standardOutputUrl' instead." #-}

-- | The status of this plugin. You can run a document with multiple plugins.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpStatus :: Lens.Lens' CommandPlugin (Core.Maybe Types.CommandPluginStatus)
cpStatus = Lens.field @"status"
{-# DEPRECATED cpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

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
cpStatusDetails :: Lens.Lens' CommandPlugin (Core.Maybe Types.StatusDetails)
cpStatusDetails = Lens.field @"statusDetails"
{-# DEPRECATED cpStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

instance Core.FromJSON CommandPlugin where
  parseJSON =
    Core.withObject "CommandPlugin" Core.$
      \x ->
        CommandPlugin'
          Core.<$> (x Core..:? "Name")
          Core.<*> (x Core..:? "Output")
          Core.<*> (x Core..:? "OutputS3BucketName")
          Core.<*> (x Core..:? "OutputS3KeyPrefix")
          Core.<*> (x Core..:? "OutputS3Region")
          Core.<*> (x Core..:? "ResponseCode")
          Core.<*> (x Core..:? "ResponseFinishDateTime")
          Core.<*> (x Core..:? "ResponseStartDateTime")
          Core.<*> (x Core..:? "StandardErrorUrl")
          Core.<*> (x Core..:? "StandardOutputUrl")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusDetails")
