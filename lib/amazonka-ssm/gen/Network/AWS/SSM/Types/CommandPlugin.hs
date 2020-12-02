{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandPlugin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandPlugin where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.CommandPluginStatus

-- | Describes plugin details.
--
--
--
-- /See:/ 'commandPlugin' smart constructor.
data CommandPlugin = CommandPlugin'
  { _cpStatus ::
      !(Maybe CommandPluginStatus),
    _cpResponseStartDateTime :: !(Maybe POSIX),
    _cpOutputS3KeyPrefix :: !(Maybe Text),
    _cpStandardErrorURL :: !(Maybe Text),
    _cpResponseCode :: !(Maybe Int),
    _cpStatusDetails :: !(Maybe Text),
    _cpOutput :: !(Maybe Text),
    _cpStandardOutputURL :: !(Maybe Text),
    _cpName :: !(Maybe Text),
    _cpOutputS3Region :: !(Maybe Text),
    _cpOutputS3BucketName :: !(Maybe Text),
    _cpResponseFinishDateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CommandPlugin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpStatus' - The status of this plugin. You can run a document with multiple plugins.
--
-- * 'cpResponseStartDateTime' - The time the plugin started running.
--
-- * 'cpOutputS3KeyPrefix' - The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript  doc-example-bucket is the name of the S3 bucket; ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix; i-02573cafcfEXAMPLE is the instance ID; awsrunShellScript is the name of the plugin.
--
-- * 'cpStandardErrorURL' - The URL for the complete text written by the plugin to stderr. If execution is not yet complete, then this string is empty.
--
-- * 'cpResponseCode' - A numeric response code generated after running the plugin.
--
-- * 'cpStatusDetails' - A detailed status of the plugin execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:     * Pending: The command has not been sent to the instance.     * In Progress: The command has been sent to the instance but has not reached a terminal state.     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Undeliverable: The command can't be delivered to the instance. The instance might not exist, or it might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit, and they don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
--
-- * 'cpOutput' - Output of the plugin execution.
--
-- * 'cpStandardOutputURL' - The URL for the complete text written by the plugin to stdout in Amazon S3. If the S3 bucket for the command was not specified, then this string is empty.
--
-- * 'cpName' - The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
--
-- * 'cpOutputS3Region' - (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the S3 bucket region.
--
-- * 'cpOutputS3BucketName' - The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript  doc-example-bucket is the name of the S3 bucket; ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix; i-02573cafcfEXAMPLE is the instance ID; awsrunShellScript is the name of the plugin.
--
-- * 'cpResponseFinishDateTime' - The time the plugin stopped running. Could stop prematurely if, for example, a cancel command was sent.
commandPlugin ::
  CommandPlugin
commandPlugin =
  CommandPlugin'
    { _cpStatus = Nothing,
      _cpResponseStartDateTime = Nothing,
      _cpOutputS3KeyPrefix = Nothing,
      _cpStandardErrorURL = Nothing,
      _cpResponseCode = Nothing,
      _cpStatusDetails = Nothing,
      _cpOutput = Nothing,
      _cpStandardOutputURL = Nothing,
      _cpName = Nothing,
      _cpOutputS3Region = Nothing,
      _cpOutputS3BucketName = Nothing,
      _cpResponseFinishDateTime = Nothing
    }

-- | The status of this plugin. You can run a document with multiple plugins.
cpStatus :: Lens' CommandPlugin (Maybe CommandPluginStatus)
cpStatus = lens _cpStatus (\s a -> s {_cpStatus = a})

-- | The time the plugin started running.
cpResponseStartDateTime :: Lens' CommandPlugin (Maybe UTCTime)
cpResponseStartDateTime = lens _cpResponseStartDateTime (\s a -> s {_cpResponseStartDateTime = a}) . mapping _Time

-- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript  doc-example-bucket is the name of the S3 bucket; ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix; i-02573cafcfEXAMPLE is the instance ID; awsrunShellScript is the name of the plugin.
cpOutputS3KeyPrefix :: Lens' CommandPlugin (Maybe Text)
cpOutputS3KeyPrefix = lens _cpOutputS3KeyPrefix (\s a -> s {_cpOutputS3KeyPrefix = a})

-- | The URL for the complete text written by the plugin to stderr. If execution is not yet complete, then this string is empty.
cpStandardErrorURL :: Lens' CommandPlugin (Maybe Text)
cpStandardErrorURL = lens _cpStandardErrorURL (\s a -> s {_cpStandardErrorURL = a})

-- | A numeric response code generated after running the plugin.
cpResponseCode :: Lens' CommandPlugin (Maybe Int)
cpResponseCode = lens _cpResponseCode (\s a -> s {_cpResponseCode = a})

-- | A detailed status of the plugin execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:     * Pending: The command has not been sent to the instance.     * In Progress: The command has been sent to the instance but has not reached a terminal state.     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Undeliverable: The command can't be delivered to the instance. The instance might not exist, or it might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit, and they don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
cpStatusDetails :: Lens' CommandPlugin (Maybe Text)
cpStatusDetails = lens _cpStatusDetails (\s a -> s {_cpStatusDetails = a})

-- | Output of the plugin execution.
cpOutput :: Lens' CommandPlugin (Maybe Text)
cpOutput = lens _cpOutput (\s a -> s {_cpOutput = a})

-- | The URL for the complete text written by the plugin to stdout in Amazon S3. If the S3 bucket for the command was not specified, then this string is empty.
cpStandardOutputURL :: Lens' CommandPlugin (Maybe Text)
cpStandardOutputURL = lens _cpStandardOutputURL (\s a -> s {_cpStandardOutputURL = a})

-- | The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
cpName :: Lens' CommandPlugin (Maybe Text)
cpName = lens _cpName (\s a -> s {_cpName = a})

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the S3 bucket region.
cpOutputS3Region :: Lens' CommandPlugin (Maybe Text)
cpOutputS3Region = lens _cpOutputS3Region (\s a -> s {_cpOutputS3Region = a})

-- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: doc-example-bucket/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-02573cafcfEXAMPLE/awsrunShellScript  doc-example-bucket is the name of the S3 bucket; ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix; i-02573cafcfEXAMPLE is the instance ID; awsrunShellScript is the name of the plugin.
cpOutputS3BucketName :: Lens' CommandPlugin (Maybe Text)
cpOutputS3BucketName = lens _cpOutputS3BucketName (\s a -> s {_cpOutputS3BucketName = a})

-- | The time the plugin stopped running. Could stop prematurely if, for example, a cancel command was sent.
cpResponseFinishDateTime :: Lens' CommandPlugin (Maybe UTCTime)
cpResponseFinishDateTime = lens _cpResponseFinishDateTime (\s a -> s {_cpResponseFinishDateTime = a}) . mapping _Time

instance FromJSON CommandPlugin where
  parseJSON =
    withObject
      "CommandPlugin"
      ( \x ->
          CommandPlugin'
            <$> (x .:? "Status")
            <*> (x .:? "ResponseStartDateTime")
            <*> (x .:? "OutputS3KeyPrefix")
            <*> (x .:? "StandardErrorUrl")
            <*> (x .:? "ResponseCode")
            <*> (x .:? "StatusDetails")
            <*> (x .:? "Output")
            <*> (x .:? "StandardOutputUrl")
            <*> (x .:? "Name")
            <*> (x .:? "OutputS3Region")
            <*> (x .:? "OutputS3BucketName")
            <*> (x .:? "ResponseFinishDateTime")
      )

instance Hashable CommandPlugin

instance NFData CommandPlugin
