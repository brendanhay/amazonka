{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetCommandInvocation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about command execution for an invocation or plugin.
--
--
module Network.AWS.SSM.GetCommandInvocation
    (
    -- * Creating a Request
      getCommandInvocation
    , GetCommandInvocation
    -- * Request Lenses
    , gciPluginName
    , gciCommandId
    , gciInstanceId

    -- * Destructuring the Response
    , getCommandInvocationResponse
    , GetCommandInvocationResponse
    -- * Response Lenses
    , gcirsInstanceId
    , gcirsStatus
    , gcirsStandardErrorContent
    , gcirsExecutionElapsedTime
    , gcirsDocumentName
    , gcirsStandardErrorURL
    , gcirsExecutionStartDateTime
    , gcirsResponseCode
    , gcirsStatusDetails
    , gcirsExecutionEndDateTime
    , gcirsStandardOutputURL
    , gcirsCommandId
    , gcirsDocumentVersion
    , gcirsStandardOutputContent
    , gcirsComment
    , gcirsPluginName
    , gcirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getCommandInvocation' smart constructor.
data GetCommandInvocation = GetCommandInvocation'
  { _gciPluginName :: !(Maybe Text)
  , _gciCommandId  :: !Text
  , _gciInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCommandInvocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gciPluginName' - (Optional) The name of the plugin for which you want detailed results. If the document contains only one plugin, the name can be omitted and the details will be returned.
--
-- * 'gciCommandId' - (Required) The parent command ID of the invocation plugin.
--
-- * 'gciInstanceId' - (Required) The ID of the managed instance targeted by the command. A managed instance can be an Amazon EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
getCommandInvocation
    :: Text -- ^ 'gciCommandId'
    -> Text -- ^ 'gciInstanceId'
    -> GetCommandInvocation
getCommandInvocation pCommandId_ pInstanceId_ =
  GetCommandInvocation'
    { _gciPluginName = Nothing
    , _gciCommandId = pCommandId_
    , _gciInstanceId = pInstanceId_
    }


-- | (Optional) The name of the plugin for which you want detailed results. If the document contains only one plugin, the name can be omitted and the details will be returned.
gciPluginName :: Lens' GetCommandInvocation (Maybe Text)
gciPluginName = lens _gciPluginName (\ s a -> s{_gciPluginName = a})

-- | (Required) The parent command ID of the invocation plugin.
gciCommandId :: Lens' GetCommandInvocation Text
gciCommandId = lens _gciCommandId (\ s a -> s{_gciCommandId = a})

-- | (Required) The ID of the managed instance targeted by the command. A managed instance can be an Amazon EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
gciInstanceId :: Lens' GetCommandInvocation Text
gciInstanceId = lens _gciInstanceId (\ s a -> s{_gciInstanceId = a})

instance AWSRequest GetCommandInvocation where
        type Rs GetCommandInvocation =
             GetCommandInvocationResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetCommandInvocationResponse' <$>
                   (x .?> "InstanceId") <*> (x .?> "Status") <*>
                     (x .?> "StandardErrorContent")
                     <*> (x .?> "ExecutionElapsedTime")
                     <*> (x .?> "DocumentName")
                     <*> (x .?> "StandardErrorUrl")
                     <*> (x .?> "ExecutionStartDateTime")
                     <*> (x .?> "ResponseCode")
                     <*> (x .?> "StatusDetails")
                     <*> (x .?> "ExecutionEndDateTime")
                     <*> (x .?> "StandardOutputUrl")
                     <*> (x .?> "CommandId")
                     <*> (x .?> "DocumentVersion")
                     <*> (x .?> "StandardOutputContent")
                     <*> (x .?> "Comment")
                     <*> (x .?> "PluginName")
                     <*> (pure (fromEnum s)))

instance Hashable GetCommandInvocation where

instance NFData GetCommandInvocation where

instance ToHeaders GetCommandInvocation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetCommandInvocation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCommandInvocation where
        toJSON GetCommandInvocation'{..}
          = object
              (catMaybes
                 [("PluginName" .=) <$> _gciPluginName,
                  Just ("CommandId" .= _gciCommandId),
                  Just ("InstanceId" .= _gciInstanceId)])

instance ToPath GetCommandInvocation where
        toPath = const "/"

instance ToQuery GetCommandInvocation where
        toQuery = const mempty

-- | /See:/ 'getCommandInvocationResponse' smart constructor.
data GetCommandInvocationResponse = GetCommandInvocationResponse'
  { _gcirsInstanceId             :: !(Maybe Text)
  , _gcirsStatus                 :: !(Maybe CommandInvocationStatus)
  , _gcirsStandardErrorContent   :: !(Maybe Text)
  , _gcirsExecutionElapsedTime   :: !(Maybe Text)
  , _gcirsDocumentName           :: !(Maybe Text)
  , _gcirsStandardErrorURL       :: !(Maybe Text)
  , _gcirsExecutionStartDateTime :: !(Maybe Text)
  , _gcirsResponseCode           :: !(Maybe Int)
  , _gcirsStatusDetails          :: !(Maybe Text)
  , _gcirsExecutionEndDateTime   :: !(Maybe Text)
  , _gcirsStandardOutputURL      :: !(Maybe Text)
  , _gcirsCommandId              :: !(Maybe Text)
  , _gcirsDocumentVersion        :: !(Maybe Text)
  , _gcirsStandardOutputContent  :: !(Maybe Text)
  , _gcirsComment                :: !(Maybe Text)
  , _gcirsPluginName             :: !(Maybe Text)
  , _gcirsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCommandInvocationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcirsInstanceId' - The ID of the managed instance targeted by the command. A managed instance can be an Amazon EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
--
-- * 'gcirsStatus' - The status of this invocation plugin. This status can be different than StatusDetails.
--
-- * 'gcirsStandardErrorContent' - The first 8,000 characters written by the plugin to stderr. If the command has not finished executing, then this string is empty.
--
-- * 'gcirsExecutionElapsedTime' - Duration since ExecutionStartDateTime.
--
-- * 'gcirsDocumentName' - The name of the document that was executed. For example, AWS-RunShellScript.
--
-- * 'gcirsStandardErrorURL' - The URL for the complete text written by the plugin to stderr. If the command has not finished executing, then this string is empty.
--
-- * 'gcirsExecutionStartDateTime' - The date and time the plugin started executing. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedBefore@ filter. @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@  If the plugin has not started to execute, the string is empty.
--
-- * 'gcirsResponseCode' - The error level response code for the plugin script. If the response code is -1, then the command has not started executing on the instance, or it was not received by the instance.
--
-- * 'gcirsStatusDetails' - A detailed status of the command execution for an invocation. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-about-status.html Run Command Status> . StatusDetails can be one of the following values:     * Pending: The command has not been sent to the instance.     * In Progress: The command has been sent to the instance but has not reached a terminal state.     * Delayed: The system attempted to send the command to the target, but the target was not available. The instance might not be available because of network issues, the instance was stopped, etc. The system will try to deliver the command again.     * Success: The command or plugin was executed successfully. This is a terminal state.     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Execution Timed Out: The command started to execute on the instance, but the execution was not complete before the timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.     * Failed: The command wasn't executed successfully on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Undeliverable: The command can't be delivered to the instance. The instance might not exist or might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit and don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
--
-- * 'gcirsExecutionEndDateTime' - The date and time the plugin was finished executing. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedAfter@ filter. @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@  If the plugin has not started to execute, the string is empty.
--
-- * 'gcirsStandardOutputURL' - The URL for the complete text written by the plugin to stdout in Amazon S3. If an Amazon S3 bucket was not specified, then this string is empty.
--
-- * 'gcirsCommandId' - The parent command ID of the invocation plugin.
--
-- * 'gcirsDocumentVersion' - The SSM document version used in the request.
--
-- * 'gcirsStandardOutputContent' - The first 24,000 characters written by the plugin to stdout. If the command has not finished executing, if ExecutionStatus is neither Succeeded nor Failed, then this string is empty.
--
-- * 'gcirsComment' - The comment text for the command.
--
-- * 'gcirsPluginName' - The name of the plugin for which you want detailed results. For example, aws:RunShellScript is a plugin.
--
-- * 'gcirsResponseStatus' - -- | The response status code.
getCommandInvocationResponse
    :: Int -- ^ 'gcirsResponseStatus'
    -> GetCommandInvocationResponse
getCommandInvocationResponse pResponseStatus_ =
  GetCommandInvocationResponse'
    { _gcirsInstanceId = Nothing
    , _gcirsStatus = Nothing
    , _gcirsStandardErrorContent = Nothing
    , _gcirsExecutionElapsedTime = Nothing
    , _gcirsDocumentName = Nothing
    , _gcirsStandardErrorURL = Nothing
    , _gcirsExecutionStartDateTime = Nothing
    , _gcirsResponseCode = Nothing
    , _gcirsStatusDetails = Nothing
    , _gcirsExecutionEndDateTime = Nothing
    , _gcirsStandardOutputURL = Nothing
    , _gcirsCommandId = Nothing
    , _gcirsDocumentVersion = Nothing
    , _gcirsStandardOutputContent = Nothing
    , _gcirsComment = Nothing
    , _gcirsPluginName = Nothing
    , _gcirsResponseStatus = pResponseStatus_
    }


-- | The ID of the managed instance targeted by the command. A managed instance can be an Amazon EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
gcirsInstanceId :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsInstanceId = lens _gcirsInstanceId (\ s a -> s{_gcirsInstanceId = a})

-- | The status of this invocation plugin. This status can be different than StatusDetails.
gcirsStatus :: Lens' GetCommandInvocationResponse (Maybe CommandInvocationStatus)
gcirsStatus = lens _gcirsStatus (\ s a -> s{_gcirsStatus = a})

-- | The first 8,000 characters written by the plugin to stderr. If the command has not finished executing, then this string is empty.
gcirsStandardErrorContent :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsStandardErrorContent = lens _gcirsStandardErrorContent (\ s a -> s{_gcirsStandardErrorContent = a})

-- | Duration since ExecutionStartDateTime.
gcirsExecutionElapsedTime :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsExecutionElapsedTime = lens _gcirsExecutionElapsedTime (\ s a -> s{_gcirsExecutionElapsedTime = a})

-- | The name of the document that was executed. For example, AWS-RunShellScript.
gcirsDocumentName :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsDocumentName = lens _gcirsDocumentName (\ s a -> s{_gcirsDocumentName = a})

-- | The URL for the complete text written by the plugin to stderr. If the command has not finished executing, then this string is empty.
gcirsStandardErrorURL :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsStandardErrorURL = lens _gcirsStandardErrorURL (\ s a -> s{_gcirsStandardErrorURL = a})

-- | The date and time the plugin started executing. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedBefore@ filter. @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@  If the plugin has not started to execute, the string is empty.
gcirsExecutionStartDateTime :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsExecutionStartDateTime = lens _gcirsExecutionStartDateTime (\ s a -> s{_gcirsExecutionStartDateTime = a})

-- | The error level response code for the plugin script. If the response code is -1, then the command has not started executing on the instance, or it was not received by the instance.
gcirsResponseCode :: Lens' GetCommandInvocationResponse (Maybe Int)
gcirsResponseCode = lens _gcirsResponseCode (\ s a -> s{_gcirsResponseCode = a})

-- | A detailed status of the command execution for an invocation. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-about-status.html Run Command Status> . StatusDetails can be one of the following values:     * Pending: The command has not been sent to the instance.     * In Progress: The command has been sent to the instance but has not reached a terminal state.     * Delayed: The system attempted to send the command to the target, but the target was not available. The instance might not be available because of network issues, the instance was stopped, etc. The system will try to deliver the command again.     * Success: The command or plugin was executed successfully. This is a terminal state.     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Execution Timed Out: The command started to execute on the instance, but the execution was not complete before the timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.     * Failed: The command wasn't executed successfully on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Undeliverable: The command can't be delivered to the instance. The instance might not exist or might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit and don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
gcirsStatusDetails :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsStatusDetails = lens _gcirsStatusDetails (\ s a -> s{_gcirsStatusDetails = a})

-- | The date and time the plugin was finished executing. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedAfter@ filter. @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@  If the plugin has not started to execute, the string is empty.
gcirsExecutionEndDateTime :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsExecutionEndDateTime = lens _gcirsExecutionEndDateTime (\ s a -> s{_gcirsExecutionEndDateTime = a})

-- | The URL for the complete text written by the plugin to stdout in Amazon S3. If an Amazon S3 bucket was not specified, then this string is empty.
gcirsStandardOutputURL :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsStandardOutputURL = lens _gcirsStandardOutputURL (\ s a -> s{_gcirsStandardOutputURL = a})

-- | The parent command ID of the invocation plugin.
gcirsCommandId :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsCommandId = lens _gcirsCommandId (\ s a -> s{_gcirsCommandId = a})

-- | The SSM document version used in the request.
gcirsDocumentVersion :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsDocumentVersion = lens _gcirsDocumentVersion (\ s a -> s{_gcirsDocumentVersion = a})

-- | The first 24,000 characters written by the plugin to stdout. If the command has not finished executing, if ExecutionStatus is neither Succeeded nor Failed, then this string is empty.
gcirsStandardOutputContent :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsStandardOutputContent = lens _gcirsStandardOutputContent (\ s a -> s{_gcirsStandardOutputContent = a})

-- | The comment text for the command.
gcirsComment :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsComment = lens _gcirsComment (\ s a -> s{_gcirsComment = a})

-- | The name of the plugin for which you want detailed results. For example, aws:RunShellScript is a plugin.
gcirsPluginName :: Lens' GetCommandInvocationResponse (Maybe Text)
gcirsPluginName = lens _gcirsPluginName (\ s a -> s{_gcirsPluginName = a})

-- | -- | The response status code.
gcirsResponseStatus :: Lens' GetCommandInvocationResponse Int
gcirsResponseStatus = lens _gcirsResponseStatus (\ s a -> s{_gcirsResponseStatus = a})

instance NFData GetCommandInvocationResponse where
