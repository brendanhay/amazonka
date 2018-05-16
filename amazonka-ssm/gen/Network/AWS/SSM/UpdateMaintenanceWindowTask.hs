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
-- Module      : Network.AWS.SSM.UpdateMaintenanceWindowTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a task assigned to a Maintenance Window. You can't change the task type, but you can change the following values:
--
--
--     * TaskARN. For example, you can change a RUN_COMMAND task from AWS-RunPowerShellScript to AWS-RunShellScript.
--
--     * ServiceRoleArn
--
--     * TaskInvocationParameters
--
--     * Priority
--
--     * MaxConcurrency
--
--     * MaxErrors
--
--
--
-- If a parameter is null, then the corresponding field is not modified. Also, if you set Replace to true, then all fields required by the 'RegisterTaskWithMaintenanceWindow' action are required for this request. Optional fields that aren't specified are set to null.
--
module Network.AWS.SSM.UpdateMaintenanceWindowTask
    (
    -- * Creating a Request
      updateMaintenanceWindowTask
    , UpdateMaintenanceWindowTask
    -- * Request Lenses
    , umwtServiceRoleARN
    , umwtReplace
    , umwtTaskParameters
    , umwtPriority
    , umwtTaskARN
    , umwtMaxErrors
    , umwtTaskInvocationParameters
    , umwtName
    , umwtTargets
    , umwtLoggingInfo
    , umwtDescription
    , umwtMaxConcurrency
    , umwtWindowId
    , umwtWindowTaskId

    -- * Destructuring the Response
    , updateMaintenanceWindowTaskResponse
    , UpdateMaintenanceWindowTaskResponse
    -- * Response Lenses
    , umwtrsServiceRoleARN
    , umwtrsWindowTaskId
    , umwtrsTaskParameters
    , umwtrsPriority
    , umwtrsTaskARN
    , umwtrsMaxErrors
    , umwtrsTaskInvocationParameters
    , umwtrsName
    , umwtrsTargets
    , umwtrsLoggingInfo
    , umwtrsDescription
    , umwtrsMaxConcurrency
    , umwtrsWindowId
    , umwtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'updateMaintenanceWindowTask' smart constructor.
data UpdateMaintenanceWindowTask = UpdateMaintenanceWindowTask'
  { _umwtServiceRoleARN :: !(Maybe Text)
  , _umwtReplace :: !(Maybe Bool)
  , _umwtTaskParameters :: !(Maybe (Sensitive (Map Text (Sensitive MaintenanceWindowTaskParameterValueExpression))))
  , _umwtPriority :: !(Maybe Nat)
  , _umwtTaskARN :: !(Maybe Text)
  , _umwtMaxErrors :: !(Maybe Text)
  , _umwtTaskInvocationParameters :: !(Maybe MaintenanceWindowTaskInvocationParameters)
  , _umwtName :: !(Maybe Text)
  , _umwtTargets :: !(Maybe [Target])
  , _umwtLoggingInfo :: !(Maybe LoggingInfo)
  , _umwtDescription :: !(Maybe (Sensitive Text))
  , _umwtMaxConcurrency :: !(Maybe Text)
  , _umwtWindowId :: !Text
  , _umwtWindowTaskId :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateMaintenanceWindowTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umwtServiceRoleARN' - The IAM service role ARN to modify. The system assumes this role during task execution.
--
-- * 'umwtReplace' - If True, then all fields that are required by the RegisterTaskWithMaintenanceWndow action are also required for this API request. Optional fields that are not specified are set to null.
--
-- * 'umwtTaskParameters' - The parameters to modify. The map has the following format: Key: string, between 1 and 255 characters Value: an array of strings, each string is between 1 and 255 characters
--
-- * 'umwtPriority' - The new task priority to specify. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
--
-- * 'umwtTaskARN' - The task ARN to modify.
--
-- * 'umwtMaxErrors' - The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number of errors that are allowed before the task stops being scheduled.
--
-- * 'umwtTaskInvocationParameters' - The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
--
-- * 'umwtName' - The new task name to specify.
--
-- * 'umwtTargets' - The targets (either instances or tags) to modify. Instances are specified using Key=instanceids,Values=instanceID_1,instanceID_2. Tags are specified using Key=tag_name,Values=tag_value.
--
-- * 'umwtLoggingInfo' - The new logging location in Amazon S3 to specify.
--
-- * 'umwtDescription' - The new task description to specify.
--
-- * 'umwtMaxConcurrency' - The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is the number of targets that are allowed to run this task in parallel.
--
-- * 'umwtWindowId' - The Maintenance Window ID that contains the task to modify.
--
-- * 'umwtWindowTaskId' - The task ID to modify.
updateMaintenanceWindowTask
    :: Text -- ^ 'umwtWindowId'
    -> Text -- ^ 'umwtWindowTaskId'
    -> UpdateMaintenanceWindowTask
updateMaintenanceWindowTask pWindowId_ pWindowTaskId_ =
  UpdateMaintenanceWindowTask'
    { _umwtServiceRoleARN = Nothing
    , _umwtReplace = Nothing
    , _umwtTaskParameters = Nothing
    , _umwtPriority = Nothing
    , _umwtTaskARN = Nothing
    , _umwtMaxErrors = Nothing
    , _umwtTaskInvocationParameters = Nothing
    , _umwtName = Nothing
    , _umwtTargets = Nothing
    , _umwtLoggingInfo = Nothing
    , _umwtDescription = Nothing
    , _umwtMaxConcurrency = Nothing
    , _umwtWindowId = pWindowId_
    , _umwtWindowTaskId = pWindowTaskId_
    }


-- | The IAM service role ARN to modify. The system assumes this role during task execution.
umwtServiceRoleARN :: Lens' UpdateMaintenanceWindowTask (Maybe Text)
umwtServiceRoleARN = lens _umwtServiceRoleARN (\ s a -> s{_umwtServiceRoleARN = a})

-- | If True, then all fields that are required by the RegisterTaskWithMaintenanceWndow action are also required for this API request. Optional fields that are not specified are set to null.
umwtReplace :: Lens' UpdateMaintenanceWindowTask (Maybe Bool)
umwtReplace = lens _umwtReplace (\ s a -> s{_umwtReplace = a})

-- | The parameters to modify. The map has the following format: Key: string, between 1 and 255 characters Value: an array of strings, each string is between 1 and 255 characters
umwtTaskParameters :: Lens' UpdateMaintenanceWindowTask (Maybe (HashMap Text MaintenanceWindowTaskParameterValueExpression))
umwtTaskParameters = lens _umwtTaskParameters (\ s a -> s{_umwtTaskParameters = a}) . mapping (_Sensitive . _Map)

-- | The new task priority to specify. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
umwtPriority :: Lens' UpdateMaintenanceWindowTask (Maybe Natural)
umwtPriority = lens _umwtPriority (\ s a -> s{_umwtPriority = a}) . mapping _Nat

-- | The task ARN to modify.
umwtTaskARN :: Lens' UpdateMaintenanceWindowTask (Maybe Text)
umwtTaskARN = lens _umwtTaskARN (\ s a -> s{_umwtTaskARN = a})

-- | The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number of errors that are allowed before the task stops being scheduled.
umwtMaxErrors :: Lens' UpdateMaintenanceWindowTask (Maybe Text)
umwtMaxErrors = lens _umwtMaxErrors (\ s a -> s{_umwtMaxErrors = a})

-- | The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
umwtTaskInvocationParameters :: Lens' UpdateMaintenanceWindowTask (Maybe MaintenanceWindowTaskInvocationParameters)
umwtTaskInvocationParameters = lens _umwtTaskInvocationParameters (\ s a -> s{_umwtTaskInvocationParameters = a})

-- | The new task name to specify.
umwtName :: Lens' UpdateMaintenanceWindowTask (Maybe Text)
umwtName = lens _umwtName (\ s a -> s{_umwtName = a})

-- | The targets (either instances or tags) to modify. Instances are specified using Key=instanceids,Values=instanceID_1,instanceID_2. Tags are specified using Key=tag_name,Values=tag_value.
umwtTargets :: Lens' UpdateMaintenanceWindowTask [Target]
umwtTargets = lens _umwtTargets (\ s a -> s{_umwtTargets = a}) . _Default . _Coerce

-- | The new logging location in Amazon S3 to specify.
umwtLoggingInfo :: Lens' UpdateMaintenanceWindowTask (Maybe LoggingInfo)
umwtLoggingInfo = lens _umwtLoggingInfo (\ s a -> s{_umwtLoggingInfo = a})

-- | The new task description to specify.
umwtDescription :: Lens' UpdateMaintenanceWindowTask (Maybe Text)
umwtDescription = lens _umwtDescription (\ s a -> s{_umwtDescription = a}) . mapping _Sensitive

-- | The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is the number of targets that are allowed to run this task in parallel.
umwtMaxConcurrency :: Lens' UpdateMaintenanceWindowTask (Maybe Text)
umwtMaxConcurrency = lens _umwtMaxConcurrency (\ s a -> s{_umwtMaxConcurrency = a})

-- | The Maintenance Window ID that contains the task to modify.
umwtWindowId :: Lens' UpdateMaintenanceWindowTask Text
umwtWindowId = lens _umwtWindowId (\ s a -> s{_umwtWindowId = a})

-- | The task ID to modify.
umwtWindowTaskId :: Lens' UpdateMaintenanceWindowTask Text
umwtWindowTaskId = lens _umwtWindowTaskId (\ s a -> s{_umwtWindowTaskId = a})

instance AWSRequest UpdateMaintenanceWindowTask where
        type Rs UpdateMaintenanceWindowTask =
             UpdateMaintenanceWindowTaskResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateMaintenanceWindowTaskResponse' <$>
                   (x .?> "ServiceRoleArn") <*> (x .?> "WindowTaskId")
                     <*> (x .?> "TaskParameters" .!@ mempty)
                     <*> (x .?> "Priority")
                     <*> (x .?> "TaskArn")
                     <*> (x .?> "MaxErrors")
                     <*> (x .?> "TaskInvocationParameters")
                     <*> (x .?> "Name")
                     <*> (x .?> "Targets" .!@ mempty)
                     <*> (x .?> "LoggingInfo")
                     <*> (x .?> "Description")
                     <*> (x .?> "MaxConcurrency")
                     <*> (x .?> "WindowId")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateMaintenanceWindowTask where

instance NFData UpdateMaintenanceWindowTask where

instance ToHeaders UpdateMaintenanceWindowTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.UpdateMaintenanceWindowTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateMaintenanceWindowTask where
        toJSON UpdateMaintenanceWindowTask'{..}
          = object
              (catMaybes
                 [("ServiceRoleArn" .=) <$> _umwtServiceRoleARN,
                  ("Replace" .=) <$> _umwtReplace,
                  ("TaskParameters" .=) <$> _umwtTaskParameters,
                  ("Priority" .=) <$> _umwtPriority,
                  ("TaskArn" .=) <$> _umwtTaskARN,
                  ("MaxErrors" .=) <$> _umwtMaxErrors,
                  ("TaskInvocationParameters" .=) <$>
                    _umwtTaskInvocationParameters,
                  ("Name" .=) <$> _umwtName,
                  ("Targets" .=) <$> _umwtTargets,
                  ("LoggingInfo" .=) <$> _umwtLoggingInfo,
                  ("Description" .=) <$> _umwtDescription,
                  ("MaxConcurrency" .=) <$> _umwtMaxConcurrency,
                  Just ("WindowId" .= _umwtWindowId),
                  Just ("WindowTaskId" .= _umwtWindowTaskId)])

instance ToPath UpdateMaintenanceWindowTask where
        toPath = const "/"

instance ToQuery UpdateMaintenanceWindowTask where
        toQuery = const mempty

-- | /See:/ 'updateMaintenanceWindowTaskResponse' smart constructor.
data UpdateMaintenanceWindowTaskResponse = UpdateMaintenanceWindowTaskResponse'
  { _umwtrsServiceRoleARN :: !(Maybe Text)
  , _umwtrsWindowTaskId :: !(Maybe Text)
  , _umwtrsTaskParameters :: !(Maybe (Sensitive (Map Text (Sensitive MaintenanceWindowTaskParameterValueExpression))))
  , _umwtrsPriority :: !(Maybe Nat)
  , _umwtrsTaskARN :: !(Maybe Text)
  , _umwtrsMaxErrors :: !(Maybe Text)
  , _umwtrsTaskInvocationParameters :: !(Maybe MaintenanceWindowTaskInvocationParameters)
  , _umwtrsName :: !(Maybe Text)
  , _umwtrsTargets :: !(Maybe [Target])
  , _umwtrsLoggingInfo :: !(Maybe LoggingInfo)
  , _umwtrsDescription :: !(Maybe (Sensitive Text))
  , _umwtrsMaxConcurrency :: !(Maybe Text)
  , _umwtrsWindowId :: !(Maybe Text)
  , _umwtrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateMaintenanceWindowTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umwtrsServiceRoleARN' - The updated service role ARN value.
--
-- * 'umwtrsWindowTaskId' - The task ID of the Maintenance Window that was updated.
--
-- * 'umwtrsTaskParameters' - The updated parameter values.
--
-- * 'umwtrsPriority' - The updated priority value.
--
-- * 'umwtrsTaskARN' - The updated task ARN value.
--
-- * 'umwtrsMaxErrors' - The updated MaxErrors value.
--
-- * 'umwtrsTaskInvocationParameters' - The updated parameter values.
--
-- * 'umwtrsName' - The updated task name.
--
-- * 'umwtrsTargets' - The updated target values.
--
-- * 'umwtrsLoggingInfo' - The updated logging information in Amazon S3.
--
-- * 'umwtrsDescription' - The updated task description.
--
-- * 'umwtrsMaxConcurrency' - The updated MaxConcurrency value.
--
-- * 'umwtrsWindowId' - The ID of the Maintenance Window that was updated.
--
-- * 'umwtrsResponseStatus' - -- | The response status code.
updateMaintenanceWindowTaskResponse
    :: Int -- ^ 'umwtrsResponseStatus'
    -> UpdateMaintenanceWindowTaskResponse
updateMaintenanceWindowTaskResponse pResponseStatus_ =
  UpdateMaintenanceWindowTaskResponse'
    { _umwtrsServiceRoleARN = Nothing
    , _umwtrsWindowTaskId = Nothing
    , _umwtrsTaskParameters = Nothing
    , _umwtrsPriority = Nothing
    , _umwtrsTaskARN = Nothing
    , _umwtrsMaxErrors = Nothing
    , _umwtrsTaskInvocationParameters = Nothing
    , _umwtrsName = Nothing
    , _umwtrsTargets = Nothing
    , _umwtrsLoggingInfo = Nothing
    , _umwtrsDescription = Nothing
    , _umwtrsMaxConcurrency = Nothing
    , _umwtrsWindowId = Nothing
    , _umwtrsResponseStatus = pResponseStatus_
    }


-- | The updated service role ARN value.
umwtrsServiceRoleARN :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe Text)
umwtrsServiceRoleARN = lens _umwtrsServiceRoleARN (\ s a -> s{_umwtrsServiceRoleARN = a})

-- | The task ID of the Maintenance Window that was updated.
umwtrsWindowTaskId :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe Text)
umwtrsWindowTaskId = lens _umwtrsWindowTaskId (\ s a -> s{_umwtrsWindowTaskId = a})

-- | The updated parameter values.
umwtrsTaskParameters :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe (HashMap Text MaintenanceWindowTaskParameterValueExpression))
umwtrsTaskParameters = lens _umwtrsTaskParameters (\ s a -> s{_umwtrsTaskParameters = a}) . mapping (_Sensitive . _Map)

-- | The updated priority value.
umwtrsPriority :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe Natural)
umwtrsPriority = lens _umwtrsPriority (\ s a -> s{_umwtrsPriority = a}) . mapping _Nat

-- | The updated task ARN value.
umwtrsTaskARN :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe Text)
umwtrsTaskARN = lens _umwtrsTaskARN (\ s a -> s{_umwtrsTaskARN = a})

-- | The updated MaxErrors value.
umwtrsMaxErrors :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe Text)
umwtrsMaxErrors = lens _umwtrsMaxErrors (\ s a -> s{_umwtrsMaxErrors = a})

-- | The updated parameter values.
umwtrsTaskInvocationParameters :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe MaintenanceWindowTaskInvocationParameters)
umwtrsTaskInvocationParameters = lens _umwtrsTaskInvocationParameters (\ s a -> s{_umwtrsTaskInvocationParameters = a})

-- | The updated task name.
umwtrsName :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe Text)
umwtrsName = lens _umwtrsName (\ s a -> s{_umwtrsName = a})

-- | The updated target values.
umwtrsTargets :: Lens' UpdateMaintenanceWindowTaskResponse [Target]
umwtrsTargets = lens _umwtrsTargets (\ s a -> s{_umwtrsTargets = a}) . _Default . _Coerce

-- | The updated logging information in Amazon S3.
umwtrsLoggingInfo :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe LoggingInfo)
umwtrsLoggingInfo = lens _umwtrsLoggingInfo (\ s a -> s{_umwtrsLoggingInfo = a})

-- | The updated task description.
umwtrsDescription :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe Text)
umwtrsDescription = lens _umwtrsDescription (\ s a -> s{_umwtrsDescription = a}) . mapping _Sensitive

-- | The updated MaxConcurrency value.
umwtrsMaxConcurrency :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe Text)
umwtrsMaxConcurrency = lens _umwtrsMaxConcurrency (\ s a -> s{_umwtrsMaxConcurrency = a})

-- | The ID of the Maintenance Window that was updated.
umwtrsWindowId :: Lens' UpdateMaintenanceWindowTaskResponse (Maybe Text)
umwtrsWindowId = lens _umwtrsWindowId (\ s a -> s{_umwtrsWindowId = a})

-- | -- | The response status code.
umwtrsResponseStatus :: Lens' UpdateMaintenanceWindowTaskResponse Int
umwtrsResponseStatus = lens _umwtrsResponseStatus (\ s a -> s{_umwtrsResponseStatus = a})

instance NFData UpdateMaintenanceWindowTaskResponse
         where
