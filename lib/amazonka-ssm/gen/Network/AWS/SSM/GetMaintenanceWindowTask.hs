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
-- Module      : Network.AWS.SSM.GetMaintenanceWindowTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tasks in a Maintenance Window.
--
--
module Network.AWS.SSM.GetMaintenanceWindowTask
    (
    -- * Creating a Request
      getMaintenanceWindowTask
    , GetMaintenanceWindowTask
    -- * Request Lenses
    , gmwtWindowId
    , gmwtWindowTaskId

    -- * Destructuring the Response
    , getMaintenanceWindowTaskResponse
    , GetMaintenanceWindowTaskResponse
    -- * Response Lenses
    , gmwtrsServiceRoleARN
    , gmwtrsWindowTaskId
    , gmwtrsTaskParameters
    , gmwtrsPriority
    , gmwtrsTaskType
    , gmwtrsTaskARN
    , gmwtrsMaxErrors
    , gmwtrsTaskInvocationParameters
    , gmwtrsName
    , gmwtrsTargets
    , gmwtrsLoggingInfo
    , gmwtrsDescription
    , gmwtrsMaxConcurrency
    , gmwtrsWindowId
    , gmwtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getMaintenanceWindowTask' smart constructor.
data GetMaintenanceWindowTask = GetMaintenanceWindowTask'
  { _gmwtWindowId     :: !Text
  , _gmwtWindowTaskId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMaintenanceWindowTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwtWindowId' - The Maintenance Window ID that includes the task to retrieve.
--
-- * 'gmwtWindowTaskId' - The Maintenance Window task ID to retrieve.
getMaintenanceWindowTask
    :: Text -- ^ 'gmwtWindowId'
    -> Text -- ^ 'gmwtWindowTaskId'
    -> GetMaintenanceWindowTask
getMaintenanceWindowTask pWindowId_ pWindowTaskId_ =
  GetMaintenanceWindowTask'
    {_gmwtWindowId = pWindowId_, _gmwtWindowTaskId = pWindowTaskId_}


-- | The Maintenance Window ID that includes the task to retrieve.
gmwtWindowId :: Lens' GetMaintenanceWindowTask Text
gmwtWindowId = lens _gmwtWindowId (\ s a -> s{_gmwtWindowId = a})

-- | The Maintenance Window task ID to retrieve.
gmwtWindowTaskId :: Lens' GetMaintenanceWindowTask Text
gmwtWindowTaskId = lens _gmwtWindowTaskId (\ s a -> s{_gmwtWindowTaskId = a})

instance AWSRequest GetMaintenanceWindowTask where
        type Rs GetMaintenanceWindowTask =
             GetMaintenanceWindowTaskResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetMaintenanceWindowTaskResponse' <$>
                   (x .?> "ServiceRoleArn") <*> (x .?> "WindowTaskId")
                     <*> (x .?> "TaskParameters" .!@ mempty)
                     <*> (x .?> "Priority")
                     <*> (x .?> "TaskType")
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

instance Hashable GetMaintenanceWindowTask where

instance NFData GetMaintenanceWindowTask where

instance ToHeaders GetMaintenanceWindowTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetMaintenanceWindowTask" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetMaintenanceWindowTask where
        toJSON GetMaintenanceWindowTask'{..}
          = object
              (catMaybes
                 [Just ("WindowId" .= _gmwtWindowId),
                  Just ("WindowTaskId" .= _gmwtWindowTaskId)])

instance ToPath GetMaintenanceWindowTask where
        toPath = const "/"

instance ToQuery GetMaintenanceWindowTask where
        toQuery = const mempty

-- | /See:/ 'getMaintenanceWindowTaskResponse' smart constructor.
data GetMaintenanceWindowTaskResponse = GetMaintenanceWindowTaskResponse'
  { _gmwtrsServiceRoleARN :: !(Maybe Text)
  , _gmwtrsWindowTaskId :: !(Maybe Text)
  , _gmwtrsTaskParameters :: !(Maybe (Sensitive (Map Text (Sensitive MaintenanceWindowTaskParameterValueExpression))))
  , _gmwtrsPriority :: !(Maybe Nat)
  , _gmwtrsTaskType :: !(Maybe MaintenanceWindowTaskType)
  , _gmwtrsTaskARN :: !(Maybe Text)
  , _gmwtrsMaxErrors :: !(Maybe Text)
  , _gmwtrsTaskInvocationParameters :: !(Maybe MaintenanceWindowTaskInvocationParameters)
  , _gmwtrsName :: !(Maybe Text)
  , _gmwtrsTargets :: !(Maybe [Target])
  , _gmwtrsLoggingInfo :: !(Maybe LoggingInfo)
  , _gmwtrsDescription :: !(Maybe (Sensitive Text))
  , _gmwtrsMaxConcurrency :: !(Maybe Text)
  , _gmwtrsWindowId :: !(Maybe Text)
  , _gmwtrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMaintenanceWindowTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwtrsServiceRoleARN' - The IAM service role to assume during task execution.
--
-- * 'gmwtrsWindowTaskId' - The retrieved Maintenance Window task ID.
--
-- * 'gmwtrsTaskParameters' - The parameters to pass to the task when it executes.
--
-- * 'gmwtrsPriority' - The priority of the task when it executes. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
--
-- * 'gmwtrsTaskType' - The type of task to execute.
--
-- * 'gmwtrsTaskARN' - The resource that the task used during execution. For RUN_COMMAND and AUTOMATION task types, the TaskArn is the Systems Manager Document name/ARN. For LAMBDA tasks, the value is the function name/ARN. For STEP_FUNCTION tasks, the value is the state machine ARN.
--
-- * 'gmwtrsMaxErrors' - The maximum number of errors allowed before the task stops being scheduled.
--
-- * 'gmwtrsTaskInvocationParameters' - The parameters to pass to the task when it executes.
--
-- * 'gmwtrsName' - The retrieved task name.
--
-- * 'gmwtrsTargets' - The targets where the task should execute.
--
-- * 'gmwtrsLoggingInfo' - The location in Amazon S3 where the task results are logged.
--
-- * 'gmwtrsDescription' - The retrieved task description.
--
-- * 'gmwtrsMaxConcurrency' - The maximum number of targets allowed to run this task in parallel.
--
-- * 'gmwtrsWindowId' - The retrieved Maintenance Window ID.
--
-- * 'gmwtrsResponseStatus' - -- | The response status code.
getMaintenanceWindowTaskResponse
    :: Int -- ^ 'gmwtrsResponseStatus'
    -> GetMaintenanceWindowTaskResponse
getMaintenanceWindowTaskResponse pResponseStatus_ =
  GetMaintenanceWindowTaskResponse'
    { _gmwtrsServiceRoleARN = Nothing
    , _gmwtrsWindowTaskId = Nothing
    , _gmwtrsTaskParameters = Nothing
    , _gmwtrsPriority = Nothing
    , _gmwtrsTaskType = Nothing
    , _gmwtrsTaskARN = Nothing
    , _gmwtrsMaxErrors = Nothing
    , _gmwtrsTaskInvocationParameters = Nothing
    , _gmwtrsName = Nothing
    , _gmwtrsTargets = Nothing
    , _gmwtrsLoggingInfo = Nothing
    , _gmwtrsDescription = Nothing
    , _gmwtrsMaxConcurrency = Nothing
    , _gmwtrsWindowId = Nothing
    , _gmwtrsResponseStatus = pResponseStatus_
    }


-- | The IAM service role to assume during task execution.
gmwtrsServiceRoleARN :: Lens' GetMaintenanceWindowTaskResponse (Maybe Text)
gmwtrsServiceRoleARN = lens _gmwtrsServiceRoleARN (\ s a -> s{_gmwtrsServiceRoleARN = a})

-- | The retrieved Maintenance Window task ID.
gmwtrsWindowTaskId :: Lens' GetMaintenanceWindowTaskResponse (Maybe Text)
gmwtrsWindowTaskId = lens _gmwtrsWindowTaskId (\ s a -> s{_gmwtrsWindowTaskId = a})

-- | The parameters to pass to the task when it executes.
gmwtrsTaskParameters :: Lens' GetMaintenanceWindowTaskResponse (Maybe (HashMap Text MaintenanceWindowTaskParameterValueExpression))
gmwtrsTaskParameters = lens _gmwtrsTaskParameters (\ s a -> s{_gmwtrsTaskParameters = a}) . mapping (_Sensitive . _Map)

-- | The priority of the task when it executes. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
gmwtrsPriority :: Lens' GetMaintenanceWindowTaskResponse (Maybe Natural)
gmwtrsPriority = lens _gmwtrsPriority (\ s a -> s{_gmwtrsPriority = a}) . mapping _Nat

-- | The type of task to execute.
gmwtrsTaskType :: Lens' GetMaintenanceWindowTaskResponse (Maybe MaintenanceWindowTaskType)
gmwtrsTaskType = lens _gmwtrsTaskType (\ s a -> s{_gmwtrsTaskType = a})

-- | The resource that the task used during execution. For RUN_COMMAND and AUTOMATION task types, the TaskArn is the Systems Manager Document name/ARN. For LAMBDA tasks, the value is the function name/ARN. For STEP_FUNCTION tasks, the value is the state machine ARN.
gmwtrsTaskARN :: Lens' GetMaintenanceWindowTaskResponse (Maybe Text)
gmwtrsTaskARN = lens _gmwtrsTaskARN (\ s a -> s{_gmwtrsTaskARN = a})

-- | The maximum number of errors allowed before the task stops being scheduled.
gmwtrsMaxErrors :: Lens' GetMaintenanceWindowTaskResponse (Maybe Text)
gmwtrsMaxErrors = lens _gmwtrsMaxErrors (\ s a -> s{_gmwtrsMaxErrors = a})

-- | The parameters to pass to the task when it executes.
gmwtrsTaskInvocationParameters :: Lens' GetMaintenanceWindowTaskResponse (Maybe MaintenanceWindowTaskInvocationParameters)
gmwtrsTaskInvocationParameters = lens _gmwtrsTaskInvocationParameters (\ s a -> s{_gmwtrsTaskInvocationParameters = a})

-- | The retrieved task name.
gmwtrsName :: Lens' GetMaintenanceWindowTaskResponse (Maybe Text)
gmwtrsName = lens _gmwtrsName (\ s a -> s{_gmwtrsName = a})

-- | The targets where the task should execute.
gmwtrsTargets :: Lens' GetMaintenanceWindowTaskResponse [Target]
gmwtrsTargets = lens _gmwtrsTargets (\ s a -> s{_gmwtrsTargets = a}) . _Default . _Coerce

-- | The location in Amazon S3 where the task results are logged.
gmwtrsLoggingInfo :: Lens' GetMaintenanceWindowTaskResponse (Maybe LoggingInfo)
gmwtrsLoggingInfo = lens _gmwtrsLoggingInfo (\ s a -> s{_gmwtrsLoggingInfo = a})

-- | The retrieved task description.
gmwtrsDescription :: Lens' GetMaintenanceWindowTaskResponse (Maybe Text)
gmwtrsDescription = lens _gmwtrsDescription (\ s a -> s{_gmwtrsDescription = a}) . mapping _Sensitive

-- | The maximum number of targets allowed to run this task in parallel.
gmwtrsMaxConcurrency :: Lens' GetMaintenanceWindowTaskResponse (Maybe Text)
gmwtrsMaxConcurrency = lens _gmwtrsMaxConcurrency (\ s a -> s{_gmwtrsMaxConcurrency = a})

-- | The retrieved Maintenance Window ID.
gmwtrsWindowId :: Lens' GetMaintenanceWindowTaskResponse (Maybe Text)
gmwtrsWindowId = lens _gmwtrsWindowId (\ s a -> s{_gmwtrsWindowId = a})

-- | -- | The response status code.
gmwtrsResponseStatus :: Lens' GetMaintenanceWindowTaskResponse Int
gmwtrsResponseStatus = lens _gmwtrsResponseStatus (\ s a -> s{_gmwtrsResponseStatus = a})

instance NFData GetMaintenanceWindowTaskResponse
         where
