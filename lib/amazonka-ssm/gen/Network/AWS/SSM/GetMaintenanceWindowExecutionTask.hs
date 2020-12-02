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
-- Module      : Network.AWS.SSM.GetMaintenanceWindowExecutionTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details about a specific task executed as part of a Maintenance Window execution.
--
--
module Network.AWS.SSM.GetMaintenanceWindowExecutionTask
    (
    -- * Creating a Request
      getMaintenanceWindowExecutionTask
    , GetMaintenanceWindowExecutionTask
    -- * Request Lenses
    , gmwetWindowExecutionId
    , gmwetTaskId

    -- * Destructuring the Response
    , getMaintenanceWindowExecutionTaskResponse
    , GetMaintenanceWindowExecutionTaskResponse
    -- * Response Lenses
    , gmwetrsStatus
    , gmwetrsTaskParameters
    , gmwetrsTaskExecutionId
    , gmwetrsPriority
    , gmwetrsStartTime
    , gmwetrsTaskARN
    , gmwetrsWindowExecutionId
    , gmwetrsStatusDetails
    , gmwetrsMaxErrors
    , gmwetrsEndTime
    , gmwetrsType
    , gmwetrsMaxConcurrency
    , gmwetrsServiceRole
    , gmwetrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getMaintenanceWindowExecutionTask' smart constructor.
data GetMaintenanceWindowExecutionTask = GetMaintenanceWindowExecutionTask'
  { _gmwetWindowExecutionId :: !Text
  , _gmwetTaskId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMaintenanceWindowExecutionTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwetWindowExecutionId' - The ID of the Maintenance Window execution that includes the task.
--
-- * 'gmwetTaskId' - The ID of the specific task execution in the Maintenance Window task that should be retrieved.
getMaintenanceWindowExecutionTask
    :: Text -- ^ 'gmwetWindowExecutionId'
    -> Text -- ^ 'gmwetTaskId'
    -> GetMaintenanceWindowExecutionTask
getMaintenanceWindowExecutionTask pWindowExecutionId_ pTaskId_ =
  GetMaintenanceWindowExecutionTask'
    {_gmwetWindowExecutionId = pWindowExecutionId_, _gmwetTaskId = pTaskId_}


-- | The ID of the Maintenance Window execution that includes the task.
gmwetWindowExecutionId :: Lens' GetMaintenanceWindowExecutionTask Text
gmwetWindowExecutionId = lens _gmwetWindowExecutionId (\ s a -> s{_gmwetWindowExecutionId = a})

-- | The ID of the specific task execution in the Maintenance Window task that should be retrieved.
gmwetTaskId :: Lens' GetMaintenanceWindowExecutionTask Text
gmwetTaskId = lens _gmwetTaskId (\ s a -> s{_gmwetTaskId = a})

instance AWSRequest GetMaintenanceWindowExecutionTask
         where
        type Rs GetMaintenanceWindowExecutionTask =
             GetMaintenanceWindowExecutionTaskResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetMaintenanceWindowExecutionTaskResponse' <$>
                   (x .?> "Status") <*>
                     (x .?> "TaskParameters" .!@ mempty)
                     <*> (x .?> "TaskExecutionId")
                     <*> (x .?> "Priority")
                     <*> (x .?> "StartTime")
                     <*> (x .?> "TaskArn")
                     <*> (x .?> "WindowExecutionId")
                     <*> (x .?> "StatusDetails")
                     <*> (x .?> "MaxErrors")
                     <*> (x .?> "EndTime")
                     <*> (x .?> "Type")
                     <*> (x .?> "MaxConcurrency")
                     <*> (x .?> "ServiceRole")
                     <*> (pure (fromEnum s)))

instance Hashable GetMaintenanceWindowExecutionTask
         where

instance NFData GetMaintenanceWindowExecutionTask
         where

instance ToHeaders GetMaintenanceWindowExecutionTask
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetMaintenanceWindowExecutionTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetMaintenanceWindowExecutionTask
         where
        toJSON GetMaintenanceWindowExecutionTask'{..}
          = object
              (catMaybes
                 [Just
                    ("WindowExecutionId" .= _gmwetWindowExecutionId),
                  Just ("TaskId" .= _gmwetTaskId)])

instance ToPath GetMaintenanceWindowExecutionTask
         where
        toPath = const "/"

instance ToQuery GetMaintenanceWindowExecutionTask
         where
        toQuery = const mempty

-- | /See:/ 'getMaintenanceWindowExecutionTaskResponse' smart constructor.
data GetMaintenanceWindowExecutionTaskResponse = GetMaintenanceWindowExecutionTaskResponse'
  { _gmwetrsStatus :: !(Maybe MaintenanceWindowExecutionStatus)
  , _gmwetrsTaskParameters :: !(Maybe (Sensitive [Sensitive (Map Text (Sensitive MaintenanceWindowTaskParameterValueExpression))]))
  , _gmwetrsTaskExecutionId :: !(Maybe Text)
  , _gmwetrsPriority :: !(Maybe Nat)
  , _gmwetrsStartTime :: !(Maybe POSIX)
  , _gmwetrsTaskARN :: !(Maybe Text)
  , _gmwetrsWindowExecutionId :: !(Maybe Text)
  , _gmwetrsStatusDetails :: !(Maybe Text)
  , _gmwetrsMaxErrors :: !(Maybe Text)
  , _gmwetrsEndTime :: !(Maybe POSIX)
  , _gmwetrsType :: !(Maybe MaintenanceWindowTaskType)
  , _gmwetrsMaxConcurrency :: !(Maybe Text)
  , _gmwetrsServiceRole :: !(Maybe Text)
  , _gmwetrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMaintenanceWindowExecutionTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwetrsStatus' - The status of the task.
--
-- * 'gmwetrsTaskParameters' - The parameters passed to the task when it was executed. The map has the following format: Key: string, between 1 and 255 characters Value: an array of strings, each string is between 1 and 255 characters
--
-- * 'gmwetrsTaskExecutionId' - The ID of the specific task execution in the Maintenance Window task that was retrieved.
--
-- * 'gmwetrsPriority' - The priority of the task.
--
-- * 'gmwetrsStartTime' - The time the task execution started.
--
-- * 'gmwetrsTaskARN' - The ARN of the executed task.
--
-- * 'gmwetrsWindowExecutionId' - The ID of the Maintenance Window execution that includes the task.
--
-- * 'gmwetrsStatusDetails' - The details explaining the Status. Only available for certain status values.
--
-- * 'gmwetrsMaxErrors' - The defined maximum number of task execution errors allowed before scheduling of the task execution would have been stopped.
--
-- * 'gmwetrsEndTime' - The time the task execution completed.
--
-- * 'gmwetrsType' - The type of task executed.
--
-- * 'gmwetrsMaxConcurrency' - The defined maximum number of task executions that could be run in parallel.
--
-- * 'gmwetrsServiceRole' - The role that was assumed when executing the task.
--
-- * 'gmwetrsResponseStatus' - -- | The response status code.
getMaintenanceWindowExecutionTaskResponse
    :: Int -- ^ 'gmwetrsResponseStatus'
    -> GetMaintenanceWindowExecutionTaskResponse
getMaintenanceWindowExecutionTaskResponse pResponseStatus_ =
  GetMaintenanceWindowExecutionTaskResponse'
    { _gmwetrsStatus = Nothing
    , _gmwetrsTaskParameters = Nothing
    , _gmwetrsTaskExecutionId = Nothing
    , _gmwetrsPriority = Nothing
    , _gmwetrsStartTime = Nothing
    , _gmwetrsTaskARN = Nothing
    , _gmwetrsWindowExecutionId = Nothing
    , _gmwetrsStatusDetails = Nothing
    , _gmwetrsMaxErrors = Nothing
    , _gmwetrsEndTime = Nothing
    , _gmwetrsType = Nothing
    , _gmwetrsMaxConcurrency = Nothing
    , _gmwetrsServiceRole = Nothing
    , _gmwetrsResponseStatus = pResponseStatus_
    }


-- | The status of the task.
gmwetrsStatus :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe MaintenanceWindowExecutionStatus)
gmwetrsStatus = lens _gmwetrsStatus (\ s a -> s{_gmwetrsStatus = a})

-- | The parameters passed to the task when it was executed. The map has the following format: Key: string, between 1 and 255 characters Value: an array of strings, each string is between 1 and 255 characters
gmwetrsTaskParameters :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe [HashMap Text MaintenanceWindowTaskParameterValueExpression])
gmwetrsTaskParameters = lens _gmwetrsTaskParameters (\ s a -> s{_gmwetrsTaskParameters = a}) . mapping (_Sensitive . _Coerce)

-- | The ID of the specific task execution in the Maintenance Window task that was retrieved.
gmwetrsTaskExecutionId :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe Text)
gmwetrsTaskExecutionId = lens _gmwetrsTaskExecutionId (\ s a -> s{_gmwetrsTaskExecutionId = a})

-- | The priority of the task.
gmwetrsPriority :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe Natural)
gmwetrsPriority = lens _gmwetrsPriority (\ s a -> s{_gmwetrsPriority = a}) . mapping _Nat

-- | The time the task execution started.
gmwetrsStartTime :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe UTCTime)
gmwetrsStartTime = lens _gmwetrsStartTime (\ s a -> s{_gmwetrsStartTime = a}) . mapping _Time

-- | The ARN of the executed task.
gmwetrsTaskARN :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe Text)
gmwetrsTaskARN = lens _gmwetrsTaskARN (\ s a -> s{_gmwetrsTaskARN = a})

-- | The ID of the Maintenance Window execution that includes the task.
gmwetrsWindowExecutionId :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe Text)
gmwetrsWindowExecutionId = lens _gmwetrsWindowExecutionId (\ s a -> s{_gmwetrsWindowExecutionId = a})

-- | The details explaining the Status. Only available for certain status values.
gmwetrsStatusDetails :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe Text)
gmwetrsStatusDetails = lens _gmwetrsStatusDetails (\ s a -> s{_gmwetrsStatusDetails = a})

-- | The defined maximum number of task execution errors allowed before scheduling of the task execution would have been stopped.
gmwetrsMaxErrors :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe Text)
gmwetrsMaxErrors = lens _gmwetrsMaxErrors (\ s a -> s{_gmwetrsMaxErrors = a})

-- | The time the task execution completed.
gmwetrsEndTime :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe UTCTime)
gmwetrsEndTime = lens _gmwetrsEndTime (\ s a -> s{_gmwetrsEndTime = a}) . mapping _Time

-- | The type of task executed.
gmwetrsType :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe MaintenanceWindowTaskType)
gmwetrsType = lens _gmwetrsType (\ s a -> s{_gmwetrsType = a})

-- | The defined maximum number of task executions that could be run in parallel.
gmwetrsMaxConcurrency :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe Text)
gmwetrsMaxConcurrency = lens _gmwetrsMaxConcurrency (\ s a -> s{_gmwetrsMaxConcurrency = a})

-- | The role that was assumed when executing the task.
gmwetrsServiceRole :: Lens' GetMaintenanceWindowExecutionTaskResponse (Maybe Text)
gmwetrsServiceRole = lens _gmwetrsServiceRole (\ s a -> s{_gmwetrsServiceRole = a})

-- | -- | The response status code.
gmwetrsResponseStatus :: Lens' GetMaintenanceWindowExecutionTaskResponse Int
gmwetrsResponseStatus = lens _gmwetrsResponseStatus (\ s a -> s{_gmwetrsResponseStatus = a})

instance NFData
           GetMaintenanceWindowExecutionTaskResponse
         where
