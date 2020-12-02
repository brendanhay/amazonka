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
-- Module      : Network.AWS.SSM.GetMaintenanceWindowExecutionTaskInvocation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a task invocation. A task invocation is a specific task executing on a specific target. Maintenance Windows report status for all invocations.
--
--
module Network.AWS.SSM.GetMaintenanceWindowExecutionTaskInvocation
    (
    -- * Creating a Request
      getMaintenanceWindowExecutionTaskInvocation
    , GetMaintenanceWindowExecutionTaskInvocation
    -- * Request Lenses
    , gmwetiWindowExecutionId
    , gmwetiTaskId
    , gmwetiInvocationId

    -- * Destructuring the Response
    , getMaintenanceWindowExecutionTaskInvocationResponse
    , GetMaintenanceWindowExecutionTaskInvocationResponse
    -- * Response Lenses
    , gmwetirsStatus
    , gmwetirsExecutionId
    , gmwetirsTaskExecutionId
    , gmwetirsStartTime
    , gmwetirsInvocationId
    , gmwetirsOwnerInformation
    , gmwetirsTaskType
    , gmwetirsWindowTargetId
    , gmwetirsWindowExecutionId
    , gmwetirsStatusDetails
    , gmwetirsEndTime
    , gmwetirsParameters
    , gmwetirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getMaintenanceWindowExecutionTaskInvocation' smart constructor.
data GetMaintenanceWindowExecutionTaskInvocation = GetMaintenanceWindowExecutionTaskInvocation'
  { _gmwetiWindowExecutionId :: !Text
  , _gmwetiTaskId            :: !Text
  , _gmwetiInvocationId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMaintenanceWindowExecutionTaskInvocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwetiWindowExecutionId' - The ID of the Maintenance Window execution for which the task is a part.
--
-- * 'gmwetiTaskId' - The ID of the specific task in the Maintenance Window task that should be retrieved.
--
-- * 'gmwetiInvocationId' - The invocation ID to retrieve.
getMaintenanceWindowExecutionTaskInvocation
    :: Text -- ^ 'gmwetiWindowExecutionId'
    -> Text -- ^ 'gmwetiTaskId'
    -> Text -- ^ 'gmwetiInvocationId'
    -> GetMaintenanceWindowExecutionTaskInvocation
getMaintenanceWindowExecutionTaskInvocation pWindowExecutionId_ pTaskId_ pInvocationId_ =
  GetMaintenanceWindowExecutionTaskInvocation'
    { _gmwetiWindowExecutionId = pWindowExecutionId_
    , _gmwetiTaskId = pTaskId_
    , _gmwetiInvocationId = pInvocationId_
    }


-- | The ID of the Maintenance Window execution for which the task is a part.
gmwetiWindowExecutionId :: Lens' GetMaintenanceWindowExecutionTaskInvocation Text
gmwetiWindowExecutionId = lens _gmwetiWindowExecutionId (\ s a -> s{_gmwetiWindowExecutionId = a})

-- | The ID of the specific task in the Maintenance Window task that should be retrieved.
gmwetiTaskId :: Lens' GetMaintenanceWindowExecutionTaskInvocation Text
gmwetiTaskId = lens _gmwetiTaskId (\ s a -> s{_gmwetiTaskId = a})

-- | The invocation ID to retrieve.
gmwetiInvocationId :: Lens' GetMaintenanceWindowExecutionTaskInvocation Text
gmwetiInvocationId = lens _gmwetiInvocationId (\ s a -> s{_gmwetiInvocationId = a})

instance AWSRequest
           GetMaintenanceWindowExecutionTaskInvocation
         where
        type Rs GetMaintenanceWindowExecutionTaskInvocation =
             GetMaintenanceWindowExecutionTaskInvocationResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetMaintenanceWindowExecutionTaskInvocationResponse'
                   <$>
                   (x .?> "Status") <*> (x .?> "ExecutionId") <*>
                     (x .?> "TaskExecutionId")
                     <*> (x .?> "StartTime")
                     <*> (x .?> "InvocationId")
                     <*> (x .?> "OwnerInformation")
                     <*> (x .?> "TaskType")
                     <*> (x .?> "WindowTargetId")
                     <*> (x .?> "WindowExecutionId")
                     <*> (x .?> "StatusDetails")
                     <*> (x .?> "EndTime")
                     <*> (x .?> "Parameters")
                     <*> (pure (fromEnum s)))

instance Hashable
           GetMaintenanceWindowExecutionTaskInvocation
         where

instance NFData
           GetMaintenanceWindowExecutionTaskInvocation
         where

instance ToHeaders
           GetMaintenanceWindowExecutionTaskInvocation
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetMaintenanceWindowExecutionTaskInvocation"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           GetMaintenanceWindowExecutionTaskInvocation
         where
        toJSON
          GetMaintenanceWindowExecutionTaskInvocation'{..}
          = object
              (catMaybes
                 [Just
                    ("WindowExecutionId" .= _gmwetiWindowExecutionId),
                  Just ("TaskId" .= _gmwetiTaskId),
                  Just ("InvocationId" .= _gmwetiInvocationId)])

instance ToPath
           GetMaintenanceWindowExecutionTaskInvocation
         where
        toPath = const "/"

instance ToQuery
           GetMaintenanceWindowExecutionTaskInvocation
         where
        toQuery = const mempty

-- | /See:/ 'getMaintenanceWindowExecutionTaskInvocationResponse' smart constructor.
data GetMaintenanceWindowExecutionTaskInvocationResponse = GetMaintenanceWindowExecutionTaskInvocationResponse'
  { _gmwetirsStatus            :: !(Maybe MaintenanceWindowExecutionStatus)
  , _gmwetirsExecutionId       :: !(Maybe Text)
  , _gmwetirsTaskExecutionId   :: !(Maybe Text)
  , _gmwetirsStartTime         :: !(Maybe POSIX)
  , _gmwetirsInvocationId      :: !(Maybe Text)
  , _gmwetirsOwnerInformation  :: !(Maybe (Sensitive Text))
  , _gmwetirsTaskType          :: !(Maybe MaintenanceWindowTaskType)
  , _gmwetirsWindowTargetId    :: !(Maybe Text)
  , _gmwetirsWindowExecutionId :: !(Maybe Text)
  , _gmwetirsStatusDetails     :: !(Maybe Text)
  , _gmwetirsEndTime           :: !(Maybe POSIX)
  , _gmwetirsParameters        :: !(Maybe (Sensitive Text))
  , _gmwetirsResponseStatus    :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMaintenanceWindowExecutionTaskInvocationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwetirsStatus' - The task status for an invocation.
--
-- * 'gmwetirsExecutionId' - The execution ID.
--
-- * 'gmwetirsTaskExecutionId' - The task execution ID.
--
-- * 'gmwetirsStartTime' - The time that the task started executing on the target.
--
-- * 'gmwetirsInvocationId' - The invocation ID.
--
-- * 'gmwetirsOwnerInformation' - User-provided value to be included in any CloudWatch events raised while running tasks for these targets in this Maintenance Window.
--
-- * 'gmwetirsTaskType' - Retrieves the task type for a Maintenance Window. Task types include the following: LAMBDA, STEP_FUNCTION, AUTOMATION, RUN_COMMAND.
--
-- * 'gmwetirsWindowTargetId' - The Maintenance Window target ID.
--
-- * 'gmwetirsWindowExecutionId' - The Maintenance Window execution ID.
--
-- * 'gmwetirsStatusDetails' - The details explaining the status. Details are only available for certain status values.
--
-- * 'gmwetirsEndTime' - The time that the task finished executing on the target.
--
-- * 'gmwetirsParameters' - The parameters used at the time that the task executed.
--
-- * 'gmwetirsResponseStatus' - -- | The response status code.
getMaintenanceWindowExecutionTaskInvocationResponse
    :: Int -- ^ 'gmwetirsResponseStatus'
    -> GetMaintenanceWindowExecutionTaskInvocationResponse
getMaintenanceWindowExecutionTaskInvocationResponse pResponseStatus_ =
  GetMaintenanceWindowExecutionTaskInvocationResponse'
    { _gmwetirsStatus = Nothing
    , _gmwetirsExecutionId = Nothing
    , _gmwetirsTaskExecutionId = Nothing
    , _gmwetirsStartTime = Nothing
    , _gmwetirsInvocationId = Nothing
    , _gmwetirsOwnerInformation = Nothing
    , _gmwetirsTaskType = Nothing
    , _gmwetirsWindowTargetId = Nothing
    , _gmwetirsWindowExecutionId = Nothing
    , _gmwetirsStatusDetails = Nothing
    , _gmwetirsEndTime = Nothing
    , _gmwetirsParameters = Nothing
    , _gmwetirsResponseStatus = pResponseStatus_
    }


-- | The task status for an invocation.
gmwetirsStatus :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe MaintenanceWindowExecutionStatus)
gmwetirsStatus = lens _gmwetirsStatus (\ s a -> s{_gmwetirsStatus = a})

-- | The execution ID.
gmwetirsExecutionId :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe Text)
gmwetirsExecutionId = lens _gmwetirsExecutionId (\ s a -> s{_gmwetirsExecutionId = a})

-- | The task execution ID.
gmwetirsTaskExecutionId :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe Text)
gmwetirsTaskExecutionId = lens _gmwetirsTaskExecutionId (\ s a -> s{_gmwetirsTaskExecutionId = a})

-- | The time that the task started executing on the target.
gmwetirsStartTime :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe UTCTime)
gmwetirsStartTime = lens _gmwetirsStartTime (\ s a -> s{_gmwetirsStartTime = a}) . mapping _Time

-- | The invocation ID.
gmwetirsInvocationId :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe Text)
gmwetirsInvocationId = lens _gmwetirsInvocationId (\ s a -> s{_gmwetirsInvocationId = a})

-- | User-provided value to be included in any CloudWatch events raised while running tasks for these targets in this Maintenance Window.
gmwetirsOwnerInformation :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe Text)
gmwetirsOwnerInformation = lens _gmwetirsOwnerInformation (\ s a -> s{_gmwetirsOwnerInformation = a}) . mapping _Sensitive

-- | Retrieves the task type for a Maintenance Window. Task types include the following: LAMBDA, STEP_FUNCTION, AUTOMATION, RUN_COMMAND.
gmwetirsTaskType :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe MaintenanceWindowTaskType)
gmwetirsTaskType = lens _gmwetirsTaskType (\ s a -> s{_gmwetirsTaskType = a})

-- | The Maintenance Window target ID.
gmwetirsWindowTargetId :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe Text)
gmwetirsWindowTargetId = lens _gmwetirsWindowTargetId (\ s a -> s{_gmwetirsWindowTargetId = a})

-- | The Maintenance Window execution ID.
gmwetirsWindowExecutionId :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe Text)
gmwetirsWindowExecutionId = lens _gmwetirsWindowExecutionId (\ s a -> s{_gmwetirsWindowExecutionId = a})

-- | The details explaining the status. Details are only available for certain status values.
gmwetirsStatusDetails :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe Text)
gmwetirsStatusDetails = lens _gmwetirsStatusDetails (\ s a -> s{_gmwetirsStatusDetails = a})

-- | The time that the task finished executing on the target.
gmwetirsEndTime :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe UTCTime)
gmwetirsEndTime = lens _gmwetirsEndTime (\ s a -> s{_gmwetirsEndTime = a}) . mapping _Time

-- | The parameters used at the time that the task executed.
gmwetirsParameters :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse (Maybe Text)
gmwetirsParameters = lens _gmwetirsParameters (\ s a -> s{_gmwetirsParameters = a}) . mapping _Sensitive

-- | -- | The response status code.
gmwetirsResponseStatus :: Lens' GetMaintenanceWindowExecutionTaskInvocationResponse Int
gmwetirsResponseStatus = lens _gmwetirsResponseStatus (\ s a -> s{_gmwetirsResponseStatus = a})

instance NFData
           GetMaintenanceWindowExecutionTaskInvocationResponse
         where
