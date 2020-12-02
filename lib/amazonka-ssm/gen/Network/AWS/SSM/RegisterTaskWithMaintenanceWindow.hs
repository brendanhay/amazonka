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
-- Module      : Network.AWS.SSM.RegisterTaskWithMaintenanceWindow
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new task to a Maintenance Window.
--
--
module Network.AWS.SSM.RegisterTaskWithMaintenanceWindow
    (
    -- * Creating a Request
      registerTaskWithMaintenanceWindow
    , RegisterTaskWithMaintenanceWindow
    -- * Request Lenses
    , rtwmwTaskParameters
    , rtwmwPriority
    , rtwmwClientToken
    , rtwmwTaskInvocationParameters
    , rtwmwName
    , rtwmwLoggingInfo
    , rtwmwDescription
    , rtwmwWindowId
    , rtwmwTargets
    , rtwmwTaskARN
    , rtwmwServiceRoleARN
    , rtwmwTaskType
    , rtwmwMaxConcurrency
    , rtwmwMaxErrors

    -- * Destructuring the Response
    , registerTaskWithMaintenanceWindowResponse
    , RegisterTaskWithMaintenanceWindowResponse
    -- * Response Lenses
    , rtwmwrsWindowTaskId
    , rtwmwrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'registerTaskWithMaintenanceWindow' smart constructor.
data RegisterTaskWithMaintenanceWindow = RegisterTaskWithMaintenanceWindow'
  { _rtwmwTaskParameters :: !(Maybe (Sensitive (Map Text (Sensitive MaintenanceWindowTaskParameterValueExpression))))
  , _rtwmwPriority :: !(Maybe Nat)
  , _rtwmwClientToken :: !(Maybe Text)
  , _rtwmwTaskInvocationParameters :: !(Maybe MaintenanceWindowTaskInvocationParameters)
  , _rtwmwName :: !(Maybe Text)
  , _rtwmwLoggingInfo :: !(Maybe LoggingInfo)
  , _rtwmwDescription :: !(Maybe (Sensitive Text))
  , _rtwmwWindowId :: !Text
  , _rtwmwTargets :: ![Target]
  , _rtwmwTaskARN :: !Text
  , _rtwmwServiceRoleARN :: !Text
  , _rtwmwTaskType :: !MaintenanceWindowTaskType
  , _rtwmwMaxConcurrency :: !Text
  , _rtwmwMaxErrors :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterTaskWithMaintenanceWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtwmwTaskParameters' - The parameters that should be passed to the task when it is executed.
--
-- * 'rtwmwPriority' - The priority of the task in the Maintenance Window, the lower the number the higher the priority. Tasks in a Maintenance Window are scheduled in priority order with tasks that have the same priority scheduled in parallel.
--
-- * 'rtwmwClientToken' - User-provided idempotency token.
--
-- * 'rtwmwTaskInvocationParameters' - The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
--
-- * 'rtwmwName' - An optional name for the task.
--
-- * 'rtwmwLoggingInfo' - A structure containing information about an Amazon S3 bucket to write instance-level logs to.
--
-- * 'rtwmwDescription' - An optional description for the task.
--
-- * 'rtwmwWindowId' - The ID of the Maintenance Window the task should be added to.
--
-- * 'rtwmwTargets' - The targets (either instances or Maintenance Window targets). Specify instances using the following format:  @Key=InstanceIds,Values=<instance-id-1>,<instance-id-2>@  Specify Maintenance Window targets using the following format: @Key=<WindowTargetIds>,Values=<window-target-id-1>,<window-target-id-2>@
--
-- * 'rtwmwTaskARN' - The ARN of the task to execute
--
-- * 'rtwmwServiceRoleARN' - The role that should be assumed when executing the task.
--
-- * 'rtwmwTaskType' - The type of task being registered.
--
-- * 'rtwmwMaxConcurrency' - The maximum number of targets this task can be run for in parallel.
--
-- * 'rtwmwMaxErrors' - The maximum number of errors allowed before this task stops being scheduled.
registerTaskWithMaintenanceWindow
    :: Text -- ^ 'rtwmwWindowId'
    -> Text -- ^ 'rtwmwTaskARN'
    -> Text -- ^ 'rtwmwServiceRoleARN'
    -> MaintenanceWindowTaskType -- ^ 'rtwmwTaskType'
    -> Text -- ^ 'rtwmwMaxConcurrency'
    -> Text -- ^ 'rtwmwMaxErrors'
    -> RegisterTaskWithMaintenanceWindow
registerTaskWithMaintenanceWindow pWindowId_ pTaskARN_ pServiceRoleARN_ pTaskType_ pMaxConcurrency_ pMaxErrors_ =
  RegisterTaskWithMaintenanceWindow'
    { _rtwmwTaskParameters = Nothing
    , _rtwmwPriority = Nothing
    , _rtwmwClientToken = Nothing
    , _rtwmwTaskInvocationParameters = Nothing
    , _rtwmwName = Nothing
    , _rtwmwLoggingInfo = Nothing
    , _rtwmwDescription = Nothing
    , _rtwmwWindowId = pWindowId_
    , _rtwmwTargets = mempty
    , _rtwmwTaskARN = pTaskARN_
    , _rtwmwServiceRoleARN = pServiceRoleARN_
    , _rtwmwTaskType = pTaskType_
    , _rtwmwMaxConcurrency = pMaxConcurrency_
    , _rtwmwMaxErrors = pMaxErrors_
    }


-- | The parameters that should be passed to the task when it is executed.
rtwmwTaskParameters :: Lens' RegisterTaskWithMaintenanceWindow (Maybe (HashMap Text MaintenanceWindowTaskParameterValueExpression))
rtwmwTaskParameters = lens _rtwmwTaskParameters (\ s a -> s{_rtwmwTaskParameters = a}) . mapping (_Sensitive . _Map)

-- | The priority of the task in the Maintenance Window, the lower the number the higher the priority. Tasks in a Maintenance Window are scheduled in priority order with tasks that have the same priority scheduled in parallel.
rtwmwPriority :: Lens' RegisterTaskWithMaintenanceWindow (Maybe Natural)
rtwmwPriority = lens _rtwmwPriority (\ s a -> s{_rtwmwPriority = a}) . mapping _Nat

-- | User-provided idempotency token.
rtwmwClientToken :: Lens' RegisterTaskWithMaintenanceWindow (Maybe Text)
rtwmwClientToken = lens _rtwmwClientToken (\ s a -> s{_rtwmwClientToken = a})

-- | The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
rtwmwTaskInvocationParameters :: Lens' RegisterTaskWithMaintenanceWindow (Maybe MaintenanceWindowTaskInvocationParameters)
rtwmwTaskInvocationParameters = lens _rtwmwTaskInvocationParameters (\ s a -> s{_rtwmwTaskInvocationParameters = a})

-- | An optional name for the task.
rtwmwName :: Lens' RegisterTaskWithMaintenanceWindow (Maybe Text)
rtwmwName = lens _rtwmwName (\ s a -> s{_rtwmwName = a})

-- | A structure containing information about an Amazon S3 bucket to write instance-level logs to.
rtwmwLoggingInfo :: Lens' RegisterTaskWithMaintenanceWindow (Maybe LoggingInfo)
rtwmwLoggingInfo = lens _rtwmwLoggingInfo (\ s a -> s{_rtwmwLoggingInfo = a})

-- | An optional description for the task.
rtwmwDescription :: Lens' RegisterTaskWithMaintenanceWindow (Maybe Text)
rtwmwDescription = lens _rtwmwDescription (\ s a -> s{_rtwmwDescription = a}) . mapping _Sensitive

-- | The ID of the Maintenance Window the task should be added to.
rtwmwWindowId :: Lens' RegisterTaskWithMaintenanceWindow Text
rtwmwWindowId = lens _rtwmwWindowId (\ s a -> s{_rtwmwWindowId = a})

-- | The targets (either instances or Maintenance Window targets). Specify instances using the following format:  @Key=InstanceIds,Values=<instance-id-1>,<instance-id-2>@  Specify Maintenance Window targets using the following format: @Key=<WindowTargetIds>,Values=<window-target-id-1>,<window-target-id-2>@
rtwmwTargets :: Lens' RegisterTaskWithMaintenanceWindow [Target]
rtwmwTargets = lens _rtwmwTargets (\ s a -> s{_rtwmwTargets = a}) . _Coerce

-- | The ARN of the task to execute
rtwmwTaskARN :: Lens' RegisterTaskWithMaintenanceWindow Text
rtwmwTaskARN = lens _rtwmwTaskARN (\ s a -> s{_rtwmwTaskARN = a})

-- | The role that should be assumed when executing the task.
rtwmwServiceRoleARN :: Lens' RegisterTaskWithMaintenanceWindow Text
rtwmwServiceRoleARN = lens _rtwmwServiceRoleARN (\ s a -> s{_rtwmwServiceRoleARN = a})

-- | The type of task being registered.
rtwmwTaskType :: Lens' RegisterTaskWithMaintenanceWindow MaintenanceWindowTaskType
rtwmwTaskType = lens _rtwmwTaskType (\ s a -> s{_rtwmwTaskType = a})

-- | The maximum number of targets this task can be run for in parallel.
rtwmwMaxConcurrency :: Lens' RegisterTaskWithMaintenanceWindow Text
rtwmwMaxConcurrency = lens _rtwmwMaxConcurrency (\ s a -> s{_rtwmwMaxConcurrency = a})

-- | The maximum number of errors allowed before this task stops being scheduled.
rtwmwMaxErrors :: Lens' RegisterTaskWithMaintenanceWindow Text
rtwmwMaxErrors = lens _rtwmwMaxErrors (\ s a -> s{_rtwmwMaxErrors = a})

instance AWSRequest RegisterTaskWithMaintenanceWindow
         where
        type Rs RegisterTaskWithMaintenanceWindow =
             RegisterTaskWithMaintenanceWindowResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 RegisterTaskWithMaintenanceWindowResponse' <$>
                   (x .?> "WindowTaskId") <*> (pure (fromEnum s)))

instance Hashable RegisterTaskWithMaintenanceWindow
         where

instance NFData RegisterTaskWithMaintenanceWindow
         where

instance ToHeaders RegisterTaskWithMaintenanceWindow
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.RegisterTaskWithMaintenanceWindow" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterTaskWithMaintenanceWindow
         where
        toJSON RegisterTaskWithMaintenanceWindow'{..}
          = object
              (catMaybes
                 [("TaskParameters" .=) <$> _rtwmwTaskParameters,
                  ("Priority" .=) <$> _rtwmwPriority,
                  ("ClientToken" .=) <$> _rtwmwClientToken,
                  ("TaskInvocationParameters" .=) <$>
                    _rtwmwTaskInvocationParameters,
                  ("Name" .=) <$> _rtwmwName,
                  ("LoggingInfo" .=) <$> _rtwmwLoggingInfo,
                  ("Description" .=) <$> _rtwmwDescription,
                  Just ("WindowId" .= _rtwmwWindowId),
                  Just ("Targets" .= _rtwmwTargets),
                  Just ("TaskArn" .= _rtwmwTaskARN),
                  Just ("ServiceRoleArn" .= _rtwmwServiceRoleARN),
                  Just ("TaskType" .= _rtwmwTaskType),
                  Just ("MaxConcurrency" .= _rtwmwMaxConcurrency),
                  Just ("MaxErrors" .= _rtwmwMaxErrors)])

instance ToPath RegisterTaskWithMaintenanceWindow
         where
        toPath = const "/"

instance ToQuery RegisterTaskWithMaintenanceWindow
         where
        toQuery = const mempty

-- | /See:/ 'registerTaskWithMaintenanceWindowResponse' smart constructor.
data RegisterTaskWithMaintenanceWindowResponse = RegisterTaskWithMaintenanceWindowResponse'
  { _rtwmwrsWindowTaskId   :: !(Maybe Text)
  , _rtwmwrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterTaskWithMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtwmwrsWindowTaskId' - The id of the task in the Maintenance Window.
--
-- * 'rtwmwrsResponseStatus' - -- | The response status code.
registerTaskWithMaintenanceWindowResponse
    :: Int -- ^ 'rtwmwrsResponseStatus'
    -> RegisterTaskWithMaintenanceWindowResponse
registerTaskWithMaintenanceWindowResponse pResponseStatus_ =
  RegisterTaskWithMaintenanceWindowResponse'
    {_rtwmwrsWindowTaskId = Nothing, _rtwmwrsResponseStatus = pResponseStatus_}


-- | The id of the task in the Maintenance Window.
rtwmwrsWindowTaskId :: Lens' RegisterTaskWithMaintenanceWindowResponse (Maybe Text)
rtwmwrsWindowTaskId = lens _rtwmwrsWindowTaskId (\ s a -> s{_rtwmwrsWindowTaskId = a})

-- | -- | The response status code.
rtwmwrsResponseStatus :: Lens' RegisterTaskWithMaintenanceWindowResponse Int
rtwmwrsResponseStatus = lens _rtwmwrsResponseStatus (\ s a -> s{_rtwmwrsResponseStatus = a})

instance NFData
           RegisterTaskWithMaintenanceWindowResponse
         where
