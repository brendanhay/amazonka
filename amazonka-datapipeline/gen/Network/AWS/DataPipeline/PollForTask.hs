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
-- Module      : Network.AWS.DataPipeline.PollForTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @PollForTask@ to receive a task to perform from AWS Data Pipeline. The task runner specifies which tasks it can perform by setting a value for the @workerGroup@ parameter. The task returned can come from any of the pipelines that match the @workerGroup@ value passed in by the task runner and that was launched using the IAM user credentials specified by the task runner.
--
--
-- If tasks are ready in the work queue, @PollForTask@ returns a response immediately. If no tasks are available in the queue, @PollForTask@ uses long-polling and holds on to a poll connection for up to a 90 seconds, during which time the first newly scheduled task is handed to the task runner. To accomodate this, set the socket timeout in your task runner to 90 seconds. The task runner should not call @PollForTask@ again on the same @workerGroup@ until it receives a response, and this can take up to 90 seconds.
--
module Network.AWS.DataPipeline.PollForTask
    (
    -- * Creating a Request
      pollForTask
    , PollForTask
    -- * Request Lenses
    , pftHostname
    , pftInstanceIdentity
    , pftWorkerGroup

    -- * Destructuring the Response
    , pollForTaskResponse
    , PollForTaskResponse
    -- * Response Lenses
    , pftrsTaskObject
    , pftrsResponseStatus
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for PollForTask.
--
--
--
-- /See:/ 'pollForTask' smart constructor.
data PollForTask = PollForTask'
  { _pftHostname         :: !(Maybe Text)
  , _pftInstanceIdentity :: !(Maybe InstanceIdentity)
  , _pftWorkerGroup      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PollForTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pftHostname' - The public DNS name of the calling task runner.
--
-- * 'pftInstanceIdentity' - Identity information for the EC2 instance that is hosting the task runner. You can get this value from the instance using @http://169.254.169.254/latest/meta-data/instance-id@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata> in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.
--
-- * 'pftWorkerGroup' - The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ in the call to @PollForTask@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
pollForTask
    :: Text -- ^ 'pftWorkerGroup'
    -> PollForTask
pollForTask pWorkerGroup_ =
  PollForTask'
    { _pftHostname = Nothing
    , _pftInstanceIdentity = Nothing
    , _pftWorkerGroup = pWorkerGroup_
    }


-- | The public DNS name of the calling task runner.
pftHostname :: Lens' PollForTask (Maybe Text)
pftHostname = lens _pftHostname (\ s a -> s{_pftHostname = a})

-- | Identity information for the EC2 instance that is hosting the task runner. You can get this value from the instance using @http://169.254.169.254/latest/meta-data/instance-id@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata> in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.
pftInstanceIdentity :: Lens' PollForTask (Maybe InstanceIdentity)
pftInstanceIdentity = lens _pftInstanceIdentity (\ s a -> s{_pftInstanceIdentity = a})

-- | The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ in the call to @PollForTask@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
pftWorkerGroup :: Lens' PollForTask Text
pftWorkerGroup = lens _pftWorkerGroup (\ s a -> s{_pftWorkerGroup = a})

instance AWSRequest PollForTask where
        type Rs PollForTask = PollForTaskResponse
        request = postJSON dataPipeline
        response
          = receiveJSON
              (\ s h x ->
                 PollForTaskResponse' <$>
                   (x .?> "taskObject") <*> (pure (fromEnum s)))

instance Hashable PollForTask where

instance NFData PollForTask where

instance ToHeaders PollForTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.PollForTask" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PollForTask where
        toJSON PollForTask'{..}
          = object
              (catMaybes
                 [("hostname" .=) <$> _pftHostname,
                  ("instanceIdentity" .=) <$> _pftInstanceIdentity,
                  Just ("workerGroup" .= _pftWorkerGroup)])

instance ToPath PollForTask where
        toPath = const "/"

instance ToQuery PollForTask where
        toQuery = const mempty

-- | Contains the output of PollForTask.
--
--
--
-- /See:/ 'pollForTaskResponse' smart constructor.
data PollForTaskResponse = PollForTaskResponse'
  { _pftrsTaskObject     :: !(Maybe TaskObject)
  , _pftrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PollForTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pftrsTaskObject' - The information needed to complete the task that is being assigned to the task runner. One of the fields returned in this object is @taskId@ , which contains an identifier for the task being assigned. The calling task runner uses @taskId@ in subsequent calls to 'ReportTaskProgress' and 'SetTaskStatus' .
--
-- * 'pftrsResponseStatus' - -- | The response status code.
pollForTaskResponse
    :: Int -- ^ 'pftrsResponseStatus'
    -> PollForTaskResponse
pollForTaskResponse pResponseStatus_ =
  PollForTaskResponse'
    {_pftrsTaskObject = Nothing, _pftrsResponseStatus = pResponseStatus_}


-- | The information needed to complete the task that is being assigned to the task runner. One of the fields returned in this object is @taskId@ , which contains an identifier for the task being assigned. The calling task runner uses @taskId@ in subsequent calls to 'ReportTaskProgress' and 'SetTaskStatus' .
pftrsTaskObject :: Lens' PollForTaskResponse (Maybe TaskObject)
pftrsTaskObject = lens _pftrsTaskObject (\ s a -> s{_pftrsTaskObject = a})

-- | -- | The response status code.
pftrsResponseStatus :: Lens' PollForTaskResponse Int
pftrsResponseStatus = lens _pftrsResponseStatus (\ s a -> s{_pftrsResponseStatus = a})

instance NFData PollForTaskResponse where
