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
-- Module      : Network.AWS.StepFunctions.GetActivityTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to retrieve a task (with the specified activity ARN) which has been scheduled for execution by a running state machine. This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available (i.e. an execution of a task of this type is needed.) The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns a @taskToken@ with a null string.
--
--
-- /Important:/ Workers should set their client side socket timeout to at least 65 seconds (5 seconds higher than the maximum time the service may hold the poll request).
--
module Network.AWS.StepFunctions.GetActivityTask
    (
    -- * Creating a Request
      getActivityTask
    , GetActivityTask
    -- * Request Lenses
    , gatWorkerName
    , gatActivityARN

    -- * Destructuring the Response
    , getActivityTaskResponse
    , GetActivityTaskResponse
    -- * Response Lenses
    , gatrsInput
    , gatrsTaskToken
    , gatrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'getActivityTask' smart constructor.
data GetActivityTask = GetActivityTask'
  { _gatWorkerName  :: !(Maybe Text)
  , _gatActivityARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetActivityTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gatWorkerName' - You can provide an arbitrary name in order to identify the worker that the task is assigned to. This name is used when it is logged in the execution history.
--
-- * 'gatActivityARN' - The Amazon Resource Name (ARN) of the activity to retrieve tasks from (assigned when you create the task using 'CreateActivity' .)
getActivityTask
    :: Text -- ^ 'gatActivityARN'
    -> GetActivityTask
getActivityTask pActivityARN_ =
  GetActivityTask' {_gatWorkerName = Nothing, _gatActivityARN = pActivityARN_}


-- | You can provide an arbitrary name in order to identify the worker that the task is assigned to. This name is used when it is logged in the execution history.
gatWorkerName :: Lens' GetActivityTask (Maybe Text)
gatWorkerName = lens _gatWorkerName (\ s a -> s{_gatWorkerName = a})

-- | The Amazon Resource Name (ARN) of the activity to retrieve tasks from (assigned when you create the task using 'CreateActivity' .)
gatActivityARN :: Lens' GetActivityTask Text
gatActivityARN = lens _gatActivityARN (\ s a -> s{_gatActivityARN = a})

instance AWSRequest GetActivityTask where
        type Rs GetActivityTask = GetActivityTaskResponse
        request = postJSON stepFunctions
        response
          = receiveJSON
              (\ s h x ->
                 GetActivityTaskResponse' <$>
                   (x .?> "input") <*> (x .?> "taskToken") <*>
                     (pure (fromEnum s)))

instance Hashable GetActivityTask where

instance NFData GetActivityTask where

instance ToHeaders GetActivityTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.GetActivityTask" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON GetActivityTask where
        toJSON GetActivityTask'{..}
          = object
              (catMaybes
                 [("workerName" .=) <$> _gatWorkerName,
                  Just ("activityArn" .= _gatActivityARN)])

instance ToPath GetActivityTask where
        toPath = const "/"

instance ToQuery GetActivityTask where
        toQuery = const mempty

-- | /See:/ 'getActivityTaskResponse' smart constructor.
data GetActivityTaskResponse = GetActivityTaskResponse'
  { _gatrsInput          :: !(Maybe Text)
  , _gatrsTaskToken      :: !(Maybe Text)
  , _gatrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetActivityTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gatrsInput' - The string that contains the JSON input data for the task.
--
-- * 'gatrsTaskToken' - A token that identifies the scheduled task. This token must be copied and included in subsequent calls to 'SendTaskHeartbeat' , 'SendTaskSuccess' or 'SendTaskFailure' in order to report the progress or completion of the task.
--
-- * 'gatrsResponseStatus' - -- | The response status code.
getActivityTaskResponse
    :: Int -- ^ 'gatrsResponseStatus'
    -> GetActivityTaskResponse
getActivityTaskResponse pResponseStatus_ =
  GetActivityTaskResponse'
    { _gatrsInput = Nothing
    , _gatrsTaskToken = Nothing
    , _gatrsResponseStatus = pResponseStatus_
    }


-- | The string that contains the JSON input data for the task.
gatrsInput :: Lens' GetActivityTaskResponse (Maybe Text)
gatrsInput = lens _gatrsInput (\ s a -> s{_gatrsInput = a})

-- | A token that identifies the scheduled task. This token must be copied and included in subsequent calls to 'SendTaskHeartbeat' , 'SendTaskSuccess' or 'SendTaskFailure' in order to report the progress or completion of the task.
gatrsTaskToken :: Lens' GetActivityTaskResponse (Maybe Text)
gatrsTaskToken = lens _gatrsTaskToken (\ s a -> s{_gatrsTaskToken = a})

-- | -- | The response status code.
gatrsResponseStatus :: Lens' GetActivityTaskResponse Int
gatrsResponseStatus = lens _gatrsResponseStatus (\ s a -> s{_gatrsResponseStatus = a})

instance NFData GetActivityTaskResponse where
