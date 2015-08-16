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
-- Module      : Network.AWS.DataPipeline.SetTaskStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call 'SetTaskStatus' to notify AWS Data Pipeline that a
-- task is completed and provide information about the final status. A task
-- runner makes this call regardless of whether the task was sucessful. A
-- task runner does not need to call 'SetTaskStatus' for tasks that are
-- canceled by the web service during a call to ReportTaskProgress.
--
-- /See:/ <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_SetTaskStatus.html AWS API Reference> for SetTaskStatus.
module Network.AWS.DataPipeline.SetTaskStatus
    (
    -- * Creating a Request
      setTaskStatus
    , SetTaskStatus
    -- * Request Lenses
    , stsErrorStackTrace
    , stsErrorId
    , stsErrorMessage
    , stsTaskId
    , stsTaskStatus

    -- * Destructuring the Response
    , setTaskStatusResponse
    , SetTaskStatusResponse
    -- * Response Lenses
    , stsrsStatus
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.DataPipeline.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for SetTaskStatus.
--
-- /See:/ 'setTaskStatus' smart constructor.
data SetTaskStatus = SetTaskStatus'
    { _stsErrorStackTrace :: !(Maybe Text)
    , _stsErrorId         :: !(Maybe Text)
    , _stsErrorMessage    :: !(Maybe Text)
    , _stsTaskId          :: !Text
    , _stsTaskStatus      :: !TaskStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetTaskStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsErrorStackTrace'
--
-- * 'stsErrorId'
--
-- * 'stsErrorMessage'
--
-- * 'stsTaskId'
--
-- * 'stsTaskStatus'
setTaskStatus
    :: Text -- ^ 'stsTaskId'
    -> TaskStatus -- ^ 'stsTaskStatus'
    -> SetTaskStatus
setTaskStatus pTaskId_ pTaskStatus_ =
    SetTaskStatus'
    { _stsErrorStackTrace = Nothing
    , _stsErrorId = Nothing
    , _stsErrorMessage = Nothing
    , _stsTaskId = pTaskId_
    , _stsTaskStatus = pTaskStatus_
    }

-- | If an error occurred during the task, this value specifies the stack
-- trace associated with the error. This value is set on the physical
-- attempt object. It is used to display error information to the user. The
-- web service does not parse this value.
stsErrorStackTrace :: Lens' SetTaskStatus (Maybe Text)
stsErrorStackTrace = lens _stsErrorStackTrace (\ s a -> s{_stsErrorStackTrace = a});

-- | If an error occurred during the task, this value specifies the error
-- code. This value is set on the physical attempt object. It is used to
-- display error information to the user. It should not start with string
-- \"Service_\" which is reserved by the system.
stsErrorId :: Lens' SetTaskStatus (Maybe Text)
stsErrorId = lens _stsErrorId (\ s a -> s{_stsErrorId = a});

-- | If an error occurred during the task, this value specifies a text
-- description of the error. This value is set on the physical attempt
-- object. It is used to display error information to the user. The web
-- service does not parse this value.
stsErrorMessage :: Lens' SetTaskStatus (Maybe Text)
stsErrorMessage = lens _stsErrorMessage (\ s a -> s{_stsErrorMessage = a});

-- | The ID of the task assigned to the task runner. This value is provided
-- in the response for PollForTask.
stsTaskId :: Lens' SetTaskStatus Text
stsTaskId = lens _stsTaskId (\ s a -> s{_stsTaskId = a});

-- | If 'FINISHED', the task successfully completed. If 'FAILED', the task
-- ended unsuccessfully. Preconditions use false.
stsTaskStatus :: Lens' SetTaskStatus TaskStatus
stsTaskStatus = lens _stsTaskStatus (\ s a -> s{_stsTaskStatus = a});

instance AWSRequest SetTaskStatus where
        type Sv SetTaskStatus = DataPipeline
        type Rs SetTaskStatus = SetTaskStatusResponse
        request = postJSON
        response
          = receiveEmpty
              (\ s h x ->
                 SetTaskStatusResponse' <$> (pure (fromEnum s)))

instance ToHeaders SetTaskStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.SetTaskStatus" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetTaskStatus where
        toJSON SetTaskStatus'{..}
          = object
              ["errorStackTrace" .= _stsErrorStackTrace,
               "errorId" .= _stsErrorId,
               "errorMessage" .= _stsErrorMessage,
               "taskId" .= _stsTaskId,
               "taskStatus" .= _stsTaskStatus]

instance ToPath SetTaskStatus where
        toPath = const "/"

instance ToQuery SetTaskStatus where
        toQuery = const mempty

-- | Contains the output of SetTaskStatus.
--
-- /See:/ 'setTaskStatusResponse' smart constructor.
newtype SetTaskStatusResponse = SetTaskStatusResponse'
    { _stsrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetTaskStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsrsStatus'
setTaskStatusResponse
    :: Int -- ^ 'stsrsStatus'
    -> SetTaskStatusResponse
setTaskStatusResponse pStatus_ =
    SetTaskStatusResponse'
    { _stsrsStatus = pStatus_
    }

-- | The response status code.
stsrsStatus :: Lens' SetTaskStatusResponse Int
stsrsStatus = lens _stsrsStatus (\ s a -> s{_stsrsStatus = a});
