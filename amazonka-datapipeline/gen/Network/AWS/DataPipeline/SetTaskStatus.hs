{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.SetTaskStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @SetTaskStatus@ to notify AWS Data Pipeline that a
-- task is completed and provide information about the final status. A task
-- runner makes this call regardless of whether the task was sucessful. A
-- task runner does not need to call @SetTaskStatus@ for tasks that are
-- canceled by the web service during a call to ReportTaskProgress.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_SetTaskStatus.html>
module Network.AWS.DataPipeline.SetTaskStatus
    (
    -- * Request
      SetTaskStatus
    -- ** Request constructor
    , setTaskStatus
    -- ** Request lenses
    , stsrqErrorStackTrace
    , stsrqErrorId
    , stsrqErrorMessage
    , stsrqTaskId
    , stsrqTaskStatus

    -- * Response
    , SetTaskStatusResponse
    -- ** Response constructor
    , setTaskStatusResponse
    -- ** Response lenses
    , stsrsStatus
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for SetTaskStatus.
--
-- /See:/ 'setTaskStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stsrqErrorStackTrace'
--
-- * 'stsrqErrorId'
--
-- * 'stsrqErrorMessage'
--
-- * 'stsrqTaskId'
--
-- * 'stsrqTaskStatus'
data SetTaskStatus = SetTaskStatus'
    { _stsrqErrorStackTrace :: !(Maybe Text)
    , _stsrqErrorId         :: !(Maybe Text)
    , _stsrqErrorMessage    :: !(Maybe Text)
    , _stsrqTaskId          :: !Text
    , _stsrqTaskStatus      :: !TaskStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetTaskStatus' smart constructor.
setTaskStatus :: Text -> TaskStatus -> SetTaskStatus
setTaskStatus pTaskId_ pTaskStatus_ =
    SetTaskStatus'
    { _stsrqErrorStackTrace = Nothing
    , _stsrqErrorId = Nothing
    , _stsrqErrorMessage = Nothing
    , _stsrqTaskId = pTaskId_
    , _stsrqTaskStatus = pTaskStatus_
    }

-- | If an error occurred during the task, this value specifies the stack
-- trace associated with the error. This value is set on the physical
-- attempt object. It is used to display error information to the user. The
-- web service does not parse this value.
stsrqErrorStackTrace :: Lens' SetTaskStatus (Maybe Text)
stsrqErrorStackTrace = lens _stsrqErrorStackTrace (\ s a -> s{_stsrqErrorStackTrace = a});

-- | If an error occurred during the task, this value specifies the error
-- code. This value is set on the physical attempt object. It is used to
-- display error information to the user. It should not start with string
-- \"Service_\" which is reserved by the system.
stsrqErrorId :: Lens' SetTaskStatus (Maybe Text)
stsrqErrorId = lens _stsrqErrorId (\ s a -> s{_stsrqErrorId = a});

-- | If an error occurred during the task, this value specifies a text
-- description of the error. This value is set on the physical attempt
-- object. It is used to display error information to the user. The web
-- service does not parse this value.
stsrqErrorMessage :: Lens' SetTaskStatus (Maybe Text)
stsrqErrorMessage = lens _stsrqErrorMessage (\ s a -> s{_stsrqErrorMessage = a});

-- | The ID of the task assigned to the task runner. This value is provided
-- in the response for PollForTask.
stsrqTaskId :: Lens' SetTaskStatus Text
stsrqTaskId = lens _stsrqTaskId (\ s a -> s{_stsrqTaskId = a});

-- | If @FINISHED@, the task successfully completed. If @FAILED@, the task
-- ended unsuccessfully. Preconditions use false.
stsrqTaskStatus :: Lens' SetTaskStatus TaskStatus
stsrqTaskStatus = lens _stsrqTaskStatus (\ s a -> s{_stsrqTaskStatus = a});

instance AWSRequest SetTaskStatus where
        type Sv SetTaskStatus = DataPipeline
        type Rs SetTaskStatus = SetTaskStatusResponse
        request = postJSON
        response
          = receiveJSON
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
              ["errorStackTrace" .= _stsrqErrorStackTrace,
               "errorId" .= _stsrqErrorId,
               "errorMessage" .= _stsrqErrorMessage,
               "taskId" .= _stsrqTaskId,
               "taskStatus" .= _stsrqTaskStatus]

instance ToPath SetTaskStatus where
        toPath = const "/"

instance ToQuery SetTaskStatus where
        toQuery = const mempty

-- | Contains the output of SetTaskStatus.
--
-- /See:/ 'setTaskStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stsrsStatus'
newtype SetTaskStatusResponse = SetTaskStatusResponse'
    { _stsrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetTaskStatusResponse' smart constructor.
setTaskStatusResponse :: Int -> SetTaskStatusResponse
setTaskStatusResponse pStatus_ =
    SetTaskStatusResponse'
    { _stsrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
stsrsStatus :: Lens' SetTaskStatusResponse Int
stsrsStatus = lens _stsrsStatus (\ s a -> s{_stsrsStatus = a});
