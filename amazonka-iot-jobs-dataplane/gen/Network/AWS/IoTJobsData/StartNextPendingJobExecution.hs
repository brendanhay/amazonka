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
-- Module      : Network.AWS.IoTJobsData.StartNextPendingJobExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets and starts the next pending (status IN_PROGRESS or QUEUED) job execution for a thing.
--
--
module Network.AWS.IoTJobsData.StartNextPendingJobExecution
    (
    -- * Creating a Request
      startNextPendingJobExecution
    , StartNextPendingJobExecution
    -- * Request Lenses
    , snpjeStepTimeoutInMinutes
    , snpjeStatusDetails
    , snpjeThingName

    -- * Destructuring the Response
    , startNextPendingJobExecutionResponse
    , StartNextPendingJobExecutionResponse
    -- * Response Lenses
    , snpjersExecution
    , snpjersResponseStatus
    ) where

import Network.AWS.IoTJobsData.Types
import Network.AWS.IoTJobsData.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startNextPendingJobExecution' smart constructor.
data StartNextPendingJobExecution = StartNextPendingJobExecution'
  { _snpjeStepTimeoutInMinutes :: !(Maybe Integer)
  , _snpjeStatusDetails        :: !(Maybe (Map Text Text))
  , _snpjeThingName            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartNextPendingJobExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snpjeStepTimeoutInMinutes' - Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in field @stepTimeoutInMinutes@ ) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
--
-- * 'snpjeStatusDetails' - A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
--
-- * 'snpjeThingName' - The name of the thing associated with the device.
startNextPendingJobExecution
    :: Text -- ^ 'snpjeThingName'
    -> StartNextPendingJobExecution
startNextPendingJobExecution pThingName_ =
  StartNextPendingJobExecution'
    { _snpjeStepTimeoutInMinutes = Nothing
    , _snpjeStatusDetails = Nothing
    , _snpjeThingName = pThingName_
    }


-- | Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in field @stepTimeoutInMinutes@ ) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
snpjeStepTimeoutInMinutes :: Lens' StartNextPendingJobExecution (Maybe Integer)
snpjeStepTimeoutInMinutes = lens _snpjeStepTimeoutInMinutes (\ s a -> s{_snpjeStepTimeoutInMinutes = a})

-- | A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
snpjeStatusDetails :: Lens' StartNextPendingJobExecution (HashMap Text Text)
snpjeStatusDetails = lens _snpjeStatusDetails (\ s a -> s{_snpjeStatusDetails = a}) . _Default . _Map

-- | The name of the thing associated with the device.
snpjeThingName :: Lens' StartNextPendingJobExecution Text
snpjeThingName = lens _snpjeThingName (\ s a -> s{_snpjeThingName = a})

instance AWSRequest StartNextPendingJobExecution
         where
        type Rs StartNextPendingJobExecution =
             StartNextPendingJobExecutionResponse
        request = putJSON ioTJobsData
        response
          = receiveJSON
              (\ s h x ->
                 StartNextPendingJobExecutionResponse' <$>
                   (x .?> "execution") <*> (pure (fromEnum s)))

instance Hashable StartNextPendingJobExecution where

instance NFData StartNextPendingJobExecution where

instance ToHeaders StartNextPendingJobExecution where
        toHeaders = const mempty

instance ToJSON StartNextPendingJobExecution where
        toJSON StartNextPendingJobExecution'{..}
          = object
              (catMaybes
                 [("stepTimeoutInMinutes" .=) <$>
                    _snpjeStepTimeoutInMinutes,
                  ("statusDetails" .=) <$> _snpjeStatusDetails])

instance ToPath StartNextPendingJobExecution where
        toPath StartNextPendingJobExecution'{..}
          = mconcat
              ["/things/", toBS _snpjeThingName, "/jobs/$next"]

instance ToQuery StartNextPendingJobExecution where
        toQuery = const mempty

-- | /See:/ 'startNextPendingJobExecutionResponse' smart constructor.
data StartNextPendingJobExecutionResponse = StartNextPendingJobExecutionResponse'
  { _snpjersExecution      :: !(Maybe JobExecution)
  , _snpjersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartNextPendingJobExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snpjersExecution' - A JobExecution object.
--
-- * 'snpjersResponseStatus' - -- | The response status code.
startNextPendingJobExecutionResponse
    :: Int -- ^ 'snpjersResponseStatus'
    -> StartNextPendingJobExecutionResponse
startNextPendingJobExecutionResponse pResponseStatus_ =
  StartNextPendingJobExecutionResponse'
    {_snpjersExecution = Nothing, _snpjersResponseStatus = pResponseStatus_}


-- | A JobExecution object.
snpjersExecution :: Lens' StartNextPendingJobExecutionResponse (Maybe JobExecution)
snpjersExecution = lens _snpjersExecution (\ s a -> s{_snpjersExecution = a})

-- | -- | The response status code.
snpjersResponseStatus :: Lens' StartNextPendingJobExecutionResponse Int
snpjersResponseStatus = lens _snpjersResponseStatus (\ s a -> s{_snpjersResponseStatus = a})

instance NFData StartNextPendingJobExecutionResponse
         where
