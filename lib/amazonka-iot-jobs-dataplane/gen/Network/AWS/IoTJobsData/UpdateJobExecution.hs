{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.UpdateJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a job execution.
module Network.AWS.IoTJobsData.UpdateJobExecution
  ( -- * Creating a Request
    updateJobExecution,
    UpdateJobExecution,

    -- * Request Lenses
    ujeIncludeJobDocument,
    ujeStepTimeoutInMinutes,
    ujeStatusDetails,
    ujeExecutionNumber,
    ujeExpectedVersion,
    ujeIncludeJobExecutionState,
    ujeJobId,
    ujeThingName,
    ujeStatus,

    -- * Destructuring the Response
    updateJobExecutionResponse,
    UpdateJobExecutionResponse,

    -- * Response Lenses
    ujersJobDocument,
    ujersExecutionState,
    ujersResponseStatus,
  )
where

import Network.AWS.IoTJobsData.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateJobExecution' smart constructor.
data UpdateJobExecution = UpdateJobExecution'
  { _ujeIncludeJobDocument ::
      !(Maybe Bool),
    _ujeStepTimeoutInMinutes :: !(Maybe Integer),
    _ujeStatusDetails :: !(Maybe (Map Text (Text))),
    _ujeExecutionNumber :: !(Maybe Integer),
    _ujeExpectedVersion :: !(Maybe Integer),
    _ujeIncludeJobExecutionState :: !(Maybe Bool),
    _ujeJobId :: !Text,
    _ujeThingName :: !Text,
    _ujeStatus :: !JobExecutionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJobExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujeIncludeJobDocument' - Optional. When set to true, the response contains the job document. The default is false.
--
-- * 'ujeStepTimeoutInMinutes' - Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by again calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in this field) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting or resetting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
--
-- * 'ujeStatusDetails' - Optional. A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
--
-- * 'ujeExecutionNumber' - Optional. A number that identifies a particular job execution on a particular device.
--
-- * 'ujeExpectedVersion' - Optional. The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
--
-- * 'ujeIncludeJobExecutionState' - Optional. When included and set to true, the response contains the JobExecutionState data. The default is false.
--
-- * 'ujeJobId' - The unique identifier assigned to this job when it was created.
--
-- * 'ujeThingName' - The name of the thing associated with the device.
--
-- * 'ujeStatus' - The new status for the job execution (IN_PROGRESS, FAILED, SUCCESS, or REJECTED). This must be specified on every update.
updateJobExecution ::
  -- | 'ujeJobId'
  Text ->
  -- | 'ujeThingName'
  Text ->
  -- | 'ujeStatus'
  JobExecutionStatus ->
  UpdateJobExecution
updateJobExecution pJobId_ pThingName_ pStatus_ =
  UpdateJobExecution'
    { _ujeIncludeJobDocument = Nothing,
      _ujeStepTimeoutInMinutes = Nothing,
      _ujeStatusDetails = Nothing,
      _ujeExecutionNumber = Nothing,
      _ujeExpectedVersion = Nothing,
      _ujeIncludeJobExecutionState = Nothing,
      _ujeJobId = pJobId_,
      _ujeThingName = pThingName_,
      _ujeStatus = pStatus_
    }

-- | Optional. When set to true, the response contains the job document. The default is false.
ujeIncludeJobDocument :: Lens' UpdateJobExecution (Maybe Bool)
ujeIncludeJobDocument = lens _ujeIncludeJobDocument (\s a -> s {_ujeIncludeJobDocument = a})

-- | Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by again calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in this field) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting or resetting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
ujeStepTimeoutInMinutes :: Lens' UpdateJobExecution (Maybe Integer)
ujeStepTimeoutInMinutes = lens _ujeStepTimeoutInMinutes (\s a -> s {_ujeStepTimeoutInMinutes = a})

-- | Optional. A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
ujeStatusDetails :: Lens' UpdateJobExecution (HashMap Text (Text))
ujeStatusDetails = lens _ujeStatusDetails (\s a -> s {_ujeStatusDetails = a}) . _Default . _Map

-- | Optional. A number that identifies a particular job execution on a particular device.
ujeExecutionNumber :: Lens' UpdateJobExecution (Maybe Integer)
ujeExecutionNumber = lens _ujeExecutionNumber (\s a -> s {_ujeExecutionNumber = a})

-- | Optional. The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
ujeExpectedVersion :: Lens' UpdateJobExecution (Maybe Integer)
ujeExpectedVersion = lens _ujeExpectedVersion (\s a -> s {_ujeExpectedVersion = a})

-- | Optional. When included and set to true, the response contains the JobExecutionState data. The default is false.
ujeIncludeJobExecutionState :: Lens' UpdateJobExecution (Maybe Bool)
ujeIncludeJobExecutionState = lens _ujeIncludeJobExecutionState (\s a -> s {_ujeIncludeJobExecutionState = a})

-- | The unique identifier assigned to this job when it was created.
ujeJobId :: Lens' UpdateJobExecution Text
ujeJobId = lens _ujeJobId (\s a -> s {_ujeJobId = a})

-- | The name of the thing associated with the device.
ujeThingName :: Lens' UpdateJobExecution Text
ujeThingName = lens _ujeThingName (\s a -> s {_ujeThingName = a})

-- | The new status for the job execution (IN_PROGRESS, FAILED, SUCCESS, or REJECTED). This must be specified on every update.
ujeStatus :: Lens' UpdateJobExecution JobExecutionStatus
ujeStatus = lens _ujeStatus (\s a -> s {_ujeStatus = a})

instance AWSRequest UpdateJobExecution where
  type Rs UpdateJobExecution = UpdateJobExecutionResponse
  request = postJSON ioTJobsData
  response =
    receiveJSON
      ( \s h x ->
          UpdateJobExecutionResponse'
            <$> (x .?> "jobDocument")
            <*> (x .?> "executionState")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateJobExecution

instance NFData UpdateJobExecution

instance ToHeaders UpdateJobExecution where
  toHeaders = const mempty

instance ToJSON UpdateJobExecution where
  toJSON UpdateJobExecution' {..} =
    object
      ( catMaybes
          [ ("includeJobDocument" .=) <$> _ujeIncludeJobDocument,
            ("stepTimeoutInMinutes" .=) <$> _ujeStepTimeoutInMinutes,
            ("statusDetails" .=) <$> _ujeStatusDetails,
            ("executionNumber" .=) <$> _ujeExecutionNumber,
            ("expectedVersion" .=) <$> _ujeExpectedVersion,
            ("includeJobExecutionState" .=) <$> _ujeIncludeJobExecutionState,
            Just ("status" .= _ujeStatus)
          ]
      )

instance ToPath UpdateJobExecution where
  toPath UpdateJobExecution' {..} =
    mconcat
      ["/things/", toBS _ujeThingName, "/jobs/", toBS _ujeJobId]

instance ToQuery UpdateJobExecution where
  toQuery = const mempty

-- | /See:/ 'updateJobExecutionResponse' smart constructor.
data UpdateJobExecutionResponse = UpdateJobExecutionResponse'
  { _ujersJobDocument ::
      !(Maybe Text),
    _ujersExecutionState ::
      !(Maybe JobExecutionState),
    _ujersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJobExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujersJobDocument' - The contents of the Job Documents.
--
-- * 'ujersExecutionState' - A JobExecutionState object.
--
-- * 'ujersResponseStatus' - -- | The response status code.
updateJobExecutionResponse ::
  -- | 'ujersResponseStatus'
  Int ->
  UpdateJobExecutionResponse
updateJobExecutionResponse pResponseStatus_ =
  UpdateJobExecutionResponse'
    { _ujersJobDocument = Nothing,
      _ujersExecutionState = Nothing,
      _ujersResponseStatus = pResponseStatus_
    }

-- | The contents of the Job Documents.
ujersJobDocument :: Lens' UpdateJobExecutionResponse (Maybe Text)
ujersJobDocument = lens _ujersJobDocument (\s a -> s {_ujersJobDocument = a})

-- | A JobExecutionState object.
ujersExecutionState :: Lens' UpdateJobExecutionResponse (Maybe JobExecutionState)
ujersExecutionState = lens _ujersExecutionState (\s a -> s {_ujersExecutionState = a})

-- | -- | The response status code.
ujersResponseStatus :: Lens' UpdateJobExecutionResponse Int
ujersResponseStatus = lens _ujersResponseStatus (\s a -> s {_ujersResponseStatus = a})

instance NFData UpdateJobExecutionResponse
