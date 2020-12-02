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
-- Module      : Network.AWS.StepFunctions.StartSyncExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a Synchronous Express state machine execution.
module Network.AWS.StepFunctions.StartSyncExecution
  ( -- * Creating a Request
    startSyncExecution,
    StartSyncExecution,

    -- * Request Lenses
    sseInput,
    sseName,
    sseTraceHeader,
    sseStateMachineARN,

    -- * Destructuring the Response
    startSyncExecutionResponse,
    StartSyncExecutionResponse,

    -- * Response Lenses
    ssersInputDetails,
    ssersError,
    ssersInput,
    ssersCause,
    ssersName,
    ssersStateMachineARN,
    ssersOutput,
    ssersOutputDetails,
    ssersTraceHeader,
    ssersBillingDetails,
    ssersResponseStatus,
    ssersExecutionARN,
    ssersStartDate,
    ssersStopDate,
    ssersStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'startSyncExecution' smart constructor.
data StartSyncExecution = StartSyncExecution'
  { _sseInput ::
      !(Maybe (Sensitive Text)),
    _sseName :: !(Maybe Text),
    _sseTraceHeader :: !(Maybe Text),
    _sseStateMachineARN :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartSyncExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sseInput' - The string that contains the JSON input data for the execution, for example: @"input": "{\"first_name\" : \"test\"}"@  Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'sseName' - The name of the execution.
--
-- * 'sseTraceHeader' - Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
--
-- * 'sseStateMachineARN' - The Amazon Resource Name (ARN) of the state machine to execute.
startSyncExecution ::
  -- | 'sseStateMachineARN'
  Text ->
  StartSyncExecution
startSyncExecution pStateMachineARN_ =
  StartSyncExecution'
    { _sseInput = Nothing,
      _sseName = Nothing,
      _sseTraceHeader = Nothing,
      _sseStateMachineARN = pStateMachineARN_
    }

-- | The string that contains the JSON input data for the execution, for example: @"input": "{\"first_name\" : \"test\"}"@  Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
sseInput :: Lens' StartSyncExecution (Maybe Text)
sseInput = lens _sseInput (\s a -> s {_sseInput = a}) . mapping _Sensitive

-- | The name of the execution.
sseName :: Lens' StartSyncExecution (Maybe Text)
sseName = lens _sseName (\s a -> s {_sseName = a})

-- | Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
sseTraceHeader :: Lens' StartSyncExecution (Maybe Text)
sseTraceHeader = lens _sseTraceHeader (\s a -> s {_sseTraceHeader = a})

-- | The Amazon Resource Name (ARN) of the state machine to execute.
sseStateMachineARN :: Lens' StartSyncExecution Text
sseStateMachineARN = lens _sseStateMachineARN (\s a -> s {_sseStateMachineARN = a})

instance AWSRequest StartSyncExecution where
  type Rs StartSyncExecution = StartSyncExecutionResponse
  request = postJSON stepFunctions
  response =
    receiveJSON
      ( \s h x ->
          StartSyncExecutionResponse'
            <$> (x .?> "inputDetails")
            <*> (x .?> "error")
            <*> (x .?> "input")
            <*> (x .?> "cause")
            <*> (x .?> "name")
            <*> (x .?> "stateMachineArn")
            <*> (x .?> "output")
            <*> (x .?> "outputDetails")
            <*> (x .?> "traceHeader")
            <*> (x .?> "billingDetails")
            <*> (pure (fromEnum s))
            <*> (x .:> "executionArn")
            <*> (x .:> "startDate")
            <*> (x .:> "stopDate")
            <*> (x .:> "status")
      )

instance Hashable StartSyncExecution

instance NFData StartSyncExecution

instance ToHeaders StartSyncExecution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSStepFunctions.StartSyncExecution" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON StartSyncExecution where
  toJSON StartSyncExecution' {..} =
    object
      ( catMaybes
          [ ("input" .=) <$> _sseInput,
            ("name" .=) <$> _sseName,
            ("traceHeader" .=) <$> _sseTraceHeader,
            Just ("stateMachineArn" .= _sseStateMachineARN)
          ]
      )

instance ToPath StartSyncExecution where
  toPath = const "/"

instance ToQuery StartSyncExecution where
  toQuery = const mempty

-- | /See:/ 'startSyncExecutionResponse' smart constructor.
data StartSyncExecutionResponse = StartSyncExecutionResponse'
  { _ssersInputDetails ::
      !( Maybe
           CloudWatchEventsExecutionDataDetails
       ),
    _ssersError ::
      !(Maybe (Sensitive Text)),
    _ssersInput ::
      !(Maybe (Sensitive Text)),
    _ssersCause ::
      !(Maybe (Sensitive Text)),
    _ssersName :: !(Maybe Text),
    _ssersStateMachineARN ::
      !(Maybe Text),
    _ssersOutput ::
      !(Maybe (Sensitive Text)),
    _ssersOutputDetails ::
      !( Maybe
           CloudWatchEventsExecutionDataDetails
       ),
    _ssersTraceHeader :: !(Maybe Text),
    _ssersBillingDetails ::
      !(Maybe BillingDetails),
    _ssersResponseStatus :: !Int,
    _ssersExecutionARN :: !Text,
    _ssersStartDate :: !POSIX,
    _ssersStopDate :: !POSIX,
    _ssersStatus :: !SyncExecutionStatus
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartSyncExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssersInputDetails' - Undocumented member.
--
-- * 'ssersError' - The error code of the failure.
--
-- * 'ssersInput' - The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'ssersCause' - A more detailed explanation of the cause of the failure.
--
-- * 'ssersName' - The name of the execution.
--
-- * 'ssersStateMachineARN' - The Amazon Resource Name (ARN) that identifies the state machine.
--
-- * 'ssersOutput' - The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'ssersOutputDetails' - Undocumented member.
--
-- * 'ssersTraceHeader' - The AWS X-Ray trace header that was passed to the execution.
--
-- * 'ssersBillingDetails' - An object that describes workflow billing details, including billed duration and memory use.
--
-- * 'ssersResponseStatus' - -- | The response status code.
--
-- * 'ssersExecutionARN' - The Amazon Resource Name (ARN) that identifies the execution.
--
-- * 'ssersStartDate' - The date the execution is started.
--
-- * 'ssersStopDate' - If the execution has already ended, the date the execution stopped.
--
-- * 'ssersStatus' - The current status of the execution.
startSyncExecutionResponse ::
  -- | 'ssersResponseStatus'
  Int ->
  -- | 'ssersExecutionARN'
  Text ->
  -- | 'ssersStartDate'
  UTCTime ->
  -- | 'ssersStopDate'
  UTCTime ->
  -- | 'ssersStatus'
  SyncExecutionStatus ->
  StartSyncExecutionResponse
startSyncExecutionResponse
  pResponseStatus_
  pExecutionARN_
  pStartDate_
  pStopDate_
  pStatus_ =
    StartSyncExecutionResponse'
      { _ssersInputDetails = Nothing,
        _ssersError = Nothing,
        _ssersInput = Nothing,
        _ssersCause = Nothing,
        _ssersName = Nothing,
        _ssersStateMachineARN = Nothing,
        _ssersOutput = Nothing,
        _ssersOutputDetails = Nothing,
        _ssersTraceHeader = Nothing,
        _ssersBillingDetails = Nothing,
        _ssersResponseStatus = pResponseStatus_,
        _ssersExecutionARN = pExecutionARN_,
        _ssersStartDate = _Time # pStartDate_,
        _ssersStopDate = _Time # pStopDate_,
        _ssersStatus = pStatus_
      }

-- | Undocumented member.
ssersInputDetails :: Lens' StartSyncExecutionResponse (Maybe CloudWatchEventsExecutionDataDetails)
ssersInputDetails = lens _ssersInputDetails (\s a -> s {_ssersInputDetails = a})

-- | The error code of the failure.
ssersError :: Lens' StartSyncExecutionResponse (Maybe Text)
ssersError = lens _ssersError (\s a -> s {_ssersError = a}) . mapping _Sensitive

-- | The string that contains the JSON input data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
ssersInput :: Lens' StartSyncExecutionResponse (Maybe Text)
ssersInput = lens _ssersInput (\s a -> s {_ssersInput = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
ssersCause :: Lens' StartSyncExecutionResponse (Maybe Text)
ssersCause = lens _ssersCause (\s a -> s {_ssersCause = a}) . mapping _Sensitive

-- | The name of the execution.
ssersName :: Lens' StartSyncExecutionResponse (Maybe Text)
ssersName = lens _ssersName (\s a -> s {_ssersName = a})

-- | The Amazon Resource Name (ARN) that identifies the state machine.
ssersStateMachineARN :: Lens' StartSyncExecutionResponse (Maybe Text)
ssersStateMachineARN = lens _ssersStateMachineARN (\s a -> s {_ssersStateMachineARN = a})

-- | The JSON output data of the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
ssersOutput :: Lens' StartSyncExecutionResponse (Maybe Text)
ssersOutput = lens _ssersOutput (\s a -> s {_ssersOutput = a}) . mapping _Sensitive

-- | Undocumented member.
ssersOutputDetails :: Lens' StartSyncExecutionResponse (Maybe CloudWatchEventsExecutionDataDetails)
ssersOutputDetails = lens _ssersOutputDetails (\s a -> s {_ssersOutputDetails = a})

-- | The AWS X-Ray trace header that was passed to the execution.
ssersTraceHeader :: Lens' StartSyncExecutionResponse (Maybe Text)
ssersTraceHeader = lens _ssersTraceHeader (\s a -> s {_ssersTraceHeader = a})

-- | An object that describes workflow billing details, including billed duration and memory use.
ssersBillingDetails :: Lens' StartSyncExecutionResponse (Maybe BillingDetails)
ssersBillingDetails = lens _ssersBillingDetails (\s a -> s {_ssersBillingDetails = a})

-- | -- | The response status code.
ssersResponseStatus :: Lens' StartSyncExecutionResponse Int
ssersResponseStatus = lens _ssersResponseStatus (\s a -> s {_ssersResponseStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the execution.
ssersExecutionARN :: Lens' StartSyncExecutionResponse Text
ssersExecutionARN = lens _ssersExecutionARN (\s a -> s {_ssersExecutionARN = a})

-- | The date the execution is started.
ssersStartDate :: Lens' StartSyncExecutionResponse UTCTime
ssersStartDate = lens _ssersStartDate (\s a -> s {_ssersStartDate = a}) . _Time

-- | If the execution has already ended, the date the execution stopped.
ssersStopDate :: Lens' StartSyncExecutionResponse UTCTime
ssersStopDate = lens _ssersStopDate (\s a -> s {_ssersStopDate = a}) . _Time

-- | The current status of the execution.
ssersStatus :: Lens' StartSyncExecutionResponse SyncExecutionStatus
ssersStatus = lens _ssersStatus (\s a -> s {_ssersStatus = a})

instance NFData StartSyncExecutionResponse
