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
-- Module      : Network.AWS.StepFunctions.StartExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a state machine execution.
module Network.AWS.StepFunctions.StartExecution
  ( -- * Creating a Request
    startExecution,
    StartExecution,

    -- * Request Lenses
    seInput,
    seName,
    seTraceHeader,
    seStateMachineARN,

    -- * Destructuring the Response
    startExecutionResponse,
    StartExecutionResponse,

    -- * Response Lenses
    srsResponseStatus,
    srsExecutionARN,
    srsStartDate,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'startExecution' smart constructor.
data StartExecution = StartExecution'
  { _seInput ::
      !(Maybe (Sensitive Text)),
    _seName :: !(Maybe Text),
    _seTraceHeader :: !(Maybe Text),
    _seStateMachineARN :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seInput' - The string that contains the JSON input data for the execution, for example: @"input": "{\"first_name\" : \"test\"}"@  Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'seName' - The name of the execution. This name must be unique for your AWS account, region, and state machine for 90 days. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ . A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- * 'seTraceHeader' - Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
--
-- * 'seStateMachineARN' - The Amazon Resource Name (ARN) of the state machine to execute.
startExecution ::
  -- | 'seStateMachineARN'
  Text ->
  StartExecution
startExecution pStateMachineARN_ =
  StartExecution'
    { _seInput = Nothing,
      _seName = Nothing,
      _seTraceHeader = Nothing,
      _seStateMachineARN = pStateMachineARN_
    }

-- | The string that contains the JSON input data for the execution, for example: @"input": "{\"first_name\" : \"test\"}"@  Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
seInput :: Lens' StartExecution (Maybe Text)
seInput = lens _seInput (\s a -> s {_seInput = a}) . mapping _Sensitive

-- | The name of the execution. This name must be unique for your AWS account, region, and state machine for 90 days. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ . A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
seName :: Lens' StartExecution (Maybe Text)
seName = lens _seName (\s a -> s {_seName = a})

-- | Passes the AWS X-Ray trace header. The trace header can also be passed in the request payload.
seTraceHeader :: Lens' StartExecution (Maybe Text)
seTraceHeader = lens _seTraceHeader (\s a -> s {_seTraceHeader = a})

-- | The Amazon Resource Name (ARN) of the state machine to execute.
seStateMachineARN :: Lens' StartExecution Text
seStateMachineARN = lens _seStateMachineARN (\s a -> s {_seStateMachineARN = a})

instance AWSRequest StartExecution where
  type Rs StartExecution = StartExecutionResponse
  request = postJSON stepFunctions
  response =
    receiveJSON
      ( \s h x ->
          StartExecutionResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "executionArn")
            <*> (x .:> "startDate")
      )

instance Hashable StartExecution

instance NFData StartExecution

instance ToHeaders StartExecution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSStepFunctions.StartExecution" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON StartExecution where
  toJSON StartExecution' {..} =
    object
      ( catMaybes
          [ ("input" .=) <$> _seInput,
            ("name" .=) <$> _seName,
            ("traceHeader" .=) <$> _seTraceHeader,
            Just ("stateMachineArn" .= _seStateMachineARN)
          ]
      )

instance ToPath StartExecution where
  toPath = const "/"

instance ToQuery StartExecution where
  toQuery = const mempty

-- | /See:/ 'startExecutionResponse' smart constructor.
data StartExecutionResponse = StartExecutionResponse'
  { _srsResponseStatus ::
      !Int,
    _srsExecutionARN :: !Text,
    _srsStartDate :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
--
-- * 'srsExecutionARN' - The Amazon Resource Name (ARN) that identifies the execution.
--
-- * 'srsStartDate' - The date the execution is started.
startExecutionResponse ::
  -- | 'srsResponseStatus'
  Int ->
  -- | 'srsExecutionARN'
  Text ->
  -- | 'srsStartDate'
  UTCTime ->
  StartExecutionResponse
startExecutionResponse pResponseStatus_ pExecutionARN_ pStartDate_ =
  StartExecutionResponse'
    { _srsResponseStatus = pResponseStatus_,
      _srsExecutionARN = pExecutionARN_,
      _srsStartDate = _Time # pStartDate_
    }

-- | -- | The response status code.
srsResponseStatus :: Lens' StartExecutionResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the execution.
srsExecutionARN :: Lens' StartExecutionResponse Text
srsExecutionARN = lens _srsExecutionARN (\s a -> s {_srsExecutionARN = a})

-- | The date the execution is started.
srsStartDate :: Lens' StartExecutionResponse UTCTime
srsStartDate = lens _srsStartDate (\s a -> s {_srsStartDate = a}) . _Time

instance NFData StartExecutionResponse
