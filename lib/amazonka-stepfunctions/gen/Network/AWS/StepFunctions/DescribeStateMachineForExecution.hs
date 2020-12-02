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
-- Module      : Network.AWS.StepFunctions.DescribeStateMachineForExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state machine associated with a specific execution.
--
--
-- This API action is not supported by @EXPRESS@ state machines.
module Network.AWS.StepFunctions.DescribeStateMachineForExecution
  ( -- * Creating a Request
    describeStateMachineForExecution,
    DescribeStateMachineForExecution,

    -- * Request Lenses
    dsmfeExecutionARN,

    -- * Destructuring the Response
    describeStateMachineForExecutionResponse,
    DescribeStateMachineForExecutionResponse,

    -- * Response Lenses
    dsmfersTracingConfiguration,
    dsmfersLoggingConfiguration,
    dsmfersResponseStatus,
    dsmfersStateMachineARN,
    dsmfersName,
    dsmfersDefinition,
    dsmfersRoleARN,
    dsmfersUpdateDate,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'describeStateMachineForExecution' smart constructor.
newtype DescribeStateMachineForExecution = DescribeStateMachineForExecution'
  { _dsmfeExecutionARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeStateMachineForExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsmfeExecutionARN' - The Amazon Resource Name (ARN) of the execution you want state machine information for.
describeStateMachineForExecution ::
  -- | 'dsmfeExecutionARN'
  Text ->
  DescribeStateMachineForExecution
describeStateMachineForExecution pExecutionARN_ =
  DescribeStateMachineForExecution'
    { _dsmfeExecutionARN =
        pExecutionARN_
    }

-- | The Amazon Resource Name (ARN) of the execution you want state machine information for.
dsmfeExecutionARN :: Lens' DescribeStateMachineForExecution Text
dsmfeExecutionARN = lens _dsmfeExecutionARN (\s a -> s {_dsmfeExecutionARN = a})

instance AWSRequest DescribeStateMachineForExecution where
  type
    Rs DescribeStateMachineForExecution =
      DescribeStateMachineForExecutionResponse
  request = postJSON stepFunctions
  response =
    receiveJSON
      ( \s h x ->
          DescribeStateMachineForExecutionResponse'
            <$> (x .?> "tracingConfiguration")
            <*> (x .?> "loggingConfiguration")
            <*> (pure (fromEnum s))
            <*> (x .:> "stateMachineArn")
            <*> (x .:> "name")
            <*> (x .:> "definition")
            <*> (x .:> "roleArn")
            <*> (x .:> "updateDate")
      )

instance Hashable DescribeStateMachineForExecution

instance NFData DescribeStateMachineForExecution

instance ToHeaders DescribeStateMachineForExecution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSStepFunctions.DescribeStateMachineForExecution" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON DescribeStateMachineForExecution where
  toJSON DescribeStateMachineForExecution' {..} =
    object (catMaybes [Just ("executionArn" .= _dsmfeExecutionARN)])

instance ToPath DescribeStateMachineForExecution where
  toPath = const "/"

instance ToQuery DescribeStateMachineForExecution where
  toQuery = const mempty

-- | /See:/ 'describeStateMachineForExecutionResponse' smart constructor.
data DescribeStateMachineForExecutionResponse = DescribeStateMachineForExecutionResponse'
  { _dsmfersTracingConfiguration ::
      !( Maybe
           TracingConfiguration
       ),
    _dsmfersLoggingConfiguration ::
      !( Maybe
           LoggingConfiguration
       ),
    _dsmfersResponseStatus ::
      !Int,
    _dsmfersStateMachineARN ::
      !Text,
    _dsmfersName ::
      !Text,
    _dsmfersDefinition ::
      !( Sensitive
           Text
       ),
    _dsmfersRoleARN ::
      !Text,
    _dsmfersUpdateDate ::
      !POSIX
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeStateMachineForExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsmfersTracingConfiguration' - Selects whether AWS X-Ray tracing is enabled.
--
-- * 'dsmfersLoggingConfiguration' - Undocumented member.
--
-- * 'dsmfersResponseStatus' - -- | The response status code.
--
-- * 'dsmfersStateMachineARN' - The Amazon Resource Name (ARN) of the state machine associated with the execution.
--
-- * 'dsmfersName' - The name of the state machine associated with the execution.
--
-- * 'dsmfersDefinition' - The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- * 'dsmfersRoleARN' - The Amazon Resource Name (ARN) of the IAM role of the State Machine for the execution.
--
-- * 'dsmfersUpdateDate' - The date and time the state machine associated with an execution was updated. For a newly created state machine, this is the creation date.
describeStateMachineForExecutionResponse ::
  -- | 'dsmfersResponseStatus'
  Int ->
  -- | 'dsmfersStateMachineARN'
  Text ->
  -- | 'dsmfersName'
  Text ->
  -- | 'dsmfersDefinition'
  Text ->
  -- | 'dsmfersRoleARN'
  Text ->
  -- | 'dsmfersUpdateDate'
  UTCTime ->
  DescribeStateMachineForExecutionResponse
describeStateMachineForExecutionResponse
  pResponseStatus_
  pStateMachineARN_
  pName_
  pDefinition_
  pRoleARN_
  pUpdateDate_ =
    DescribeStateMachineForExecutionResponse'
      { _dsmfersTracingConfiguration =
          Nothing,
        _dsmfersLoggingConfiguration = Nothing,
        _dsmfersResponseStatus = pResponseStatus_,
        _dsmfersStateMachineARN = pStateMachineARN_,
        _dsmfersName = pName_,
        _dsmfersDefinition = _Sensitive # pDefinition_,
        _dsmfersRoleARN = pRoleARN_,
        _dsmfersUpdateDate = _Time # pUpdateDate_
      }

-- | Selects whether AWS X-Ray tracing is enabled.
dsmfersTracingConfiguration :: Lens' DescribeStateMachineForExecutionResponse (Maybe TracingConfiguration)
dsmfersTracingConfiguration = lens _dsmfersTracingConfiguration (\s a -> s {_dsmfersTracingConfiguration = a})

-- | Undocumented member.
dsmfersLoggingConfiguration :: Lens' DescribeStateMachineForExecutionResponse (Maybe LoggingConfiguration)
dsmfersLoggingConfiguration = lens _dsmfersLoggingConfiguration (\s a -> s {_dsmfersLoggingConfiguration = a})

-- | -- | The response status code.
dsmfersResponseStatus :: Lens' DescribeStateMachineForExecutionResponse Int
dsmfersResponseStatus = lens _dsmfersResponseStatus (\s a -> s {_dsmfersResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the state machine associated with the execution.
dsmfersStateMachineARN :: Lens' DescribeStateMachineForExecutionResponse Text
dsmfersStateMachineARN = lens _dsmfersStateMachineARN (\s a -> s {_dsmfersStateMachineARN = a})

-- | The name of the state machine associated with the execution.
dsmfersName :: Lens' DescribeStateMachineForExecutionResponse Text
dsmfersName = lens _dsmfersName (\s a -> s {_dsmfersName = a})

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
dsmfersDefinition :: Lens' DescribeStateMachineForExecutionResponse Text
dsmfersDefinition = lens _dsmfersDefinition (\s a -> s {_dsmfersDefinition = a}) . _Sensitive

-- | The Amazon Resource Name (ARN) of the IAM role of the State Machine for the execution.
dsmfersRoleARN :: Lens' DescribeStateMachineForExecutionResponse Text
dsmfersRoleARN = lens _dsmfersRoleARN (\s a -> s {_dsmfersRoleARN = a})

-- | The date and time the state machine associated with an execution was updated. For a newly created state machine, this is the creation date.
dsmfersUpdateDate :: Lens' DescribeStateMachineForExecutionResponse UTCTime
dsmfersUpdateDate = lens _dsmfersUpdateDate (\s a -> s {_dsmfersUpdateDate = a}) . _Time

instance NFData DescribeStateMachineForExecutionResponse
