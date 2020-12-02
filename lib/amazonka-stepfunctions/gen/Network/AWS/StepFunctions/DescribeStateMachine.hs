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
-- Module      : Network.AWS.StepFunctions.DescribeStateMachine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a state machine.
module Network.AWS.StepFunctions.DescribeStateMachine
  ( -- * Creating a Request
    describeStateMachine,
    DescribeStateMachine,

    -- * Request Lenses
    dsmStateMachineARN,

    -- * Destructuring the Response
    describeStateMachineResponse,
    DescribeStateMachineResponse,

    -- * Response Lenses
    dsmrsStatus,
    dsmrsTracingConfiguration,
    dsmrsLoggingConfiguration,
    dsmrsResponseStatus,
    dsmrsStateMachineARN,
    dsmrsName,
    dsmrsDefinition,
    dsmrsRoleARN,
    dsmrsType,
    dsmrsCreationDate,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'describeStateMachine' smart constructor.
newtype DescribeStateMachine = DescribeStateMachine'
  { _dsmStateMachineARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeStateMachine' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsmStateMachineARN' - The Amazon Resource Name (ARN) of the state machine to describe.
describeStateMachine ::
  -- | 'dsmStateMachineARN'
  Text ->
  DescribeStateMachine
describeStateMachine pStateMachineARN_ =
  DescribeStateMachine' {_dsmStateMachineARN = pStateMachineARN_}

-- | The Amazon Resource Name (ARN) of the state machine to describe.
dsmStateMachineARN :: Lens' DescribeStateMachine Text
dsmStateMachineARN = lens _dsmStateMachineARN (\s a -> s {_dsmStateMachineARN = a})

instance AWSRequest DescribeStateMachine where
  type Rs DescribeStateMachine = DescribeStateMachineResponse
  request = postJSON stepFunctions
  response =
    receiveJSON
      ( \s h x ->
          DescribeStateMachineResponse'
            <$> (x .?> "status")
            <*> (x .?> "tracingConfiguration")
            <*> (x .?> "loggingConfiguration")
            <*> (pure (fromEnum s))
            <*> (x .:> "stateMachineArn")
            <*> (x .:> "name")
            <*> (x .:> "definition")
            <*> (x .:> "roleArn")
            <*> (x .:> "type")
            <*> (x .:> "creationDate")
      )

instance Hashable DescribeStateMachine

instance NFData DescribeStateMachine

instance ToHeaders DescribeStateMachine where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSStepFunctions.DescribeStateMachine" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON DescribeStateMachine where
  toJSON DescribeStateMachine' {..} =
    object
      (catMaybes [Just ("stateMachineArn" .= _dsmStateMachineARN)])

instance ToPath DescribeStateMachine where
  toPath = const "/"

instance ToQuery DescribeStateMachine where
  toQuery = const mempty

-- | /See:/ 'describeStateMachineResponse' smart constructor.
data DescribeStateMachineResponse = DescribeStateMachineResponse'
  { _dsmrsStatus ::
      !(Maybe StateMachineStatus),
    _dsmrsTracingConfiguration ::
      !(Maybe TracingConfiguration),
    _dsmrsLoggingConfiguration ::
      !(Maybe LoggingConfiguration),
    _dsmrsResponseStatus :: !Int,
    _dsmrsStateMachineARN :: !Text,
    _dsmrsName :: !Text,
    _dsmrsDefinition ::
      !(Sensitive Text),
    _dsmrsRoleARN :: !Text,
    _dsmrsType :: !StateMachineType,
    _dsmrsCreationDate :: !POSIX
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeStateMachineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsmrsStatus' - The current status of the state machine.
--
-- * 'dsmrsTracingConfiguration' - Selects whether AWS X-Ray tracing is enabled.
--
-- * 'dsmrsLoggingConfiguration' - Undocumented member.
--
-- * 'dsmrsResponseStatus' - -- | The response status code.
--
-- * 'dsmrsStateMachineARN' - The Amazon Resource Name (ARN) that identifies the state machine.
--
-- * 'dsmrsName' - The name of the state machine. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- * 'dsmrsDefinition' - The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- * 'dsmrsRoleARN' - The Amazon Resource Name (ARN) of the IAM role used when creating this state machine. (The IAM role maintains security by granting Step Functions access to AWS resources.)
--
-- * 'dsmrsType' - The @type@ of the state machine (@STANDARD@ or @EXPRESS@ ).
--
-- * 'dsmrsCreationDate' - The date the state machine is created.
describeStateMachineResponse ::
  -- | 'dsmrsResponseStatus'
  Int ->
  -- | 'dsmrsStateMachineARN'
  Text ->
  -- | 'dsmrsName'
  Text ->
  -- | 'dsmrsDefinition'
  Text ->
  -- | 'dsmrsRoleARN'
  Text ->
  -- | 'dsmrsType'
  StateMachineType ->
  -- | 'dsmrsCreationDate'
  UTCTime ->
  DescribeStateMachineResponse
describeStateMachineResponse
  pResponseStatus_
  pStateMachineARN_
  pName_
  pDefinition_
  pRoleARN_
  pType_
  pCreationDate_ =
    DescribeStateMachineResponse'
      { _dsmrsStatus = Nothing,
        _dsmrsTracingConfiguration = Nothing,
        _dsmrsLoggingConfiguration = Nothing,
        _dsmrsResponseStatus = pResponseStatus_,
        _dsmrsStateMachineARN = pStateMachineARN_,
        _dsmrsName = pName_,
        _dsmrsDefinition = _Sensitive # pDefinition_,
        _dsmrsRoleARN = pRoleARN_,
        _dsmrsType = pType_,
        _dsmrsCreationDate = _Time # pCreationDate_
      }

-- | The current status of the state machine.
dsmrsStatus :: Lens' DescribeStateMachineResponse (Maybe StateMachineStatus)
dsmrsStatus = lens _dsmrsStatus (\s a -> s {_dsmrsStatus = a})

-- | Selects whether AWS X-Ray tracing is enabled.
dsmrsTracingConfiguration :: Lens' DescribeStateMachineResponse (Maybe TracingConfiguration)
dsmrsTracingConfiguration = lens _dsmrsTracingConfiguration (\s a -> s {_dsmrsTracingConfiguration = a})

-- | Undocumented member.
dsmrsLoggingConfiguration :: Lens' DescribeStateMachineResponse (Maybe LoggingConfiguration)
dsmrsLoggingConfiguration = lens _dsmrsLoggingConfiguration (\s a -> s {_dsmrsLoggingConfiguration = a})

-- | -- | The response status code.
dsmrsResponseStatus :: Lens' DescribeStateMachineResponse Int
dsmrsResponseStatus = lens _dsmrsResponseStatus (\s a -> s {_dsmrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the state machine.
dsmrsStateMachineARN :: Lens' DescribeStateMachineResponse Text
dsmrsStateMachineARN = lens _dsmrsStateMachineARN (\s a -> s {_dsmrsStateMachineARN = a})

-- | The name of the state machine. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
dsmrsName :: Lens' DescribeStateMachineResponse Text
dsmrsName = lens _dsmrsName (\s a -> s {_dsmrsName = a})

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
dsmrsDefinition :: Lens' DescribeStateMachineResponse Text
dsmrsDefinition = lens _dsmrsDefinition (\s a -> s {_dsmrsDefinition = a}) . _Sensitive

-- | The Amazon Resource Name (ARN) of the IAM role used when creating this state machine. (The IAM role maintains security by granting Step Functions access to AWS resources.)
dsmrsRoleARN :: Lens' DescribeStateMachineResponse Text
dsmrsRoleARN = lens _dsmrsRoleARN (\s a -> s {_dsmrsRoleARN = a})

-- | The @type@ of the state machine (@STANDARD@ or @EXPRESS@ ).
dsmrsType :: Lens' DescribeStateMachineResponse StateMachineType
dsmrsType = lens _dsmrsType (\s a -> s {_dsmrsType = a})

-- | The date the state machine is created.
dsmrsCreationDate :: Lens' DescribeStateMachineResponse UTCTime
dsmrsCreationDate = lens _dsmrsCreationDate (\s a -> s {_dsmrsCreationDate = a}) . _Time

instance NFData DescribeStateMachineResponse
