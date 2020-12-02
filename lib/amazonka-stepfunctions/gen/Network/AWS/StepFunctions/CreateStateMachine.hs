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
-- Module      : Network.AWS.StepFunctions.CreateStateMachine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a state machine. A state machine consists of a collection of states that can do work (@Task@ states), determine to which states to transition next (@Choice@ states), stop an execution with an error (@Fail@ states), and so on. State machines are specified using a JSON-based, structured language. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> in the AWS Step Functions User Guide.
module Network.AWS.StepFunctions.CreateStateMachine
  ( -- * Creating a Request
    createStateMachine,
    CreateStateMachine,

    -- * Request Lenses
    csmTracingConfiguration,
    csmType,
    csmLoggingConfiguration,
    csmTags,
    csmName,
    csmDefinition,
    csmRoleARN,

    -- * Destructuring the Response
    createStateMachineResponse,
    CreateStateMachineResponse,

    -- * Response Lenses
    csmrsResponseStatus,
    csmrsStateMachineARN,
    csmrsCreationDate,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'createStateMachine' smart constructor.
data CreateStateMachine = CreateStateMachine'
  { _csmTracingConfiguration ::
      !(Maybe TracingConfiguration),
    _csmType :: !(Maybe StateMachineType),
    _csmLoggingConfiguration ::
      !(Maybe LoggingConfiguration),
    _csmTags :: !(Maybe [Tag]),
    _csmName :: !Text,
    _csmDefinition :: !(Sensitive Text),
    _csmRoleARN :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateStateMachine' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmTracingConfiguration' - Selects whether AWS X-Ray tracing is enabled.
--
-- * 'csmType' - Determines whether a Standard or Express state machine is created. The default is @STANDARD@ . You cannot update the @type@ of a state machine once it has been created.
--
-- * 'csmLoggingConfiguration' - Defines what execution history events are logged and where they are logged.
--
-- * 'csmTags' - Tags to be added when creating a state machine. An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> . Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
--
-- * 'csmName' - The name of the state machine.  A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- * 'csmDefinition' - The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- * 'csmRoleARN' - The Amazon Resource Name (ARN) of the IAM role to use for this state machine.
createStateMachine ::
  -- | 'csmName'
  Text ->
  -- | 'csmDefinition'
  Text ->
  -- | 'csmRoleARN'
  Text ->
  CreateStateMachine
createStateMachine pName_ pDefinition_ pRoleARN_ =
  CreateStateMachine'
    { _csmTracingConfiguration = Nothing,
      _csmType = Nothing,
      _csmLoggingConfiguration = Nothing,
      _csmTags = Nothing,
      _csmName = pName_,
      _csmDefinition = _Sensitive # pDefinition_,
      _csmRoleARN = pRoleARN_
    }

-- | Selects whether AWS X-Ray tracing is enabled.
csmTracingConfiguration :: Lens' CreateStateMachine (Maybe TracingConfiguration)
csmTracingConfiguration = lens _csmTracingConfiguration (\s a -> s {_csmTracingConfiguration = a})

-- | Determines whether a Standard or Express state machine is created. The default is @STANDARD@ . You cannot update the @type@ of a state machine once it has been created.
csmType :: Lens' CreateStateMachine (Maybe StateMachineType)
csmType = lens _csmType (\s a -> s {_csmType = a})

-- | Defines what execution history events are logged and where they are logged.
csmLoggingConfiguration :: Lens' CreateStateMachine (Maybe LoggingConfiguration)
csmLoggingConfiguration = lens _csmLoggingConfiguration (\s a -> s {_csmLoggingConfiguration = a})

-- | Tags to be added when creating a state machine. An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> . Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
csmTags :: Lens' CreateStateMachine [Tag]
csmTags = lens _csmTags (\s a -> s {_csmTags = a}) . _Default . _Coerce

-- | The name of the state machine.  A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
csmName :: Lens' CreateStateMachine Text
csmName = lens _csmName (\s a -> s {_csmName = a})

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
csmDefinition :: Lens' CreateStateMachine Text
csmDefinition = lens _csmDefinition (\s a -> s {_csmDefinition = a}) . _Sensitive

-- | The Amazon Resource Name (ARN) of the IAM role to use for this state machine.
csmRoleARN :: Lens' CreateStateMachine Text
csmRoleARN = lens _csmRoleARN (\s a -> s {_csmRoleARN = a})

instance AWSRequest CreateStateMachine where
  type Rs CreateStateMachine = CreateStateMachineResponse
  request = postJSON stepFunctions
  response =
    receiveJSON
      ( \s h x ->
          CreateStateMachineResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "stateMachineArn")
            <*> (x .:> "creationDate")
      )

instance Hashable CreateStateMachine

instance NFData CreateStateMachine

instance ToHeaders CreateStateMachine where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSStepFunctions.CreateStateMachine" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON CreateStateMachine where
  toJSON CreateStateMachine' {..} =
    object
      ( catMaybes
          [ ("tracingConfiguration" .=) <$> _csmTracingConfiguration,
            ("type" .=) <$> _csmType,
            ("loggingConfiguration" .=) <$> _csmLoggingConfiguration,
            ("tags" .=) <$> _csmTags,
            Just ("name" .= _csmName),
            Just ("definition" .= _csmDefinition),
            Just ("roleArn" .= _csmRoleARN)
          ]
      )

instance ToPath CreateStateMachine where
  toPath = const "/"

instance ToQuery CreateStateMachine where
  toQuery = const mempty

-- | /See:/ 'createStateMachineResponse' smart constructor.
data CreateStateMachineResponse = CreateStateMachineResponse'
  { _csmrsResponseStatus ::
      !Int,
    _csmrsStateMachineARN :: !Text,
    _csmrsCreationDate :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateStateMachineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmrsResponseStatus' - -- | The response status code.
--
-- * 'csmrsStateMachineARN' - The Amazon Resource Name (ARN) that identifies the created state machine.
--
-- * 'csmrsCreationDate' - The date the state machine is created.
createStateMachineResponse ::
  -- | 'csmrsResponseStatus'
  Int ->
  -- | 'csmrsStateMachineARN'
  Text ->
  -- | 'csmrsCreationDate'
  UTCTime ->
  CreateStateMachineResponse
createStateMachineResponse
  pResponseStatus_
  pStateMachineARN_
  pCreationDate_ =
    CreateStateMachineResponse'
      { _csmrsResponseStatus =
          pResponseStatus_,
        _csmrsStateMachineARN = pStateMachineARN_,
        _csmrsCreationDate = _Time # pCreationDate_
      }

-- | -- | The response status code.
csmrsResponseStatus :: Lens' CreateStateMachineResponse Int
csmrsResponseStatus = lens _csmrsResponseStatus (\s a -> s {_csmrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the created state machine.
csmrsStateMachineARN :: Lens' CreateStateMachineResponse Text
csmrsStateMachineARN = lens _csmrsStateMachineARN (\s a -> s {_csmrsStateMachineARN = a})

-- | The date the state machine is created.
csmrsCreationDate :: Lens' CreateStateMachineResponse UTCTime
csmrsCreationDate = lens _csmrsCreationDate (\s a -> s {_csmrsCreationDate = a}) . _Time

instance NFData CreateStateMachineResponse
