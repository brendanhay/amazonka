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
-- Module      : Network.AWS.StepFunctions.UpdateStateMachine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing state machine by modifying its @definition@ , @roleArn@ , or @loggingConfiguration@ . Running executions will continue to use the previous @definition@ and @roleArn@ . You must include at least one of @definition@ or @roleArn@ or you will receive a @MissingRequiredParameter@ error.
module Network.AWS.StepFunctions.UpdateStateMachine
  ( -- * Creating a Request
    updateStateMachine,
    UpdateStateMachine,

    -- * Request Lenses
    usmDefinition,
    usmTracingConfiguration,
    usmLoggingConfiguration,
    usmRoleARN,
    usmStateMachineARN,

    -- * Destructuring the Response
    updateStateMachineResponse,
    UpdateStateMachineResponse,

    -- * Response Lenses
    usmrsResponseStatus,
    usmrsUpdateDate,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'updateStateMachine' smart constructor.
data UpdateStateMachine = UpdateStateMachine'
  { _usmDefinition ::
      !(Maybe (Sensitive Text)),
    _usmTracingConfiguration ::
      !(Maybe TracingConfiguration),
    _usmLoggingConfiguration ::
      !(Maybe LoggingConfiguration),
    _usmRoleARN :: !(Maybe Text),
    _usmStateMachineARN :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateStateMachine' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usmDefinition' - The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- * 'usmTracingConfiguration' - Selects whether AWS X-Ray tracing is enabled.
--
-- * 'usmLoggingConfiguration' - The @LoggingConfiguration@ data type is used to set CloudWatch Logs options.
--
-- * 'usmRoleARN' - The Amazon Resource Name (ARN) of the IAM role of the state machine.
--
-- * 'usmStateMachineARN' - The Amazon Resource Name (ARN) of the state machine.
updateStateMachine ::
  -- | 'usmStateMachineARN'
  Text ->
  UpdateStateMachine
updateStateMachine pStateMachineARN_ =
  UpdateStateMachine'
    { _usmDefinition = Nothing,
      _usmTracingConfiguration = Nothing,
      _usmLoggingConfiguration = Nothing,
      _usmRoleARN = Nothing,
      _usmStateMachineARN = pStateMachineARN_
    }

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
usmDefinition :: Lens' UpdateStateMachine (Maybe Text)
usmDefinition = lens _usmDefinition (\s a -> s {_usmDefinition = a}) . mapping _Sensitive

-- | Selects whether AWS X-Ray tracing is enabled.
usmTracingConfiguration :: Lens' UpdateStateMachine (Maybe TracingConfiguration)
usmTracingConfiguration = lens _usmTracingConfiguration (\s a -> s {_usmTracingConfiguration = a})

-- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs options.
usmLoggingConfiguration :: Lens' UpdateStateMachine (Maybe LoggingConfiguration)
usmLoggingConfiguration = lens _usmLoggingConfiguration (\s a -> s {_usmLoggingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the IAM role of the state machine.
usmRoleARN :: Lens' UpdateStateMachine (Maybe Text)
usmRoleARN = lens _usmRoleARN (\s a -> s {_usmRoleARN = a})

-- | The Amazon Resource Name (ARN) of the state machine.
usmStateMachineARN :: Lens' UpdateStateMachine Text
usmStateMachineARN = lens _usmStateMachineARN (\s a -> s {_usmStateMachineARN = a})

instance AWSRequest UpdateStateMachine where
  type Rs UpdateStateMachine = UpdateStateMachineResponse
  request = postJSON stepFunctions
  response =
    receiveJSON
      ( \s h x ->
          UpdateStateMachineResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "updateDate")
      )

instance Hashable UpdateStateMachine

instance NFData UpdateStateMachine

instance ToHeaders UpdateStateMachine where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSStepFunctions.UpdateStateMachine" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON UpdateStateMachine where
  toJSON UpdateStateMachine' {..} =
    object
      ( catMaybes
          [ ("definition" .=) <$> _usmDefinition,
            ("tracingConfiguration" .=) <$> _usmTracingConfiguration,
            ("loggingConfiguration" .=) <$> _usmLoggingConfiguration,
            ("roleArn" .=) <$> _usmRoleARN,
            Just ("stateMachineArn" .= _usmStateMachineARN)
          ]
      )

instance ToPath UpdateStateMachine where
  toPath = const "/"

instance ToQuery UpdateStateMachine where
  toQuery = const mempty

-- | /See:/ 'updateStateMachineResponse' smart constructor.
data UpdateStateMachineResponse = UpdateStateMachineResponse'
  { _usmrsResponseStatus ::
      !Int,
    _usmrsUpdateDate :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateStateMachineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usmrsResponseStatus' - -- | The response status code.
--
-- * 'usmrsUpdateDate' - The date and time the state machine was updated.
updateStateMachineResponse ::
  -- | 'usmrsResponseStatus'
  Int ->
  -- | 'usmrsUpdateDate'
  UTCTime ->
  UpdateStateMachineResponse
updateStateMachineResponse pResponseStatus_ pUpdateDate_ =
  UpdateStateMachineResponse'
    { _usmrsResponseStatus =
        pResponseStatus_,
      _usmrsUpdateDate = _Time # pUpdateDate_
    }

-- | -- | The response status code.
usmrsResponseStatus :: Lens' UpdateStateMachineResponse Int
usmrsResponseStatus = lens _usmrsResponseStatus (\s a -> s {_usmrsResponseStatus = a})

-- | The date and time the state machine was updated.
usmrsUpdateDate :: Lens' UpdateStateMachineResponse UTCTime
usmrsUpdateDate = lens _usmrsUpdateDate (\s a -> s {_usmrsUpdateDate = a}) . _Time

instance NFData UpdateStateMachineResponse
