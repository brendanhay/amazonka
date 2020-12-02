{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StepFunctionsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StepFunctionsAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Starts execution of a Step Functions state machine.
--
--
--
-- /See:/ 'stepFunctionsAction' smart constructor.
data StepFunctionsAction = StepFunctionsAction'
  { _sfaExecutionNamePrefix ::
      !(Maybe Text),
    _sfaStateMachineName :: !Text,
    _sfaRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StepFunctionsAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfaExecutionNamePrefix' - (Optional) A name will be given to the state machine execution consisting of this prefix followed by a UUID. Step Functions automatically creates a unique name for each state machine execution if one is not provided.
--
-- * 'sfaStateMachineName' - The name of the Step Functions state machine whose execution will be started.
--
-- * 'sfaRoleARN' - The ARN of the role that grants IoT permission to start execution of a state machine ("Action":"states:StartExecution").
stepFunctionsAction ::
  -- | 'sfaStateMachineName'
  Text ->
  -- | 'sfaRoleARN'
  Text ->
  StepFunctionsAction
stepFunctionsAction pStateMachineName_ pRoleARN_ =
  StepFunctionsAction'
    { _sfaExecutionNamePrefix = Nothing,
      _sfaStateMachineName = pStateMachineName_,
      _sfaRoleARN = pRoleARN_
    }

-- | (Optional) A name will be given to the state machine execution consisting of this prefix followed by a UUID. Step Functions automatically creates a unique name for each state machine execution if one is not provided.
sfaExecutionNamePrefix :: Lens' StepFunctionsAction (Maybe Text)
sfaExecutionNamePrefix = lens _sfaExecutionNamePrefix (\s a -> s {_sfaExecutionNamePrefix = a})

-- | The name of the Step Functions state machine whose execution will be started.
sfaStateMachineName :: Lens' StepFunctionsAction Text
sfaStateMachineName = lens _sfaStateMachineName (\s a -> s {_sfaStateMachineName = a})

-- | The ARN of the role that grants IoT permission to start execution of a state machine ("Action":"states:StartExecution").
sfaRoleARN :: Lens' StepFunctionsAction Text
sfaRoleARN = lens _sfaRoleARN (\s a -> s {_sfaRoleARN = a})

instance FromJSON StepFunctionsAction where
  parseJSON =
    withObject
      "StepFunctionsAction"
      ( \x ->
          StepFunctionsAction'
            <$> (x .:? "executionNamePrefix")
            <*> (x .: "stateMachineName")
            <*> (x .: "roleArn")
      )

instance Hashable StepFunctionsAction

instance NFData StepFunctionsAction

instance ToJSON StepFunctionsAction where
  toJSON StepFunctionsAction' {..} =
    object
      ( catMaybes
          [ ("executionNamePrefix" .=) <$> _sfaExecutionNamePrefix,
            Just ("stateMachineName" .= _sfaStateMachineName),
            Just ("roleArn" .= _sfaRoleARN)
          ]
      )
