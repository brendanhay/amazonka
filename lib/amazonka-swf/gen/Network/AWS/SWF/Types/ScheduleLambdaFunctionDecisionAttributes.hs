{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Decision attributes specified in @scheduleLambdaFunctionDecisionAttributes@ within the list of decisions @decisions@ passed to 'RespondDecisionTaskCompleted' .
--
--
--
-- /See:/ 'scheduleLambdaFunctionDecisionAttributes' smart constructor.
data ScheduleLambdaFunctionDecisionAttributes = ScheduleLambdaFunctionDecisionAttributes'
  { _slfdaControl ::
      !( Maybe
           Text
       ),
    _slfdaInput ::
      !( Maybe
           Text
       ),
    _slfdaStartToCloseTimeout ::
      !( Maybe
           Text
       ),
    _slfdaId ::
      !Text,
    _slfdaName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduleLambdaFunctionDecisionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slfdaControl' - The data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
--
-- * 'slfdaInput' - The optional input data to be supplied to the Lambda function.
--
-- * 'slfdaStartToCloseTimeout' - The timeout value, in seconds, after which the Lambda function is considered to be failed once it has started. This can be any integer from 1-300 (1s-5m). If no value is supplied, than a default value of 300s is assumed.
--
-- * 'slfdaId' - A string that identifies the Lambda function execution in the event history.
--
-- * 'slfdaName' - The name, or ARN, of the Lambda function to schedule.
scheduleLambdaFunctionDecisionAttributes ::
  -- | 'slfdaId'
  Text ->
  -- | 'slfdaName'
  Text ->
  ScheduleLambdaFunctionDecisionAttributes
scheduleLambdaFunctionDecisionAttributes pId_ pName_ =
  ScheduleLambdaFunctionDecisionAttributes'
    { _slfdaControl =
        Nothing,
      _slfdaInput = Nothing,
      _slfdaStartToCloseTimeout = Nothing,
      _slfdaId = pId_,
      _slfdaName = pName_
    }

-- | The data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
slfdaControl :: Lens' ScheduleLambdaFunctionDecisionAttributes (Maybe Text)
slfdaControl = lens _slfdaControl (\s a -> s {_slfdaControl = a})

-- | The optional input data to be supplied to the Lambda function.
slfdaInput :: Lens' ScheduleLambdaFunctionDecisionAttributes (Maybe Text)
slfdaInput = lens _slfdaInput (\s a -> s {_slfdaInput = a})

-- | The timeout value, in seconds, after which the Lambda function is considered to be failed once it has started. This can be any integer from 1-300 (1s-5m). If no value is supplied, than a default value of 300s is assumed.
slfdaStartToCloseTimeout :: Lens' ScheduleLambdaFunctionDecisionAttributes (Maybe Text)
slfdaStartToCloseTimeout = lens _slfdaStartToCloseTimeout (\s a -> s {_slfdaStartToCloseTimeout = a})

-- | A string that identifies the Lambda function execution in the event history.
slfdaId :: Lens' ScheduleLambdaFunctionDecisionAttributes Text
slfdaId = lens _slfdaId (\s a -> s {_slfdaId = a})

-- | The name, or ARN, of the Lambda function to schedule.
slfdaName :: Lens' ScheduleLambdaFunctionDecisionAttributes Text
slfdaName = lens _slfdaName (\s a -> s {_slfdaName = a})

instance Hashable ScheduleLambdaFunctionDecisionAttributes

instance NFData ScheduleLambdaFunctionDecisionAttributes

instance ToJSON ScheduleLambdaFunctionDecisionAttributes where
  toJSON ScheduleLambdaFunctionDecisionAttributes' {..} =
    object
      ( catMaybes
          [ ("control" .=) <$> _slfdaControl,
            ("input" .=) <$> _slfdaInput,
            ("startToCloseTimeout" .=) <$> _slfdaStartToCloseTimeout,
            Just ("id" .= _slfdaId),
            Just ("name" .= _slfdaName)
          ]
      )
