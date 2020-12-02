{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @LambdaFunctionScheduled@ event. It isn't set for other event types.
--
--
--
-- /See:/ 'lambdaFunctionScheduledEventAttributes' smart constructor.
data LambdaFunctionScheduledEventAttributes = LambdaFunctionScheduledEventAttributes'
  { _lfseaControl ::
      !(Maybe Text),
    _lfseaInput ::
      !(Maybe Text),
    _lfseaStartToCloseTimeout ::
      !(Maybe Text),
    _lfseaId ::
      !Text,
    _lfseaName ::
      !Text,
    _lfseaDecisionTaskCompletedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionScheduledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfseaControl' - Data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
--
-- * 'lfseaInput' - The input provided to the Lambda task.
--
-- * 'lfseaStartToCloseTimeout' - The maximum amount of time a worker can take to process the Lambda task.
--
-- * 'lfseaId' - The unique ID of the Lambda task.
--
-- * 'lfseaName' - The name of the Lambda function.
--
-- * 'lfseaDecisionTaskCompletedEventId' - The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this activity task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lambdaFunctionScheduledEventAttributes ::
  -- | 'lfseaId'
  Text ->
  -- | 'lfseaName'
  Text ->
  -- | 'lfseaDecisionTaskCompletedEventId'
  Integer ->
  LambdaFunctionScheduledEventAttributes
lambdaFunctionScheduledEventAttributes
  pId_
  pName_
  pDecisionTaskCompletedEventId_ =
    LambdaFunctionScheduledEventAttributes'
      { _lfseaControl = Nothing,
        _lfseaInput = Nothing,
        _lfseaStartToCloseTimeout = Nothing,
        _lfseaId = pId_,
        _lfseaName = pName_,
        _lfseaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
lfseaControl :: Lens' LambdaFunctionScheduledEventAttributes (Maybe Text)
lfseaControl = lens _lfseaControl (\s a -> s {_lfseaControl = a})

-- | The input provided to the Lambda task.
lfseaInput :: Lens' LambdaFunctionScheduledEventAttributes (Maybe Text)
lfseaInput = lens _lfseaInput (\s a -> s {_lfseaInput = a})

-- | The maximum amount of time a worker can take to process the Lambda task.
lfseaStartToCloseTimeout :: Lens' LambdaFunctionScheduledEventAttributes (Maybe Text)
lfseaStartToCloseTimeout = lens _lfseaStartToCloseTimeout (\s a -> s {_lfseaStartToCloseTimeout = a})

-- | The unique ID of the Lambda task.
lfseaId :: Lens' LambdaFunctionScheduledEventAttributes Text
lfseaId = lens _lfseaId (\s a -> s {_lfseaId = a})

-- | The name of the Lambda function.
lfseaName :: Lens' LambdaFunctionScheduledEventAttributes Text
lfseaName = lens _lfseaName (\s a -> s {_lfseaName = a})

-- | The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this activity task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lfseaDecisionTaskCompletedEventId :: Lens' LambdaFunctionScheduledEventAttributes Integer
lfseaDecisionTaskCompletedEventId = lens _lfseaDecisionTaskCompletedEventId (\s a -> s {_lfseaDecisionTaskCompletedEventId = a})

instance FromJSON LambdaFunctionScheduledEventAttributes where
  parseJSON =
    withObject
      "LambdaFunctionScheduledEventAttributes"
      ( \x ->
          LambdaFunctionScheduledEventAttributes'
            <$> (x .:? "control")
            <*> (x .:? "input")
            <*> (x .:? "startToCloseTimeout")
            <*> (x .: "id")
            <*> (x .: "name")
            <*> (x .: "decisionTaskCompletedEventId")
      )

instance Hashable LambdaFunctionScheduledEventAttributes

instance NFData LambdaFunctionScheduledEventAttributes
