{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @LambdaFunctionCompleted@ event. It isn't set for other event types.
--
--
--
-- /See:/ 'lambdaFunctionCompletedEventAttributes' smart constructor.
data LambdaFunctionCompletedEventAttributes = LambdaFunctionCompletedEventAttributes'
  { _lfceaResult ::
      !(Maybe Text),
    _lfceaScheduledEventId ::
      !Integer,
    _lfceaStartedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionCompletedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfceaResult' - The results of the Lambda task.
--
-- * 'lfceaScheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when this Lambda task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- * 'lfceaStartedEventId' - The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lambdaFunctionCompletedEventAttributes ::
  -- | 'lfceaScheduledEventId'
  Integer ->
  -- | 'lfceaStartedEventId'
  Integer ->
  LambdaFunctionCompletedEventAttributes
lambdaFunctionCompletedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    LambdaFunctionCompletedEventAttributes'
      { _lfceaResult = Nothing,
        _lfceaScheduledEventId = pScheduledEventId_,
        _lfceaStartedEventId = pStartedEventId_
      }

-- | The results of the Lambda task.
lfceaResult :: Lens' LambdaFunctionCompletedEventAttributes (Maybe Text)
lfceaResult = lens _lfceaResult (\s a -> s {_lfceaResult = a})

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this Lambda task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lfceaScheduledEventId :: Lens' LambdaFunctionCompletedEventAttributes Integer
lfceaScheduledEventId = lens _lfceaScheduledEventId (\s a -> s {_lfceaScheduledEventId = a})

-- | The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lfceaStartedEventId :: Lens' LambdaFunctionCompletedEventAttributes Integer
lfceaStartedEventId = lens _lfceaStartedEventId (\s a -> s {_lfceaStartedEventId = a})

instance FromJSON LambdaFunctionCompletedEventAttributes where
  parseJSON =
    withObject
      "LambdaFunctionCompletedEventAttributes"
      ( \x ->
          LambdaFunctionCompletedEventAttributes'
            <$> (x .:? "result")
            <*> (x .: "scheduledEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable LambdaFunctionCompletedEventAttributes

instance NFData LambdaFunctionCompletedEventAttributes
