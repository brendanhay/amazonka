{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionTimedOutEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.LambdaFunctionTimeoutType

-- | Provides details of the @LambdaFunctionTimedOut@ event.
--
--
--
-- /See:/ 'lambdaFunctionTimedOutEventAttributes' smart constructor.
data LambdaFunctionTimedOutEventAttributes = LambdaFunctionTimedOutEventAttributes'
  { _lftoeaTimeoutType ::
      !( Maybe
           LambdaFunctionTimeoutType
       ),
    _lftoeaScheduledEventId ::
      !Integer,
    _lftoeaStartedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lftoeaTimeoutType' - The type of the timeout that caused this event.
--
-- * 'lftoeaScheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- * 'lftoeaStartedEventId' - The ID of the @ActivityTaskStarted@ event that was recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lambdaFunctionTimedOutEventAttributes ::
  -- | 'lftoeaScheduledEventId'
  Integer ->
  -- | 'lftoeaStartedEventId'
  Integer ->
  LambdaFunctionTimedOutEventAttributes
lambdaFunctionTimedOutEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    LambdaFunctionTimedOutEventAttributes'
      { _lftoeaTimeoutType =
          Nothing,
        _lftoeaScheduledEventId = pScheduledEventId_,
        _lftoeaStartedEventId = pStartedEventId_
      }

-- | The type of the timeout that caused this event.
lftoeaTimeoutType :: Lens' LambdaFunctionTimedOutEventAttributes (Maybe LambdaFunctionTimeoutType)
lftoeaTimeoutType = lens _lftoeaTimeoutType (\s a -> s {_lftoeaTimeoutType = a})

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lftoeaScheduledEventId :: Lens' LambdaFunctionTimedOutEventAttributes Integer
lftoeaScheduledEventId = lens _lftoeaScheduledEventId (\s a -> s {_lftoeaScheduledEventId = a})

-- | The ID of the @ActivityTaskStarted@ event that was recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lftoeaStartedEventId :: Lens' LambdaFunctionTimedOutEventAttributes Integer
lftoeaStartedEventId = lens _lftoeaStartedEventId (\s a -> s {_lftoeaStartedEventId = a})

instance FromJSON LambdaFunctionTimedOutEventAttributes where
  parseJSON =
    withObject
      "LambdaFunctionTimedOutEventAttributes"
      ( \x ->
          LambdaFunctionTimedOutEventAttributes'
            <$> (x .:? "timeoutType")
            <*> (x .: "scheduledEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable LambdaFunctionTimedOutEventAttributes

instance NFData LambdaFunctionTimedOutEventAttributes
