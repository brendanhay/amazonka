{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @LambdaFunctionFailed@ event. It isn't set for other event types.
--
--
--
-- /See:/ 'lambdaFunctionFailedEventAttributes' smart constructor.
data LambdaFunctionFailedEventAttributes = LambdaFunctionFailedEventAttributes'
  { _lffeaReason ::
      !(Maybe Text),
    _lffeaDetails ::
      !(Maybe Text),
    _lffeaScheduledEventId ::
      !Integer,
    _lffeaStartedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lffeaReason' - The reason provided for the failure.
--
-- * 'lffeaDetails' - The details of the failure.
--
-- * 'lffeaScheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- * 'lffeaStartedEventId' - The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lambdaFunctionFailedEventAttributes ::
  -- | 'lffeaScheduledEventId'
  Integer ->
  -- | 'lffeaStartedEventId'
  Integer ->
  LambdaFunctionFailedEventAttributes
lambdaFunctionFailedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    LambdaFunctionFailedEventAttributes'
      { _lffeaReason = Nothing,
        _lffeaDetails = Nothing,
        _lffeaScheduledEventId = pScheduledEventId_,
        _lffeaStartedEventId = pStartedEventId_
      }

-- | The reason provided for the failure.
lffeaReason :: Lens' LambdaFunctionFailedEventAttributes (Maybe Text)
lffeaReason = lens _lffeaReason (\s a -> s {_lffeaReason = a})

-- | The details of the failure.
lffeaDetails :: Lens' LambdaFunctionFailedEventAttributes (Maybe Text)
lffeaDetails = lens _lffeaDetails (\s a -> s {_lffeaDetails = a})

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lffeaScheduledEventId :: Lens' LambdaFunctionFailedEventAttributes Integer
lffeaScheduledEventId = lens _lffeaScheduledEventId (\s a -> s {_lffeaScheduledEventId = a})

-- | The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lffeaStartedEventId :: Lens' LambdaFunctionFailedEventAttributes Integer
lffeaStartedEventId = lens _lffeaStartedEventId (\s a -> s {_lffeaStartedEventId = a})

instance FromJSON LambdaFunctionFailedEventAttributes where
  parseJSON =
    withObject
      "LambdaFunctionFailedEventAttributes"
      ( \x ->
          LambdaFunctionFailedEventAttributes'
            <$> (x .:? "reason")
            <*> (x .:? "details")
            <*> (x .: "scheduledEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable LambdaFunctionFailedEventAttributes

instance NFData LambdaFunctionFailedEventAttributes
