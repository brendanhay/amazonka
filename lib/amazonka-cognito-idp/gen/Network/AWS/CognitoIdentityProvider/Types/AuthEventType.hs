{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AuthEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AuthEventType where

import Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType
import Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
import Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType
import Network.AWS.CognitoIdentityProvider.Types.EventResponseType
import Network.AWS.CognitoIdentityProvider.Types.EventRiskType
import Network.AWS.CognitoIdentityProvider.Types.EventType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The authentication event type.
--
--
--
-- /See:/ 'authEventType' smart constructor.
data AuthEventType = AuthEventType'
  { _aetEventRisk ::
      !(Maybe EventRiskType),
    _aetEventResponse :: !(Maybe EventResponseType),
    _aetEventContextData :: !(Maybe EventContextDataType),
    _aetChallengeResponses :: !(Maybe [ChallengeResponseType]),
    _aetEventType :: !(Maybe EventType),
    _aetCreationDate :: !(Maybe POSIX),
    _aetEventFeedback :: !(Maybe EventFeedbackType),
    _aetEventId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuthEventType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aetEventRisk' - The event risk.
--
-- * 'aetEventResponse' - The event response.
--
-- * 'aetEventContextData' - The user context data captured at the time of an event request. It provides additional information about the client from which event the request is received.
--
-- * 'aetChallengeResponses' - The challenge responses.
--
-- * 'aetEventType' - The event type.
--
-- * 'aetCreationDate' - The creation date
--
-- * 'aetEventFeedback' - A flag specifying the user feedback captured at the time of an event request is good or bad.
--
-- * 'aetEventId' - The event ID.
authEventType ::
  AuthEventType
authEventType =
  AuthEventType'
    { _aetEventRisk = Nothing,
      _aetEventResponse = Nothing,
      _aetEventContextData = Nothing,
      _aetChallengeResponses = Nothing,
      _aetEventType = Nothing,
      _aetCreationDate = Nothing,
      _aetEventFeedback = Nothing,
      _aetEventId = Nothing
    }

-- | The event risk.
aetEventRisk :: Lens' AuthEventType (Maybe EventRiskType)
aetEventRisk = lens _aetEventRisk (\s a -> s {_aetEventRisk = a})

-- | The event response.
aetEventResponse :: Lens' AuthEventType (Maybe EventResponseType)
aetEventResponse = lens _aetEventResponse (\s a -> s {_aetEventResponse = a})

-- | The user context data captured at the time of an event request. It provides additional information about the client from which event the request is received.
aetEventContextData :: Lens' AuthEventType (Maybe EventContextDataType)
aetEventContextData = lens _aetEventContextData (\s a -> s {_aetEventContextData = a})

-- | The challenge responses.
aetChallengeResponses :: Lens' AuthEventType [ChallengeResponseType]
aetChallengeResponses = lens _aetChallengeResponses (\s a -> s {_aetChallengeResponses = a}) . _Default . _Coerce

-- | The event type.
aetEventType :: Lens' AuthEventType (Maybe EventType)
aetEventType = lens _aetEventType (\s a -> s {_aetEventType = a})

-- | The creation date
aetCreationDate :: Lens' AuthEventType (Maybe UTCTime)
aetCreationDate = lens _aetCreationDate (\s a -> s {_aetCreationDate = a}) . mapping _Time

-- | A flag specifying the user feedback captured at the time of an event request is good or bad.
aetEventFeedback :: Lens' AuthEventType (Maybe EventFeedbackType)
aetEventFeedback = lens _aetEventFeedback (\s a -> s {_aetEventFeedback = a})

-- | The event ID.
aetEventId :: Lens' AuthEventType (Maybe Text)
aetEventId = lens _aetEventId (\s a -> s {_aetEventId = a})

instance FromJSON AuthEventType where
  parseJSON =
    withObject
      "AuthEventType"
      ( \x ->
          AuthEventType'
            <$> (x .:? "EventRisk")
            <*> (x .:? "EventResponse")
            <*> (x .:? "EventContextData")
            <*> (x .:? "ChallengeResponses" .!= mempty)
            <*> (x .:? "EventType")
            <*> (x .:? "CreationDate")
            <*> (x .:? "EventFeedback")
            <*> (x .:? "EventId")
      )

instance Hashable AuthEventType

instance NFData AuthEventType
