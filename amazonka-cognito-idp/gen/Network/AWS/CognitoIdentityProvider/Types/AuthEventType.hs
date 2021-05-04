{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AuthEventType
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The authentication event type.
--
-- /See:/ 'newAuthEventType' smart constructor.
data AuthEventType = AuthEventType'
  { -- | The event type.
    eventType :: Prelude.Maybe EventType,
    -- | The event ID.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The challenge responses.
    challengeResponses :: Prelude.Maybe [ChallengeResponseType],
    -- | The user context data captured at the time of an event request. It
    -- provides additional information about the client from which event the
    -- request is received.
    eventContextData :: Prelude.Maybe EventContextDataType,
    -- | The creation date
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The event risk.
    eventRisk :: Prelude.Maybe EventRiskType,
    -- | The event response.
    eventResponse :: Prelude.Maybe EventResponseType,
    -- | A flag specifying the user feedback captured at the time of an event
    -- request is good or bad.
    eventFeedback :: Prelude.Maybe EventFeedbackType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AuthEventType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventType', 'authEventType_eventType' - The event type.
--
-- 'eventId', 'authEventType_eventId' - The event ID.
--
-- 'challengeResponses', 'authEventType_challengeResponses' - The challenge responses.
--
-- 'eventContextData', 'authEventType_eventContextData' - The user context data captured at the time of an event request. It
-- provides additional information about the client from which event the
-- request is received.
--
-- 'creationDate', 'authEventType_creationDate' - The creation date
--
-- 'eventRisk', 'authEventType_eventRisk' - The event risk.
--
-- 'eventResponse', 'authEventType_eventResponse' - The event response.
--
-- 'eventFeedback', 'authEventType_eventFeedback' - A flag specifying the user feedback captured at the time of an event
-- request is good or bad.
newAuthEventType ::
  AuthEventType
newAuthEventType =
  AuthEventType'
    { eventType = Prelude.Nothing,
      eventId = Prelude.Nothing,
      challengeResponses = Prelude.Nothing,
      eventContextData = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      eventRisk = Prelude.Nothing,
      eventResponse = Prelude.Nothing,
      eventFeedback = Prelude.Nothing
    }

-- | The event type.
authEventType_eventType :: Lens.Lens' AuthEventType (Prelude.Maybe EventType)
authEventType_eventType = Lens.lens (\AuthEventType' {eventType} -> eventType) (\s@AuthEventType' {} a -> s {eventType = a} :: AuthEventType)

-- | The event ID.
authEventType_eventId :: Lens.Lens' AuthEventType (Prelude.Maybe Prelude.Text)
authEventType_eventId = Lens.lens (\AuthEventType' {eventId} -> eventId) (\s@AuthEventType' {} a -> s {eventId = a} :: AuthEventType)

-- | The challenge responses.
authEventType_challengeResponses :: Lens.Lens' AuthEventType (Prelude.Maybe [ChallengeResponseType])
authEventType_challengeResponses = Lens.lens (\AuthEventType' {challengeResponses} -> challengeResponses) (\s@AuthEventType' {} a -> s {challengeResponses = a} :: AuthEventType) Prelude.. Lens.mapping Prelude._Coerce

-- | The user context data captured at the time of an event request. It
-- provides additional information about the client from which event the
-- request is received.
authEventType_eventContextData :: Lens.Lens' AuthEventType (Prelude.Maybe EventContextDataType)
authEventType_eventContextData = Lens.lens (\AuthEventType' {eventContextData} -> eventContextData) (\s@AuthEventType' {} a -> s {eventContextData = a} :: AuthEventType)

-- | The creation date
authEventType_creationDate :: Lens.Lens' AuthEventType (Prelude.Maybe Prelude.UTCTime)
authEventType_creationDate = Lens.lens (\AuthEventType' {creationDate} -> creationDate) (\s@AuthEventType' {} a -> s {creationDate = a} :: AuthEventType) Prelude.. Lens.mapping Prelude._Time

-- | The event risk.
authEventType_eventRisk :: Lens.Lens' AuthEventType (Prelude.Maybe EventRiskType)
authEventType_eventRisk = Lens.lens (\AuthEventType' {eventRisk} -> eventRisk) (\s@AuthEventType' {} a -> s {eventRisk = a} :: AuthEventType)

-- | The event response.
authEventType_eventResponse :: Lens.Lens' AuthEventType (Prelude.Maybe EventResponseType)
authEventType_eventResponse = Lens.lens (\AuthEventType' {eventResponse} -> eventResponse) (\s@AuthEventType' {} a -> s {eventResponse = a} :: AuthEventType)

-- | A flag specifying the user feedback captured at the time of an event
-- request is good or bad.
authEventType_eventFeedback :: Lens.Lens' AuthEventType (Prelude.Maybe EventFeedbackType)
authEventType_eventFeedback = Lens.lens (\AuthEventType' {eventFeedback} -> eventFeedback) (\s@AuthEventType' {} a -> s {eventFeedback = a} :: AuthEventType)

instance Prelude.FromJSON AuthEventType where
  parseJSON =
    Prelude.withObject
      "AuthEventType"
      ( \x ->
          AuthEventType'
            Prelude.<$> (x Prelude..:? "EventType")
            Prelude.<*> (x Prelude..:? "EventId")
            Prelude.<*> ( x Prelude..:? "ChallengeResponses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "EventContextData")
            Prelude.<*> (x Prelude..:? "CreationDate")
            Prelude.<*> (x Prelude..:? "EventRisk")
            Prelude.<*> (x Prelude..:? "EventResponse")
            Prelude.<*> (x Prelude..:? "EventFeedback")
      )

instance Prelude.Hashable AuthEventType

instance Prelude.NFData AuthEventType
