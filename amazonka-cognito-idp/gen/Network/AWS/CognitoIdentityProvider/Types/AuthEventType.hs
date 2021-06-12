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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The authentication event type.
--
-- /See:/ 'newAuthEventType' smart constructor.
data AuthEventType = AuthEventType'
  { -- | The event type.
    eventType :: Core.Maybe EventType,
    -- | The event ID.
    eventId :: Core.Maybe Core.Text,
    -- | The challenge responses.
    challengeResponses :: Core.Maybe [ChallengeResponseType],
    -- | The user context data captured at the time of an event request. It
    -- provides additional information about the client from which event the
    -- request is received.
    eventContextData :: Core.Maybe EventContextDataType,
    -- | The creation date
    creationDate :: Core.Maybe Core.POSIX,
    -- | The event risk.
    eventRisk :: Core.Maybe EventRiskType,
    -- | The event response.
    eventResponse :: Core.Maybe EventResponseType,
    -- | A flag specifying the user feedback captured at the time of an event
    -- request is good or bad.
    eventFeedback :: Core.Maybe EventFeedbackType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { eventType = Core.Nothing,
      eventId = Core.Nothing,
      challengeResponses = Core.Nothing,
      eventContextData = Core.Nothing,
      creationDate = Core.Nothing,
      eventRisk = Core.Nothing,
      eventResponse = Core.Nothing,
      eventFeedback = Core.Nothing
    }

-- | The event type.
authEventType_eventType :: Lens.Lens' AuthEventType (Core.Maybe EventType)
authEventType_eventType = Lens.lens (\AuthEventType' {eventType} -> eventType) (\s@AuthEventType' {} a -> s {eventType = a} :: AuthEventType)

-- | The event ID.
authEventType_eventId :: Lens.Lens' AuthEventType (Core.Maybe Core.Text)
authEventType_eventId = Lens.lens (\AuthEventType' {eventId} -> eventId) (\s@AuthEventType' {} a -> s {eventId = a} :: AuthEventType)

-- | The challenge responses.
authEventType_challengeResponses :: Lens.Lens' AuthEventType (Core.Maybe [ChallengeResponseType])
authEventType_challengeResponses = Lens.lens (\AuthEventType' {challengeResponses} -> challengeResponses) (\s@AuthEventType' {} a -> s {challengeResponses = a} :: AuthEventType) Core.. Lens.mapping Lens._Coerce

-- | The user context data captured at the time of an event request. It
-- provides additional information about the client from which event the
-- request is received.
authEventType_eventContextData :: Lens.Lens' AuthEventType (Core.Maybe EventContextDataType)
authEventType_eventContextData = Lens.lens (\AuthEventType' {eventContextData} -> eventContextData) (\s@AuthEventType' {} a -> s {eventContextData = a} :: AuthEventType)

-- | The creation date
authEventType_creationDate :: Lens.Lens' AuthEventType (Core.Maybe Core.UTCTime)
authEventType_creationDate = Lens.lens (\AuthEventType' {creationDate} -> creationDate) (\s@AuthEventType' {} a -> s {creationDate = a} :: AuthEventType) Core.. Lens.mapping Core._Time

-- | The event risk.
authEventType_eventRisk :: Lens.Lens' AuthEventType (Core.Maybe EventRiskType)
authEventType_eventRisk = Lens.lens (\AuthEventType' {eventRisk} -> eventRisk) (\s@AuthEventType' {} a -> s {eventRisk = a} :: AuthEventType)

-- | The event response.
authEventType_eventResponse :: Lens.Lens' AuthEventType (Core.Maybe EventResponseType)
authEventType_eventResponse = Lens.lens (\AuthEventType' {eventResponse} -> eventResponse) (\s@AuthEventType' {} a -> s {eventResponse = a} :: AuthEventType)

-- | A flag specifying the user feedback captured at the time of an event
-- request is good or bad.
authEventType_eventFeedback :: Lens.Lens' AuthEventType (Core.Maybe EventFeedbackType)
authEventType_eventFeedback = Lens.lens (\AuthEventType' {eventFeedback} -> eventFeedback) (\s@AuthEventType' {} a -> s {eventFeedback = a} :: AuthEventType)

instance Core.FromJSON AuthEventType where
  parseJSON =
    Core.withObject
      "AuthEventType"
      ( \x ->
          AuthEventType'
            Core.<$> (x Core..:? "EventType")
            Core.<*> (x Core..:? "EventId")
            Core.<*> ( x Core..:? "ChallengeResponses"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "EventContextData")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "EventRisk")
            Core.<*> (x Core..:? "EventResponse")
            Core.<*> (x Core..:? "EventFeedback")
      )

instance Core.Hashable AuthEventType

instance Core.NFData AuthEventType
