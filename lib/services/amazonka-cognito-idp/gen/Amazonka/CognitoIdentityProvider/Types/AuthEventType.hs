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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AuthEventType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AuthEventType where

import Amazonka.CognitoIdentityProvider.Types.ChallengeResponseType
import Amazonka.CognitoIdentityProvider.Types.EventContextDataType
import Amazonka.CognitoIdentityProvider.Types.EventFeedbackType
import Amazonka.CognitoIdentityProvider.Types.EventResponseType
import Amazonka.CognitoIdentityProvider.Types.EventRiskType
import Amazonka.CognitoIdentityProvider.Types.EventType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The authentication event type.
--
-- /See:/ 'newAuthEventType' smart constructor.
data AuthEventType = AuthEventType'
  { -- | The event type.
    eventType :: Prelude.Maybe EventType,
    -- | The event response.
    eventResponse :: Prelude.Maybe EventResponseType,
    -- | The user context data captured at the time of an event request. This
    -- value provides additional information about the client from which event
    -- the request is received.
    eventContextData :: Prelude.Maybe EventContextDataType,
    -- | The creation date
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The event risk.
    eventRisk :: Prelude.Maybe EventRiskType,
    -- | The event ID.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | A flag specifying the user feedback captured at the time of an event
    -- request is good or bad.
    eventFeedback :: Prelude.Maybe EventFeedbackType,
    -- | The challenge responses.
    challengeResponses :: Prelude.Maybe [ChallengeResponseType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'eventResponse', 'authEventType_eventResponse' - The event response.
--
-- 'eventContextData', 'authEventType_eventContextData' - The user context data captured at the time of an event request. This
-- value provides additional information about the client from which event
-- the request is received.
--
-- 'creationDate', 'authEventType_creationDate' - The creation date
--
-- 'eventRisk', 'authEventType_eventRisk' - The event risk.
--
-- 'eventId', 'authEventType_eventId' - The event ID.
--
-- 'eventFeedback', 'authEventType_eventFeedback' - A flag specifying the user feedback captured at the time of an event
-- request is good or bad.
--
-- 'challengeResponses', 'authEventType_challengeResponses' - The challenge responses.
newAuthEventType ::
  AuthEventType
newAuthEventType =
  AuthEventType'
    { eventType = Prelude.Nothing,
      eventResponse = Prelude.Nothing,
      eventContextData = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      eventRisk = Prelude.Nothing,
      eventId = Prelude.Nothing,
      eventFeedback = Prelude.Nothing,
      challengeResponses = Prelude.Nothing
    }

-- | The event type.
authEventType_eventType :: Lens.Lens' AuthEventType (Prelude.Maybe EventType)
authEventType_eventType = Lens.lens (\AuthEventType' {eventType} -> eventType) (\s@AuthEventType' {} a -> s {eventType = a} :: AuthEventType)

-- | The event response.
authEventType_eventResponse :: Lens.Lens' AuthEventType (Prelude.Maybe EventResponseType)
authEventType_eventResponse = Lens.lens (\AuthEventType' {eventResponse} -> eventResponse) (\s@AuthEventType' {} a -> s {eventResponse = a} :: AuthEventType)

-- | The user context data captured at the time of an event request. This
-- value provides additional information about the client from which event
-- the request is received.
authEventType_eventContextData :: Lens.Lens' AuthEventType (Prelude.Maybe EventContextDataType)
authEventType_eventContextData = Lens.lens (\AuthEventType' {eventContextData} -> eventContextData) (\s@AuthEventType' {} a -> s {eventContextData = a} :: AuthEventType)

-- | The creation date
authEventType_creationDate :: Lens.Lens' AuthEventType (Prelude.Maybe Prelude.UTCTime)
authEventType_creationDate = Lens.lens (\AuthEventType' {creationDate} -> creationDate) (\s@AuthEventType' {} a -> s {creationDate = a} :: AuthEventType) Prelude.. Lens.mapping Core._Time

-- | The event risk.
authEventType_eventRisk :: Lens.Lens' AuthEventType (Prelude.Maybe EventRiskType)
authEventType_eventRisk = Lens.lens (\AuthEventType' {eventRisk} -> eventRisk) (\s@AuthEventType' {} a -> s {eventRisk = a} :: AuthEventType)

-- | The event ID.
authEventType_eventId :: Lens.Lens' AuthEventType (Prelude.Maybe Prelude.Text)
authEventType_eventId = Lens.lens (\AuthEventType' {eventId} -> eventId) (\s@AuthEventType' {} a -> s {eventId = a} :: AuthEventType)

-- | A flag specifying the user feedback captured at the time of an event
-- request is good or bad.
authEventType_eventFeedback :: Lens.Lens' AuthEventType (Prelude.Maybe EventFeedbackType)
authEventType_eventFeedback = Lens.lens (\AuthEventType' {eventFeedback} -> eventFeedback) (\s@AuthEventType' {} a -> s {eventFeedback = a} :: AuthEventType)

-- | The challenge responses.
authEventType_challengeResponses :: Lens.Lens' AuthEventType (Prelude.Maybe [ChallengeResponseType])
authEventType_challengeResponses = Lens.lens (\AuthEventType' {challengeResponses} -> challengeResponses) (\s@AuthEventType' {} a -> s {challengeResponses = a} :: AuthEventType) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AuthEventType where
  parseJSON =
    Core.withObject
      "AuthEventType"
      ( \x ->
          AuthEventType'
            Prelude.<$> (x Core..:? "EventType")
            Prelude.<*> (x Core..:? "EventResponse")
            Prelude.<*> (x Core..:? "EventContextData")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "EventRisk")
            Prelude.<*> (x Core..:? "EventId")
            Prelude.<*> (x Core..:? "EventFeedback")
            Prelude.<*> ( x Core..:? "ChallengeResponses"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AuthEventType where
  hashWithSalt _salt AuthEventType' {..} =
    _salt `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` eventResponse
      `Prelude.hashWithSalt` eventContextData
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` eventRisk
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventFeedback
      `Prelude.hashWithSalt` challengeResponses

instance Prelude.NFData AuthEventType where
  rnf AuthEventType' {..} =
    Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf eventResponse
      `Prelude.seq` Prelude.rnf eventContextData
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf eventRisk
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf eventFeedback
      `Prelude.seq` Prelude.rnf challengeResponses
