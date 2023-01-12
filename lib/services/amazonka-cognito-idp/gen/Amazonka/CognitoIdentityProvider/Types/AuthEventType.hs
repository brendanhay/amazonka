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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The authentication event type.
--
-- /See:/ 'newAuthEventType' smart constructor.
data AuthEventType = AuthEventType'
  { -- | The challenge responses.
    challengeResponses :: Prelude.Maybe [ChallengeResponseType],
    -- | The creation date
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The user context data captured at the time of an event request. This
    -- value provides additional information about the client from which event
    -- the request is received.
    eventContextData :: Prelude.Maybe EventContextDataType,
    -- | A flag specifying the user feedback captured at the time of an event
    -- request is good or bad.
    eventFeedback :: Prelude.Maybe EventFeedbackType,
    -- | The event ID.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The event response.
    eventResponse :: Prelude.Maybe EventResponseType,
    -- | The event risk.
    eventRisk :: Prelude.Maybe EventRiskType,
    -- | The event type.
    eventType :: Prelude.Maybe EventType
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
-- 'challengeResponses', 'authEventType_challengeResponses' - The challenge responses.
--
-- 'creationDate', 'authEventType_creationDate' - The creation date
--
-- 'eventContextData', 'authEventType_eventContextData' - The user context data captured at the time of an event request. This
-- value provides additional information about the client from which event
-- the request is received.
--
-- 'eventFeedback', 'authEventType_eventFeedback' - A flag specifying the user feedback captured at the time of an event
-- request is good or bad.
--
-- 'eventId', 'authEventType_eventId' - The event ID.
--
-- 'eventResponse', 'authEventType_eventResponse' - The event response.
--
-- 'eventRisk', 'authEventType_eventRisk' - The event risk.
--
-- 'eventType', 'authEventType_eventType' - The event type.
newAuthEventType ::
  AuthEventType
newAuthEventType =
  AuthEventType'
    { challengeResponses =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      eventContextData = Prelude.Nothing,
      eventFeedback = Prelude.Nothing,
      eventId = Prelude.Nothing,
      eventResponse = Prelude.Nothing,
      eventRisk = Prelude.Nothing,
      eventType = Prelude.Nothing
    }

-- | The challenge responses.
authEventType_challengeResponses :: Lens.Lens' AuthEventType (Prelude.Maybe [ChallengeResponseType])
authEventType_challengeResponses = Lens.lens (\AuthEventType' {challengeResponses} -> challengeResponses) (\s@AuthEventType' {} a -> s {challengeResponses = a} :: AuthEventType) Prelude.. Lens.mapping Lens.coerced

-- | The creation date
authEventType_creationDate :: Lens.Lens' AuthEventType (Prelude.Maybe Prelude.UTCTime)
authEventType_creationDate = Lens.lens (\AuthEventType' {creationDate} -> creationDate) (\s@AuthEventType' {} a -> s {creationDate = a} :: AuthEventType) Prelude.. Lens.mapping Data._Time

-- | The user context data captured at the time of an event request. This
-- value provides additional information about the client from which event
-- the request is received.
authEventType_eventContextData :: Lens.Lens' AuthEventType (Prelude.Maybe EventContextDataType)
authEventType_eventContextData = Lens.lens (\AuthEventType' {eventContextData} -> eventContextData) (\s@AuthEventType' {} a -> s {eventContextData = a} :: AuthEventType)

-- | A flag specifying the user feedback captured at the time of an event
-- request is good or bad.
authEventType_eventFeedback :: Lens.Lens' AuthEventType (Prelude.Maybe EventFeedbackType)
authEventType_eventFeedback = Lens.lens (\AuthEventType' {eventFeedback} -> eventFeedback) (\s@AuthEventType' {} a -> s {eventFeedback = a} :: AuthEventType)

-- | The event ID.
authEventType_eventId :: Lens.Lens' AuthEventType (Prelude.Maybe Prelude.Text)
authEventType_eventId = Lens.lens (\AuthEventType' {eventId} -> eventId) (\s@AuthEventType' {} a -> s {eventId = a} :: AuthEventType)

-- | The event response.
authEventType_eventResponse :: Lens.Lens' AuthEventType (Prelude.Maybe EventResponseType)
authEventType_eventResponse = Lens.lens (\AuthEventType' {eventResponse} -> eventResponse) (\s@AuthEventType' {} a -> s {eventResponse = a} :: AuthEventType)

-- | The event risk.
authEventType_eventRisk :: Lens.Lens' AuthEventType (Prelude.Maybe EventRiskType)
authEventType_eventRisk = Lens.lens (\AuthEventType' {eventRisk} -> eventRisk) (\s@AuthEventType' {} a -> s {eventRisk = a} :: AuthEventType)

-- | The event type.
authEventType_eventType :: Lens.Lens' AuthEventType (Prelude.Maybe EventType)
authEventType_eventType = Lens.lens (\AuthEventType' {eventType} -> eventType) (\s@AuthEventType' {} a -> s {eventType = a} :: AuthEventType)

instance Data.FromJSON AuthEventType where
  parseJSON =
    Data.withObject
      "AuthEventType"
      ( \x ->
          AuthEventType'
            Prelude.<$> ( x Data..:? "ChallengeResponses"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "EventContextData")
            Prelude.<*> (x Data..:? "EventFeedback")
            Prelude.<*> (x Data..:? "EventId")
            Prelude.<*> (x Data..:? "EventResponse")
            Prelude.<*> (x Data..:? "EventRisk")
            Prelude.<*> (x Data..:? "EventType")
      )

instance Prelude.Hashable AuthEventType where
  hashWithSalt _salt AuthEventType' {..} =
    _salt `Prelude.hashWithSalt` challengeResponses
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` eventContextData
      `Prelude.hashWithSalt` eventFeedback
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventResponse
      `Prelude.hashWithSalt` eventRisk
      `Prelude.hashWithSalt` eventType

instance Prelude.NFData AuthEventType where
  rnf AuthEventType' {..} =
    Prelude.rnf challengeResponses
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf eventContextData
      `Prelude.seq` Prelude.rnf eventFeedback
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf eventResponse
      `Prelude.seq` Prelude.rnf eventRisk
      `Prelude.seq` Prelude.rnf eventType
