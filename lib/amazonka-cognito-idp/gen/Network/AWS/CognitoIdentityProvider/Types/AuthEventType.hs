{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AuthEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AuthEventType
  ( AuthEventType (..),

    -- * Smart constructor
    mkAuthEventType,

    -- * Lenses
    aetEventRisk,
    aetEventResponse,
    aetEventContextData,
    aetChallengeResponses,
    aetEventType,
    aetCreationDate,
    aetEventFeedback,
    aetEventId,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType
import Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
import Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType
import Network.AWS.CognitoIdentityProvider.Types.EventResponseType
import Network.AWS.CognitoIdentityProvider.Types.EventRiskType
import Network.AWS.CognitoIdentityProvider.Types.EventType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The authentication event type.
--
-- /See:/ 'mkAuthEventType' smart constructor.
data AuthEventType = AuthEventType'
  { eventRisk ::
      Lude.Maybe EventRiskType,
    eventResponse :: Lude.Maybe EventResponseType,
    eventContextData :: Lude.Maybe EventContextDataType,
    challengeResponses :: Lude.Maybe [ChallengeResponseType],
    eventType :: Lude.Maybe EventType,
    creationDate :: Lude.Maybe Lude.Timestamp,
    eventFeedback :: Lude.Maybe EventFeedbackType,
    eventId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthEventType' with the minimum fields required to make a request.
--
-- * 'challengeResponses' - The challenge responses.
-- * 'creationDate' - The creation date
-- * 'eventContextData' - The user context data captured at the time of an event request. It provides additional information about the client from which event the request is received.
-- * 'eventFeedback' - A flag specifying the user feedback captured at the time of an event request is good or bad.
-- * 'eventId' - The event ID.
-- * 'eventResponse' - The event response.
-- * 'eventRisk' - The event risk.
-- * 'eventType' - The event type.
mkAuthEventType ::
  AuthEventType
mkAuthEventType =
  AuthEventType'
    { eventRisk = Lude.Nothing,
      eventResponse = Lude.Nothing,
      eventContextData = Lude.Nothing,
      challengeResponses = Lude.Nothing,
      eventType = Lude.Nothing,
      creationDate = Lude.Nothing,
      eventFeedback = Lude.Nothing,
      eventId = Lude.Nothing
    }

-- | The event risk.
--
-- /Note:/ Consider using 'eventRisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventRisk :: Lens.Lens' AuthEventType (Lude.Maybe EventRiskType)
aetEventRisk = Lens.lens (eventRisk :: AuthEventType -> Lude.Maybe EventRiskType) (\s a -> s {eventRisk = a} :: AuthEventType)
{-# DEPRECATED aetEventRisk "Use generic-lens or generic-optics with 'eventRisk' instead." #-}

-- | The event response.
--
-- /Note:/ Consider using 'eventResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventResponse :: Lens.Lens' AuthEventType (Lude.Maybe EventResponseType)
aetEventResponse = Lens.lens (eventResponse :: AuthEventType -> Lude.Maybe EventResponseType) (\s a -> s {eventResponse = a} :: AuthEventType)
{-# DEPRECATED aetEventResponse "Use generic-lens or generic-optics with 'eventResponse' instead." #-}

-- | The user context data captured at the time of an event request. It provides additional information about the client from which event the request is received.
--
-- /Note:/ Consider using 'eventContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventContextData :: Lens.Lens' AuthEventType (Lude.Maybe EventContextDataType)
aetEventContextData = Lens.lens (eventContextData :: AuthEventType -> Lude.Maybe EventContextDataType) (\s a -> s {eventContextData = a} :: AuthEventType)
{-# DEPRECATED aetEventContextData "Use generic-lens or generic-optics with 'eventContextData' instead." #-}

-- | The challenge responses.
--
-- /Note:/ Consider using 'challengeResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetChallengeResponses :: Lens.Lens' AuthEventType (Lude.Maybe [ChallengeResponseType])
aetChallengeResponses = Lens.lens (challengeResponses :: AuthEventType -> Lude.Maybe [ChallengeResponseType]) (\s a -> s {challengeResponses = a} :: AuthEventType)
{-# DEPRECATED aetChallengeResponses "Use generic-lens or generic-optics with 'challengeResponses' instead." #-}

-- | The event type.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventType :: Lens.Lens' AuthEventType (Lude.Maybe EventType)
aetEventType = Lens.lens (eventType :: AuthEventType -> Lude.Maybe EventType) (\s a -> s {eventType = a} :: AuthEventType)
{-# DEPRECATED aetEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | The creation date
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetCreationDate :: Lens.Lens' AuthEventType (Lude.Maybe Lude.Timestamp)
aetCreationDate = Lens.lens (creationDate :: AuthEventType -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: AuthEventType)
{-# DEPRECATED aetCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A flag specifying the user feedback captured at the time of an event request is good or bad.
--
-- /Note:/ Consider using 'eventFeedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventFeedback :: Lens.Lens' AuthEventType (Lude.Maybe EventFeedbackType)
aetEventFeedback = Lens.lens (eventFeedback :: AuthEventType -> Lude.Maybe EventFeedbackType) (\s a -> s {eventFeedback = a} :: AuthEventType)
{-# DEPRECATED aetEventFeedback "Use generic-lens or generic-optics with 'eventFeedback' instead." #-}

-- | The event ID.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventId :: Lens.Lens' AuthEventType (Lude.Maybe Lude.Text)
aetEventId = Lens.lens (eventId :: AuthEventType -> Lude.Maybe Lude.Text) (\s a -> s {eventId = a} :: AuthEventType)
{-# DEPRECATED aetEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.FromJSON AuthEventType where
  parseJSON =
    Lude.withObject
      "AuthEventType"
      ( \x ->
          AuthEventType'
            Lude.<$> (x Lude..:? "EventRisk")
            Lude.<*> (x Lude..:? "EventResponse")
            Lude.<*> (x Lude..:? "EventContextData")
            Lude.<*> (x Lude..:? "ChallengeResponses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EventType")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "EventFeedback")
            Lude.<*> (x Lude..:? "EventId")
      )
