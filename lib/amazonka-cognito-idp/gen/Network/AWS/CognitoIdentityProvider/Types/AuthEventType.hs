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
    aetChallengeResponses,
    aetCreationDate,
    aetEventContextData,
    aetEventFeedback,
    aetEventId,
    aetEventResponse,
    aetEventRisk,
    aetEventType,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EventContextDataType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EventId as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EventResponseType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EventRiskType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EventType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The authentication event type.
--
-- /See:/ 'mkAuthEventType' smart constructor.
data AuthEventType = AuthEventType'
  { -- | The challenge responses.
    challengeResponses :: Core.Maybe [Types.ChallengeResponseType],
    -- | The creation date
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The user context data captured at the time of an event request. It provides additional information about the client from which event the request is received.
    eventContextData :: Core.Maybe Types.EventContextDataType,
    -- | A flag specifying the user feedback captured at the time of an event request is good or bad.
    eventFeedback :: Core.Maybe Types.EventFeedbackType,
    -- | The event ID.
    eventId :: Core.Maybe Types.EventId,
    -- | The event response.
    eventResponse :: Core.Maybe Types.EventResponseType,
    -- | The event risk.
    eventRisk :: Core.Maybe Types.EventRiskType,
    -- | The event type.
    eventType :: Core.Maybe Types.EventType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AuthEventType' value with any optional fields omitted.
mkAuthEventType ::
  AuthEventType
mkAuthEventType =
  AuthEventType'
    { challengeResponses = Core.Nothing,
      creationDate = Core.Nothing,
      eventContextData = Core.Nothing,
      eventFeedback = Core.Nothing,
      eventId = Core.Nothing,
      eventResponse = Core.Nothing,
      eventRisk = Core.Nothing,
      eventType = Core.Nothing
    }

-- | The challenge responses.
--
-- /Note:/ Consider using 'challengeResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetChallengeResponses :: Lens.Lens' AuthEventType (Core.Maybe [Types.ChallengeResponseType])
aetChallengeResponses = Lens.field @"challengeResponses"
{-# DEPRECATED aetChallengeResponses "Use generic-lens or generic-optics with 'challengeResponses' instead." #-}

-- | The creation date
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetCreationDate :: Lens.Lens' AuthEventType (Core.Maybe Core.NominalDiffTime)
aetCreationDate = Lens.field @"creationDate"
{-# DEPRECATED aetCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user context data captured at the time of an event request. It provides additional information about the client from which event the request is received.
--
-- /Note:/ Consider using 'eventContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventContextData :: Lens.Lens' AuthEventType (Core.Maybe Types.EventContextDataType)
aetEventContextData = Lens.field @"eventContextData"
{-# DEPRECATED aetEventContextData "Use generic-lens or generic-optics with 'eventContextData' instead." #-}

-- | A flag specifying the user feedback captured at the time of an event request is good or bad.
--
-- /Note:/ Consider using 'eventFeedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventFeedback :: Lens.Lens' AuthEventType (Core.Maybe Types.EventFeedbackType)
aetEventFeedback = Lens.field @"eventFeedback"
{-# DEPRECATED aetEventFeedback "Use generic-lens or generic-optics with 'eventFeedback' instead." #-}

-- | The event ID.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventId :: Lens.Lens' AuthEventType (Core.Maybe Types.EventId)
aetEventId = Lens.field @"eventId"
{-# DEPRECATED aetEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

-- | The event response.
--
-- /Note:/ Consider using 'eventResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventResponse :: Lens.Lens' AuthEventType (Core.Maybe Types.EventResponseType)
aetEventResponse = Lens.field @"eventResponse"
{-# DEPRECATED aetEventResponse "Use generic-lens or generic-optics with 'eventResponse' instead." #-}

-- | The event risk.
--
-- /Note:/ Consider using 'eventRisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventRisk :: Lens.Lens' AuthEventType (Core.Maybe Types.EventRiskType)
aetEventRisk = Lens.field @"eventRisk"
{-# DEPRECATED aetEventRisk "Use generic-lens or generic-optics with 'eventRisk' instead." #-}

-- | The event type.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventType :: Lens.Lens' AuthEventType (Core.Maybe Types.EventType)
aetEventType = Lens.field @"eventType"
{-# DEPRECATED aetEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

instance Core.FromJSON AuthEventType where
  parseJSON =
    Core.withObject "AuthEventType" Core.$
      \x ->
        AuthEventType'
          Core.<$> (x Core..:? "ChallengeResponses")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "EventContextData")
          Core.<*> (x Core..:? "EventFeedback")
          Core.<*> (x Core..:? "EventId")
          Core.<*> (x Core..:? "EventResponse")
          Core.<*> (x Core..:? "EventRisk")
          Core.<*> (x Core..:? "EventType")
