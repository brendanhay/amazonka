{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AuthEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AuthEventType
  ( AuthEventType (..)
  -- * Smart constructor
  , mkAuthEventType
  -- * Lenses
  , aetChallengeResponses
  , aetCreationDate
  , aetEventContextData
  , aetEventFeedback
  , aetEventId
  , aetEventResponse
  , aetEventRisk
  , aetEventType
  ) where

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
  { challengeResponses :: Core.Maybe [Types.ChallengeResponseType]
    -- ^ The challenge responses.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation date
  , eventContextData :: Core.Maybe Types.EventContextDataType
    -- ^ The user context data captured at the time of an event request. It provides additional information about the client from which event the request is received.
  , eventFeedback :: Core.Maybe Types.EventFeedbackType
    -- ^ A flag specifying the user feedback captured at the time of an event request is good or bad. 
  , eventId :: Core.Maybe Types.EventId
    -- ^ The event ID.
  , eventResponse :: Core.Maybe Types.EventResponseType
    -- ^ The event response.
  , eventRisk :: Core.Maybe Types.EventRiskType
    -- ^ The event risk.
  , eventType :: Core.Maybe Types.EventType
    -- ^ The event type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AuthEventType' value with any optional fields omitted.
mkAuthEventType
    :: AuthEventType
mkAuthEventType
  = AuthEventType'{challengeResponses = Core.Nothing,
                   creationDate = Core.Nothing, eventContextData = Core.Nothing,
                   eventFeedback = Core.Nothing, eventId = Core.Nothing,
                   eventResponse = Core.Nothing, eventRisk = Core.Nothing,
                   eventType = Core.Nothing}

-- | The challenge responses.
--
-- /Note:/ Consider using 'challengeResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetChallengeResponses :: Lens.Lens' AuthEventType (Core.Maybe [Types.ChallengeResponseType])
aetChallengeResponses = Lens.field @"challengeResponses"
{-# INLINEABLE aetChallengeResponses #-}
{-# DEPRECATED challengeResponses "Use generic-lens or generic-optics with 'challengeResponses' instead"  #-}

-- | The creation date
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetCreationDate :: Lens.Lens' AuthEventType (Core.Maybe Core.NominalDiffTime)
aetCreationDate = Lens.field @"creationDate"
{-# INLINEABLE aetCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The user context data captured at the time of an event request. It provides additional information about the client from which event the request is received.
--
-- /Note:/ Consider using 'eventContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventContextData :: Lens.Lens' AuthEventType (Core.Maybe Types.EventContextDataType)
aetEventContextData = Lens.field @"eventContextData"
{-# INLINEABLE aetEventContextData #-}
{-# DEPRECATED eventContextData "Use generic-lens or generic-optics with 'eventContextData' instead"  #-}

-- | A flag specifying the user feedback captured at the time of an event request is good or bad. 
--
-- /Note:/ Consider using 'eventFeedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventFeedback :: Lens.Lens' AuthEventType (Core.Maybe Types.EventFeedbackType)
aetEventFeedback = Lens.field @"eventFeedback"
{-# INLINEABLE aetEventFeedback #-}
{-# DEPRECATED eventFeedback "Use generic-lens or generic-optics with 'eventFeedback' instead"  #-}

-- | The event ID.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventId :: Lens.Lens' AuthEventType (Core.Maybe Types.EventId)
aetEventId = Lens.field @"eventId"
{-# INLINEABLE aetEventId #-}
{-# DEPRECATED eventId "Use generic-lens or generic-optics with 'eventId' instead"  #-}

-- | The event response.
--
-- /Note:/ Consider using 'eventResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventResponse :: Lens.Lens' AuthEventType (Core.Maybe Types.EventResponseType)
aetEventResponse = Lens.field @"eventResponse"
{-# INLINEABLE aetEventResponse #-}
{-# DEPRECATED eventResponse "Use generic-lens or generic-optics with 'eventResponse' instead"  #-}

-- | The event risk.
--
-- /Note:/ Consider using 'eventRisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventRisk :: Lens.Lens' AuthEventType (Core.Maybe Types.EventRiskType)
aetEventRisk = Lens.field @"eventRisk"
{-# INLINEABLE aetEventRisk #-}
{-# DEPRECATED eventRisk "Use generic-lens or generic-optics with 'eventRisk' instead"  #-}

-- | The event type.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetEventType :: Lens.Lens' AuthEventType (Core.Maybe Types.EventType)
aetEventType = Lens.field @"eventType"
{-# INLINEABLE aetEventType #-}
{-# DEPRECATED eventType "Use generic-lens or generic-optics with 'eventType' instead"  #-}

instance Core.FromJSON AuthEventType where
        parseJSON
          = Core.withObject "AuthEventType" Core.$
              \ x ->
                AuthEventType' Core.<$>
                  (x Core..:? "ChallengeResponses") Core.<*>
                    x Core..:? "CreationDate"
                    Core.<*> x Core..:? "EventContextData"
                    Core.<*> x Core..:? "EventFeedback"
                    Core.<*> x Core..:? "EventId"
                    Core.<*> x Core..:? "EventResponse"
                    Core.<*> x Core..:? "EventRisk"
                    Core.<*> x Core..:? "EventType"
