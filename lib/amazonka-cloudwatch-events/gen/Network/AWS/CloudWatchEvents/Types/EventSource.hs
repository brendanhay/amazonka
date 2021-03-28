{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.EventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.EventSource
  ( EventSource (..)
  -- * Smart constructor
  , mkEventSource
  -- * Lenses
  , esArn
  , esCreatedBy
  , esCreationTime
  , esExpirationTime
  , esName
  , esState
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.EventSourceState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A partner event source is created by an SaaS partner. If a customer creates a partner event bus that matches this event source, that AWS account can receive events from the partner's applications or services.
--
-- /See:/ 'mkEventSource' smart constructor.
data EventSource = EventSource'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN of the event source.
  , createdBy :: Core.Maybe Core.Text
    -- ^ The name of the partner that created the event source.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the event source was created.
  , expirationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the event source will expire, if the AWS account doesn't create a matching event bus for it.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the event source.
  , state :: Core.Maybe Types.EventSourceState
    -- ^ The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EventSource' value with any optional fields omitted.
mkEventSource
    :: EventSource
mkEventSource
  = EventSource'{arn = Core.Nothing, createdBy = Core.Nothing,
                 creationTime = Core.Nothing, expirationTime = Core.Nothing,
                 name = Core.Nothing, state = Core.Nothing}

-- | The ARN of the event source.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esArn :: Lens.Lens' EventSource (Core.Maybe Core.Text)
esArn = Lens.field @"arn"
{-# INLINEABLE esArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name of the partner that created the event source.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCreatedBy :: Lens.Lens' EventSource (Core.Maybe Core.Text)
esCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE esCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | The date and time the event source was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCreationTime :: Lens.Lens' EventSource (Core.Maybe Core.NominalDiffTime)
esCreationTime = Lens.field @"creationTime"
{-# INLINEABLE esCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The date and time that the event source will expire, if the AWS account doesn't create a matching event bus for it.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esExpirationTime :: Lens.Lens' EventSource (Core.Maybe Core.NominalDiffTime)
esExpirationTime = Lens.field @"expirationTime"
{-# INLINEABLE esExpirationTime #-}
{-# DEPRECATED expirationTime "Use generic-lens or generic-optics with 'expirationTime' instead"  #-}

-- | The name of the event source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esName :: Lens.Lens' EventSource (Core.Maybe Core.Text)
esName = Lens.field @"name"
{-# INLINEABLE esName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esState :: Lens.Lens' EventSource (Core.Maybe Types.EventSourceState)
esState = Lens.field @"state"
{-# INLINEABLE esState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON EventSource where
        parseJSON
          = Core.withObject "EventSource" Core.$
              \ x ->
                EventSource' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "CreatedBy" Core.<*>
                    x Core..:? "CreationTime"
                    Core.<*> x Core..:? "ExpirationTime"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "State"
