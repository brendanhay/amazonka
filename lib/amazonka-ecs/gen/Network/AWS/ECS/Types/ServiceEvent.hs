{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ServiceEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ServiceEvent
  ( ServiceEvent (..),

    -- * Smart constructor
    mkServiceEvent,

    -- * Lenses
    seCreatedAt,
    seId,
    seMessage,
  )
where

import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on an event associated with a service.
--
-- /See:/ 'mkServiceEvent' smart constructor.
data ServiceEvent = ServiceEvent'
  { -- | The Unix timestamp for when the event was triggered.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The ID string of the event.
    id :: Core.Maybe Types.String,
    -- | The event message.
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ServiceEvent' value with any optional fields omitted.
mkServiceEvent ::
  ServiceEvent
mkServiceEvent =
  ServiceEvent'
    { createdAt = Core.Nothing,
      id = Core.Nothing,
      message = Core.Nothing
    }

-- | The Unix timestamp for when the event was triggered.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seCreatedAt :: Lens.Lens' ServiceEvent (Core.Maybe Core.NominalDiffTime)
seCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED seCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ID string of the event.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seId :: Lens.Lens' ServiceEvent (Core.Maybe Types.String)
seId = Lens.field @"id"
{-# DEPRECATED seId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The event message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMessage :: Lens.Lens' ServiceEvent (Core.Maybe Types.String)
seMessage = Lens.field @"message"
{-# DEPRECATED seMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON ServiceEvent where
  parseJSON =
    Core.withObject "ServiceEvent" Core.$
      \x ->
        ServiceEvent'
          Core.<$> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "message")
