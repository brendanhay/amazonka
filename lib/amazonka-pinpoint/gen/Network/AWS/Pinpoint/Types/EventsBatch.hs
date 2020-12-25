{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventsBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventsBatch
  ( EventsBatch (..),

    -- * Smart constructor
    mkEventsBatch,

    -- * Lenses
    ebEndpoint,
    ebEvents,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Event as Types
import qualified Network.AWS.Pinpoint.Types.PublicEndpoint as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies a batch of endpoints and events to process.
--
-- /See:/ 'mkEventsBatch' smart constructor.
data EventsBatch = EventsBatch'
  { -- | A set of properties and attributes that are associated with the endpoint.
    endpoint :: Types.PublicEndpoint,
    -- | A set of properties that are associated with the event.
    events :: Core.HashMap Core.Text Types.Event
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventsBatch' value with any optional fields omitted.
mkEventsBatch ::
  -- | 'endpoint'
  Types.PublicEndpoint ->
  EventsBatch
mkEventsBatch endpoint =
  EventsBatch' {endpoint, events = Core.mempty}

-- | A set of properties and attributes that are associated with the endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebEndpoint :: Lens.Lens' EventsBatch Types.PublicEndpoint
ebEndpoint = Lens.field @"endpoint"
{-# DEPRECATED ebEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | A set of properties that are associated with the event.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebEvents :: Lens.Lens' EventsBatch (Core.HashMap Core.Text Types.Event)
ebEvents = Lens.field @"events"
{-# DEPRECATED ebEvents "Use generic-lens or generic-optics with 'events' instead." #-}

instance Core.FromJSON EventsBatch where
  toJSON EventsBatch {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Endpoint" Core..= endpoint),
            Core.Just ("Events" Core..= events)
          ]
      )
