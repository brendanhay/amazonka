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
    ebEvents,
    ebEndpoint,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Event
import Network.AWS.Pinpoint.Types.PublicEndpoint
import qualified Network.AWS.Prelude as Lude

-- | Specifies a batch of endpoints and events to process.
--
-- /See:/ 'mkEventsBatch' smart constructor.
data EventsBatch = EventsBatch'
  { -- | A set of properties that are associated with the event.
    events :: Lude.HashMap Lude.Text (Event),
    -- | A set of properties and attributes that are associated with the endpoint.
    endpoint :: PublicEndpoint
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventsBatch' with the minimum fields required to make a request.
--
-- * 'events' - A set of properties that are associated with the event.
-- * 'endpoint' - A set of properties and attributes that are associated with the endpoint.
mkEventsBatch ::
  -- | 'endpoint'
  PublicEndpoint ->
  EventsBatch
mkEventsBatch pEndpoint_ =
  EventsBatch' {events = Lude.mempty, endpoint = pEndpoint_}

-- | A set of properties that are associated with the event.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebEvents :: Lens.Lens' EventsBatch (Lude.HashMap Lude.Text (Event))
ebEvents = Lens.lens (events :: EventsBatch -> Lude.HashMap Lude.Text (Event)) (\s a -> s {events = a} :: EventsBatch)
{-# DEPRECATED ebEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | A set of properties and attributes that are associated with the endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebEndpoint :: Lens.Lens' EventsBatch PublicEndpoint
ebEndpoint = Lens.lens (endpoint :: EventsBatch -> PublicEndpoint) (\s a -> s {endpoint = a} :: EventsBatch)
{-# DEPRECATED ebEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

instance Lude.ToJSON EventsBatch where
  toJSON EventsBatch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Events" Lude..= events),
            Lude.Just ("Endpoint" Lude..= endpoint)
          ]
      )
