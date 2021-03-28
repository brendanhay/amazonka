{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ItemResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ItemResponse
  ( ItemResponse (..)
  -- * Smart constructor
  , mkItemResponse
  -- * Lenses
  , irEndpointItemResponse
  , irEventsItemResponse
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EndpointItemResponse as Types
import qualified Network.AWS.Pinpoint.Types.EventItemResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the results of a request to create or update an endpoint that's associated with an event.
--
-- /See:/ 'mkItemResponse' smart constructor.
data ItemResponse = ItemResponse'
  { endpointItemResponse :: Core.Maybe Types.EndpointItemResponse
    -- ^ The response that was received after the endpoint data was accepted.
  , eventsItemResponse :: Core.Maybe (Core.HashMap Core.Text Types.EventItemResponse)
    -- ^ A multipart response object that contains a key and a value for each event in the request. In each object, the event ID is the key and an EventItemResponse object is the value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ItemResponse' value with any optional fields omitted.
mkItemResponse
    :: ItemResponse
mkItemResponse
  = ItemResponse'{endpointItemResponse = Core.Nothing,
                  eventsItemResponse = Core.Nothing}

-- | The response that was received after the endpoint data was accepted.
--
-- /Note:/ Consider using 'endpointItemResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irEndpointItemResponse :: Lens.Lens' ItemResponse (Core.Maybe Types.EndpointItemResponse)
irEndpointItemResponse = Lens.field @"endpointItemResponse"
{-# INLINEABLE irEndpointItemResponse #-}
{-# DEPRECATED endpointItemResponse "Use generic-lens or generic-optics with 'endpointItemResponse' instead"  #-}

-- | A multipart response object that contains a key and a value for each event in the request. In each object, the event ID is the key and an EventItemResponse object is the value.
--
-- /Note:/ Consider using 'eventsItemResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irEventsItemResponse :: Lens.Lens' ItemResponse (Core.Maybe (Core.HashMap Core.Text Types.EventItemResponse))
irEventsItemResponse = Lens.field @"eventsItemResponse"
{-# INLINEABLE irEventsItemResponse #-}
{-# DEPRECATED eventsItemResponse "Use generic-lens or generic-optics with 'eventsItemResponse' instead"  #-}

instance Core.FromJSON ItemResponse where
        parseJSON
          = Core.withObject "ItemResponse" Core.$
              \ x ->
                ItemResponse' Core.<$>
                  (x Core..:? "EndpointItemResponse") Core.<*>
                    x Core..:? "EventsItemResponse"
