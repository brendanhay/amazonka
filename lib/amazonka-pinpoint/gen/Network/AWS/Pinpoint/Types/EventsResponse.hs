{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EventsResponse
  ( EventsResponse (..)
  -- * Smart constructor
  , mkEventsResponse
  -- * Lenses
  , erResults
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ItemResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about endpoints and the events that they're associated with.
--
-- /See:/ 'mkEventsResponse' smart constructor.
newtype EventsResponse = EventsResponse'
  { results :: Core.Maybe (Core.HashMap Core.Text Types.ItemResponse)
    -- ^ A map that contains a multipart response for each endpoint. For each item in this object, the endpoint ID is the key and the item response is the value. If no item response exists, the value can also be one of the following: 202, the request was processed successfully; or 400, the payload wasn't valid or required fields were missing.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EventsResponse' value with any optional fields omitted.
mkEventsResponse
    :: EventsResponse
mkEventsResponse = EventsResponse'{results = Core.Nothing}

-- | A map that contains a multipart response for each endpoint. For each item in this object, the endpoint ID is the key and the item response is the value. If no item response exists, the value can also be one of the following: 202, the request was processed successfully; or 400, the payload wasn't valid or required fields were missing.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erResults :: Lens.Lens' EventsResponse (Core.Maybe (Core.HashMap Core.Text Types.ItemResponse))
erResults = Lens.field @"results"
{-# INLINEABLE erResults #-}
{-# DEPRECATED results "Use generic-lens or generic-optics with 'results' instead"  #-}

instance Core.FromJSON EventsResponse where
        parseJSON
          = Core.withObject "EventsResponse" Core.$
              \ x -> EventsResponse' Core.<$> (x Core..:? "Results")
