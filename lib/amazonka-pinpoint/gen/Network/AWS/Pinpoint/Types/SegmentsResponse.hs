{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SegmentsResponse
  ( SegmentsResponse (..)
  -- * Smart constructor
  , mkSegmentsResponse
  -- * Lenses
  , srItem
  , srNextToken
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.SegmentResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about all the segments that are associated with an application.
--
-- /See:/ 'mkSegmentsResponse' smart constructor.
data SegmentsResponse = SegmentsResponse'
  { item :: [Types.SegmentResponse]
    -- ^ An array of responses, one for each segment that's associated with the application (Segments resource) or each version of a segment that's associated with the application (Segment Versions resource).
  , nextToken :: Core.Maybe Core.Text
    -- ^ The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentsResponse' value with any optional fields omitted.
mkSegmentsResponse
    :: SegmentsResponse
mkSegmentsResponse
  = SegmentsResponse'{item = Core.mempty, nextToken = Core.Nothing}

-- | An array of responses, one for each segment that's associated with the application (Segments resource) or each version of a segment that's associated with the application (Segment Versions resource).
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srItem :: Lens.Lens' SegmentsResponse [Types.SegmentResponse]
srItem = Lens.field @"item"
{-# INLINEABLE srItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srNextToken :: Lens.Lens' SegmentsResponse (Core.Maybe Core.Text)
srNextToken = Lens.field @"nextToken"
{-# INLINEABLE srNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON SegmentsResponse where
        parseJSON
          = Core.withObject "SegmentsResponse" Core.$
              \ x ->
                SegmentsResponse' Core.<$>
                  (x Core..:? "Item" Core..!= Core.mempty) Core.<*>
                    x Core..:? "NextToken"
