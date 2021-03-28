{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ActivitiesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ActivitiesResponse
  ( ActivitiesResponse (..)
  -- * Smart constructor
  , mkActivitiesResponse
  -- * Lenses
  , arItem
  , arNextToken
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ActivityResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the activities that were performed by a campaign.
--
-- /See:/ 'mkActivitiesResponse' smart constructor.
data ActivitiesResponse = ActivitiesResponse'
  { item :: [Types.ActivityResponse]
    -- ^ An array of responses, one for each activity that was performed by the campaign.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivitiesResponse' value with any optional fields omitted.
mkActivitiesResponse
    :: ActivitiesResponse
mkActivitiesResponse
  = ActivitiesResponse'{item = Core.mempty, nextToken = Core.Nothing}

-- | An array of responses, one for each activity that was performed by the campaign.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arItem :: Lens.Lens' ActivitiesResponse [Types.ActivityResponse]
arItem = Lens.field @"item"
{-# INLINEABLE arItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arNextToken :: Lens.Lens' ActivitiesResponse (Core.Maybe Core.Text)
arNextToken = Lens.field @"nextToken"
{-# INLINEABLE arNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON ActivitiesResponse where
        parseJSON
          = Core.withObject "ActivitiesResponse" Core.$
              \ x ->
                ActivitiesResponse' Core.<$>
                  (x Core..:? "Item" Core..!= Core.mempty) Core.<*>
                    x Core..:? "NextToken"
