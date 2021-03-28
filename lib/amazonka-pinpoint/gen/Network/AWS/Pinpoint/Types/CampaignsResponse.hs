{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.CampaignsResponse
  ( CampaignsResponse (..)
  -- * Smart constructor
  , mkCampaignsResponse
  -- * Lenses
  , crItem
  , crNextToken
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.CampaignResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the configuration and other settings for all the campaigns that are associated with an application.
--
-- /See:/ 'mkCampaignsResponse' smart constructor.
data CampaignsResponse = CampaignsResponse'
  { item :: [Types.CampaignResponse]
    -- ^ An array of responses, one for each campaign that's associated with the application.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CampaignsResponse' value with any optional fields omitted.
mkCampaignsResponse
    :: CampaignsResponse
mkCampaignsResponse
  = CampaignsResponse'{item = Core.mempty, nextToken = Core.Nothing}

-- | An array of responses, one for each campaign that's associated with the application.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crItem :: Lens.Lens' CampaignsResponse [Types.CampaignResponse]
crItem = Lens.field @"item"
{-# INLINEABLE crItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crNextToken :: Lens.Lens' CampaignsResponse (Core.Maybe Core.Text)
crNextToken = Lens.field @"nextToken"
{-# INLINEABLE crNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON CampaignsResponse where
        parseJSON
          = Core.withObject "CampaignsResponse" Core.$
              \ x ->
                CampaignsResponse' Core.<$>
                  (x Core..:? "Item" Core..!= Core.mempty) Core.<*>
                    x Core..:? "NextToken"
