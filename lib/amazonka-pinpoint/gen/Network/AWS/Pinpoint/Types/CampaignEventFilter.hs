{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignEventFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.CampaignEventFilter
  ( CampaignEventFilter (..)
  -- * Smart constructor
  , mkCampaignEventFilter
  -- * Lenses
  , cefFilterType
  , cefDimensions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EventDimensions as Types
import qualified Network.AWS.Pinpoint.Types.FilterType as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for events that cause a campaign to be sent.
--
-- /See:/ 'mkCampaignEventFilter' smart constructor.
data CampaignEventFilter = CampaignEventFilter'
  { filterType :: Types.FilterType
    -- ^ The type of event that causes the campaign to be sent. Valid values are: SYSTEM, sends the campaign when a system event occurs; and, ENDPOINT, sends the campaign when an endpoint event (<link>Events resource) occurs.
  , dimensions :: Types.EventDimensions
    -- ^ The dimension settings of the event filter for the campaign.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CampaignEventFilter' value with any optional fields omitted.
mkCampaignEventFilter
    :: Types.FilterType -- ^ 'filterType'
    -> Types.EventDimensions -- ^ 'dimensions'
    -> CampaignEventFilter
mkCampaignEventFilter filterType dimensions
  = CampaignEventFilter'{filterType, dimensions}

-- | The type of event that causes the campaign to be sent. Valid values are: SYSTEM, sends the campaign when a system event occurs; and, ENDPOINT, sends the campaign when an endpoint event (<link>Events resource) occurs.
--
-- /Note:/ Consider using 'filterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cefFilterType :: Lens.Lens' CampaignEventFilter Types.FilterType
cefFilterType = Lens.field @"filterType"
{-# INLINEABLE cefFilterType #-}
{-# DEPRECATED filterType "Use generic-lens or generic-optics with 'filterType' instead"  #-}

-- | The dimension settings of the event filter for the campaign.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cefDimensions :: Lens.Lens' CampaignEventFilter Types.EventDimensions
cefDimensions = Lens.field @"dimensions"
{-# INLINEABLE cefDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

instance Core.FromJSON CampaignEventFilter where
        toJSON CampaignEventFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FilterType" Core..= filterType),
                  Core.Just ("Dimensions" Core..= dimensions)])

instance Core.FromJSON CampaignEventFilter where
        parseJSON
          = Core.withObject "CampaignEventFilter" Core.$
              \ x ->
                CampaignEventFilter' Core.<$>
                  (x Core..: "FilterType") Core.<*> x Core..: "Dimensions"
