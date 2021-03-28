{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RegionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.RegionInfo
  ( RegionInfo (..)
  -- * Smart constructor
  , mkRegionInfo
  -- * Lenses
  , riEndpoint
  , riOptInStatus
  , riRegionName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Region.
--
-- /See:/ 'mkRegionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { endpoint :: Core.Maybe Core.Text
    -- ^ The Region service endpoint.
  , optInStatus :: Core.Maybe Core.Text
    -- ^ The Region opt-in status. The possible values are @opt-in-not-required@ , @opted-in@ , and @not-opted-in@ .
  , regionName :: Core.Maybe Core.Text
    -- ^ The name of the Region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegionInfo' value with any optional fields omitted.
mkRegionInfo
    :: RegionInfo
mkRegionInfo
  = RegionInfo'{endpoint = Core.Nothing, optInStatus = Core.Nothing,
                regionName = Core.Nothing}

-- | The Region service endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEndpoint :: Lens.Lens' RegionInfo (Core.Maybe Core.Text)
riEndpoint = Lens.field @"endpoint"
{-# INLINEABLE riEndpoint #-}
{-# DEPRECATED endpoint "Use generic-lens or generic-optics with 'endpoint' instead"  #-}

-- | The Region opt-in status. The possible values are @opt-in-not-required@ , @opted-in@ , and @not-opted-in@ .
--
-- /Note:/ Consider using 'optInStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riOptInStatus :: Lens.Lens' RegionInfo (Core.Maybe Core.Text)
riOptInStatus = Lens.field @"optInStatus"
{-# INLINEABLE riOptInStatus #-}
{-# DEPRECATED optInStatus "Use generic-lens or generic-optics with 'optInStatus' instead"  #-}

-- | The name of the Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRegionName :: Lens.Lens' RegionInfo (Core.Maybe Core.Text)
riRegionName = Lens.field @"regionName"
{-# INLINEABLE riRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

instance Core.FromXML RegionInfo where
        parseXML x
          = RegionInfo' Core.<$>
              (x Core..@? "regionEndpoint") Core.<*> x Core..@? "optInStatus"
                Core.<*> x Core..@? "regionName"
