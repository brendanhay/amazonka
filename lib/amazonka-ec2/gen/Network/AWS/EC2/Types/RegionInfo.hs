{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RegionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RegionInfo
  ( RegionInfo (..),

    -- * Smart constructor
    mkRegionInfo,

    -- * Lenses
    riEndpoint,
    riOptInStatus,
    riRegionName,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Region.
--
-- /See:/ 'mkRegionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { -- | The Region service endpoint.
    endpoint :: Core.Maybe Types.String,
    -- | The Region opt-in status. The possible values are @opt-in-not-required@ , @opted-in@ , and @not-opted-in@ .
    optInStatus :: Core.Maybe Types.String,
    -- | The name of the Region.
    regionName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegionInfo' value with any optional fields omitted.
mkRegionInfo ::
  RegionInfo
mkRegionInfo =
  RegionInfo'
    { endpoint = Core.Nothing,
      optInStatus = Core.Nothing,
      regionName = Core.Nothing
    }

-- | The Region service endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEndpoint :: Lens.Lens' RegionInfo (Core.Maybe Types.String)
riEndpoint = Lens.field @"endpoint"
{-# DEPRECATED riEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The Region opt-in status. The possible values are @opt-in-not-required@ , @opted-in@ , and @not-opted-in@ .
--
-- /Note:/ Consider using 'optInStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riOptInStatus :: Lens.Lens' RegionInfo (Core.Maybe Types.String)
riOptInStatus = Lens.field @"optInStatus"
{-# DEPRECATED riOptInStatus "Use generic-lens or generic-optics with 'optInStatus' instead." #-}

-- | The name of the Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRegionName :: Lens.Lens' RegionInfo (Core.Maybe Types.String)
riRegionName = Lens.field @"regionName"
{-# DEPRECATED riRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Core.FromXML RegionInfo where
  parseXML x =
    RegionInfo'
      Core.<$> (x Core..@? "regionEndpoint")
      Core.<*> (x Core..@? "optInStatus")
      Core.<*> (x Core..@? "regionName")
