{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.RegionsInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.RegionsInfo
  ( RegionsInfo (..)
  -- * Smart constructor
  , mkRegionsInfo
  -- * Lenses
  , riAdditionalRegions
  , riPrimaryRegion
  ) where

import qualified Network.AWS.DirectoryService.Types.RegionName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the Regions that are configured for multi-Region replication.
--
-- /See:/ 'mkRegionsInfo' smart constructor.
data RegionsInfo = RegionsInfo'
  { additionalRegions :: Core.Maybe [Types.RegionName]
    -- ^ Lists the Regions where the directory has been replicated, excluding the primary Region.
  , primaryRegion :: Core.Maybe Types.RegionName
    -- ^ The Region from where the AWS Managed Microsoft AD directory was originally created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegionsInfo' value with any optional fields omitted.
mkRegionsInfo
    :: RegionsInfo
mkRegionsInfo
  = RegionsInfo'{additionalRegions = Core.Nothing,
                 primaryRegion = Core.Nothing}

-- | Lists the Regions where the directory has been replicated, excluding the primary Region.
--
-- /Note:/ Consider using 'additionalRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAdditionalRegions :: Lens.Lens' RegionsInfo (Core.Maybe [Types.RegionName])
riAdditionalRegions = Lens.field @"additionalRegions"
{-# INLINEABLE riAdditionalRegions #-}
{-# DEPRECATED additionalRegions "Use generic-lens or generic-optics with 'additionalRegions' instead"  #-}

-- | The Region from where the AWS Managed Microsoft AD directory was originally created.
--
-- /Note:/ Consider using 'primaryRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPrimaryRegion :: Lens.Lens' RegionsInfo (Core.Maybe Types.RegionName)
riPrimaryRegion = Lens.field @"primaryRegion"
{-# INLINEABLE riPrimaryRegion #-}
{-# DEPRECATED primaryRegion "Use generic-lens or generic-optics with 'primaryRegion' instead"  #-}

instance Core.FromJSON RegionsInfo where
        parseJSON
          = Core.withObject "RegionsInfo" Core.$
              \ x ->
                RegionsInfo' Core.<$>
                  (x Core..:? "AdditionalRegions") Core.<*>
                    x Core..:? "PrimaryRegion"
