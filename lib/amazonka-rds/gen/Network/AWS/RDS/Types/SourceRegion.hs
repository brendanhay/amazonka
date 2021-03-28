{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.SourceRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.SourceRegion
  ( SourceRegion (..)
  -- * Smart constructor
  , mkSourceRegion
  -- * Lenses
  , srEndpoint
  , srRegionName
  , srStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains an AWS Region name as the result of a successful call to the @DescribeSourceRegions@ action.
--
-- /See:/ 'mkSourceRegion' smart constructor.
data SourceRegion = SourceRegion'
  { endpoint :: Core.Maybe Core.Text
    -- ^ The endpoint for the source AWS Region endpoint.
  , regionName :: Core.Maybe Core.Text
    -- ^ The name of the source AWS Region.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the source AWS Region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceRegion' value with any optional fields omitted.
mkSourceRegion
    :: SourceRegion
mkSourceRegion
  = SourceRegion'{endpoint = Core.Nothing, regionName = Core.Nothing,
                  status = Core.Nothing}

-- | The endpoint for the source AWS Region endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srEndpoint :: Lens.Lens' SourceRegion (Core.Maybe Core.Text)
srEndpoint = Lens.field @"endpoint"
{-# INLINEABLE srEndpoint #-}
{-# DEPRECATED endpoint "Use generic-lens or generic-optics with 'endpoint' instead"  #-}

-- | The name of the source AWS Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRegionName :: Lens.Lens' SourceRegion (Core.Maybe Core.Text)
srRegionName = Lens.field @"regionName"
{-# INLINEABLE srRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

-- | The status of the source AWS Region.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStatus :: Lens.Lens' SourceRegion (Core.Maybe Core.Text)
srStatus = Lens.field @"status"
{-# INLINEABLE srStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML SourceRegion where
        parseXML x
          = SourceRegion' Core.<$>
              (x Core..@? "Endpoint") Core.<*> x Core..@? "RegionName" Core.<*>
                x Core..@? "Status"
