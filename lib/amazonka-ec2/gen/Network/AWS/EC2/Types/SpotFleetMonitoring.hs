{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotFleetMonitoring
  ( SpotFleetMonitoring (..)
  -- * Smart constructor
  , mkSpotFleetMonitoring
  -- * Lenses
  , sfmEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes whether monitoring is enabled.
--
-- /See:/ 'mkSpotFleetMonitoring' smart constructor.
newtype SpotFleetMonitoring = SpotFleetMonitoring'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Enables monitoring for the instance.
--
-- Default: @false@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SpotFleetMonitoring' value with any optional fields omitted.
mkSpotFleetMonitoring
    :: SpotFleetMonitoring
mkSpotFleetMonitoring
  = SpotFleetMonitoring'{enabled = Core.Nothing}

-- | Enables monitoring for the instance.
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfmEnabled :: Lens.Lens' SpotFleetMonitoring (Core.Maybe Core.Bool)
sfmEnabled = Lens.field @"enabled"
{-# INLINEABLE sfmEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.ToQuery SpotFleetMonitoring where
        toQuery SpotFleetMonitoring{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Enabled") enabled

instance Core.FromXML SpotFleetMonitoring where
        parseXML x = SpotFleetMonitoring' Core.<$> (x Core..@? "enabled")
