{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.ConnectionDraining
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.ConnectionDraining
  ( ConnectionDraining (..)
  -- * Smart constructor
  , mkConnectionDraining
  -- * Lenses
  , cdEnabled
  , cdTimeout
  ) where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the @ConnectionDraining@ attribute.
--
-- /See:/ 'mkConnectionDraining' smart constructor.
data ConnectionDraining = ConnectionDraining'
  { enabled :: Core.Bool
    -- ^ Specifies whether connection draining is enabled for the load balancer.
  , timeout :: Core.Maybe Core.Int
    -- ^ The maximum time, in seconds, to keep the existing connections open before deregistering the instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionDraining' value with any optional fields omitted.
mkConnectionDraining
    :: Core.Bool -- ^ 'enabled'
    -> ConnectionDraining
mkConnectionDraining enabled
  = ConnectionDraining'{enabled, timeout = Core.Nothing}

-- | Specifies whether connection draining is enabled for the load balancer.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEnabled :: Lens.Lens' ConnectionDraining Core.Bool
cdEnabled = Lens.field @"enabled"
{-# INLINEABLE cdEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The maximum time, in seconds, to keep the existing connections open before deregistering the instances.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTimeout :: Lens.Lens' ConnectionDraining (Core.Maybe Core.Int)
cdTimeout = Lens.field @"timeout"
{-# INLINEABLE cdTimeout #-}
{-# DEPRECATED timeout "Use generic-lens or generic-optics with 'timeout' instead"  #-}

instance Core.ToQuery ConnectionDraining where
        toQuery ConnectionDraining{..}
          = Core.toQueryPair "Enabled" enabled Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Timeout") timeout

instance Core.FromXML ConnectionDraining where
        parseXML x
          = ConnectionDraining' Core.<$>
              (x Core..@ "Enabled") Core.<*> x Core..@? "Timeout"
