{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpnStaticRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpnStaticRoute
  ( VpnStaticRoute (..)
  -- * Smart constructor
  , mkVpnStaticRoute
  -- * Lenses
  , vsrDestinationCidrBlock
  , vsrSource
  , vsrState
  ) where

import qualified Network.AWS.EC2.Types.VpnState as Types
import qualified Network.AWS.EC2.Types.VpnStaticRouteSource as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a static route for a VPN connection.
--
-- /See:/ 'mkVpnStaticRoute' smart constructor.
data VpnStaticRoute = VpnStaticRoute'
  { destinationCidrBlock :: Core.Maybe Core.Text
    -- ^ The CIDR block associated with the local subnet of the customer data center.
  , source :: Core.Maybe Types.VpnStaticRouteSource
    -- ^ Indicates how the routes were provided.
  , state :: Core.Maybe Types.VpnState
    -- ^ The current state of the static route.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpnStaticRoute' value with any optional fields omitted.
mkVpnStaticRoute
    :: VpnStaticRoute
mkVpnStaticRoute
  = VpnStaticRoute'{destinationCidrBlock = Core.Nothing,
                    source = Core.Nothing, state = Core.Nothing}

-- | The CIDR block associated with the local subnet of the customer data center.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsrDestinationCidrBlock :: Lens.Lens' VpnStaticRoute (Core.Maybe Core.Text)
vsrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE vsrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | Indicates how the routes were provided.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsrSource :: Lens.Lens' VpnStaticRoute (Core.Maybe Types.VpnStaticRouteSource)
vsrSource = Lens.field @"source"
{-# INLINEABLE vsrSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | The current state of the static route.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsrState :: Lens.Lens' VpnStaticRoute (Core.Maybe Types.VpnState)
vsrState = Lens.field @"state"
{-# INLINEABLE vsrState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromXML VpnStaticRoute where
        parseXML x
          = VpnStaticRoute' Core.<$>
              (x Core..@? "destinationCidrBlock") Core.<*> x Core..@? "source"
                Core.<*> x Core..@? "state"
