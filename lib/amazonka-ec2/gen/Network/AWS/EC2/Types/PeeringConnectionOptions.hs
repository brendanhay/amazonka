{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PeeringConnectionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PeeringConnectionOptions
  ( PeeringConnectionOptions (..)
  -- * Smart constructor
  , mkPeeringConnectionOptions
  -- * Lenses
  , pcoAllowDnsResolutionFromRemoteVpc
  , pcoAllowEgressFromLocalClassicLinkToRemoteVpc
  , pcoAllowEgressFromLocalVpcToRemoteClassicLink
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the VPC peering connection options.
--
-- /See:/ 'mkPeeringConnectionOptions' smart constructor.
data PeeringConnectionOptions = PeeringConnectionOptions'
  { allowDnsResolutionFromRemoteVpc :: Core.Maybe Core.Bool
    -- ^ If true, the public DNS hostnames of instances in the specified VPC resolve to private IP addresses when queried from instances in the peer VPC.
  , allowEgressFromLocalClassicLinkToRemoteVpc :: Core.Maybe Core.Bool
    -- ^ If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
  , allowEgressFromLocalVpcToRemoteClassicLink :: Core.Maybe Core.Bool
    -- ^ If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PeeringConnectionOptions' value with any optional fields omitted.
mkPeeringConnectionOptions
    :: PeeringConnectionOptions
mkPeeringConnectionOptions
  = PeeringConnectionOptions'{allowDnsResolutionFromRemoteVpc =
                                Core.Nothing,
                              allowEgressFromLocalClassicLinkToRemoteVpc = Core.Nothing,
                              allowEgressFromLocalVpcToRemoteClassicLink = Core.Nothing}

-- | If true, the public DNS hostnames of instances in the specified VPC resolve to private IP addresses when queried from instances in the peer VPC.
--
-- /Note:/ Consider using 'allowDnsResolutionFromRemoteVpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcoAllowDnsResolutionFromRemoteVpc :: Lens.Lens' PeeringConnectionOptions (Core.Maybe Core.Bool)
pcoAllowDnsResolutionFromRemoteVpc = Lens.field @"allowDnsResolutionFromRemoteVpc"
{-# INLINEABLE pcoAllowDnsResolutionFromRemoteVpc #-}
{-# DEPRECATED allowDnsResolutionFromRemoteVpc "Use generic-lens or generic-optics with 'allowDnsResolutionFromRemoteVpc' instead"  #-}

-- | If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
--
-- /Note:/ Consider using 'allowEgressFromLocalClassicLinkToRemoteVpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcoAllowEgressFromLocalClassicLinkToRemoteVpc :: Lens.Lens' PeeringConnectionOptions (Core.Maybe Core.Bool)
pcoAllowEgressFromLocalClassicLinkToRemoteVpc = Lens.field @"allowEgressFromLocalClassicLinkToRemoteVpc"
{-# INLINEABLE pcoAllowEgressFromLocalClassicLinkToRemoteVpc #-}
{-# DEPRECATED allowEgressFromLocalClassicLinkToRemoteVpc "Use generic-lens or generic-optics with 'allowEgressFromLocalClassicLinkToRemoteVpc' instead"  #-}

-- | If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
--
-- /Note:/ Consider using 'allowEgressFromLocalVpcToRemoteClassicLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcoAllowEgressFromLocalVpcToRemoteClassicLink :: Lens.Lens' PeeringConnectionOptions (Core.Maybe Core.Bool)
pcoAllowEgressFromLocalVpcToRemoteClassicLink = Lens.field @"allowEgressFromLocalVpcToRemoteClassicLink"
{-# INLINEABLE pcoAllowEgressFromLocalVpcToRemoteClassicLink #-}
{-# DEPRECATED allowEgressFromLocalVpcToRemoteClassicLink "Use generic-lens or generic-optics with 'allowEgressFromLocalVpcToRemoteClassicLink' instead"  #-}

instance Core.FromXML PeeringConnectionOptions where
        parseXML x
          = PeeringConnectionOptions' Core.<$>
              (x Core..@? "allowDnsResolutionFromRemoteVpc") Core.<*>
                x Core..@? "allowEgressFromLocalClassicLinkToRemoteVpc"
                Core.<*> x Core..@? "allowEgressFromLocalVpcToRemoteClassicLink"
