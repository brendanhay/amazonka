{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcPeeringConnectionOptionsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcPeeringConnectionOptionsDescription
  ( VpcPeeringConnectionOptionsDescription (..)
  -- * Smart constructor
  , mkVpcPeeringConnectionOptionsDescription
  -- * Lenses
  , vpcodAllowDnsResolutionFromRemoteVpc
  , vpcodAllowEgressFromLocalClassicLinkToRemoteVpc
  , vpcodAllowEgressFromLocalVpcToRemoteClassicLink
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the VPC peering connection options.
--
-- /See:/ 'mkVpcPeeringConnectionOptionsDescription' smart constructor.
data VpcPeeringConnectionOptionsDescription = VpcPeeringConnectionOptionsDescription'
  { allowDnsResolutionFromRemoteVpc :: Core.Maybe Core.Bool
    -- ^ Indicates whether a local VPC can resolve public DNS hostnames to private IP addresses when queried from instances in a peer VPC.
  , allowEgressFromLocalClassicLinkToRemoteVpc :: Core.Maybe Core.Bool
    -- ^ Indicates whether a local ClassicLink connection can communicate with the peer VPC over the VPC peering connection.
  , allowEgressFromLocalVpcToRemoteClassicLink :: Core.Maybe Core.Bool
    -- ^ Indicates whether a local VPC can communicate with a ClassicLink connection in the peer VPC over the VPC peering connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcPeeringConnectionOptionsDescription' value with any optional fields omitted.
mkVpcPeeringConnectionOptionsDescription
    :: VpcPeeringConnectionOptionsDescription
mkVpcPeeringConnectionOptionsDescription
  = VpcPeeringConnectionOptionsDescription'{allowDnsResolutionFromRemoteVpc
                                              = Core.Nothing,
                                            allowEgressFromLocalClassicLinkToRemoteVpc =
                                              Core.Nothing,
                                            allowEgressFromLocalVpcToRemoteClassicLink =
                                              Core.Nothing}

-- | Indicates whether a local VPC can resolve public DNS hostnames to private IP addresses when queried from instances in a peer VPC.
--
-- /Note:/ Consider using 'allowDnsResolutionFromRemoteVpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcodAllowDnsResolutionFromRemoteVpc :: Lens.Lens' VpcPeeringConnectionOptionsDescription (Core.Maybe Core.Bool)
vpcodAllowDnsResolutionFromRemoteVpc = Lens.field @"allowDnsResolutionFromRemoteVpc"
{-# INLINEABLE vpcodAllowDnsResolutionFromRemoteVpc #-}
{-# DEPRECATED allowDnsResolutionFromRemoteVpc "Use generic-lens or generic-optics with 'allowDnsResolutionFromRemoteVpc' instead"  #-}

-- | Indicates whether a local ClassicLink connection can communicate with the peer VPC over the VPC peering connection.
--
-- /Note:/ Consider using 'allowEgressFromLocalClassicLinkToRemoteVpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcodAllowEgressFromLocalClassicLinkToRemoteVpc :: Lens.Lens' VpcPeeringConnectionOptionsDescription (Core.Maybe Core.Bool)
vpcodAllowEgressFromLocalClassicLinkToRemoteVpc = Lens.field @"allowEgressFromLocalClassicLinkToRemoteVpc"
{-# INLINEABLE vpcodAllowEgressFromLocalClassicLinkToRemoteVpc #-}
{-# DEPRECATED allowEgressFromLocalClassicLinkToRemoteVpc "Use generic-lens or generic-optics with 'allowEgressFromLocalClassicLinkToRemoteVpc' instead"  #-}

-- | Indicates whether a local VPC can communicate with a ClassicLink connection in the peer VPC over the VPC peering connection.
--
-- /Note:/ Consider using 'allowEgressFromLocalVpcToRemoteClassicLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcodAllowEgressFromLocalVpcToRemoteClassicLink :: Lens.Lens' VpcPeeringConnectionOptionsDescription (Core.Maybe Core.Bool)
vpcodAllowEgressFromLocalVpcToRemoteClassicLink = Lens.field @"allowEgressFromLocalVpcToRemoteClassicLink"
{-# INLINEABLE vpcodAllowEgressFromLocalVpcToRemoteClassicLink #-}
{-# DEPRECATED allowEgressFromLocalVpcToRemoteClassicLink "Use generic-lens or generic-optics with 'allowEgressFromLocalVpcToRemoteClassicLink' instead"  #-}

instance Core.FromXML VpcPeeringConnectionOptionsDescription where
        parseXML x
          = VpcPeeringConnectionOptionsDescription' Core.<$>
              (x Core..@? "allowDnsResolutionFromRemoteVpc") Core.<*>
                x Core..@? "allowEgressFromLocalClassicLinkToRemoteVpc"
                Core.<*> x Core..@? "allowEgressFromLocalVpcToRemoteClassicLink"
