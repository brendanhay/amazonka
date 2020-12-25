{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PeeringConnectionOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PeeringConnectionOptionsRequest
  ( PeeringConnectionOptionsRequest (..),

    -- * Smart constructor
    mkPeeringConnectionOptionsRequest,

    -- * Lenses
    pcorAllowDnsResolutionFromRemoteVpc,
    pcorAllowEgressFromLocalClassicLinkToRemoteVpc,
    pcorAllowEgressFromLocalVpcToRemoteClassicLink,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The VPC peering connection options.
--
-- /See:/ 'mkPeeringConnectionOptionsRequest' smart constructor.
data PeeringConnectionOptionsRequest = PeeringConnectionOptionsRequest'
  { -- | If true, enables a local VPC to resolve public DNS hostnames to private IP addresses when queried from instances in the peer VPC.
    allowDnsResolutionFromRemoteVpc :: Core.Maybe Core.Bool,
    -- | If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
    allowEgressFromLocalClassicLinkToRemoteVpc :: Core.Maybe Core.Bool,
    -- | If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
    allowEgressFromLocalVpcToRemoteClassicLink :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PeeringConnectionOptionsRequest' value with any optional fields omitted.
mkPeeringConnectionOptionsRequest ::
  PeeringConnectionOptionsRequest
mkPeeringConnectionOptionsRequest =
  PeeringConnectionOptionsRequest'
    { allowDnsResolutionFromRemoteVpc =
        Core.Nothing,
      allowEgressFromLocalClassicLinkToRemoteVpc = Core.Nothing,
      allowEgressFromLocalVpcToRemoteClassicLink = Core.Nothing
    }

-- | If true, enables a local VPC to resolve public DNS hostnames to private IP addresses when queried from instances in the peer VPC.
--
-- /Note:/ Consider using 'allowDnsResolutionFromRemoteVpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcorAllowDnsResolutionFromRemoteVpc :: Lens.Lens' PeeringConnectionOptionsRequest (Core.Maybe Core.Bool)
pcorAllowDnsResolutionFromRemoteVpc = Lens.field @"allowDnsResolutionFromRemoteVpc"
{-# DEPRECATED pcorAllowDnsResolutionFromRemoteVpc "Use generic-lens or generic-optics with 'allowDnsResolutionFromRemoteVpc' instead." #-}

-- | If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
--
-- /Note:/ Consider using 'allowEgressFromLocalClassicLinkToRemoteVpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcorAllowEgressFromLocalClassicLinkToRemoteVpc :: Lens.Lens' PeeringConnectionOptionsRequest (Core.Maybe Core.Bool)
pcorAllowEgressFromLocalClassicLinkToRemoteVpc = Lens.field @"allowEgressFromLocalClassicLinkToRemoteVpc"
{-# DEPRECATED pcorAllowEgressFromLocalClassicLinkToRemoteVpc "Use generic-lens or generic-optics with 'allowEgressFromLocalClassicLinkToRemoteVpc' instead." #-}

-- | If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
--
-- /Note:/ Consider using 'allowEgressFromLocalVpcToRemoteClassicLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcorAllowEgressFromLocalVpcToRemoteClassicLink :: Lens.Lens' PeeringConnectionOptionsRequest (Core.Maybe Core.Bool)
pcorAllowEgressFromLocalVpcToRemoteClassicLink = Lens.field @"allowEgressFromLocalVpcToRemoteClassicLink"
{-# DEPRECATED pcorAllowEgressFromLocalVpcToRemoteClassicLink "Use generic-lens or generic-optics with 'allowEgressFromLocalVpcToRemoteClassicLink' instead." #-}
