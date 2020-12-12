{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCPeeringConnectionOptionsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCPeeringConnectionOptionsDescription
  ( VPCPeeringConnectionOptionsDescription (..),

    -- * Smart constructor
    mkVPCPeeringConnectionOptionsDescription,

    -- * Lenses
    vpcodAllowEgressFromLocalVPCToRemoteClassicLink,
    vpcodAllowEgressFromLocalClassicLinkToRemoteVPC,
    vpcodAllowDNSResolutionFromRemoteVPC,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the VPC peering connection options.
--
-- /See:/ 'mkVPCPeeringConnectionOptionsDescription' smart constructor.
data VPCPeeringConnectionOptionsDescription = VPCPeeringConnectionOptionsDescription'
  { allowEgressFromLocalVPCToRemoteClassicLink ::
      Lude.Maybe
        Lude.Bool,
    allowEgressFromLocalClassicLinkToRemoteVPC ::
      Lude.Maybe
        Lude.Bool,
    allowDNSResolutionFromRemoteVPC ::
      Lude.Maybe
        Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCPeeringConnectionOptionsDescription' with the minimum fields required to make a request.
--
-- * 'allowDNSResolutionFromRemoteVPC' - Indicates whether a local VPC can resolve public DNS hostnames to private IP addresses when queried from instances in a peer VPC.
-- * 'allowEgressFromLocalClassicLinkToRemoteVPC' - Indicates whether a local ClassicLink connection can communicate with the peer VPC over the VPC peering connection.
-- * 'allowEgressFromLocalVPCToRemoteClassicLink' - Indicates whether a local VPC can communicate with a ClassicLink connection in the peer VPC over the VPC peering connection.
mkVPCPeeringConnectionOptionsDescription ::
  VPCPeeringConnectionOptionsDescription
mkVPCPeeringConnectionOptionsDescription =
  VPCPeeringConnectionOptionsDescription'
    { allowEgressFromLocalVPCToRemoteClassicLink =
        Lude.Nothing,
      allowEgressFromLocalClassicLinkToRemoteVPC =
        Lude.Nothing,
      allowDNSResolutionFromRemoteVPC = Lude.Nothing
    }

-- | Indicates whether a local VPC can communicate with a ClassicLink connection in the peer VPC over the VPC peering connection.
--
-- /Note:/ Consider using 'allowEgressFromLocalVPCToRemoteClassicLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcodAllowEgressFromLocalVPCToRemoteClassicLink :: Lens.Lens' VPCPeeringConnectionOptionsDescription (Lude.Maybe Lude.Bool)
vpcodAllowEgressFromLocalVPCToRemoteClassicLink = Lens.lens (allowEgressFromLocalVPCToRemoteClassicLink :: VPCPeeringConnectionOptionsDescription -> Lude.Maybe Lude.Bool) (\s a -> s {allowEgressFromLocalVPCToRemoteClassicLink = a} :: VPCPeeringConnectionOptionsDescription)
{-# DEPRECATED vpcodAllowEgressFromLocalVPCToRemoteClassicLink "Use generic-lens or generic-optics with 'allowEgressFromLocalVPCToRemoteClassicLink' instead." #-}

-- | Indicates whether a local ClassicLink connection can communicate with the peer VPC over the VPC peering connection.
--
-- /Note:/ Consider using 'allowEgressFromLocalClassicLinkToRemoteVPC' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcodAllowEgressFromLocalClassicLinkToRemoteVPC :: Lens.Lens' VPCPeeringConnectionOptionsDescription (Lude.Maybe Lude.Bool)
vpcodAllowEgressFromLocalClassicLinkToRemoteVPC = Lens.lens (allowEgressFromLocalClassicLinkToRemoteVPC :: VPCPeeringConnectionOptionsDescription -> Lude.Maybe Lude.Bool) (\s a -> s {allowEgressFromLocalClassicLinkToRemoteVPC = a} :: VPCPeeringConnectionOptionsDescription)
{-# DEPRECATED vpcodAllowEgressFromLocalClassicLinkToRemoteVPC "Use generic-lens or generic-optics with 'allowEgressFromLocalClassicLinkToRemoteVPC' instead." #-}

-- | Indicates whether a local VPC can resolve public DNS hostnames to private IP addresses when queried from instances in a peer VPC.
--
-- /Note:/ Consider using 'allowDNSResolutionFromRemoteVPC' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcodAllowDNSResolutionFromRemoteVPC :: Lens.Lens' VPCPeeringConnectionOptionsDescription (Lude.Maybe Lude.Bool)
vpcodAllowDNSResolutionFromRemoteVPC = Lens.lens (allowDNSResolutionFromRemoteVPC :: VPCPeeringConnectionOptionsDescription -> Lude.Maybe Lude.Bool) (\s a -> s {allowDNSResolutionFromRemoteVPC = a} :: VPCPeeringConnectionOptionsDescription)
{-# DEPRECATED vpcodAllowDNSResolutionFromRemoteVPC "Use generic-lens or generic-optics with 'allowDNSResolutionFromRemoteVPC' instead." #-}

instance Lude.FromXML VPCPeeringConnectionOptionsDescription where
  parseXML x =
    VPCPeeringConnectionOptionsDescription'
      Lude.<$> (x Lude..@? "allowEgressFromLocalVpcToRemoteClassicLink")
      Lude.<*> (x Lude..@? "allowEgressFromLocalClassicLinkToRemoteVpc")
      Lude.<*> (x Lude..@? "allowDnsResolutionFromRemoteVpc")
