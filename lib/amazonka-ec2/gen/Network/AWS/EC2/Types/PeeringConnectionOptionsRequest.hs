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
    pcorAllowEgressFromLocalVPCToRemoteClassicLink,
    pcorAllowEgressFromLocalClassicLinkToRemoteVPC,
    pcorAllowDNSResolutionFromRemoteVPC,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The VPC peering connection options.
--
-- /See:/ 'mkPeeringConnectionOptionsRequest' smart constructor.
data PeeringConnectionOptionsRequest = PeeringConnectionOptionsRequest'
  { allowEgressFromLocalVPCToRemoteClassicLink ::
      Lude.Maybe Lude.Bool,
    allowEgressFromLocalClassicLinkToRemoteVPC ::
      Lude.Maybe Lude.Bool,
    allowDNSResolutionFromRemoteVPC ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PeeringConnectionOptionsRequest' with the minimum fields required to make a request.
--
-- * 'allowDNSResolutionFromRemoteVPC' - If true, enables a local VPC to resolve public DNS hostnames to private IP addresses when queried from instances in the peer VPC.
-- * 'allowEgressFromLocalClassicLinkToRemoteVPC' - If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
-- * 'allowEgressFromLocalVPCToRemoteClassicLink' - If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
mkPeeringConnectionOptionsRequest ::
  PeeringConnectionOptionsRequest
mkPeeringConnectionOptionsRequest =
  PeeringConnectionOptionsRequest'
    { allowEgressFromLocalVPCToRemoteClassicLink =
        Lude.Nothing,
      allowEgressFromLocalClassicLinkToRemoteVPC = Lude.Nothing,
      allowDNSResolutionFromRemoteVPC = Lude.Nothing
    }

-- | If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
--
-- /Note:/ Consider using 'allowEgressFromLocalVPCToRemoteClassicLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcorAllowEgressFromLocalVPCToRemoteClassicLink :: Lens.Lens' PeeringConnectionOptionsRequest (Lude.Maybe Lude.Bool)
pcorAllowEgressFromLocalVPCToRemoteClassicLink = Lens.lens (allowEgressFromLocalVPCToRemoteClassicLink :: PeeringConnectionOptionsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {allowEgressFromLocalVPCToRemoteClassicLink = a} :: PeeringConnectionOptionsRequest)
{-# DEPRECATED pcorAllowEgressFromLocalVPCToRemoteClassicLink "Use generic-lens or generic-optics with 'allowEgressFromLocalVPCToRemoteClassicLink' instead." #-}

-- | If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
--
-- /Note:/ Consider using 'allowEgressFromLocalClassicLinkToRemoteVPC' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcorAllowEgressFromLocalClassicLinkToRemoteVPC :: Lens.Lens' PeeringConnectionOptionsRequest (Lude.Maybe Lude.Bool)
pcorAllowEgressFromLocalClassicLinkToRemoteVPC = Lens.lens (allowEgressFromLocalClassicLinkToRemoteVPC :: PeeringConnectionOptionsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {allowEgressFromLocalClassicLinkToRemoteVPC = a} :: PeeringConnectionOptionsRequest)
{-# DEPRECATED pcorAllowEgressFromLocalClassicLinkToRemoteVPC "Use generic-lens or generic-optics with 'allowEgressFromLocalClassicLinkToRemoteVPC' instead." #-}

-- | If true, enables a local VPC to resolve public DNS hostnames to private IP addresses when queried from instances in the peer VPC.
--
-- /Note:/ Consider using 'allowDNSResolutionFromRemoteVPC' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcorAllowDNSResolutionFromRemoteVPC :: Lens.Lens' PeeringConnectionOptionsRequest (Lude.Maybe Lude.Bool)
pcorAllowDNSResolutionFromRemoteVPC = Lens.lens (allowDNSResolutionFromRemoteVPC :: PeeringConnectionOptionsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {allowDNSResolutionFromRemoteVPC = a} :: PeeringConnectionOptionsRequest)
{-# DEPRECATED pcorAllowDNSResolutionFromRemoteVPC "Use generic-lens or generic-optics with 'allowDNSResolutionFromRemoteVPC' instead." #-}

instance Lude.ToQuery PeeringConnectionOptionsRequest where
  toQuery PeeringConnectionOptionsRequest' {..} =
    Lude.mconcat
      [ "AllowEgressFromLocalVpcToRemoteClassicLink"
          Lude.=: allowEgressFromLocalVPCToRemoteClassicLink,
        "AllowEgressFromLocalClassicLinkToRemoteVpc"
          Lude.=: allowEgressFromLocalClassicLinkToRemoteVPC,
        "AllowDnsResolutionFromRemoteVpc"
          Lude.=: allowDNSResolutionFromRemoteVPC
      ]
