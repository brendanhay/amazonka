{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PeeringConnectionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PeeringConnectionOptions
  ( PeeringConnectionOptions (..),

    -- * Smart constructor
    mkPeeringConnectionOptions,

    -- * Lenses
    pcoAllowEgressFromLocalVPCToRemoteClassicLink,
    pcoAllowEgressFromLocalClassicLinkToRemoteVPC,
    pcoAllowDNSResolutionFromRemoteVPC,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the VPC peering connection options.
--
-- /See:/ 'mkPeeringConnectionOptions' smart constructor.
data PeeringConnectionOptions = PeeringConnectionOptions'
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

-- | Creates a value of 'PeeringConnectionOptions' with the minimum fields required to make a request.
--
-- * 'allowDNSResolutionFromRemoteVPC' - If true, the public DNS hostnames of instances in the specified VPC resolve to private IP addresses when queried from instances in the peer VPC.
-- * 'allowEgressFromLocalClassicLinkToRemoteVPC' - If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
-- * 'allowEgressFromLocalVPCToRemoteClassicLink' - If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
mkPeeringConnectionOptions ::
  PeeringConnectionOptions
mkPeeringConnectionOptions =
  PeeringConnectionOptions'
    { allowEgressFromLocalVPCToRemoteClassicLink =
        Lude.Nothing,
      allowEgressFromLocalClassicLinkToRemoteVPC = Lude.Nothing,
      allowDNSResolutionFromRemoteVPC = Lude.Nothing
    }

-- | If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
--
-- /Note:/ Consider using 'allowEgressFromLocalVPCToRemoteClassicLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcoAllowEgressFromLocalVPCToRemoteClassicLink :: Lens.Lens' PeeringConnectionOptions (Lude.Maybe Lude.Bool)
pcoAllowEgressFromLocalVPCToRemoteClassicLink = Lens.lens (allowEgressFromLocalVPCToRemoteClassicLink :: PeeringConnectionOptions -> Lude.Maybe Lude.Bool) (\s a -> s {allowEgressFromLocalVPCToRemoteClassicLink = a} :: PeeringConnectionOptions)
{-# DEPRECATED pcoAllowEgressFromLocalVPCToRemoteClassicLink "Use generic-lens or generic-optics with 'allowEgressFromLocalVPCToRemoteClassicLink' instead." #-}

-- | If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
--
-- /Note:/ Consider using 'allowEgressFromLocalClassicLinkToRemoteVPC' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcoAllowEgressFromLocalClassicLinkToRemoteVPC :: Lens.Lens' PeeringConnectionOptions (Lude.Maybe Lude.Bool)
pcoAllowEgressFromLocalClassicLinkToRemoteVPC = Lens.lens (allowEgressFromLocalClassicLinkToRemoteVPC :: PeeringConnectionOptions -> Lude.Maybe Lude.Bool) (\s a -> s {allowEgressFromLocalClassicLinkToRemoteVPC = a} :: PeeringConnectionOptions)
{-# DEPRECATED pcoAllowEgressFromLocalClassicLinkToRemoteVPC "Use generic-lens or generic-optics with 'allowEgressFromLocalClassicLinkToRemoteVPC' instead." #-}

-- | If true, the public DNS hostnames of instances in the specified VPC resolve to private IP addresses when queried from instances in the peer VPC.
--
-- /Note:/ Consider using 'allowDNSResolutionFromRemoteVPC' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcoAllowDNSResolutionFromRemoteVPC :: Lens.Lens' PeeringConnectionOptions (Lude.Maybe Lude.Bool)
pcoAllowDNSResolutionFromRemoteVPC = Lens.lens (allowDNSResolutionFromRemoteVPC :: PeeringConnectionOptions -> Lude.Maybe Lude.Bool) (\s a -> s {allowDNSResolutionFromRemoteVPC = a} :: PeeringConnectionOptions)
{-# DEPRECATED pcoAllowDNSResolutionFromRemoteVPC "Use generic-lens or generic-optics with 'allowDNSResolutionFromRemoteVPC' instead." #-}

instance Lude.FromXML PeeringConnectionOptions where
  parseXML x =
    PeeringConnectionOptions'
      Lude.<$> (x Lude..@? "allowEgressFromLocalVpcToRemoteClassicLink")
      Lude.<*> (x Lude..@? "allowEgressFromLocalClassicLinkToRemoteVpc")
      Lude.<*> (x Lude..@? "allowDnsResolutionFromRemoteVpc")
