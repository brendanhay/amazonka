{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpnGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpnGateway
  ( VpnGateway (..)
  -- * Smart constructor
  , mkVpnGateway
  -- * Lenses
  , vgAmazonSideAsn
  , vgAvailabilityZone
  , vgState
  , vgTags
  , vgType
  , vgVpcAttachments
  , vgVpnGatewayId
  ) where

import qualified Network.AWS.EC2.Types.GatewayType as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.VpcAttachment as Types
import qualified Network.AWS.EC2.Types.VpnState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a virtual private gateway.
--
-- /See:/ 'mkVpnGateway' smart constructor.
data VpnGateway = VpnGateway'
  { amazonSideAsn :: Core.Maybe Core.Integer
    -- ^ The private Autonomous System Number (ASN) for the Amazon side of a BGP session.
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone where the virtual private gateway was created, if applicable. This field may be empty or not returned.
  , state :: Core.Maybe Types.VpnState
    -- ^ The current state of the virtual private gateway.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the virtual private gateway.
  , type' :: Core.Maybe Types.GatewayType
    -- ^ The type of VPN connection the virtual private gateway supports.
  , vpcAttachments :: Core.Maybe [Types.VpcAttachment]
    -- ^ Any VPCs attached to the virtual private gateway.
  , vpnGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the virtual private gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpnGateway' value with any optional fields omitted.
mkVpnGateway
    :: VpnGateway
mkVpnGateway
  = VpnGateway'{amazonSideAsn = Core.Nothing,
                availabilityZone = Core.Nothing, state = Core.Nothing,
                tags = Core.Nothing, type' = Core.Nothing,
                vpcAttachments = Core.Nothing, vpnGatewayId = Core.Nothing}

-- | The private Autonomous System Number (ASN) for the Amazon side of a BGP session.
--
-- /Note:/ Consider using 'amazonSideAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgAmazonSideAsn :: Lens.Lens' VpnGateway (Core.Maybe Core.Integer)
vgAmazonSideAsn = Lens.field @"amazonSideAsn"
{-# INLINEABLE vgAmazonSideAsn #-}
{-# DEPRECATED amazonSideAsn "Use generic-lens or generic-optics with 'amazonSideAsn' instead"  #-}

-- | The Availability Zone where the virtual private gateway was created, if applicable. This field may be empty or not returned.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgAvailabilityZone :: Lens.Lens' VpnGateway (Core.Maybe Core.Text)
vgAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE vgAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The current state of the virtual private gateway.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgState :: Lens.Lens' VpnGateway (Core.Maybe Types.VpnState)
vgState = Lens.field @"state"
{-# INLINEABLE vgState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Any tags assigned to the virtual private gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgTags :: Lens.Lens' VpnGateway (Core.Maybe [Types.Tag])
vgTags = Lens.field @"tags"
{-# INLINEABLE vgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The type of VPN connection the virtual private gateway supports.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgType :: Lens.Lens' VpnGateway (Core.Maybe Types.GatewayType)
vgType = Lens.field @"type'"
{-# INLINEABLE vgType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | Any VPCs attached to the virtual private gateway.
--
-- /Note:/ Consider using 'vpcAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgVpcAttachments :: Lens.Lens' VpnGateway (Core.Maybe [Types.VpcAttachment])
vgVpcAttachments = Lens.field @"vpcAttachments"
{-# INLINEABLE vgVpcAttachments #-}
{-# DEPRECATED vpcAttachments "Use generic-lens or generic-optics with 'vpcAttachments' instead"  #-}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgVpnGatewayId :: Lens.Lens' VpnGateway (Core.Maybe Core.Text)
vgVpnGatewayId = Lens.field @"vpnGatewayId"
{-# INLINEABLE vgVpnGatewayId #-}
{-# DEPRECATED vpnGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead"  #-}

instance Core.FromXML VpnGateway where
        parseXML x
          = VpnGateway' Core.<$>
              (x Core..@? "amazonSideAsn") Core.<*> x Core..@? "availabilityZone"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "type"
                Core.<*>
                x Core..@? "attachments" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "vpnGatewayId"
