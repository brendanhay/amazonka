{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ModifyTransitGatewayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ModifyTransitGatewayOptions
  ( ModifyTransitGatewayOptions (..),

    -- * Smart constructor
    mkModifyTransitGatewayOptions,

    -- * Lenses
    mtgoAssociationDefaultRouteTableId,
    mtgoAutoAcceptSharedAttachments,
    mtgoDefaultRouteTableAssociation,
    mtgoDefaultRouteTablePropagation,
    mtgoDnsSupport,
    mtgoPropagationDefaultRouteTableId,
    mtgoVpnEcmpSupport,
  )
where

import qualified Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue as Types
import qualified Network.AWS.EC2.Types.DefaultRouteTableAssociationValue as Types
import qualified Network.AWS.EC2.Types.DefaultRouteTablePropagationValue as Types
import qualified Network.AWS.EC2.Types.DnsSupportValue as Types
import qualified Network.AWS.EC2.Types.TransitGatewayRouteTableId as Types
import qualified Network.AWS.EC2.Types.VpnEcmpSupportValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The transit gateway options.
--
-- /See:/ 'mkModifyTransitGatewayOptions' smart constructor.
data ModifyTransitGatewayOptions = ModifyTransitGatewayOptions'
  { -- | The ID of the default association route table.
    associationDefaultRouteTableId :: Core.Maybe Types.TransitGatewayRouteTableId,
    -- | Enable or disable automatic acceptance of attachment requests.
    autoAcceptSharedAttachments :: Core.Maybe Types.AutoAcceptSharedAttachmentsValue,
    -- | Enable or disable automatic association with the default association route table.
    defaultRouteTableAssociation :: Core.Maybe Types.DefaultRouteTableAssociationValue,
    -- | Enable or disable automatic propagation of routes to the default propagation route table.
    defaultRouteTablePropagation :: Core.Maybe Types.DefaultRouteTablePropagationValue,
    -- | Enable or disable DNS support.
    dnsSupport :: Core.Maybe Types.DnsSupportValue,
    -- | The ID of the default propagation route table.
    propagationDefaultRouteTableId :: Core.Maybe Types.TransitGatewayRouteTableId,
    -- | Enable or disable Equal Cost Multipath Protocol support.
    vpnEcmpSupport :: Core.Maybe Types.VpnEcmpSupportValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTransitGatewayOptions' value with any optional fields omitted.
mkModifyTransitGatewayOptions ::
  ModifyTransitGatewayOptions
mkModifyTransitGatewayOptions =
  ModifyTransitGatewayOptions'
    { associationDefaultRouteTableId =
        Core.Nothing,
      autoAcceptSharedAttachments = Core.Nothing,
      defaultRouteTableAssociation = Core.Nothing,
      defaultRouteTablePropagation = Core.Nothing,
      dnsSupport = Core.Nothing,
      propagationDefaultRouteTableId = Core.Nothing,
      vpnEcmpSupport = Core.Nothing
    }

-- | The ID of the default association route table.
--
-- /Note:/ Consider using 'associationDefaultRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoAssociationDefaultRouteTableId :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe Types.TransitGatewayRouteTableId)
mtgoAssociationDefaultRouteTableId = Lens.field @"associationDefaultRouteTableId"
{-# DEPRECATED mtgoAssociationDefaultRouteTableId "Use generic-lens or generic-optics with 'associationDefaultRouteTableId' instead." #-}

-- | Enable or disable automatic acceptance of attachment requests.
--
-- /Note:/ Consider using 'autoAcceptSharedAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoAutoAcceptSharedAttachments :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe Types.AutoAcceptSharedAttachmentsValue)
mtgoAutoAcceptSharedAttachments = Lens.field @"autoAcceptSharedAttachments"
{-# DEPRECATED mtgoAutoAcceptSharedAttachments "Use generic-lens or generic-optics with 'autoAcceptSharedAttachments' instead." #-}

-- | Enable or disable automatic association with the default association route table.
--
-- /Note:/ Consider using 'defaultRouteTableAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoDefaultRouteTableAssociation :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe Types.DefaultRouteTableAssociationValue)
mtgoDefaultRouteTableAssociation = Lens.field @"defaultRouteTableAssociation"
{-# DEPRECATED mtgoDefaultRouteTableAssociation "Use generic-lens or generic-optics with 'defaultRouteTableAssociation' instead." #-}

-- | Enable or disable automatic propagation of routes to the default propagation route table.
--
-- /Note:/ Consider using 'defaultRouteTablePropagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoDefaultRouteTablePropagation :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe Types.DefaultRouteTablePropagationValue)
mtgoDefaultRouteTablePropagation = Lens.field @"defaultRouteTablePropagation"
{-# DEPRECATED mtgoDefaultRouteTablePropagation "Use generic-lens or generic-optics with 'defaultRouteTablePropagation' instead." #-}

-- | Enable or disable DNS support.
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoDnsSupport :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe Types.DnsSupportValue)
mtgoDnsSupport = Lens.field @"dnsSupport"
{-# DEPRECATED mtgoDnsSupport "Use generic-lens or generic-optics with 'dnsSupport' instead." #-}

-- | The ID of the default propagation route table.
--
-- /Note:/ Consider using 'propagationDefaultRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoPropagationDefaultRouteTableId :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe Types.TransitGatewayRouteTableId)
mtgoPropagationDefaultRouteTableId = Lens.field @"propagationDefaultRouteTableId"
{-# DEPRECATED mtgoPropagationDefaultRouteTableId "Use generic-lens or generic-optics with 'propagationDefaultRouteTableId' instead." #-}

-- | Enable or disable Equal Cost Multipath Protocol support.
--
-- /Note:/ Consider using 'vpnEcmpSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoVpnEcmpSupport :: Lens.Lens' ModifyTransitGatewayOptions (Core.Maybe Types.VpnEcmpSupportValue)
mtgoVpnEcmpSupport = Lens.field @"vpnEcmpSupport"
{-# DEPRECATED mtgoVpnEcmpSupport "Use generic-lens or generic-optics with 'vpnEcmpSupport' instead." #-}
