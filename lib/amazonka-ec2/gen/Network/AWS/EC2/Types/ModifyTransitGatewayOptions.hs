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
    mtgoVPNEcmpSupport,
    mtgoAutoAcceptSharedAttachments,
    mtgoPropagationDefaultRouteTableId,
    mtgoDefaultRouteTableAssociation,
    mtgoAssociationDefaultRouteTableId,
    mtgoDefaultRouteTablePropagation,
    mtgoDNSSupport,
  )
where

import Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
import Network.AWS.EC2.Types.DNSSupportValue
import Network.AWS.EC2.Types.DefaultRouteTableAssociationValue
import Network.AWS.EC2.Types.DefaultRouteTablePropagationValue
import Network.AWS.EC2.Types.VPNEcmpSupportValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The transit gateway options.
--
-- /See:/ 'mkModifyTransitGatewayOptions' smart constructor.
data ModifyTransitGatewayOptions = ModifyTransitGatewayOptions'
  { vpnEcmpSupport ::
      Lude.Maybe VPNEcmpSupportValue,
    autoAcceptSharedAttachments ::
      Lude.Maybe
        AutoAcceptSharedAttachmentsValue,
    propagationDefaultRouteTableId ::
      Lude.Maybe Lude.Text,
    defaultRouteTableAssociation ::
      Lude.Maybe
        DefaultRouteTableAssociationValue,
    associationDefaultRouteTableId ::
      Lude.Maybe Lude.Text,
    defaultRouteTablePropagation ::
      Lude.Maybe
        DefaultRouteTablePropagationValue,
    dnsSupport ::
      Lude.Maybe DNSSupportValue
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTransitGatewayOptions' with the minimum fields required to make a request.
--
-- * 'associationDefaultRouteTableId' - The ID of the default association route table.
-- * 'autoAcceptSharedAttachments' - Enable or disable automatic acceptance of attachment requests.
-- * 'defaultRouteTableAssociation' - Enable or disable automatic association with the default association route table.
-- * 'defaultRouteTablePropagation' - Enable or disable automatic propagation of routes to the default propagation route table.
-- * 'dnsSupport' - Enable or disable DNS support.
-- * 'propagationDefaultRouteTableId' - The ID of the default propagation route table.
-- * 'vpnEcmpSupport' - Enable or disable Equal Cost Multipath Protocol support.
mkModifyTransitGatewayOptions ::
  ModifyTransitGatewayOptions
mkModifyTransitGatewayOptions =
  ModifyTransitGatewayOptions'
    { vpnEcmpSupport = Lude.Nothing,
      autoAcceptSharedAttachments = Lude.Nothing,
      propagationDefaultRouteTableId = Lude.Nothing,
      defaultRouteTableAssociation = Lude.Nothing,
      associationDefaultRouteTableId = Lude.Nothing,
      defaultRouteTablePropagation = Lude.Nothing,
      dnsSupport = Lude.Nothing
    }

-- | Enable or disable Equal Cost Multipath Protocol support.
--
-- /Note:/ Consider using 'vpnEcmpSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoVPNEcmpSupport :: Lens.Lens' ModifyTransitGatewayOptions (Lude.Maybe VPNEcmpSupportValue)
mtgoVPNEcmpSupport = Lens.lens (vpnEcmpSupport :: ModifyTransitGatewayOptions -> Lude.Maybe VPNEcmpSupportValue) (\s a -> s {vpnEcmpSupport = a} :: ModifyTransitGatewayOptions)
{-# DEPRECATED mtgoVPNEcmpSupport "Use generic-lens or generic-optics with 'vpnEcmpSupport' instead." #-}

-- | Enable or disable automatic acceptance of attachment requests.
--
-- /Note:/ Consider using 'autoAcceptSharedAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoAutoAcceptSharedAttachments :: Lens.Lens' ModifyTransitGatewayOptions (Lude.Maybe AutoAcceptSharedAttachmentsValue)
mtgoAutoAcceptSharedAttachments = Lens.lens (autoAcceptSharedAttachments :: ModifyTransitGatewayOptions -> Lude.Maybe AutoAcceptSharedAttachmentsValue) (\s a -> s {autoAcceptSharedAttachments = a} :: ModifyTransitGatewayOptions)
{-# DEPRECATED mtgoAutoAcceptSharedAttachments "Use generic-lens or generic-optics with 'autoAcceptSharedAttachments' instead." #-}

-- | The ID of the default propagation route table.
--
-- /Note:/ Consider using 'propagationDefaultRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoPropagationDefaultRouteTableId :: Lens.Lens' ModifyTransitGatewayOptions (Lude.Maybe Lude.Text)
mtgoPropagationDefaultRouteTableId = Lens.lens (propagationDefaultRouteTableId :: ModifyTransitGatewayOptions -> Lude.Maybe Lude.Text) (\s a -> s {propagationDefaultRouteTableId = a} :: ModifyTransitGatewayOptions)
{-# DEPRECATED mtgoPropagationDefaultRouteTableId "Use generic-lens or generic-optics with 'propagationDefaultRouteTableId' instead." #-}

-- | Enable or disable automatic association with the default association route table.
--
-- /Note:/ Consider using 'defaultRouteTableAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoDefaultRouteTableAssociation :: Lens.Lens' ModifyTransitGatewayOptions (Lude.Maybe DefaultRouteTableAssociationValue)
mtgoDefaultRouteTableAssociation = Lens.lens (defaultRouteTableAssociation :: ModifyTransitGatewayOptions -> Lude.Maybe DefaultRouteTableAssociationValue) (\s a -> s {defaultRouteTableAssociation = a} :: ModifyTransitGatewayOptions)
{-# DEPRECATED mtgoDefaultRouteTableAssociation "Use generic-lens or generic-optics with 'defaultRouteTableAssociation' instead." #-}

-- | The ID of the default association route table.
--
-- /Note:/ Consider using 'associationDefaultRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoAssociationDefaultRouteTableId :: Lens.Lens' ModifyTransitGatewayOptions (Lude.Maybe Lude.Text)
mtgoAssociationDefaultRouteTableId = Lens.lens (associationDefaultRouteTableId :: ModifyTransitGatewayOptions -> Lude.Maybe Lude.Text) (\s a -> s {associationDefaultRouteTableId = a} :: ModifyTransitGatewayOptions)
{-# DEPRECATED mtgoAssociationDefaultRouteTableId "Use generic-lens or generic-optics with 'associationDefaultRouteTableId' instead." #-}

-- | Enable or disable automatic propagation of routes to the default propagation route table.
--
-- /Note:/ Consider using 'defaultRouteTablePropagation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoDefaultRouteTablePropagation :: Lens.Lens' ModifyTransitGatewayOptions (Lude.Maybe DefaultRouteTablePropagationValue)
mtgoDefaultRouteTablePropagation = Lens.lens (defaultRouteTablePropagation :: ModifyTransitGatewayOptions -> Lude.Maybe DefaultRouteTablePropagationValue) (\s a -> s {defaultRouteTablePropagation = a} :: ModifyTransitGatewayOptions)
{-# DEPRECATED mtgoDefaultRouteTablePropagation "Use generic-lens or generic-optics with 'defaultRouteTablePropagation' instead." #-}

-- | Enable or disable DNS support.
--
-- /Note:/ Consider using 'dnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgoDNSSupport :: Lens.Lens' ModifyTransitGatewayOptions (Lude.Maybe DNSSupportValue)
mtgoDNSSupport = Lens.lens (dnsSupport :: ModifyTransitGatewayOptions -> Lude.Maybe DNSSupportValue) (\s a -> s {dnsSupport = a} :: ModifyTransitGatewayOptions)
{-# DEPRECATED mtgoDNSSupport "Use generic-lens or generic-optics with 'dnsSupport' instead." #-}

instance Lude.ToQuery ModifyTransitGatewayOptions where
  toQuery ModifyTransitGatewayOptions' {..} =
    Lude.mconcat
      [ "VpnEcmpSupport" Lude.=: vpnEcmpSupport,
        "AutoAcceptSharedAttachments" Lude.=: autoAcceptSharedAttachments,
        "PropagationDefaultRouteTableId"
          Lude.=: propagationDefaultRouteTableId,
        "DefaultRouteTableAssociation"
          Lude.=: defaultRouteTableAssociation,
        "AssociationDefaultRouteTableId"
          Lude.=: associationDefaultRouteTableId,
        "DefaultRouteTablePropagation"
          Lude.=: defaultRouteTablePropagation,
        "DnsSupport" Lude.=: dnsSupport
      ]
