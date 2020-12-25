{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
  ( TransitGatewayAttachmentAssociation (..),

    -- * Smart constructor
    mkTransitGatewayAttachmentAssociation,

    -- * Lenses
    tgaaState,
    tgaaTransitGatewayRouteTableId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAssociationState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an association.
--
-- /See:/ 'mkTransitGatewayAttachmentAssociation' smart constructor.
data TransitGatewayAttachmentAssociation = TransitGatewayAttachmentAssociation'
  { -- | The state of the association.
    state :: Core.Maybe Types.TransitGatewayAssociationState,
    -- | The ID of the route table for the transit gateway.
    transitGatewayRouteTableId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayAttachmentAssociation' value with any optional fields omitted.
mkTransitGatewayAttachmentAssociation ::
  TransitGatewayAttachmentAssociation
mkTransitGatewayAttachmentAssociation =
  TransitGatewayAttachmentAssociation'
    { state = Core.Nothing,
      transitGatewayRouteTableId = Core.Nothing
    }

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaaState :: Lens.Lens' TransitGatewayAttachmentAssociation (Core.Maybe Types.TransitGatewayAssociationState)
tgaaState = Lens.field @"state"
{-# DEPRECATED tgaaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the route table for the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaaTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayAttachmentAssociation (Core.Maybe Types.String)
tgaaTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED tgaaTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

instance Core.FromXML TransitGatewayAttachmentAssociation where
  parseXML x =
    TransitGatewayAttachmentAssociation'
      Core.<$> (x Core..@? "state")
      Core.<*> (x Core..@? "transitGatewayRouteTableId")
