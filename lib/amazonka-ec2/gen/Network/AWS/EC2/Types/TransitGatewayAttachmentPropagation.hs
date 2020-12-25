{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation
  ( TransitGatewayAttachmentPropagation (..),

    -- * Smart constructor
    mkTransitGatewayAttachmentPropagation,

    -- * Lenses
    tgapState,
    tgapTransitGatewayRouteTableId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.TransitGatewayPropagationState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a propagation route table.
--
-- /See:/ 'mkTransitGatewayAttachmentPropagation' smart constructor.
data TransitGatewayAttachmentPropagation = TransitGatewayAttachmentPropagation'
  { -- | The state of the propagation route table.
    state :: Core.Maybe Types.TransitGatewayPropagationState,
    -- | The ID of the propagation route table.
    transitGatewayRouteTableId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayAttachmentPropagation' value with any optional fields omitted.
mkTransitGatewayAttachmentPropagation ::
  TransitGatewayAttachmentPropagation
mkTransitGatewayAttachmentPropagation =
  TransitGatewayAttachmentPropagation'
    { state = Core.Nothing,
      transitGatewayRouteTableId = Core.Nothing
    }

-- | The state of the propagation route table.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgapState :: Lens.Lens' TransitGatewayAttachmentPropagation (Core.Maybe Types.TransitGatewayPropagationState)
tgapState = Lens.field @"state"
{-# DEPRECATED tgapState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the propagation route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgapTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayAttachmentPropagation (Core.Maybe Types.String)
tgapTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED tgapTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

instance Core.FromXML TransitGatewayAttachmentPropagation where
  parseXML x =
    TransitGatewayAttachmentPropagation'
      Core.<$> (x Core..@? "state")
      Core.<*> (x Core..@? "transitGatewayRouteTableId")
