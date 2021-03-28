{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayRoute
  ( TransitGatewayRoute (..)
  -- * Smart constructor
  , mkTransitGatewayRoute
  -- * Lenses
  , tgrDestinationCidrBlock
  , tgrPrefixListId
  , tgrState
  , tgrTransitGatewayAttachments
  , tgrType
  ) where

import qualified Network.AWS.EC2.Types.PrefixListResourceId as Types
import qualified Network.AWS.EC2.Types.TransitGatewayRouteAttachment as Types
import qualified Network.AWS.EC2.Types.TransitGatewayRouteState as Types
import qualified Network.AWS.EC2.Types.TransitGatewayRouteType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a route for a transit gateway route table.
--
-- /See:/ 'mkTransitGatewayRoute' smart constructor.
data TransitGatewayRoute = TransitGatewayRoute'
  { destinationCidrBlock :: Core.Maybe Core.Text
    -- ^ The CIDR block used for destination matches.
  , prefixListId :: Core.Maybe Types.PrefixListResourceId
    -- ^ The ID of the prefix list used for destination matches.
  , state :: Core.Maybe Types.TransitGatewayRouteState
    -- ^ The state of the route.
  , transitGatewayAttachments :: Core.Maybe [Types.TransitGatewayRouteAttachment]
    -- ^ The attachments.
  , type' :: Core.Maybe Types.TransitGatewayRouteType
    -- ^ The route type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayRoute' value with any optional fields omitted.
mkTransitGatewayRoute
    :: TransitGatewayRoute
mkTransitGatewayRoute
  = TransitGatewayRoute'{destinationCidrBlock = Core.Nothing,
                         prefixListId = Core.Nothing, state = Core.Nothing,
                         transitGatewayAttachments = Core.Nothing, type' = Core.Nothing}

-- | The CIDR block used for destination matches.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrDestinationCidrBlock :: Lens.Lens' TransitGatewayRoute (Core.Maybe Core.Text)
tgrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE tgrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The ID of the prefix list used for destination matches.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrPrefixListId :: Lens.Lens' TransitGatewayRoute (Core.Maybe Types.PrefixListResourceId)
tgrPrefixListId = Lens.field @"prefixListId"
{-# INLINEABLE tgrPrefixListId #-}
{-# DEPRECATED prefixListId "Use generic-lens or generic-optics with 'prefixListId' instead"  #-}

-- | The state of the route.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrState :: Lens.Lens' TransitGatewayRoute (Core.Maybe Types.TransitGatewayRouteState)
tgrState = Lens.field @"state"
{-# INLINEABLE tgrState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrTransitGatewayAttachments :: Lens.Lens' TransitGatewayRoute (Core.Maybe [Types.TransitGatewayRouteAttachment])
tgrTransitGatewayAttachments = Lens.field @"transitGatewayAttachments"
{-# INLINEABLE tgrTransitGatewayAttachments #-}
{-# DEPRECATED transitGatewayAttachments "Use generic-lens or generic-optics with 'transitGatewayAttachments' instead"  #-}

-- | The route type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrType :: Lens.Lens' TransitGatewayRoute (Core.Maybe Types.TransitGatewayRouteType)
tgrType = Lens.field @"type'"
{-# INLINEABLE tgrType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromXML TransitGatewayRoute where
        parseXML x
          = TransitGatewayRoute' Core.<$>
              (x Core..@? "destinationCidrBlock") Core.<*>
                x Core..@? "prefixListId"
                Core.<*> x Core..@? "state"
                Core.<*>
                x Core..@? "transitGatewayAttachments" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "type"
