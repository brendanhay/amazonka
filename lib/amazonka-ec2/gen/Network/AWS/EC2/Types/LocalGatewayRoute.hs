{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LocalGatewayRoute
  ( LocalGatewayRoute (..)
  -- * Smart constructor
  , mkLocalGatewayRoute
  -- * Lenses
  , lgrDestinationCidrBlock
  , lgrLocalGatewayRouteTableArn
  , lgrLocalGatewayRouteTableId
  , lgrLocalGatewayVirtualInterfaceGroupId
  , lgrOwnerId
  , lgrState
  , lgrType
  ) where

import qualified Network.AWS.EC2.Types.LocalGatewayRouteState as Types
import qualified Network.AWS.EC2.Types.LocalGatewayRouteTableArn as Types
import qualified Network.AWS.EC2.Types.LocalGatewayRouteTableId as Types
import qualified Network.AWS.EC2.Types.LocalGatewayRouteType as Types
import qualified Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceGroupId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a route for a local gateway route table.
--
-- /See:/ 'mkLocalGatewayRoute' smart constructor.
data LocalGatewayRoute = LocalGatewayRoute'
  { destinationCidrBlock :: Core.Maybe Core.Text
    -- ^ The CIDR block used for destination matches.
  , localGatewayRouteTableArn :: Core.Maybe Types.LocalGatewayRouteTableArn
    -- ^ The Amazon Resource Name (ARN) of the local gateway route table.
  , localGatewayRouteTableId :: Core.Maybe Types.LocalGatewayRouteTableId
    -- ^ The ID of the local gateway route table.
  , localGatewayVirtualInterfaceGroupId :: Core.Maybe Types.LocalGatewayVirtualInterfaceGroupId
    -- ^ The ID of the virtual interface group.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID that owns the local gateway route.
  , state :: Core.Maybe Types.LocalGatewayRouteState
    -- ^ The state of the route.
  , type' :: Core.Maybe Types.LocalGatewayRouteType
    -- ^ The route type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalGatewayRoute' value with any optional fields omitted.
mkLocalGatewayRoute
    :: LocalGatewayRoute
mkLocalGatewayRoute
  = LocalGatewayRoute'{destinationCidrBlock = Core.Nothing,
                       localGatewayRouteTableArn = Core.Nothing,
                       localGatewayRouteTableId = Core.Nothing,
                       localGatewayVirtualInterfaceGroupId = Core.Nothing,
                       ownerId = Core.Nothing, state = Core.Nothing, type' = Core.Nothing}

-- | The CIDR block used for destination matches.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrDestinationCidrBlock :: Lens.Lens' LocalGatewayRoute (Core.Maybe Core.Text)
lgrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE lgrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The Amazon Resource Name (ARN) of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrLocalGatewayRouteTableArn :: Lens.Lens' LocalGatewayRoute (Core.Maybe Types.LocalGatewayRouteTableArn)
lgrLocalGatewayRouteTableArn = Lens.field @"localGatewayRouteTableArn"
{-# INLINEABLE lgrLocalGatewayRouteTableArn #-}
{-# DEPRECATED localGatewayRouteTableArn "Use generic-lens or generic-optics with 'localGatewayRouteTableArn' instead"  #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrLocalGatewayRouteTableId :: Lens.Lens' LocalGatewayRoute (Core.Maybe Types.LocalGatewayRouteTableId)
lgrLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# INLINEABLE lgrLocalGatewayRouteTableId #-}
{-# DEPRECATED localGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead"  #-}

-- | The ID of the virtual interface group.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrLocalGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayRoute (Core.Maybe Types.LocalGatewayVirtualInterfaceGroupId)
lgrLocalGatewayVirtualInterfaceGroupId = Lens.field @"localGatewayVirtualInterfaceGroupId"
{-# INLINEABLE lgrLocalGatewayVirtualInterfaceGroupId #-}
{-# DEPRECATED localGatewayVirtualInterfaceGroupId "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroupId' instead"  #-}

-- | The AWS account ID that owns the local gateway route.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrOwnerId :: Lens.Lens' LocalGatewayRoute (Core.Maybe Core.Text)
lgrOwnerId = Lens.field @"ownerId"
{-# INLINEABLE lgrOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The state of the route.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrState :: Lens.Lens' LocalGatewayRoute (Core.Maybe Types.LocalGatewayRouteState)
lgrState = Lens.field @"state"
{-# INLINEABLE lgrState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The route type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrType :: Lens.Lens' LocalGatewayRoute (Core.Maybe Types.LocalGatewayRouteType)
lgrType = Lens.field @"type'"
{-# INLINEABLE lgrType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromXML LocalGatewayRoute where
        parseXML x
          = LocalGatewayRoute' Core.<$>
              (x Core..@? "destinationCidrBlock") Core.<*>
                x Core..@? "localGatewayRouteTableArn"
                Core.<*> x Core..@? "localGatewayRouteTableId"
                Core.<*> x Core..@? "localGatewayVirtualInterfaceGroupId"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "type"
