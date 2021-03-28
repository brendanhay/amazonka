{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
  ( LocalGatewayRouteTableVirtualInterfaceGroupAssociation (..)
  -- * Smart constructor
  , mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  -- * Lenses
  , lgrtvigaLocalGatewayId
  , lgrtvigaLocalGatewayRouteTableArn
  , lgrtvigaLocalGatewayRouteTableId
  , lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId
  , lgrtvigaLocalGatewayVirtualInterfaceGroupId
  , lgrtvigaOwnerId
  , lgrtvigaState
  , lgrtvigaTags
  ) where

import qualified Network.AWS.EC2.Types.LocalGatewayRouteTableArn as Types
import qualified Network.AWS.EC2.Types.LocalGatewayRouteTableId as Types
import qualified Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociationId as Types
import qualified Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceGroupId as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an association between a local gateway route table and a virtual interface group.
--
-- /See:/ 'mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation' smart constructor.
data LocalGatewayRouteTableVirtualInterfaceGroupAssociation = LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
  { localGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the local gateway.
  , localGatewayRouteTableArn :: Core.Maybe Types.LocalGatewayRouteTableArn
    -- ^ The Amazon Resource Name (ARN) of the local gateway route table for the virtual interface group.
  , localGatewayRouteTableId :: Core.Maybe Types.LocalGatewayRouteTableId
    -- ^ The ID of the local gateway route table.
  , localGatewayRouteTableVirtualInterfaceGroupAssociationId :: Core.Maybe Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociationId
    -- ^ The ID of the association.
  , localGatewayVirtualInterfaceGroupId :: Core.Maybe Types.LocalGatewayVirtualInterfaceGroupId
    -- ^ The ID of the virtual interface group.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID that owns the local gateway virtual interface group association.
  , state :: Core.Maybe Core.Text
    -- ^ The state of the association.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags assigned to the association.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalGatewayRouteTableVirtualInterfaceGroupAssociation' value with any optional fields omitted.
mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation
    :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation
mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  = LocalGatewayRouteTableVirtualInterfaceGroupAssociation'{localGatewayId
                                                              = Core.Nothing,
                                                            localGatewayRouteTableArn =
                                                              Core.Nothing,
                                                            localGatewayRouteTableId = Core.Nothing,
                                                            localGatewayRouteTableVirtualInterfaceGroupAssociationId
                                                              = Core.Nothing,
                                                            localGatewayVirtualInterfaceGroupId =
                                                              Core.Nothing,
                                                            ownerId = Core.Nothing,
                                                            state = Core.Nothing,
                                                            tags = Core.Nothing}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaLocalGatewayId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Core.Text)
lgrtvigaLocalGatewayId = Lens.field @"localGatewayId"
{-# INLINEABLE lgrtvigaLocalGatewayId #-}
{-# DEPRECATED localGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the local gateway route table for the virtual interface group.
--
-- /Note:/ Consider using 'localGatewayRouteTableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaLocalGatewayRouteTableArn :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Types.LocalGatewayRouteTableArn)
lgrtvigaLocalGatewayRouteTableArn = Lens.field @"localGatewayRouteTableArn"
{-# INLINEABLE lgrtvigaLocalGatewayRouteTableArn #-}
{-# DEPRECATED localGatewayRouteTableArn "Use generic-lens or generic-optics with 'localGatewayRouteTableArn' instead"  #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaLocalGatewayRouteTableId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Types.LocalGatewayRouteTableId)
lgrtvigaLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# INLINEABLE lgrtvigaLocalGatewayRouteTableId #-}
{-# DEPRECATED localGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead"  #-}

-- | The ID of the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableVirtualInterfaceGroupAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociationId)
lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId = Lens.field @"localGatewayRouteTableVirtualInterfaceGroupAssociationId"
{-# INLINEABLE lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId #-}
{-# DEPRECATED localGatewayRouteTableVirtualInterfaceGroupAssociationId "Use generic-lens or generic-optics with 'localGatewayRouteTableVirtualInterfaceGroupAssociationId' instead"  #-}

-- | The ID of the virtual interface group.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaLocalGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Types.LocalGatewayVirtualInterfaceGroupId)
lgrtvigaLocalGatewayVirtualInterfaceGroupId = Lens.field @"localGatewayVirtualInterfaceGroupId"
{-# INLINEABLE lgrtvigaLocalGatewayVirtualInterfaceGroupId #-}
{-# DEPRECATED localGatewayVirtualInterfaceGroupId "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroupId' instead"  #-}

-- | The AWS account ID that owns the local gateway virtual interface group association.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaOwnerId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Core.Text)
lgrtvigaOwnerId = Lens.field @"ownerId"
{-# INLINEABLE lgrtvigaOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaState :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe Core.Text)
lgrtvigaState = Lens.field @"state"
{-# INLINEABLE lgrtvigaState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The tags assigned to the association.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaTags :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Core.Maybe [Types.Tag])
lgrtvigaTags = Lens.field @"tags"
{-# INLINEABLE lgrtvigaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML
           LocalGatewayRouteTableVirtualInterfaceGroupAssociation
         where
        parseXML x
          = LocalGatewayRouteTableVirtualInterfaceGroupAssociation' Core.<$>
              (x Core..@? "localGatewayId") Core.<*>
                x Core..@? "localGatewayRouteTableArn"
                Core.<*> x Core..@? "localGatewayRouteTableId"
                Core.<*>
                x Core..@?
                  "localGatewayRouteTableVirtualInterfaceGroupAssociationId"
                Core.<*> x Core..@? "localGatewayVirtualInterfaceGroupId"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
