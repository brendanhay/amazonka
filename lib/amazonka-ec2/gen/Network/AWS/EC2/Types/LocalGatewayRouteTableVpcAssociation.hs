{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteTableVpcAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LocalGatewayRouteTableVpcAssociation
  ( LocalGatewayRouteTableVpcAssociation (..)
  -- * Smart constructor
  , mkLocalGatewayRouteTableVpcAssociation
  -- * Lenses
  , lgrtvaLocalGatewayId
  , lgrtvaLocalGatewayRouteTableArn
  , lgrtvaLocalGatewayRouteTableId
  , lgrtvaLocalGatewayRouteTableVpcAssociationId
  , lgrtvaOwnerId
  , lgrtvaState
  , lgrtvaTags
  , lgrtvaVpcId
  ) where

import qualified Network.AWS.EC2.Types.LocalGatewayRouteTableArn as Types
import qualified Network.AWS.EC2.Types.LocalGatewayRouteTableVpcAssociationId as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an association between a local gateway route table and a VPC.
--
-- /See:/ 'mkLocalGatewayRouteTableVpcAssociation' smart constructor.
data LocalGatewayRouteTableVpcAssociation = LocalGatewayRouteTableVpcAssociation'
  { localGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the local gateway.
  , localGatewayRouteTableArn :: Core.Maybe Types.LocalGatewayRouteTableArn
    -- ^ The Amazon Resource Name (ARN) of the local gateway route table for the association.
  , localGatewayRouteTableId :: Core.Maybe Core.Text
    -- ^ The ID of the local gateway route table.
  , localGatewayRouteTableVpcAssociationId :: Core.Maybe Types.LocalGatewayRouteTableVpcAssociationId
    -- ^ The ID of the association.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID that owns the local gateway route table for the association.
  , state :: Core.Maybe Core.Text
    -- ^ The state of the association.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags assigned to the association.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalGatewayRouteTableVpcAssociation' value with any optional fields omitted.
mkLocalGatewayRouteTableVpcAssociation
    :: LocalGatewayRouteTableVpcAssociation
mkLocalGatewayRouteTableVpcAssociation
  = LocalGatewayRouteTableVpcAssociation'{localGatewayId =
                                            Core.Nothing,
                                          localGatewayRouteTableArn = Core.Nothing,
                                          localGatewayRouteTableId = Core.Nothing,
                                          localGatewayRouteTableVpcAssociationId = Core.Nothing,
                                          ownerId = Core.Nothing, state = Core.Nothing,
                                          tags = Core.Nothing, vpcId = Core.Nothing}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaLocalGatewayId :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Core.Maybe Core.Text)
lgrtvaLocalGatewayId = Lens.field @"localGatewayId"
{-# INLINEABLE lgrtvaLocalGatewayId #-}
{-# DEPRECATED localGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the local gateway route table for the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaLocalGatewayRouteTableArn :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Core.Maybe Types.LocalGatewayRouteTableArn)
lgrtvaLocalGatewayRouteTableArn = Lens.field @"localGatewayRouteTableArn"
{-# INLINEABLE lgrtvaLocalGatewayRouteTableArn #-}
{-# DEPRECATED localGatewayRouteTableArn "Use generic-lens or generic-optics with 'localGatewayRouteTableArn' instead"  #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaLocalGatewayRouteTableId :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Core.Maybe Core.Text)
lgrtvaLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# INLINEABLE lgrtvaLocalGatewayRouteTableId #-}
{-# DEPRECATED localGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead"  #-}

-- | The ID of the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableVpcAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaLocalGatewayRouteTableVpcAssociationId :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Core.Maybe Types.LocalGatewayRouteTableVpcAssociationId)
lgrtvaLocalGatewayRouteTableVpcAssociationId = Lens.field @"localGatewayRouteTableVpcAssociationId"
{-# INLINEABLE lgrtvaLocalGatewayRouteTableVpcAssociationId #-}
{-# DEPRECATED localGatewayRouteTableVpcAssociationId "Use generic-lens or generic-optics with 'localGatewayRouteTableVpcAssociationId' instead"  #-}

-- | The AWS account ID that owns the local gateway route table for the association.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaOwnerId :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Core.Maybe Core.Text)
lgrtvaOwnerId = Lens.field @"ownerId"
{-# INLINEABLE lgrtvaOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaState :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Core.Maybe Core.Text)
lgrtvaState = Lens.field @"state"
{-# INLINEABLE lgrtvaState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The tags assigned to the association.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaTags :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Core.Maybe [Types.Tag])
lgrtvaTags = Lens.field @"tags"
{-# INLINEABLE lgrtvaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaVpcId :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Core.Maybe Core.Text)
lgrtvaVpcId = Lens.field @"vpcId"
{-# INLINEABLE lgrtvaVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML LocalGatewayRouteTableVpcAssociation where
        parseXML x
          = LocalGatewayRouteTableVpcAssociation' Core.<$>
              (x Core..@? "localGatewayId") Core.<*>
                x Core..@? "localGatewayRouteTableArn"
                Core.<*> x Core..@? "localGatewayRouteTableId"
                Core.<*> x Core..@? "localGatewayRouteTableVpcAssociationId"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "vpcId"
