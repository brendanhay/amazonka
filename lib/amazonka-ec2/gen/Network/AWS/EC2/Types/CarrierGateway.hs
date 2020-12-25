{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CarrierGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CarrierGateway
  ( CarrierGateway (..),

    -- * Smart constructor
    mkCarrierGateway,

    -- * Lenses
    cgCarrierGatewayId,
    cgOwnerId,
    cgState,
    cgTags,
    cgVpcId,
  )
where

import qualified Network.AWS.EC2.Types.CarrierGatewayId as Types
import qualified Network.AWS.EC2.Types.CarrierGatewayState as Types
import qualified Network.AWS.EC2.Types.OwnerId as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a carrier gateway.
--
-- /See:/ 'mkCarrierGateway' smart constructor.
data CarrierGateway = CarrierGateway'
  { -- | The ID of the carrier gateway.
    carrierGatewayId :: Core.Maybe Types.CarrierGatewayId,
    -- | The AWS account ID of the owner of the carrier gateway.
    ownerId :: Core.Maybe Types.OwnerId,
    -- | The state of the carrier gateway.
    state :: Core.Maybe Types.CarrierGatewayState,
    -- | The tags assigned to the carrier gateway.
    tags :: Core.Maybe [Types.Tag],
    -- | The ID of the VPC associated with the carrier gateway.
    vpcId :: Core.Maybe Types.VpcId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CarrierGateway' value with any optional fields omitted.
mkCarrierGateway ::
  CarrierGateway
mkCarrierGateway =
  CarrierGateway'
    { carrierGatewayId = Core.Nothing,
      ownerId = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The ID of the carrier gateway.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgCarrierGatewayId :: Lens.Lens' CarrierGateway (Core.Maybe Types.CarrierGatewayId)
cgCarrierGatewayId = Lens.field @"carrierGatewayId"
{-# DEPRECATED cgCarrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead." #-}

-- | The AWS account ID of the owner of the carrier gateway.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgOwnerId :: Lens.Lens' CarrierGateway (Core.Maybe Types.OwnerId)
cgOwnerId = Lens.field @"ownerId"
{-# DEPRECATED cgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The state of the carrier gateway.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgState :: Lens.Lens' CarrierGateway (Core.Maybe Types.CarrierGatewayState)
cgState = Lens.field @"state"
{-# DEPRECATED cgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The tags assigned to the carrier gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgTags :: Lens.Lens' CarrierGateway (Core.Maybe [Types.Tag])
cgTags = Lens.field @"tags"
{-# DEPRECATED cgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the VPC associated with the carrier gateway.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgVpcId :: Lens.Lens' CarrierGateway (Core.Maybe Types.VpcId)
cgVpcId = Lens.field @"vpcId"
{-# DEPRECATED cgVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML CarrierGateway where
  parseXML x =
    CarrierGateway'
      Core.<$> (x Core..@? "carrierGatewayId")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "vpcId")
