{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PeeringTgwInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PeeringTgwInfo
  ( PeeringTgwInfo (..)
  -- * Smart constructor
  , mkPeeringTgwInfo
  -- * Lenses
  , ptiOwnerId
  , ptiRegion
  , ptiTransitGatewayId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the transit gateway in the peering attachment.
--
-- /See:/ 'mkPeeringTgwInfo' smart constructor.
data PeeringTgwInfo = PeeringTgwInfo'
  { ownerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID of the owner of the transit gateway.
  , region :: Core.Maybe Core.Text
    -- ^ The Region of the transit gateway.
  , transitGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PeeringTgwInfo' value with any optional fields omitted.
mkPeeringTgwInfo
    :: PeeringTgwInfo
mkPeeringTgwInfo
  = PeeringTgwInfo'{ownerId = Core.Nothing, region = Core.Nothing,
                    transitGatewayId = Core.Nothing}

-- | The AWS account ID of the owner of the transit gateway.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptiOwnerId :: Lens.Lens' PeeringTgwInfo (Core.Maybe Core.Text)
ptiOwnerId = Lens.field @"ownerId"
{-# INLINEABLE ptiOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The Region of the transit gateway.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptiRegion :: Lens.Lens' PeeringTgwInfo (Core.Maybe Core.Text)
ptiRegion = Lens.field @"region"
{-# INLINEABLE ptiRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptiTransitGatewayId :: Lens.Lens' PeeringTgwInfo (Core.Maybe Core.Text)
ptiTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE ptiTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

instance Core.FromXML PeeringTgwInfo where
        parseXML x
          = PeeringTgwInfo' Core.<$>
              (x Core..@? "ownerId") Core.<*> x Core..@? "region" Core.<*>
                x Core..@? "transitGatewayId"
