{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LocalGateway
  ( LocalGateway (..)
  -- * Smart constructor
  , mkLocalGateway
  -- * Lenses
  , lgLocalGatewayId
  , lgOutpostArn
  , lgOwnerId
  , lgState
  , lgTags
  ) where

import qualified Network.AWS.EC2.Types.LocalGatewayId as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a local gateway.
--
-- /See:/ 'mkLocalGateway' smart constructor.
data LocalGateway = LocalGateway'
  { localGatewayId :: Core.Maybe Types.LocalGatewayId
    -- ^ The ID of the local gateway.
  , outpostArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Outpost.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID that owns the local gateway.
  , state :: Core.Maybe Core.Text
    -- ^ The state of the local gateway.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags assigned to the local gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalGateway' value with any optional fields omitted.
mkLocalGateway
    :: LocalGateway
mkLocalGateway
  = LocalGateway'{localGatewayId = Core.Nothing,
                  outpostArn = Core.Nothing, ownerId = Core.Nothing,
                  state = Core.Nothing, tags = Core.Nothing}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgLocalGatewayId :: Lens.Lens' LocalGateway (Core.Maybe Types.LocalGatewayId)
lgLocalGatewayId = Lens.field @"localGatewayId"
{-# INLINEABLE lgLocalGatewayId #-}
{-# DEPRECATED localGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgOutpostArn :: Lens.Lens' LocalGateway (Core.Maybe Core.Text)
lgOutpostArn = Lens.field @"outpostArn"
{-# INLINEABLE lgOutpostArn #-}
{-# DEPRECATED outpostArn "Use generic-lens or generic-optics with 'outpostArn' instead"  #-}

-- | The AWS account ID that owns the local gateway.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgOwnerId :: Lens.Lens' LocalGateway (Core.Maybe Core.Text)
lgOwnerId = Lens.field @"ownerId"
{-# INLINEABLE lgOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The state of the local gateway.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgState :: Lens.Lens' LocalGateway (Core.Maybe Core.Text)
lgState = Lens.field @"state"
{-# INLINEABLE lgState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The tags assigned to the local gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgTags :: Lens.Lens' LocalGateway (Core.Maybe [Types.Tag])
lgTags = Lens.field @"tags"
{-# INLINEABLE lgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML LocalGateway where
        parseXML x
          = LocalGateway' Core.<$>
              (x Core..@? "localGatewayId") Core.<*> x Core..@? "outpostArn"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
