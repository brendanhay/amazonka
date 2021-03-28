{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.AssociatedGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.AssociatedGateway
  ( AssociatedGateway (..)
  -- * Smart constructor
  , mkAssociatedGateway
  -- * Lenses
  , agId
  , agOwnerAccount
  , agRegion
  , agType
  ) where

import qualified Network.AWS.DirectConnect.Types.GatewayIdentifier as Types
import qualified Network.AWS.DirectConnect.Types.GatewayType as Types
import qualified Network.AWS.DirectConnect.Types.OwnerAccount as Types
import qualified Network.AWS.DirectConnect.Types.Region as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the associated gateway.
--
-- /See:/ 'mkAssociatedGateway' smart constructor.
data AssociatedGateway = AssociatedGateway'
  { id :: Core.Maybe Types.GatewayIdentifier
    -- ^ The ID of the associated gateway.
  , ownerAccount :: Core.Maybe Types.OwnerAccount
    -- ^ The ID of the AWS account that owns the associated virtual private gateway or transit gateway.
  , region :: Core.Maybe Types.Region
    -- ^ The Region where the associated gateway is located.
  , type' :: Core.Maybe Types.GatewayType
    -- ^ The type of associated gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociatedGateway' value with any optional fields omitted.
mkAssociatedGateway
    :: AssociatedGateway
mkAssociatedGateway
  = AssociatedGateway'{id = Core.Nothing,
                       ownerAccount = Core.Nothing, region = Core.Nothing,
                       type' = Core.Nothing}

-- | The ID of the associated gateway.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agId :: Lens.Lens' AssociatedGateway (Core.Maybe Types.GatewayIdentifier)
agId = Lens.field @"id"
{-# INLINEABLE agId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ID of the AWS account that owns the associated virtual private gateway or transit gateway.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agOwnerAccount :: Lens.Lens' AssociatedGateway (Core.Maybe Types.OwnerAccount)
agOwnerAccount = Lens.field @"ownerAccount"
{-# INLINEABLE agOwnerAccount #-}
{-# DEPRECATED ownerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead"  #-}

-- | The Region where the associated gateway is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agRegion :: Lens.Lens' AssociatedGateway (Core.Maybe Types.Region)
agRegion = Lens.field @"region"
{-# INLINEABLE agRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The type of associated gateway.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agType :: Lens.Lens' AssociatedGateway (Core.Maybe Types.GatewayType)
agType = Lens.field @"type'"
{-# INLINEABLE agType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON AssociatedGateway where
        parseJSON
          = Core.withObject "AssociatedGateway" Core.$
              \ x ->
                AssociatedGateway' Core.<$>
                  (x Core..:? "id") Core.<*> x Core..:? "ownerAccount" Core.<*>
                    x Core..:? "region"
                    Core.<*> x Core..:? "type"
