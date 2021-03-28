{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.GatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.GatewayGroup
  ( GatewayGroup (..)
  -- * Smart constructor
  , mkGatewayGroup
  -- * Lenses
  , ggArn
  , ggDescription
  , ggName
  ) where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.Description as Types
import qualified Network.AWS.AlexaBusiness.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of the gateway group.
--
-- /See:/ 'mkGatewayGroup' smart constructor.
data GatewayGroup = GatewayGroup'
  { arn :: Core.Maybe Types.Arn
    -- ^ The ARN of the gateway group.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the gateway group.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the gateway group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GatewayGroup' value with any optional fields omitted.
mkGatewayGroup
    :: GatewayGroup
mkGatewayGroup
  = GatewayGroup'{arn = Core.Nothing, description = Core.Nothing,
                  name = Core.Nothing}

-- | The ARN of the gateway group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggArn :: Lens.Lens' GatewayGroup (Core.Maybe Types.Arn)
ggArn = Lens.field @"arn"
{-# INLINEABLE ggArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The description of the gateway group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggDescription :: Lens.Lens' GatewayGroup (Core.Maybe Types.Description)
ggDescription = Lens.field @"description"
{-# INLINEABLE ggDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the gateway group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggName :: Lens.Lens' GatewayGroup (Core.Maybe Types.Name)
ggName = Lens.field @"name"
{-# INLINEABLE ggName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON GatewayGroup where
        parseJSON
          = Core.withObject "GatewayGroup" Core.$
              \ x ->
                GatewayGroup' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Description" Core.<*>
                    x Core..:? "Name"
