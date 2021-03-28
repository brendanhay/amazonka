{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ELBInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.ELBInfo
  ( ELBInfo (..)
  -- * Smart constructor
  , mkELBInfo
  -- * Lenses
  , elbiName
  ) where

import qualified Network.AWS.CodeDeploy.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a load balancer in Elastic Load Balancing to use in a deployment. Instances are registered directly with a load balancer, and traffic is routed to the load balancer.
--
-- /See:/ 'mkELBInfo' smart constructor.
newtype ELBInfo = ELBInfo'
  { name :: Core.Maybe Types.Name
    -- ^ For blue/green deployments, the name of the load balancer that is used to route traffic from original instances to replacement instances in a blue/green deployment. For in-place deployments, the name of the load balancer that instances are deregistered from so they are not serving traffic during a deployment, and then re-registered with after the deployment is complete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ELBInfo' value with any optional fields omitted.
mkELBInfo
    :: ELBInfo
mkELBInfo = ELBInfo'{name = Core.Nothing}

-- | For blue/green deployments, the name of the load balancer that is used to route traffic from original instances to replacement instances in a blue/green deployment. For in-place deployments, the name of the load balancer that instances are deregistered from so they are not serving traffic during a deployment, and then re-registered with after the deployment is complete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elbiName :: Lens.Lens' ELBInfo (Core.Maybe Types.Name)
elbiName = Lens.field @"name"
{-# INLINEABLE elbiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON ELBInfo where
        toJSON ELBInfo{..}
          = Core.object (Core.catMaybes [("name" Core..=) Core.<$> name])

instance Core.FromJSON ELBInfo where
        parseJSON
          = Core.withObject "ELBInfo" Core.$
              \ x -> ELBInfo' Core.<$> (x Core..:? "name")
