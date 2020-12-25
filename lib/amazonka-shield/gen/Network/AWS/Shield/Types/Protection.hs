{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Protection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Protection
  ( Protection (..),

    -- * Smart constructor
    mkProtection,

    -- * Lenses
    pHealthCheckIds,
    pId,
    pName,
    pResourceArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.HealthCheckId as Types
import qualified Network.AWS.Shield.Types.Id as Types
import qualified Network.AWS.Shield.Types.Name as Types
import qualified Network.AWS.Shield.Types.ResourceArn as Types

-- | An object that represents a resource that is under DDoS protection.
--
-- /See:/ 'mkProtection' smart constructor.
data Protection = Protection'
  { -- | The unique identifier (ID) for the Route 53 health check that's associated with the protection.
    healthCheckIds :: Core.Maybe [Types.HealthCheckId],
    -- | The unique identifier (ID) of the protection.
    id :: Core.Maybe Types.Id,
    -- | The name of the protection. For example, @My CloudFront distributions@ .
    name :: Core.Maybe Types.Name,
    -- | The ARN (Amazon Resource Name) of the AWS resource that is protected.
    resourceArn :: Core.Maybe Types.ResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Protection' value with any optional fields omitted.
mkProtection ::
  Protection
mkProtection =
  Protection'
    { healthCheckIds = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      resourceArn = Core.Nothing
    }

-- | The unique identifier (ID) for the Route 53 health check that's associated with the protection.
--
-- /Note:/ Consider using 'healthCheckIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHealthCheckIds :: Lens.Lens' Protection (Core.Maybe [Types.HealthCheckId])
pHealthCheckIds = Lens.field @"healthCheckIds"
{-# DEPRECATED pHealthCheckIds "Use generic-lens or generic-optics with 'healthCheckIds' instead." #-}

-- | The unique identifier (ID) of the protection.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pId :: Lens.Lens' Protection (Core.Maybe Types.Id)
pId = Lens.field @"id"
{-# DEPRECATED pId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the protection. For example, @My CloudFront distributions@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Protection (Core.Maybe Types.Name)
pName = Lens.field @"name"
{-# DEPRECATED pName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN (Amazon Resource Name) of the AWS resource that is protected.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResourceArn :: Lens.Lens' Protection (Core.Maybe Types.ResourceArn)
pResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED pResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.FromJSON Protection where
  parseJSON =
    Core.withObject "Protection" Core.$
      \x ->
        Protection'
          Core.<$> (x Core..:? "HealthCheckIds")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "ResourceArn")
