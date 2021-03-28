{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ResourceAccessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.ResourceAccessPolicy
  ( ResourceAccessPolicy (..)
  -- * Smart constructor
  , mkResourceAccessPolicy
  -- * Lenses
  , rapResourceId
  , rapPermission
  ) where

import qualified Network.AWS.Greengrass.Types.Permission as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A policy used by the function to access a resource.
--
-- /See:/ 'mkResourceAccessPolicy' smart constructor.
data ResourceAccessPolicy = ResourceAccessPolicy'
  { resourceId :: Core.Text
    -- ^ The ID of the resource. (This ID is assigned to the resource when you create the resource definiton.)
  , permission :: Core.Maybe Types.Permission
    -- ^ The permissions that the Lambda function has to the resource. Can be one of ''rw'' (read/write) or ''ro'' (read-only).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceAccessPolicy' value with any optional fields omitted.
mkResourceAccessPolicy
    :: Core.Text -- ^ 'resourceId'
    -> ResourceAccessPolicy
mkResourceAccessPolicy resourceId
  = ResourceAccessPolicy'{resourceId, permission = Core.Nothing}

-- | The ID of the resource. (This ID is assigned to the resource when you create the resource definiton.)
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rapResourceId :: Lens.Lens' ResourceAccessPolicy Core.Text
rapResourceId = Lens.field @"resourceId"
{-# INLINEABLE rapResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The permissions that the Lambda function has to the resource. Can be one of ''rw'' (read/write) or ''ro'' (read-only).
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rapPermission :: Lens.Lens' ResourceAccessPolicy (Core.Maybe Types.Permission)
rapPermission = Lens.field @"permission"
{-# INLINEABLE rapPermission #-}
{-# DEPRECATED permission "Use generic-lens or generic-optics with 'permission' instead"  #-}

instance Core.FromJSON ResourceAccessPolicy where
        toJSON ResourceAccessPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  ("Permission" Core..=) Core.<$> permission])

instance Core.FromJSON ResourceAccessPolicy where
        parseJSON
          = Core.withObject "ResourceAccessPolicy" Core.$
              \ x ->
                ResourceAccessPolicy' Core.<$>
                  (x Core..: "ResourceId") Core.<*> x Core..:? "Permission"
