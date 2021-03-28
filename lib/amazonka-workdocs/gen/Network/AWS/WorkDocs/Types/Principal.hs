{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Principal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.Principal
  ( Principal (..)
  -- * Smart constructor
  , mkPrincipal
  -- * Lenses
  , pId
  , pRoles
  , pType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.IdType as Types
import qualified Network.AWS.WorkDocs.Types.PermissionInfo as Types
import qualified Network.AWS.WorkDocs.Types.PrincipalType as Types

-- | Describes a resource.
--
-- /See:/ 'mkPrincipal' smart constructor.
data Principal = Principal'
  { id :: Core.Maybe Types.IdType
    -- ^ The ID of the resource.
  , roles :: Core.Maybe [Types.PermissionInfo]
    -- ^ The permission information for the resource.
  , type' :: Core.Maybe Types.PrincipalType
    -- ^ The type of resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Principal' value with any optional fields omitted.
mkPrincipal
    :: Principal
mkPrincipal
  = Principal'{id = Core.Nothing, roles = Core.Nothing,
               type' = Core.Nothing}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pId :: Lens.Lens' Principal (Core.Maybe Types.IdType)
pId = Lens.field @"id"
{-# INLINEABLE pId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The permission information for the resource.
--
-- /Note:/ Consider using 'roles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRoles :: Lens.Lens' Principal (Core.Maybe [Types.PermissionInfo])
pRoles = Lens.field @"roles"
{-# INLINEABLE pRoles #-}
{-# DEPRECATED roles "Use generic-lens or generic-optics with 'roles' instead"  #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' Principal (Core.Maybe Types.PrincipalType)
pType = Lens.field @"type'"
{-# INLINEABLE pType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Principal where
        parseJSON
          = Core.withObject "Principal" Core.$
              \ x ->
                Principal' Core.<$>
                  (x Core..:? "Id") Core.<*> x Core..:? "Roles" Core.<*>
                    x Core..:? "Type"
