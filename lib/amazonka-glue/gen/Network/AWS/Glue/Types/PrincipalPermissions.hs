{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PrincipalPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.PrincipalPermissions
  ( PrincipalPermissions (..)
  -- * Smart constructor
  , mkPrincipalPermissions
  -- * Lenses
  , ppPermissions
  , ppPrincipal
  ) where

import qualified Network.AWS.Glue.Types.DataLakePrincipal as Types
import qualified Network.AWS.Glue.Types.Permission as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Permissions granted to a principal.
--
-- /See:/ 'mkPrincipalPermissions' smart constructor.
data PrincipalPermissions = PrincipalPermissions'
  { permissions :: Core.Maybe [Types.Permission]
    -- ^ The permissions that are granted to the principal.
  , principal :: Core.Maybe Types.DataLakePrincipal
    -- ^ The principal who is granted permissions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PrincipalPermissions' value with any optional fields omitted.
mkPrincipalPermissions
    :: PrincipalPermissions
mkPrincipalPermissions
  = PrincipalPermissions'{permissions = Core.Nothing,
                          principal = Core.Nothing}

-- | The permissions that are granted to the principal.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPermissions :: Lens.Lens' PrincipalPermissions (Core.Maybe [Types.Permission])
ppPermissions = Lens.field @"permissions"
{-# INLINEABLE ppPermissions #-}
{-# DEPRECATED permissions "Use generic-lens or generic-optics with 'permissions' instead"  #-}

-- | The principal who is granted permissions.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPrincipal :: Lens.Lens' PrincipalPermissions (Core.Maybe Types.DataLakePrincipal)
ppPrincipal = Lens.field @"principal"
{-# INLINEABLE ppPrincipal #-}
{-# DEPRECATED principal "Use generic-lens or generic-optics with 'principal' instead"  #-}

instance Core.FromJSON PrincipalPermissions where
        toJSON PrincipalPermissions{..}
          = Core.object
              (Core.catMaybes
                 [("Permissions" Core..=) Core.<$> permissions,
                  ("Principal" Core..=) Core.<$> principal])

instance Core.FromJSON PrincipalPermissions where
        parseJSON
          = Core.withObject "PrincipalPermissions" Core.$
              \ x ->
                PrincipalPermissions' Core.<$>
                  (x Core..:? "Permissions") Core.<*> x Core..:? "Principal"
