{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Grant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.Grant
  ( Grant (..)
  -- * Smart constructor
  , mkGrant
  -- * Lenses
  , gGrantee
  , gPermission
  ) where

import qualified Network.AWS.Glacier.Types.Grantee as Types
import qualified Network.AWS.Glacier.Types.Permission as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a grant.
--
-- /See:/ 'mkGrant' smart constructor.
data Grant = Grant'
  { grantee :: Core.Maybe Types.Grantee
    -- ^ The grantee.
  , permission :: Core.Maybe Types.Permission
    -- ^ Specifies the permission given to the grantee. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Grant' value with any optional fields omitted.
mkGrant
    :: Grant
mkGrant = Grant'{grantee = Core.Nothing, permission = Core.Nothing}

-- | The grantee.
--
-- /Note:/ Consider using 'grantee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGrantee :: Lens.Lens' Grant (Core.Maybe Types.Grantee)
gGrantee = Lens.field @"grantee"
{-# INLINEABLE gGrantee #-}
{-# DEPRECATED grantee "Use generic-lens or generic-optics with 'grantee' instead"  #-}

-- | Specifies the permission given to the grantee. 
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gPermission :: Lens.Lens' Grant (Core.Maybe Types.Permission)
gPermission = Lens.field @"permission"
{-# INLINEABLE gPermission #-}
{-# DEPRECATED permission "Use generic-lens or generic-optics with 'permission' instead"  #-}

instance Core.FromJSON Grant where
        toJSON Grant{..}
          = Core.object
              (Core.catMaybes
                 [("Grantee" Core..=) Core.<$> grantee,
                  ("Permission" Core..=) Core.<$> permission])

instance Core.FromJSON Grant where
        parseJSON
          = Core.withObject "Grant" Core.$
              \ x ->
                Grant' Core.<$>
                  (x Core..:? "Grantee") Core.<*> x Core..:? "Permission"
