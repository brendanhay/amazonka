{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Grant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Grant
  ( Grant (..),

    -- * Smart constructor
    mkGrant,

    -- * Lenses
    gGrantee,
    gPermission,
  )
where

import qualified Network.AWS.Glacier.Types.Grantee as Types
import qualified Network.AWS.Glacier.Types.Permission as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a grant.
--
-- /See:/ 'mkGrant' smart constructor.
data Grant = Grant'
  { -- | The grantee.
    grantee :: Core.Maybe Types.Grantee,
    -- | Specifies the permission given to the grantee.
    permission :: Core.Maybe Types.Permission
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Grant' value with any optional fields omitted.
mkGrant ::
  Grant
mkGrant = Grant' {grantee = Core.Nothing, permission = Core.Nothing}

-- | The grantee.
--
-- /Note:/ Consider using 'grantee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGrantee :: Lens.Lens' Grant (Core.Maybe Types.Grantee)
gGrantee = Lens.field @"grantee"
{-# DEPRECATED gGrantee "Use generic-lens or generic-optics with 'grantee' instead." #-}

-- | Specifies the permission given to the grantee.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gPermission :: Lens.Lens' Grant (Core.Maybe Types.Permission)
gPermission = Lens.field @"permission"
{-# DEPRECATED gPermission "Use generic-lens or generic-optics with 'permission' instead." #-}

instance Core.FromJSON Grant where
  toJSON Grant {..} =
    Core.object
      ( Core.catMaybes
          [ ("Grantee" Core..=) Core.<$> grantee,
            ("Permission" Core..=) Core.<$> permission
          ]
      )

instance Core.FromJSON Grant where
  parseJSON =
    Core.withObject "Grant" Core.$
      \x ->
        Grant'
          Core.<$> (x Core..:? "Grantee") Core.<*> (x Core..:? "Permission")
