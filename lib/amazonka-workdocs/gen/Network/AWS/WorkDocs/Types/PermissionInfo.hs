{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.PermissionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.PermissionInfo
  ( PermissionInfo (..),

    -- * Smart constructor
    mkPermissionInfo,

    -- * Lenses
    piRole,
    piType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.RolePermissionType as Types
import qualified Network.AWS.WorkDocs.Types.RoleType as Types

-- | Describes the permissions.
--
-- /See:/ 'mkPermissionInfo' smart constructor.
data PermissionInfo = PermissionInfo'
  { -- | The role of the user.
    role' :: Core.Maybe Types.RoleType,
    -- | The type of permissions.
    type' :: Core.Maybe Types.RolePermissionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PermissionInfo' value with any optional fields omitted.
mkPermissionInfo ::
  PermissionInfo
mkPermissionInfo =
  PermissionInfo' {role' = Core.Nothing, type' = Core.Nothing}

-- | The role of the user.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRole :: Lens.Lens' PermissionInfo (Core.Maybe Types.RoleType)
piRole = Lens.field @"role'"
{-# DEPRECATED piRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The type of permissions.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piType :: Lens.Lens' PermissionInfo (Core.Maybe Types.RolePermissionType)
piType = Lens.field @"type'"
{-# DEPRECATED piType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON PermissionInfo where
  parseJSON =
    Core.withObject "PermissionInfo" Core.$
      \x ->
        PermissionInfo'
          Core.<$> (x Core..:? "Role") Core.<*> (x Core..:? "Type")
