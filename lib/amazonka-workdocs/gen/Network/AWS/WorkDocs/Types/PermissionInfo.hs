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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.RolePermissionType
import Network.AWS.WorkDocs.Types.RoleType

-- | Describes the permissions.
--
-- /See:/ 'mkPermissionInfo' smart constructor.
data PermissionInfo = PermissionInfo'
  { role' :: Lude.Maybe RoleType,
    type' :: Lude.Maybe RolePermissionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PermissionInfo' with the minimum fields required to make a request.
--
-- * 'role'' - The role of the user.
-- * 'type'' - The type of permissions.
mkPermissionInfo ::
  PermissionInfo
mkPermissionInfo =
  PermissionInfo' {role' = Lude.Nothing, type' = Lude.Nothing}

-- | The role of the user.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRole :: Lens.Lens' PermissionInfo (Lude.Maybe RoleType)
piRole = Lens.lens (role' :: PermissionInfo -> Lude.Maybe RoleType) (\s a -> s {role' = a} :: PermissionInfo)
{-# DEPRECATED piRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The type of permissions.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piType :: Lens.Lens' PermissionInfo (Lude.Maybe RolePermissionType)
piType = Lens.lens (type' :: PermissionInfo -> Lude.Maybe RolePermissionType) (\s a -> s {type' = a} :: PermissionInfo)
{-# DEPRECATED piType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON PermissionInfo where
  parseJSON =
    Lude.withObject
      "PermissionInfo"
      ( \x ->
          PermissionInfo'
            Lude.<$> (x Lude..:? "Role") Lude.<*> (x Lude..:? "Type")
      )
