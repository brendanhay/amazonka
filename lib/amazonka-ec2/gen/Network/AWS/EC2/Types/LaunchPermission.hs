-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchPermission
  ( LaunchPermission (..),

    -- * Smart constructor
    mkLaunchPermission,

    -- * Lenses
    lGroup,
    lUserId,
  )
where

import Network.AWS.EC2.Types.PermissionGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch permission.
--
-- /See:/ 'mkLaunchPermission' smart constructor.
data LaunchPermission = LaunchPermission'
  { group ::
      Lude.Maybe PermissionGroup,
    userId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchPermission' with the minimum fields required to make a request.
--
-- * 'group' - The name of the group.
-- * 'userId' - The AWS account ID.
mkLaunchPermission ::
  LaunchPermission
mkLaunchPermission =
  LaunchPermission' {group = Lude.Nothing, userId = Lude.Nothing}

-- | The name of the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lGroup :: Lens.Lens' LaunchPermission (Lude.Maybe PermissionGroup)
lGroup = Lens.lens (group :: LaunchPermission -> Lude.Maybe PermissionGroup) (\s a -> s {group = a} :: LaunchPermission)
{-# DEPRECATED lGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lUserId :: Lens.Lens' LaunchPermission (Lude.Maybe Lude.Text)
lUserId = Lens.lens (userId :: LaunchPermission -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: LaunchPermission)
{-# DEPRECATED lUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.FromXML LaunchPermission where
  parseXML x =
    LaunchPermission'
      Lude.<$> (x Lude..@? "group") Lude.<*> (x Lude..@? "userId")

instance Lude.ToQuery LaunchPermission where
  toQuery LaunchPermission' {..} =
    Lude.mconcat ["Group" Lude.=: group, "UserId" Lude.=: userId]
