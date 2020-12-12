{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermission
  ( LoadPermission (..),

    -- * Smart constructor
    mkLoadPermission,

    -- * Lenses
    lpGroup,
    lpUserId,
  )
where

import Network.AWS.EC2.Types.PermissionGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a load permission.
--
-- /See:/ 'mkLoadPermission' smart constructor.
data LoadPermission = LoadPermission'
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

-- | Creates a value of 'LoadPermission' with the minimum fields required to make a request.
--
-- * 'group' - The name of the group.
-- * 'userId' - The AWS account ID.
mkLoadPermission ::
  LoadPermission
mkLoadPermission =
  LoadPermission' {group = Lude.Nothing, userId = Lude.Nothing}

-- | The name of the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpGroup :: Lens.Lens' LoadPermission (Lude.Maybe PermissionGroup)
lpGroup = Lens.lens (group :: LoadPermission -> Lude.Maybe PermissionGroup) (\s a -> s {group = a} :: LoadPermission)
{-# DEPRECATED lpGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpUserId :: Lens.Lens' LoadPermission (Lude.Maybe Lude.Text)
lpUserId = Lens.lens (userId :: LoadPermission -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: LoadPermission)
{-# DEPRECATED lpUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.FromXML LoadPermission where
  parseXML x =
    LoadPermission'
      Lude.<$> (x Lude..@? "group") Lude.<*> (x Lude..@? "userId")
