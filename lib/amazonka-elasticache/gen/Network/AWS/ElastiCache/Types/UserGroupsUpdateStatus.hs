{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
  ( UserGroupsUpdateStatus (..),

    -- * Smart constructor
    mkUserGroupsUpdateStatus,

    -- * Lenses
    ugusUserGroupIdsToAdd,
    ugusUserGroupIdsToRemove,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of the user group update.
--
-- /See:/ 'mkUserGroupsUpdateStatus' smart constructor.
data UserGroupsUpdateStatus = UserGroupsUpdateStatus'
  { userGroupIdsToAdd ::
      Lude.Maybe [Lude.Text],
    userGroupIdsToRemove ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserGroupsUpdateStatus' with the minimum fields required to make a request.
--
-- * 'userGroupIdsToAdd' - The list of user group IDs to add.
-- * 'userGroupIdsToRemove' - The list of user group IDs to remove.
mkUserGroupsUpdateStatus ::
  UserGroupsUpdateStatus
mkUserGroupsUpdateStatus =
  UserGroupsUpdateStatus'
    { userGroupIdsToAdd = Lude.Nothing,
      userGroupIdsToRemove = Lude.Nothing
    }

-- | The list of user group IDs to add.
--
-- /Note:/ Consider using 'userGroupIdsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugusUserGroupIdsToAdd :: Lens.Lens' UserGroupsUpdateStatus (Lude.Maybe [Lude.Text])
ugusUserGroupIdsToAdd = Lens.lens (userGroupIdsToAdd :: UserGroupsUpdateStatus -> Lude.Maybe [Lude.Text]) (\s a -> s {userGroupIdsToAdd = a} :: UserGroupsUpdateStatus)
{-# DEPRECATED ugusUserGroupIdsToAdd "Use generic-lens or generic-optics with 'userGroupIdsToAdd' instead." #-}

-- | The list of user group IDs to remove.
--
-- /Note:/ Consider using 'userGroupIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugusUserGroupIdsToRemove :: Lens.Lens' UserGroupsUpdateStatus (Lude.Maybe [Lude.Text])
ugusUserGroupIdsToRemove = Lens.lens (userGroupIdsToRemove :: UserGroupsUpdateStatus -> Lude.Maybe [Lude.Text]) (\s a -> s {userGroupIdsToRemove = a} :: UserGroupsUpdateStatus)
{-# DEPRECATED ugusUserGroupIdsToRemove "Use generic-lens or generic-optics with 'userGroupIdsToRemove' instead." #-}

instance Lude.FromXML UserGroupsUpdateStatus where
  parseXML x =
    UserGroupsUpdateStatus'
      Lude.<$> ( x Lude..@? "UserGroupIdsToAdd" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "UserGroupIdsToRemove" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
