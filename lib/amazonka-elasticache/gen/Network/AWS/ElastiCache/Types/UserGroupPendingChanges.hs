{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UserGroupPendingChanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UserGroupPendingChanges
  ( UserGroupPendingChanges (..),

    -- * Smart constructor
    mkUserGroupPendingChanges,

    -- * Lenses
    ugpcUserIdsToAdd,
    ugpcUserIdsToRemove,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns the updates being applied to the user group.
--
-- /See:/ 'mkUserGroupPendingChanges' smart constructor.
data UserGroupPendingChanges = UserGroupPendingChanges'
  { -- | The list of user IDs to add.
    userIdsToAdd :: Lude.Maybe [Lude.Text],
    -- | The list of user group IDs ro remove.
    userIdsToRemove :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserGroupPendingChanges' with the minimum fields required to make a request.
--
-- * 'userIdsToAdd' - The list of user IDs to add.
-- * 'userIdsToRemove' - The list of user group IDs ro remove.
mkUserGroupPendingChanges ::
  UserGroupPendingChanges
mkUserGroupPendingChanges =
  UserGroupPendingChanges'
    { userIdsToAdd = Lude.Nothing,
      userIdsToRemove = Lude.Nothing
    }

-- | The list of user IDs to add.
--
-- /Note:/ Consider using 'userIdsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugpcUserIdsToAdd :: Lens.Lens' UserGroupPendingChanges (Lude.Maybe [Lude.Text])
ugpcUserIdsToAdd = Lens.lens (userIdsToAdd :: UserGroupPendingChanges -> Lude.Maybe [Lude.Text]) (\s a -> s {userIdsToAdd = a} :: UserGroupPendingChanges)
{-# DEPRECATED ugpcUserIdsToAdd "Use generic-lens or generic-optics with 'userIdsToAdd' instead." #-}

-- | The list of user group IDs ro remove.
--
-- /Note:/ Consider using 'userIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugpcUserIdsToRemove :: Lens.Lens' UserGroupPendingChanges (Lude.Maybe [Lude.Text])
ugpcUserIdsToRemove = Lens.lens (userIdsToRemove :: UserGroupPendingChanges -> Lude.Maybe [Lude.Text]) (\s a -> s {userIdsToRemove = a} :: UserGroupPendingChanges)
{-# DEPRECATED ugpcUserIdsToRemove "Use generic-lens or generic-optics with 'userIdsToRemove' instead." #-}

instance Lude.FromXML UserGroupPendingChanges where
  parseXML x =
    UserGroupPendingChanges'
      Lude.<$> ( x Lude..@? "UserIdsToAdd" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "UserIdsToRemove" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
