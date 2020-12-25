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

import qualified Network.AWS.ElastiCache.Types.UserId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns the updates being applied to the user group.
--
-- /See:/ 'mkUserGroupPendingChanges' smart constructor.
data UserGroupPendingChanges = UserGroupPendingChanges'
  { -- | The list of user IDs to add.
    userIdsToAdd :: Core.Maybe [Types.UserId],
    -- | The list of user group IDs ro remove.
    userIdsToRemove :: Core.Maybe [Types.UserId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserGroupPendingChanges' value with any optional fields omitted.
mkUserGroupPendingChanges ::
  UserGroupPendingChanges
mkUserGroupPendingChanges =
  UserGroupPendingChanges'
    { userIdsToAdd = Core.Nothing,
      userIdsToRemove = Core.Nothing
    }

-- | The list of user IDs to add.
--
-- /Note:/ Consider using 'userIdsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugpcUserIdsToAdd :: Lens.Lens' UserGroupPendingChanges (Core.Maybe [Types.UserId])
ugpcUserIdsToAdd = Lens.field @"userIdsToAdd"
{-# DEPRECATED ugpcUserIdsToAdd "Use generic-lens or generic-optics with 'userIdsToAdd' instead." #-}

-- | The list of user group IDs ro remove.
--
-- /Note:/ Consider using 'userIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugpcUserIdsToRemove :: Lens.Lens' UserGroupPendingChanges (Core.Maybe [Types.UserId])
ugpcUserIdsToRemove = Lens.field @"userIdsToRemove"
{-# DEPRECATED ugpcUserIdsToRemove "Use generic-lens or generic-optics with 'userIdsToRemove' instead." #-}

instance Core.FromXML UserGroupPendingChanges where
  parseXML x =
    UserGroupPendingChanges'
      Core.<$> (x Core..@? "UserIdsToAdd" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "UserIdsToRemove" Core..<@> Core.parseXMLList "member")
