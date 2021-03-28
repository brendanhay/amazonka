{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
  ( UserGroupsUpdateStatus (..)
  -- * Smart constructor
  , mkUserGroupsUpdateStatus
  -- * Lenses
  , ugusUserGroupIdsToAdd
  , ugusUserGroupIdsToRemove
  ) where

import qualified Network.AWS.ElastiCache.Types.UserGroupId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of the user group update.
--
-- /See:/ 'mkUserGroupsUpdateStatus' smart constructor.
data UserGroupsUpdateStatus = UserGroupsUpdateStatus'
  { userGroupIdsToAdd :: Core.Maybe [Types.UserGroupId]
    -- ^ The list of user group IDs to add.
  , userGroupIdsToRemove :: Core.Maybe [Types.UserGroupId]
    -- ^ The list of user group IDs to remove.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserGroupsUpdateStatus' value with any optional fields omitted.
mkUserGroupsUpdateStatus
    :: UserGroupsUpdateStatus
mkUserGroupsUpdateStatus
  = UserGroupsUpdateStatus'{userGroupIdsToAdd = Core.Nothing,
                            userGroupIdsToRemove = Core.Nothing}

-- | The list of user group IDs to add.
--
-- /Note:/ Consider using 'userGroupIdsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugusUserGroupIdsToAdd :: Lens.Lens' UserGroupsUpdateStatus (Core.Maybe [Types.UserGroupId])
ugusUserGroupIdsToAdd = Lens.field @"userGroupIdsToAdd"
{-# INLINEABLE ugusUserGroupIdsToAdd #-}
{-# DEPRECATED userGroupIdsToAdd "Use generic-lens or generic-optics with 'userGroupIdsToAdd' instead"  #-}

-- | The list of user group IDs to remove.
--
-- /Note:/ Consider using 'userGroupIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugusUserGroupIdsToRemove :: Lens.Lens' UserGroupsUpdateStatus (Core.Maybe [Types.UserGroupId])
ugusUserGroupIdsToRemove = Lens.field @"userGroupIdsToRemove"
{-# INLINEABLE ugusUserGroupIdsToRemove #-}
{-# DEPRECATED userGroupIdsToRemove "Use generic-lens or generic-optics with 'userGroupIdsToRemove' instead"  #-}

instance Core.FromXML UserGroupsUpdateStatus where
        parseXML x
          = UserGroupsUpdateStatus' Core.<$>
              (x Core..@? "UserGroupIdsToAdd" Core..<@>
                 Core.parseXMLList "member")
                Core.<*>
                x Core..@? "UserGroupIdsToRemove" Core..<@>
                  Core.parseXMLList "member"
