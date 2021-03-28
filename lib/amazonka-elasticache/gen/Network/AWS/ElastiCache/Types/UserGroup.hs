{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UserGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.UserGroup
  ( UserGroup (..)
  -- * Smart constructor
  , mkUserGroup
  -- * Lenses
  , ugARN
  , ugEngine
  , ugPendingChanges
  , ugReplicationGroups
  , ugStatus
  , ugUserGroupId
  , ugUserIds
  ) where

import qualified Network.AWS.ElastiCache.Types.Engine as Types
import qualified Network.AWS.ElastiCache.Types.UserGroupPendingChanges as Types
import qualified Network.AWS.ElastiCache.Types.UserId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkUserGroup' smart constructor.
data UserGroup = UserGroup'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the user group.
  , engine :: Core.Maybe Types.Engine
    -- ^ Must be Redis. 
  , pendingChanges :: Core.Maybe Types.UserGroupPendingChanges
    -- ^ A list of updates being applied to the user groups.
  , replicationGroups :: Core.Maybe [Core.Text]
    -- ^ A list of replication groups that the user group can access.
  , status :: Core.Maybe Core.Text
    -- ^ Indicates user group status. Can be "creating", "active", "modifying", "deleting".
  , userGroupId :: Core.Maybe Core.Text
    -- ^ The ID of the user group.
  , userIds :: Core.Maybe [Types.UserId]
    -- ^ The list of user IDs that belong to the user group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserGroup' value with any optional fields omitted.
mkUserGroup
    :: UserGroup
mkUserGroup
  = UserGroup'{arn = Core.Nothing, engine = Core.Nothing,
               pendingChanges = Core.Nothing, replicationGroups = Core.Nothing,
               status = Core.Nothing, userGroupId = Core.Nothing,
               userIds = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the user group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugARN :: Lens.Lens' UserGroup (Core.Maybe Core.Text)
ugARN = Lens.field @"arn"
{-# INLINEABLE ugARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Must be Redis. 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugEngine :: Lens.Lens' UserGroup (Core.Maybe Types.Engine)
ugEngine = Lens.field @"engine"
{-# INLINEABLE ugEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | A list of updates being applied to the user groups.
--
-- /Note:/ Consider using 'pendingChanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugPendingChanges :: Lens.Lens' UserGroup (Core.Maybe Types.UserGroupPendingChanges)
ugPendingChanges = Lens.field @"pendingChanges"
{-# INLINEABLE ugPendingChanges #-}
{-# DEPRECATED pendingChanges "Use generic-lens or generic-optics with 'pendingChanges' instead"  #-}

-- | A list of replication groups that the user group can access.
--
-- /Note:/ Consider using 'replicationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugReplicationGroups :: Lens.Lens' UserGroup (Core.Maybe [Core.Text])
ugReplicationGroups = Lens.field @"replicationGroups"
{-# INLINEABLE ugReplicationGroups #-}
{-# DEPRECATED replicationGroups "Use generic-lens or generic-optics with 'replicationGroups' instead"  #-}

-- | Indicates user group status. Can be "creating", "active", "modifying", "deleting".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugStatus :: Lens.Lens' UserGroup (Core.Maybe Core.Text)
ugStatus = Lens.field @"status"
{-# INLINEABLE ugStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugUserGroupId :: Lens.Lens' UserGroup (Core.Maybe Core.Text)
ugUserGroupId = Lens.field @"userGroupId"
{-# INLINEABLE ugUserGroupId #-}
{-# DEPRECATED userGroupId "Use generic-lens or generic-optics with 'userGroupId' instead"  #-}

-- | The list of user IDs that belong to the user group.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugUserIds :: Lens.Lens' UserGroup (Core.Maybe [Types.UserId])
ugUserIds = Lens.field @"userIds"
{-# INLINEABLE ugUserIds #-}
{-# DEPRECATED userIds "Use generic-lens or generic-optics with 'userIds' instead"  #-}

instance Core.FromXML UserGroup where
        parseXML x
          = UserGroup' Core.<$>
              (x Core..@? "ARN") Core.<*> x Core..@? "Engine" Core.<*>
                x Core..@? "PendingChanges"
                Core.<*>
                x Core..@? "ReplicationGroups" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "UserGroupId"
                Core.<*> x Core..@? "UserIds" Core..<@> Core.parseXMLList "member"
