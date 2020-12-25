{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UserGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UserGroup
  ( UserGroup (..),

    -- * Smart constructor
    mkUserGroup,

    -- * Lenses
    ugARN,
    ugEngine,
    ugPendingChanges,
    ugReplicationGroups,
    ugStatus,
    ugUserGroupId,
    ugUserIds,
  )
where

import qualified Network.AWS.ElastiCache.Types.Engine as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.ElastiCache.Types.UserGroupPendingChanges as Types
import qualified Network.AWS.ElastiCache.Types.UserId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkUserGroup' smart constructor.
data UserGroup = UserGroup'
  { -- | The Amazon Resource Name (ARN) of the user group.
    arn :: Core.Maybe Types.String,
    -- | Must be Redis.
    engine :: Core.Maybe Types.Engine,
    -- | A list of updates being applied to the user groups.
    pendingChanges :: Core.Maybe Types.UserGroupPendingChanges,
    -- | A list of replication groups that the user group can access.
    replicationGroups :: Core.Maybe [Types.String],
    -- | Indicates user group status. Can be "creating", "active", "modifying", "deleting".
    status :: Core.Maybe Types.String,
    -- | The ID of the user group.
    userGroupId :: Core.Maybe Types.String,
    -- | The list of user IDs that belong to the user group.
    userIds :: Core.Maybe [Types.UserId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserGroup' value with any optional fields omitted.
mkUserGroup ::
  UserGroup
mkUserGroup =
  UserGroup'
    { arn = Core.Nothing,
      engine = Core.Nothing,
      pendingChanges = Core.Nothing,
      replicationGroups = Core.Nothing,
      status = Core.Nothing,
      userGroupId = Core.Nothing,
      userIds = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugARN :: Lens.Lens' UserGroup (Core.Maybe Types.String)
ugARN = Lens.field @"arn"
{-# DEPRECATED ugARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Must be Redis.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugEngine :: Lens.Lens' UserGroup (Core.Maybe Types.Engine)
ugEngine = Lens.field @"engine"
{-# DEPRECATED ugEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | A list of updates being applied to the user groups.
--
-- /Note:/ Consider using 'pendingChanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugPendingChanges :: Lens.Lens' UserGroup (Core.Maybe Types.UserGroupPendingChanges)
ugPendingChanges = Lens.field @"pendingChanges"
{-# DEPRECATED ugPendingChanges "Use generic-lens or generic-optics with 'pendingChanges' instead." #-}

-- | A list of replication groups that the user group can access.
--
-- /Note:/ Consider using 'replicationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugReplicationGroups :: Lens.Lens' UserGroup (Core.Maybe [Types.String])
ugReplicationGroups = Lens.field @"replicationGroups"
{-# DEPRECATED ugReplicationGroups "Use generic-lens or generic-optics with 'replicationGroups' instead." #-}

-- | Indicates user group status. Can be "creating", "active", "modifying", "deleting".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugStatus :: Lens.Lens' UserGroup (Core.Maybe Types.String)
ugStatus = Lens.field @"status"
{-# DEPRECATED ugStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugUserGroupId :: Lens.Lens' UserGroup (Core.Maybe Types.String)
ugUserGroupId = Lens.field @"userGroupId"
{-# DEPRECATED ugUserGroupId "Use generic-lens or generic-optics with 'userGroupId' instead." #-}

-- | The list of user IDs that belong to the user group.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugUserIds :: Lens.Lens' UserGroup (Core.Maybe [Types.UserId])
ugUserIds = Lens.field @"userIds"
{-# DEPRECATED ugUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

instance Core.FromXML UserGroup where
  parseXML x =
    UserGroup'
      Core.<$> (x Core..@? "ARN")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "PendingChanges")
      Core.<*> ( x Core..@? "ReplicationGroups"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "UserGroupId")
      Core.<*> (x Core..@? "UserIds" Core..<@> Core.parseXMLList "member")
