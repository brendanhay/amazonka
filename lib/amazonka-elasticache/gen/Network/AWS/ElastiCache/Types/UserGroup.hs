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
    ugStatus,
    ugUserIds,
    ugARN,
    ugUserGroupId,
    ugEngine,
    ugPendingChanges,
    ugReplicationGroups,
  )
where

import Network.AWS.ElastiCache.Types.UserGroupPendingChanges
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkUserGroup' smart constructor.
data UserGroup = UserGroup'
  { -- | Indicates user group status. Can be "creating", "active", "modifying", "deleting".
    status :: Lude.Maybe Lude.Text,
    -- | The list of user IDs that belong to the user group.
    userIds :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the user group.
    arn :: Lude.Maybe Lude.Text,
    -- | The ID of the user group.
    userGroupId :: Lude.Maybe Lude.Text,
    -- | Must be Redis.
    engine :: Lude.Maybe Lude.Text,
    -- | A list of updates being applied to the user groups.
    pendingChanges :: Lude.Maybe UserGroupPendingChanges,
    -- | A list of replication groups that the user group can access.
    replicationGroups :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserGroup' with the minimum fields required to make a request.
--
-- * 'status' - Indicates user group status. Can be "creating", "active", "modifying", "deleting".
-- * 'userIds' - The list of user IDs that belong to the user group.
-- * 'arn' - The Amazon Resource Name (ARN) of the user group.
-- * 'userGroupId' - The ID of the user group.
-- * 'engine' - Must be Redis.
-- * 'pendingChanges' - A list of updates being applied to the user groups.
-- * 'replicationGroups' - A list of replication groups that the user group can access.
mkUserGroup ::
  UserGroup
mkUserGroup =
  UserGroup'
    { status = Lude.Nothing,
      userIds = Lude.Nothing,
      arn = Lude.Nothing,
      userGroupId = Lude.Nothing,
      engine = Lude.Nothing,
      pendingChanges = Lude.Nothing,
      replicationGroups = Lude.Nothing
    }

-- | Indicates user group status. Can be "creating", "active", "modifying", "deleting".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugStatus :: Lens.Lens' UserGroup (Lude.Maybe Lude.Text)
ugStatus = Lens.lens (status :: UserGroup -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: UserGroup)
{-# DEPRECATED ugStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The list of user IDs that belong to the user group.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugUserIds :: Lens.Lens' UserGroup (Lude.Maybe [Lude.Text])
ugUserIds = Lens.lens (userIds :: UserGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {userIds = a} :: UserGroup)
{-# DEPRECATED ugUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

-- | The Amazon Resource Name (ARN) of the user group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugARN :: Lens.Lens' UserGroup (Lude.Maybe Lude.Text)
ugARN = Lens.lens (arn :: UserGroup -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UserGroup)
{-# DEPRECATED ugARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugUserGroupId :: Lens.Lens' UserGroup (Lude.Maybe Lude.Text)
ugUserGroupId = Lens.lens (userGroupId :: UserGroup -> Lude.Maybe Lude.Text) (\s a -> s {userGroupId = a} :: UserGroup)
{-# DEPRECATED ugUserGroupId "Use generic-lens or generic-optics with 'userGroupId' instead." #-}

-- | Must be Redis.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugEngine :: Lens.Lens' UserGroup (Lude.Maybe Lude.Text)
ugEngine = Lens.lens (engine :: UserGroup -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: UserGroup)
{-# DEPRECATED ugEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | A list of updates being applied to the user groups.
--
-- /Note:/ Consider using 'pendingChanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugPendingChanges :: Lens.Lens' UserGroup (Lude.Maybe UserGroupPendingChanges)
ugPendingChanges = Lens.lens (pendingChanges :: UserGroup -> Lude.Maybe UserGroupPendingChanges) (\s a -> s {pendingChanges = a} :: UserGroup)
{-# DEPRECATED ugPendingChanges "Use generic-lens or generic-optics with 'pendingChanges' instead." #-}

-- | A list of replication groups that the user group can access.
--
-- /Note:/ Consider using 'replicationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugReplicationGroups :: Lens.Lens' UserGroup (Lude.Maybe [Lude.Text])
ugReplicationGroups = Lens.lens (replicationGroups :: UserGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {replicationGroups = a} :: UserGroup)
{-# DEPRECATED ugReplicationGroups "Use generic-lens or generic-optics with 'replicationGroups' instead." #-}

instance Lude.FromXML UserGroup where
  parseXML x =
    UserGroup'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> ( x Lude..@? "UserIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "ARN")
      Lude.<*> (x Lude..@? "UserGroupId")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "PendingChanges")
      Lude.<*> ( x Lude..@? "ReplicationGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
