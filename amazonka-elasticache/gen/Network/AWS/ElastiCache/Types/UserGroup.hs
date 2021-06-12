{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UserGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UserGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.UserGroupPendingChanges
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newUserGroup' smart constructor.
data UserGroup = UserGroup'
  { -- | Indicates user group status. Can be \"creating\", \"active\",
    -- \"modifying\", \"deleting\".
    status :: Core.Maybe Core.Text,
    -- | A list of replication groups that the user group can access.
    replicationGroups :: Core.Maybe [Core.Text],
    -- | The Amazon Resource Name (ARN) of the user group.
    arn :: Core.Maybe Core.Text,
    -- | The list of user IDs that belong to the user group.
    userIds :: Core.Maybe [Core.Text],
    -- | The current supported value is Redis.
    engine :: Core.Maybe Core.Text,
    -- | The ID of the user group.
    userGroupId :: Core.Maybe Core.Text,
    -- | A list of updates being applied to the user groups.
    pendingChanges :: Core.Maybe UserGroupPendingChanges
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'userGroup_status' - Indicates user group status. Can be \"creating\", \"active\",
-- \"modifying\", \"deleting\".
--
-- 'replicationGroups', 'userGroup_replicationGroups' - A list of replication groups that the user group can access.
--
-- 'arn', 'userGroup_arn' - The Amazon Resource Name (ARN) of the user group.
--
-- 'userIds', 'userGroup_userIds' - The list of user IDs that belong to the user group.
--
-- 'engine', 'userGroup_engine' - The current supported value is Redis.
--
-- 'userGroupId', 'userGroup_userGroupId' - The ID of the user group.
--
-- 'pendingChanges', 'userGroup_pendingChanges' - A list of updates being applied to the user groups.
newUserGroup ::
  UserGroup
newUserGroup =
  UserGroup'
    { status = Core.Nothing,
      replicationGroups = Core.Nothing,
      arn = Core.Nothing,
      userIds = Core.Nothing,
      engine = Core.Nothing,
      userGroupId = Core.Nothing,
      pendingChanges = Core.Nothing
    }

-- | Indicates user group status. Can be \"creating\", \"active\",
-- \"modifying\", \"deleting\".
userGroup_status :: Lens.Lens' UserGroup (Core.Maybe Core.Text)
userGroup_status = Lens.lens (\UserGroup' {status} -> status) (\s@UserGroup' {} a -> s {status = a} :: UserGroup)

-- | A list of replication groups that the user group can access.
userGroup_replicationGroups :: Lens.Lens' UserGroup (Core.Maybe [Core.Text])
userGroup_replicationGroups = Lens.lens (\UserGroup' {replicationGroups} -> replicationGroups) (\s@UserGroup' {} a -> s {replicationGroups = a} :: UserGroup) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the user group.
userGroup_arn :: Lens.Lens' UserGroup (Core.Maybe Core.Text)
userGroup_arn = Lens.lens (\UserGroup' {arn} -> arn) (\s@UserGroup' {} a -> s {arn = a} :: UserGroup)

-- | The list of user IDs that belong to the user group.
userGroup_userIds :: Lens.Lens' UserGroup (Core.Maybe [Core.Text])
userGroup_userIds = Lens.lens (\UserGroup' {userIds} -> userIds) (\s@UserGroup' {} a -> s {userIds = a} :: UserGroup) Core.. Lens.mapping Lens._Coerce

-- | The current supported value is Redis.
userGroup_engine :: Lens.Lens' UserGroup (Core.Maybe Core.Text)
userGroup_engine = Lens.lens (\UserGroup' {engine} -> engine) (\s@UserGroup' {} a -> s {engine = a} :: UserGroup)

-- | The ID of the user group.
userGroup_userGroupId :: Lens.Lens' UserGroup (Core.Maybe Core.Text)
userGroup_userGroupId = Lens.lens (\UserGroup' {userGroupId} -> userGroupId) (\s@UserGroup' {} a -> s {userGroupId = a} :: UserGroup)

-- | A list of updates being applied to the user groups.
userGroup_pendingChanges :: Lens.Lens' UserGroup (Core.Maybe UserGroupPendingChanges)
userGroup_pendingChanges = Lens.lens (\UserGroup' {pendingChanges} -> pendingChanges) (\s@UserGroup' {} a -> s {pendingChanges = a} :: UserGroup)

instance Core.FromXML UserGroup where
  parseXML x =
    UserGroup'
      Core.<$> (x Core..@? "Status")
      Core.<*> ( x Core..@? "ReplicationGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "ARN")
      Core.<*> ( x Core..@? "UserIds" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "UserGroupId")
      Core.<*> (x Core..@? "PendingChanges")

instance Core.Hashable UserGroup

instance Core.NFData UserGroup
