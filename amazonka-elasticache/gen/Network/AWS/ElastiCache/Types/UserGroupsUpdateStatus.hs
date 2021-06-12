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
-- Module      : Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The status of the user group update.
--
-- /See:/ 'newUserGroupsUpdateStatus' smart constructor.
data UserGroupsUpdateStatus = UserGroupsUpdateStatus'
  { -- | The list of user group IDs to remove.
    userGroupIdsToRemove :: Core.Maybe [Core.Text],
    -- | The list of user group IDs to add.
    userGroupIdsToAdd :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserGroupsUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userGroupIdsToRemove', 'userGroupsUpdateStatus_userGroupIdsToRemove' - The list of user group IDs to remove.
--
-- 'userGroupIdsToAdd', 'userGroupsUpdateStatus_userGroupIdsToAdd' - The list of user group IDs to add.
newUserGroupsUpdateStatus ::
  UserGroupsUpdateStatus
newUserGroupsUpdateStatus =
  UserGroupsUpdateStatus'
    { userGroupIdsToRemove =
        Core.Nothing,
      userGroupIdsToAdd = Core.Nothing
    }

-- | The list of user group IDs to remove.
userGroupsUpdateStatus_userGroupIdsToRemove :: Lens.Lens' UserGroupsUpdateStatus (Core.Maybe [Core.Text])
userGroupsUpdateStatus_userGroupIdsToRemove = Lens.lens (\UserGroupsUpdateStatus' {userGroupIdsToRemove} -> userGroupIdsToRemove) (\s@UserGroupsUpdateStatus' {} a -> s {userGroupIdsToRemove = a} :: UserGroupsUpdateStatus) Core.. Lens.mapping Lens._Coerce

-- | The list of user group IDs to add.
userGroupsUpdateStatus_userGroupIdsToAdd :: Lens.Lens' UserGroupsUpdateStatus (Core.Maybe [Core.Text])
userGroupsUpdateStatus_userGroupIdsToAdd = Lens.lens (\UserGroupsUpdateStatus' {userGroupIdsToAdd} -> userGroupIdsToAdd) (\s@UserGroupsUpdateStatus' {} a -> s {userGroupIdsToAdd = a} :: UserGroupsUpdateStatus) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML UserGroupsUpdateStatus where
  parseXML x =
    UserGroupsUpdateStatus'
      Core.<$> ( x Core..@? "UserGroupIdsToRemove"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "UserGroupIdsToAdd" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable UserGroupsUpdateStatus

instance Core.NFData UserGroupsUpdateStatus
