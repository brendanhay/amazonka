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
-- Module      : Network.AWS.ElastiCache.Types.UserGroupPendingChanges
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UserGroupPendingChanges where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns the updates being applied to the user group.
--
-- /See:/ 'newUserGroupPendingChanges' smart constructor.
data UserGroupPendingChanges = UserGroupPendingChanges'
  { -- | The list of user IDs to remove.
    userIdsToRemove :: Core.Maybe [Core.Text],
    -- | The list of user IDs to add.
    userIdsToAdd :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserGroupPendingChanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userIdsToRemove', 'userGroupPendingChanges_userIdsToRemove' - The list of user IDs to remove.
--
-- 'userIdsToAdd', 'userGroupPendingChanges_userIdsToAdd' - The list of user IDs to add.
newUserGroupPendingChanges ::
  UserGroupPendingChanges
newUserGroupPendingChanges =
  UserGroupPendingChanges'
    { userIdsToRemove =
        Core.Nothing,
      userIdsToAdd = Core.Nothing
    }

-- | The list of user IDs to remove.
userGroupPendingChanges_userIdsToRemove :: Lens.Lens' UserGroupPendingChanges (Core.Maybe [Core.Text])
userGroupPendingChanges_userIdsToRemove = Lens.lens (\UserGroupPendingChanges' {userIdsToRemove} -> userIdsToRemove) (\s@UserGroupPendingChanges' {} a -> s {userIdsToRemove = a} :: UserGroupPendingChanges) Core.. Lens.mapping Lens._Coerce

-- | The list of user IDs to add.
userGroupPendingChanges_userIdsToAdd :: Lens.Lens' UserGroupPendingChanges (Core.Maybe [Core.Text])
userGroupPendingChanges_userIdsToAdd = Lens.lens (\UserGroupPendingChanges' {userIdsToAdd} -> userIdsToAdd) (\s@UserGroupPendingChanges' {} a -> s {userIdsToAdd = a} :: UserGroupPendingChanges) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML UserGroupPendingChanges where
  parseXML x =
    UserGroupPendingChanges'
      Core.<$> ( x Core..@? "UserIdsToRemove" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "UserIdsToAdd" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable UserGroupPendingChanges

instance Core.NFData UserGroupPendingChanges
