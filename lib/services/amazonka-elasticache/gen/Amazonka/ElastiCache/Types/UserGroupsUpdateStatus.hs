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
-- Module      : Amazonka.ElastiCache.Types.UserGroupsUpdateStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.UserGroupsUpdateStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The status of the user group update.
--
-- /See:/ 'newUserGroupsUpdateStatus' smart constructor.
data UserGroupsUpdateStatus = UserGroupsUpdateStatus'
  { -- | The ID of the user group to add.
    userGroupIdsToAdd :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the user group to remove.
    userGroupIdsToRemove :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserGroupsUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userGroupIdsToAdd', 'userGroupsUpdateStatus_userGroupIdsToAdd' - The ID of the user group to add.
--
-- 'userGroupIdsToRemove', 'userGroupsUpdateStatus_userGroupIdsToRemove' - The ID of the user group to remove.
newUserGroupsUpdateStatus ::
  UserGroupsUpdateStatus
newUserGroupsUpdateStatus =
  UserGroupsUpdateStatus'
    { userGroupIdsToAdd =
        Prelude.Nothing,
      userGroupIdsToRemove = Prelude.Nothing
    }

-- | The ID of the user group to add.
userGroupsUpdateStatus_userGroupIdsToAdd :: Lens.Lens' UserGroupsUpdateStatus (Prelude.Maybe [Prelude.Text])
userGroupsUpdateStatus_userGroupIdsToAdd = Lens.lens (\UserGroupsUpdateStatus' {userGroupIdsToAdd} -> userGroupIdsToAdd) (\s@UserGroupsUpdateStatus' {} a -> s {userGroupIdsToAdd = a} :: UserGroupsUpdateStatus) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the user group to remove.
userGroupsUpdateStatus_userGroupIdsToRemove :: Lens.Lens' UserGroupsUpdateStatus (Prelude.Maybe [Prelude.Text])
userGroupsUpdateStatus_userGroupIdsToRemove = Lens.lens (\UserGroupsUpdateStatus' {userGroupIdsToRemove} -> userGroupIdsToRemove) (\s@UserGroupsUpdateStatus' {} a -> s {userGroupIdsToRemove = a} :: UserGroupsUpdateStatus) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML UserGroupsUpdateStatus where
  parseXML x =
    UserGroupsUpdateStatus'
      Prelude.<$> ( x Core..@? "UserGroupIdsToAdd"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "UserGroupIdsToRemove"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable UserGroupsUpdateStatus where
  hashWithSalt _salt UserGroupsUpdateStatus' {..} =
    _salt `Prelude.hashWithSalt` userGroupIdsToAdd
      `Prelude.hashWithSalt` userGroupIdsToRemove

instance Prelude.NFData UserGroupsUpdateStatus where
  rnf UserGroupsUpdateStatus' {..} =
    Prelude.rnf userGroupIdsToAdd
      `Prelude.seq` Prelude.rnf userGroupIdsToRemove
