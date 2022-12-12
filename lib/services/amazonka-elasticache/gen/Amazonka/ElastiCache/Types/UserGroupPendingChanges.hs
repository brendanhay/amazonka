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
-- Module      : Amazonka.ElastiCache.Types.UserGroupPendingChanges
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.UserGroupPendingChanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns the updates being applied to the user group.
--
-- /See:/ 'newUserGroupPendingChanges' smart constructor.
data UserGroupPendingChanges = UserGroupPendingChanges'
  { -- | The list of user IDs to add.
    userIdsToAdd :: Prelude.Maybe [Prelude.Text],
    -- | The list of user IDs to remove.
    userIdsToRemove :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserGroupPendingChanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userIdsToAdd', 'userGroupPendingChanges_userIdsToAdd' - The list of user IDs to add.
--
-- 'userIdsToRemove', 'userGroupPendingChanges_userIdsToRemove' - The list of user IDs to remove.
newUserGroupPendingChanges ::
  UserGroupPendingChanges
newUserGroupPendingChanges =
  UserGroupPendingChanges'
    { userIdsToAdd =
        Prelude.Nothing,
      userIdsToRemove = Prelude.Nothing
    }

-- | The list of user IDs to add.
userGroupPendingChanges_userIdsToAdd :: Lens.Lens' UserGroupPendingChanges (Prelude.Maybe [Prelude.Text])
userGroupPendingChanges_userIdsToAdd = Lens.lens (\UserGroupPendingChanges' {userIdsToAdd} -> userIdsToAdd) (\s@UserGroupPendingChanges' {} a -> s {userIdsToAdd = a} :: UserGroupPendingChanges) Prelude.. Lens.mapping Lens.coerced

-- | The list of user IDs to remove.
userGroupPendingChanges_userIdsToRemove :: Lens.Lens' UserGroupPendingChanges (Prelude.Maybe [Prelude.Text])
userGroupPendingChanges_userIdsToRemove = Lens.lens (\UserGroupPendingChanges' {userIdsToRemove} -> userIdsToRemove) (\s@UserGroupPendingChanges' {} a -> s {userIdsToRemove = a} :: UserGroupPendingChanges) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML UserGroupPendingChanges where
  parseXML x =
    UserGroupPendingChanges'
      Prelude.<$> ( x Data..@? "UserIdsToAdd" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x Data..@? "UserIdsToRemove" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable UserGroupPendingChanges where
  hashWithSalt _salt UserGroupPendingChanges' {..} =
    _salt `Prelude.hashWithSalt` userIdsToAdd
      `Prelude.hashWithSalt` userIdsToRemove

instance Prelude.NFData UserGroupPendingChanges where
  rnf UserGroupPendingChanges' {..} =
    Prelude.rnf userIdsToAdd
      `Prelude.seq` Prelude.rnf userIdsToRemove
