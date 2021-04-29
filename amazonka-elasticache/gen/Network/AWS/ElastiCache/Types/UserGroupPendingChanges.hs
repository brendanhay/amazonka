{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns the updates being applied to the user group.
--
-- /See:/ 'newUserGroupPendingChanges' smart constructor.
data UserGroupPendingChanges = UserGroupPendingChanges'
  { -- | The list of user IDs to remove.
    userIdsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The list of user IDs to add.
    userIdsToAdd :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      userIdsToAdd = Prelude.Nothing
    }

-- | The list of user IDs to remove.
userGroupPendingChanges_userIdsToRemove :: Lens.Lens' UserGroupPendingChanges (Prelude.Maybe [Prelude.Text])
userGroupPendingChanges_userIdsToRemove = Lens.lens (\UserGroupPendingChanges' {userIdsToRemove} -> userIdsToRemove) (\s@UserGroupPendingChanges' {} a -> s {userIdsToRemove = a} :: UserGroupPendingChanges) Prelude.. Lens.mapping Prelude._Coerce

-- | The list of user IDs to add.
userGroupPendingChanges_userIdsToAdd :: Lens.Lens' UserGroupPendingChanges (Prelude.Maybe [Prelude.Text])
userGroupPendingChanges_userIdsToAdd = Lens.lens (\UserGroupPendingChanges' {userIdsToAdd} -> userIdsToAdd) (\s@UserGroupPendingChanges' {} a -> s {userIdsToAdd = a} :: UserGroupPendingChanges) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML UserGroupPendingChanges where
  parseXML x =
    UserGroupPendingChanges'
      Prelude.<$> ( x Prelude..@? "UserIdsToRemove"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "UserIdsToAdd"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance Prelude.Hashable UserGroupPendingChanges

instance Prelude.NFData UserGroupPendingChanges
