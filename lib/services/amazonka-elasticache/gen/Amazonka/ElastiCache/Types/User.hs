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
-- Module      : Amazonka.ElastiCache.Types.User
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.User where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.Authentication
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newUser' smart constructor.
data User = User'
  { -- | Indicates the user status. Can be \"active\", \"modifying\" or
    -- \"deleting\".
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of the user group IDs the user belongs to.
    userGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Denotes whether the user requires a password to authenticate.
    authentication :: Prelude.Maybe Authentication,
    -- | The current supported value is Redis.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The username of the user.
    userName :: Prelude.Maybe Prelude.Text,
    -- | Access permissions string used for this user.
    accessString :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'user_status' - Indicates the user status. Can be \"active\", \"modifying\" or
-- \"deleting\".
--
-- 'arn', 'user_arn' - The Amazon Resource Name (ARN) of the user.
--
-- 'userGroupIds', 'user_userGroupIds' - Returns a list of the user group IDs the user belongs to.
--
-- 'authentication', 'user_authentication' - Denotes whether the user requires a password to authenticate.
--
-- 'engine', 'user_engine' - The current supported value is Redis.
--
-- 'userName', 'user_userName' - The username of the user.
--
-- 'accessString', 'user_accessString' - Access permissions string used for this user.
--
-- 'userId', 'user_userId' - The ID of the user.
newUser ::
  User
newUser =
  User'
    { status = Prelude.Nothing,
      arn = Prelude.Nothing,
      userGroupIds = Prelude.Nothing,
      authentication = Prelude.Nothing,
      engine = Prelude.Nothing,
      userName = Prelude.Nothing,
      accessString = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | Indicates the user status. Can be \"active\", \"modifying\" or
-- \"deleting\".
user_status :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

-- | The Amazon Resource Name (ARN) of the user.
user_arn :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_arn = Lens.lens (\User' {arn} -> arn) (\s@User' {} a -> s {arn = a} :: User)

-- | Returns a list of the user group IDs the user belongs to.
user_userGroupIds :: Lens.Lens' User (Prelude.Maybe [Prelude.Text])
user_userGroupIds = Lens.lens (\User' {userGroupIds} -> userGroupIds) (\s@User' {} a -> s {userGroupIds = a} :: User) Prelude.. Lens.mapping Lens.coerced

-- | Denotes whether the user requires a password to authenticate.
user_authentication :: Lens.Lens' User (Prelude.Maybe Authentication)
user_authentication = Lens.lens (\User' {authentication} -> authentication) (\s@User' {} a -> s {authentication = a} :: User)

-- | The current supported value is Redis.
user_engine :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_engine = Lens.lens (\User' {engine} -> engine) (\s@User' {} a -> s {engine = a} :: User)

-- | The username of the user.
user_userName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_userName = Lens.lens (\User' {userName} -> userName) (\s@User' {} a -> s {userName = a} :: User)

-- | Access permissions string used for this user.
user_accessString :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_accessString = Lens.lens (\User' {accessString} -> accessString) (\s@User' {} a -> s {accessString = a} :: User)

-- | The ID of the user.
user_userId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_userId = Lens.lens (\User' {userId} -> userId) (\s@User' {} a -> s {userId = a} :: User)

instance Core.FromXML User where
  parseXML x =
    User'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "ARN")
      Prelude.<*> ( x Core..@? "UserGroupIds" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Authentication")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "UserName")
      Prelude.<*> (x Core..@? "AccessString")
      Prelude.<*> (x Core..@? "UserId")

instance Prelude.Hashable User

instance Prelude.NFData User
