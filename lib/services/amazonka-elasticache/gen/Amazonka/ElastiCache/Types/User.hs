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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.Authentication
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newUser' smart constructor.
data User = User'
  { -- | Access permissions string used for this user.
    accessString :: Prelude.Maybe Prelude.Text,
    -- | Denotes whether the user requires a password to authenticate.
    authentication :: Prelude.Maybe Authentication,
    -- | The username of the user.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Indicates the user status. Can be \"active\", \"modifying\" or
    -- \"deleting\".
    status :: Prelude.Maybe Prelude.Text,
    -- | The minimum engine version required, which is Redis 6.0
    minimumEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of the user group IDs the user belongs to.
    userGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the user.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The current supported value is Redis.
    engine :: Prelude.Maybe Prelude.Text
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
-- 'accessString', 'user_accessString' - Access permissions string used for this user.
--
-- 'authentication', 'user_authentication' - Denotes whether the user requires a password to authenticate.
--
-- 'userName', 'user_userName' - The username of the user.
--
-- 'arn', 'user_arn' - The Amazon Resource Name (ARN) of the user.
--
-- 'status', 'user_status' - Indicates the user status. Can be \"active\", \"modifying\" or
-- \"deleting\".
--
-- 'minimumEngineVersion', 'user_minimumEngineVersion' - The minimum engine version required, which is Redis 6.0
--
-- 'userGroupIds', 'user_userGroupIds' - Returns a list of the user group IDs the user belongs to.
--
-- 'userId', 'user_userId' - The ID of the user.
--
-- 'engine', 'user_engine' - The current supported value is Redis.
newUser ::
  User
newUser =
  User'
    { accessString = Prelude.Nothing,
      authentication = Prelude.Nothing,
      userName = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      minimumEngineVersion = Prelude.Nothing,
      userGroupIds = Prelude.Nothing,
      userId = Prelude.Nothing,
      engine = Prelude.Nothing
    }

-- | Access permissions string used for this user.
user_accessString :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_accessString = Lens.lens (\User' {accessString} -> accessString) (\s@User' {} a -> s {accessString = a} :: User)

-- | Denotes whether the user requires a password to authenticate.
user_authentication :: Lens.Lens' User (Prelude.Maybe Authentication)
user_authentication = Lens.lens (\User' {authentication} -> authentication) (\s@User' {} a -> s {authentication = a} :: User)

-- | The username of the user.
user_userName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_userName = Lens.lens (\User' {userName} -> userName) (\s@User' {} a -> s {userName = a} :: User)

-- | The Amazon Resource Name (ARN) of the user.
user_arn :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_arn = Lens.lens (\User' {arn} -> arn) (\s@User' {} a -> s {arn = a} :: User)

-- | Indicates the user status. Can be \"active\", \"modifying\" or
-- \"deleting\".
user_status :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

-- | The minimum engine version required, which is Redis 6.0
user_minimumEngineVersion :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_minimumEngineVersion = Lens.lens (\User' {minimumEngineVersion} -> minimumEngineVersion) (\s@User' {} a -> s {minimumEngineVersion = a} :: User)

-- | Returns a list of the user group IDs the user belongs to.
user_userGroupIds :: Lens.Lens' User (Prelude.Maybe [Prelude.Text])
user_userGroupIds = Lens.lens (\User' {userGroupIds} -> userGroupIds) (\s@User' {} a -> s {userGroupIds = a} :: User) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the user.
user_userId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_userId = Lens.lens (\User' {userId} -> userId) (\s@User' {} a -> s {userId = a} :: User)

-- | The current supported value is Redis.
user_engine :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_engine = Lens.lens (\User' {engine} -> engine) (\s@User' {} a -> s {engine = a} :: User)

instance Data.FromXML User where
  parseXML x =
    User'
      Prelude.<$> (x Data..@? "AccessString")
      Prelude.<*> (x Data..@? "Authentication")
      Prelude.<*> (x Data..@? "UserName")
      Prelude.<*> (x Data..@? "ARN")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "MinimumEngineVersion")
      Prelude.<*> ( x Data..@? "UserGroupIds" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "UserId")
      Prelude.<*> (x Data..@? "Engine")

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt `Prelude.hashWithSalt` accessString
      `Prelude.hashWithSalt` authentication
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` minimumEngineVersion
      `Prelude.hashWithSalt` userGroupIds
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` engine

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf accessString
      `Prelude.seq` Prelude.rnf authentication
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf minimumEngineVersion
      `Prelude.seq` Prelude.rnf userGroupIds
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf engine
