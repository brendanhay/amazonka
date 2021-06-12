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
-- Module      : Network.AWS.ElastiCache.Types.User
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.User where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.Authentication
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newUser' smart constructor.
data User = User'
  { -- | Indicates the user status. Can be \"active\", \"modifying\" or
    -- \"deleting\".
    status :: Core.Maybe Core.Text,
    -- | Access permissions string used for this user.
    accessString :: Core.Maybe Core.Text,
    -- | Returns a list of the user group IDs the user belongs to.
    userGroupIds :: Core.Maybe [Core.Text],
    -- | Denotes whether the user requires a password to authenticate.
    authentication :: Core.Maybe Authentication,
    -- | The Amazon Resource Name (ARN) of the user.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the user.
    userId :: Core.Maybe Core.Text,
    -- | The current supported value is Redis.
    engine :: Core.Maybe Core.Text,
    -- | The username of the user.
    userName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'accessString', 'user_accessString' - Access permissions string used for this user.
--
-- 'userGroupIds', 'user_userGroupIds' - Returns a list of the user group IDs the user belongs to.
--
-- 'authentication', 'user_authentication' - Denotes whether the user requires a password to authenticate.
--
-- 'arn', 'user_arn' - The Amazon Resource Name (ARN) of the user.
--
-- 'userId', 'user_userId' - The ID of the user.
--
-- 'engine', 'user_engine' - The current supported value is Redis.
--
-- 'userName', 'user_userName' - The username of the user.
newUser ::
  User
newUser =
  User'
    { status = Core.Nothing,
      accessString = Core.Nothing,
      userGroupIds = Core.Nothing,
      authentication = Core.Nothing,
      arn = Core.Nothing,
      userId = Core.Nothing,
      engine = Core.Nothing,
      userName = Core.Nothing
    }

-- | Indicates the user status. Can be \"active\", \"modifying\" or
-- \"deleting\".
user_status :: Lens.Lens' User (Core.Maybe Core.Text)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

-- | Access permissions string used for this user.
user_accessString :: Lens.Lens' User (Core.Maybe Core.Text)
user_accessString = Lens.lens (\User' {accessString} -> accessString) (\s@User' {} a -> s {accessString = a} :: User)

-- | Returns a list of the user group IDs the user belongs to.
user_userGroupIds :: Lens.Lens' User (Core.Maybe [Core.Text])
user_userGroupIds = Lens.lens (\User' {userGroupIds} -> userGroupIds) (\s@User' {} a -> s {userGroupIds = a} :: User) Core.. Lens.mapping Lens._Coerce

-- | Denotes whether the user requires a password to authenticate.
user_authentication :: Lens.Lens' User (Core.Maybe Authentication)
user_authentication = Lens.lens (\User' {authentication} -> authentication) (\s@User' {} a -> s {authentication = a} :: User)

-- | The Amazon Resource Name (ARN) of the user.
user_arn :: Lens.Lens' User (Core.Maybe Core.Text)
user_arn = Lens.lens (\User' {arn} -> arn) (\s@User' {} a -> s {arn = a} :: User)

-- | The ID of the user.
user_userId :: Lens.Lens' User (Core.Maybe Core.Text)
user_userId = Lens.lens (\User' {userId} -> userId) (\s@User' {} a -> s {userId = a} :: User)

-- | The current supported value is Redis.
user_engine :: Lens.Lens' User (Core.Maybe Core.Text)
user_engine = Lens.lens (\User' {engine} -> engine) (\s@User' {} a -> s {engine = a} :: User)

-- | The username of the user.
user_userName :: Lens.Lens' User (Core.Maybe Core.Text)
user_userName = Lens.lens (\User' {userName} -> userName) (\s@User' {} a -> s {userName = a} :: User)

instance Core.FromXML User where
  parseXML x =
    User'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "AccessString")
      Core.<*> ( x Core..@? "UserGroupIds" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Authentication")
      Core.<*> (x Core..@? "ARN")
      Core.<*> (x Core..@? "UserId")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "UserName")

instance Core.Hashable User

instance Core.NFData User
