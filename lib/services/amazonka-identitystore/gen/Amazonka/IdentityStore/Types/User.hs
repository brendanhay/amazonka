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
-- Module      : Amazonka.IdentityStore.Types.User
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A user object, which contains a specified user’s metadata and
-- attributes.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | Contains the user’s user name value. The length limit is 128 characters.
    -- This value can consist of letters, accented characters, symbols,
    -- numbers, and punctuation. The characters @\<>;:%@ are excluded. This
    -- value is specified at the time the user is created and stored as an
    -- attribute of the user object in the identity store.
    userName :: Core.Sensitive Prelude.Text,
    -- | The identifier for a user in the identity store.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'user_userName' - Contains the user’s user name value. The length limit is 128 characters.
-- This value can consist of letters, accented characters, symbols,
-- numbers, and punctuation. The characters @\<>;:%@ are excluded. This
-- value is specified at the time the user is created and stored as an
-- attribute of the user object in the identity store.
--
-- 'userId', 'user_userId' - The identifier for a user in the identity store.
newUser ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  User
newUser pUserName_ pUserId_ =
  User'
    { userName = Core._Sensitive Lens.# pUserName_,
      userId = pUserId_
    }

-- | Contains the user’s user name value. The length limit is 128 characters.
-- This value can consist of letters, accented characters, symbols,
-- numbers, and punctuation. The characters @\<>;:%@ are excluded. This
-- value is specified at the time the user is created and stored as an
-- attribute of the user object in the identity store.
user_userName :: Lens.Lens' User Prelude.Text
user_userName = Lens.lens (\User' {userName} -> userName) (\s@User' {} a -> s {userName = a} :: User) Prelude.. Core._Sensitive

-- | The identifier for a user in the identity store.
user_userId :: Lens.Lens' User Prelude.Text
user_userId = Lens.lens (\User' {userId} -> userId) (\s@User' {} a -> s {userId = a} :: User)

instance Core.FromJSON User where
  parseJSON =
    Core.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Core..: "UserName")
            Prelude.<*> (x Core..: "UserId")
      )

instance Prelude.Hashable User where
  hashWithSalt salt' User' {..} =
    salt' `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` userName

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf userId
