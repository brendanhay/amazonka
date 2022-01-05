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
-- Module      : Amazonka.CodeCommit.Types.UserInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.UserInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the user who made a specified commit.
--
-- /See:/ 'newUserInfo' smart constructor.
data UserInfo = UserInfo'
  { -- | The email address associated with the user who made the commit, if any.
    email :: Prelude.Maybe Prelude.Text,
    -- | The date when the specified commit was commited, in timestamp format
    -- with GMT offset.
    date :: Prelude.Maybe Prelude.Text,
    -- | The name of the user who made the specified commit.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'email', 'userInfo_email' - The email address associated with the user who made the commit, if any.
--
-- 'date', 'userInfo_date' - The date when the specified commit was commited, in timestamp format
-- with GMT offset.
--
-- 'name', 'userInfo_name' - The name of the user who made the specified commit.
newUserInfo ::
  UserInfo
newUserInfo =
  UserInfo'
    { email = Prelude.Nothing,
      date = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The email address associated with the user who made the commit, if any.
userInfo_email :: Lens.Lens' UserInfo (Prelude.Maybe Prelude.Text)
userInfo_email = Lens.lens (\UserInfo' {email} -> email) (\s@UserInfo' {} a -> s {email = a} :: UserInfo)

-- | The date when the specified commit was commited, in timestamp format
-- with GMT offset.
userInfo_date :: Lens.Lens' UserInfo (Prelude.Maybe Prelude.Text)
userInfo_date = Lens.lens (\UserInfo' {date} -> date) (\s@UserInfo' {} a -> s {date = a} :: UserInfo)

-- | The name of the user who made the specified commit.
userInfo_name :: Lens.Lens' UserInfo (Prelude.Maybe Prelude.Text)
userInfo_name = Lens.lens (\UserInfo' {name} -> name) (\s@UserInfo' {} a -> s {name = a} :: UserInfo)

instance Core.FromJSON UserInfo where
  parseJSON =
    Core.withObject
      "UserInfo"
      ( \x ->
          UserInfo'
            Prelude.<$> (x Core..:? "email")
            Prelude.<*> (x Core..:? "date")
            Prelude.<*> (x Core..:? "name")
      )

instance Prelude.Hashable UserInfo where
  hashWithSalt _salt UserInfo' {..} =
    _salt `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` name

instance Prelude.NFData UserInfo where
  rnf UserInfo' {..} =
    Prelude.rnf email
      `Prelude.seq` Prelude.rnf date
      `Prelude.seq` Prelude.rnf name
