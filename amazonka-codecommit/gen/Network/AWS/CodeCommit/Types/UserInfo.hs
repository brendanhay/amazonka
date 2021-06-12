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
-- Module      : Network.AWS.CodeCommit.Types.UserInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.UserInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the user who made a specified commit.
--
-- /See:/ 'newUserInfo' smart constructor.
data UserInfo = UserInfo'
  { -- | The name of the user who made the specified commit.
    name :: Core.Maybe Core.Text,
    -- | The date when the specified commit was commited, in timestamp format
    -- with GMT offset.
    date :: Core.Maybe Core.Text,
    -- | The email address associated with the user who made the commit, if any.
    email :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'userInfo_name' - The name of the user who made the specified commit.
--
-- 'date', 'userInfo_date' - The date when the specified commit was commited, in timestamp format
-- with GMT offset.
--
-- 'email', 'userInfo_email' - The email address associated with the user who made the commit, if any.
newUserInfo ::
  UserInfo
newUserInfo =
  UserInfo'
    { name = Core.Nothing,
      date = Core.Nothing,
      email = Core.Nothing
    }

-- | The name of the user who made the specified commit.
userInfo_name :: Lens.Lens' UserInfo (Core.Maybe Core.Text)
userInfo_name = Lens.lens (\UserInfo' {name} -> name) (\s@UserInfo' {} a -> s {name = a} :: UserInfo)

-- | The date when the specified commit was commited, in timestamp format
-- with GMT offset.
userInfo_date :: Lens.Lens' UserInfo (Core.Maybe Core.Text)
userInfo_date = Lens.lens (\UserInfo' {date} -> date) (\s@UserInfo' {} a -> s {date = a} :: UserInfo)

-- | The email address associated with the user who made the commit, if any.
userInfo_email :: Lens.Lens' UserInfo (Core.Maybe Core.Text)
userInfo_email = Lens.lens (\UserInfo' {email} -> email) (\s@UserInfo' {} a -> s {email = a} :: UserInfo)

instance Core.FromJSON UserInfo where
  parseJSON =
    Core.withObject
      "UserInfo"
      ( \x ->
          UserInfo'
            Core.<$> (x Core..:? "name")
            Core.<*> (x Core..:? "date")
            Core.<*> (x Core..:? "email")
      )

instance Core.Hashable UserInfo

instance Core.NFData UserInfo
