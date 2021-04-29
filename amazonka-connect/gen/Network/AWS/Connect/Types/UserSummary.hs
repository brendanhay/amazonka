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
-- Module      : Network.AWS.Connect.Types.UserSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UserSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains summary information about a user.
--
-- /See:/ 'newUserSummary' smart constructor.
data UserSummary = UserSummary'
  { -- | The Amazon Resource Name (ARN) of the user account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the user account.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Connect user name of the user account.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'userSummary_arn' - The Amazon Resource Name (ARN) of the user account.
--
-- 'id', 'userSummary_id' - The identifier of the user account.
--
-- 'username', 'userSummary_username' - The Amazon Connect user name of the user account.
newUserSummary ::
  UserSummary
newUserSummary =
  UserSummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user account.
userSummary_arn :: Lens.Lens' UserSummary (Prelude.Maybe Prelude.Text)
userSummary_arn = Lens.lens (\UserSummary' {arn} -> arn) (\s@UserSummary' {} a -> s {arn = a} :: UserSummary)

-- | The identifier of the user account.
userSummary_id :: Lens.Lens' UserSummary (Prelude.Maybe Prelude.Text)
userSummary_id = Lens.lens (\UserSummary' {id} -> id) (\s@UserSummary' {} a -> s {id = a} :: UserSummary)

-- | The Amazon Connect user name of the user account.
userSummary_username :: Lens.Lens' UserSummary (Prelude.Maybe Prelude.Text)
userSummary_username = Lens.lens (\UserSummary' {username} -> username) (\s@UserSummary' {} a -> s {username = a} :: UserSummary)

instance Prelude.FromJSON UserSummary where
  parseJSON =
    Prelude.withObject
      "UserSummary"
      ( \x ->
          UserSummary'
            Prelude.<$> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Username")
      )

instance Prelude.Hashable UserSummary

instance Prelude.NFData UserSummary
