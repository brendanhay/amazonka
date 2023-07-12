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
-- Module      : Amazonka.Connect.Types.UserSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON UserSummary where
  parseJSON =
    Data.withObject
      "UserSummary"
      ( \x ->
          UserSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Username")
      )

instance Prelude.Hashable UserSummary where
  hashWithSalt _salt UserSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` username

instance Prelude.NFData UserSummary where
  rnf UserSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf username
