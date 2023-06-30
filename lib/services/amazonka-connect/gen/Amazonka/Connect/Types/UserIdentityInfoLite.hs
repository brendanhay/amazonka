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
-- Module      : Amazonka.Connect.Types.UserIdentityInfoLite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserIdentityInfoLite where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The user\'s first name and last name.
--
-- /See:/ 'newUserIdentityInfoLite' smart constructor.
data UserIdentityInfoLite = UserIdentityInfoLite'
  { -- | The user\'s first name.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The user\'s last name.
    lastName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserIdentityInfoLite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firstName', 'userIdentityInfoLite_firstName' - The user\'s first name.
--
-- 'lastName', 'userIdentityInfoLite_lastName' - The user\'s last name.
newUserIdentityInfoLite ::
  UserIdentityInfoLite
newUserIdentityInfoLite =
  UserIdentityInfoLite'
    { firstName = Prelude.Nothing,
      lastName = Prelude.Nothing
    }

-- | The user\'s first name.
userIdentityInfoLite_firstName :: Lens.Lens' UserIdentityInfoLite (Prelude.Maybe Prelude.Text)
userIdentityInfoLite_firstName = Lens.lens (\UserIdentityInfoLite' {firstName} -> firstName) (\s@UserIdentityInfoLite' {} a -> s {firstName = a} :: UserIdentityInfoLite)

-- | The user\'s last name.
userIdentityInfoLite_lastName :: Lens.Lens' UserIdentityInfoLite (Prelude.Maybe Prelude.Text)
userIdentityInfoLite_lastName = Lens.lens (\UserIdentityInfoLite' {lastName} -> lastName) (\s@UserIdentityInfoLite' {} a -> s {lastName = a} :: UserIdentityInfoLite)

instance Data.FromJSON UserIdentityInfoLite where
  parseJSON =
    Data.withObject
      "UserIdentityInfoLite"
      ( \x ->
          UserIdentityInfoLite'
            Prelude.<$> (x Data..:? "FirstName")
            Prelude.<*> (x Data..:? "LastName")
      )

instance Prelude.Hashable UserIdentityInfoLite where
  hashWithSalt _salt UserIdentityInfoLite' {..} =
    _salt
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` lastName

instance Prelude.NFData UserIdentityInfoLite where
  rnf UserIdentityInfoLite' {..} =
    Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf lastName
